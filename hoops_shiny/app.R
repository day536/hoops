library(shiny)
library(reactable)
library(tidyverse)


#source("shiny_prep.R")
source("https://raw.githubusercontent.com/day536/hoops/main/shiny_prep.R")

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #add style elements
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback",
                  rel = "stylesheet"),
        tags$style(
            HTML("
                .standings {
                    font-family: Karla, 'Helvetica Neue', Helvetica, Arial, sans-serif;
                    font-size: 14px;
                }
                
                .header:hover,
                .header[aria-sort='ascending'],
                .header[aria-sort='descending'] {
                  background-color: #eee;
                }
                
                .image-cropper {
                    width: 60px;
                    height: 60px;
                    position: relative;
                    overflow: hidden;
                    border-radius: 50%;
                }
                
                .profile-pic {
                    display: inline;
                    margin: 0 auto;
                    margin-left: -25%;
                    height: 100%;
                    width: auto;
                }
                
                
            ")
        )
    ),
    
    # Application title
    titlePanel("NCAA Player Data"),
    
    #tags$img(src = "player_headshot_images/00-0034170.png"),
    
    tabsetPanel(type = "pills",
                tabPanel("Player Impact Stats",
                         div(class = "standings",
                             div(class = "title",
                                 #h2("Collegiate CTG"),
                                 "My app for evaluating collegiate basketball players"),
                             reactable::reactableOutput("player_impact_table"),
                             br(),
                             "Data scraped from basketball-reference.com"
                             )
                         ),
                tabPanel("Four Factors",
                         div(class = "standings",
                             div(class = "title",
                                # h2("Collegiate CTG"),
                                 "My app for evaluating collegiate basketball players"),
                             reactable::reactableOutput("four_factors_table"),
                             br(),
                             "Data scraped from basketball-reference.com"
                         )
                )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    player_profile_cols <- colnames(curated_player_data)[1:5]
    usgpercent_cols <- colnames(curated_player_data %>% select(contains("usgpercent")))
    ast_usagepercent_cols <- colnames(curated_player_data %>% select(contains("ast_usagepercent")))
    tspercent_cols <- colnames(curated_player_data %>% select(contains("tspercent")))
    astpercent_cols <- colnames(curated_player_data %>% select(contains("astpercent")))
    efgpercent_cols <- colnames(curated_player_data %>% select(contains("efgpercent")))
    orbpercent_cols <- colnames(curated_player_data %>% select(contains("orbpercent")))
    drbpercent_cols <- colnames(curated_player_data %>% select(contains("drbpercent")))
    blkpercent_cols <- colnames(curated_player_data %>% select(contains("blkpercent")))
    tovpercent_cols <- colnames(curated_player_data %>% select(contains("tovpercent")))
    rating_cols <- c("usgpercent")
    playoff_cols <- c("tspercent")
    
    #player_profile_column <- function(maxWidth = 100, ...){
    #    colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
    #}
    
    current_standings_column <- function(maxWidth = 65, ...){
        colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
    }
    
    rating_column <- function(maxWidth = 100, class = NULL, ...){
        colDef(class = "cell number", maxWidth = maxWidth, align = "center", ...)
    }
    
    playoff_column <- function(maxWidth = 45, class = NULL, ...){
        colDef(
            cell = format_pct,
            maxWidth = maxWidth,
            class = paste("cell number", class),
            align = "center",
            style = function(value){
                if(value < 0.01){
                    list(color = "#aaa")
                } else {
                    list(color = "#111", background = knockout_pct_color(value))
                }
            }, ...
        )
    }
    
    format_pct <- function(value){
        if (value == 0)"  \u2013 "
        #else if (value == 1) "\u2713"
        #else if (value <= .01) "<1%"
        #else if (value >= .99) ">99%"
        else formatC(paste0(round(value * 100)), width = 4)
    }
    
    make_color_pal <- function(colors, bias = 1){
        get_color <- colorRamp(colors = colors, bias = bias)
        function(x) rgb(get_color(x), maxColorValue = 255)
    }
    
    knockout_pct_color <- make_color_pal(c("#3b8dff", "#ffffff", "#ffa229"),
                                         bias = 1)
    strength_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
    
    player_impact_tbl <- reactable(
        player_impact_data,
        pagination = FALSE,
        filterable = T,
        #defaultSorted = c("tspercent"),
        defaultSortOrder = "desc",
        defaultColDef = colDef(class = "cell", headerClass = "header"),
        columnGroups = list(
            #colGroup(name = "Current Standings", columns = player_profile_cols),
            colGroup(name = "Usage %", columns = usgpercent_cols),
            colGroup(name = "TS %", columns = tspercent_cols),
            colGroup(name = "AST %", columns = astpercent_cols),
            colGroup(name = "AST:USG %", columns = ast_usagepercent_cols),
            colGroup(name = "TOV %", columns = tovpercent_cols)
        ),
        columns = list(
            player = colDef(name = "Player", maxWidth = 200),
            class = colDef(name = "Class", maxWidth = 60),
            season = colDef(name = "Season", maxWidth = 100),
            pos = colDef(name = "Pos.", maxWidth = 60),
            school = colDef(name = "School", maxWidth = 150),
            g = colDef(name = "G", maxWidth = 50),
            mp = colDef(name = "MP", maxWidth = 50),
            #g = current_standings_column(name = "Games",
            #                                      format = colFormat(digits = 0)),
            #
            #mp = current_standings_column(name = "Mins Played",
            #                                      format = colFormat(digits = 0)),
            
            usgpercent = current_standings_column(name = "#",
                                            format = colFormat(digits = 1)),
            usgpercent_percentile = playoff_column(name = "%",
                                                  format = colFormat(digits = 0)),
            tspercent = current_standings_column(name = "#",
                                              format = colFormat(digits = 2)),
            tspercent_percentile = playoff_column(name = "%",
                                       format = colFormat(digits = 0)),
            astpercent = current_standings_column(name = "#",
                                                 format = colFormat(digits = 1)),
            astpercent_percentile = playoff_column(name = "%",
                                                  format = colFormat(digits = 0)),
            ast_usagepercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 2)),
            ast_usagepercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0)),
            tovpercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 1)),
            tovpercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0))
        )
    )
    
    
    four_factors_tbl <- reactable(
        four_factors_data,
        pagination = FALSE,
        filterable = T,
        #defaultSorted = c("player"),
        defaultSortOrder = "desc",
        defaultColDef = colDef(class = "cell", headerClass = "header"),
        columnGroups = list(
            colGroup(name = "eFG %", columns = efgpercent_cols),
            colGroup(name = "ORB %", columns = orbpercent_cols),
            colGroup(name = "DRB %", columns = drbpercent_cols),
            colGroup(name = "BLK %", columns = blkpercent_cols),
            colGroup(name = "TOV %", columns = tovpercent_cols)
        ),
        columns = list(
            player = colDef(name = "Player", maxWidth = 200),
            class = colDef(name = "Class", maxWidth = 60),
            season = colDef(name = "Season", maxWidth = 100),
            pos = colDef(name = "Pos.", maxWidth = 60),
            school = colDef(name = "School", maxWidth = 150),
            g = colDef(name = "G", maxWidth = 50),
            mp = colDef(name = "MP", maxWidth = 50),
            efgpercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 2)),
            efgpercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0)),
            orbpercent = current_standings_column(name = "#",
                                                 format = colFormat(digits = 2)),
            orbpercent_percentile = playoff_column(name = "%",
                                                  format = colFormat(digits = 0)),
            drbpercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 1)),
            drbpercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0)),
            blkpercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 1)),
            blkpercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0)),
            tovpercent = current_standings_column(name = "#",
                                                  format = colFormat(digits = 1)),
            tovpercent_percentile = playoff_column(name = "%",
                                                   format = colFormat(digits = 0))
        )
    )
    
    output$player_impact_table <- reactable::renderReactable({
        player_impact_tbl
    })
    
    output$four_factors_table <- reactable::renderReactable({
        four_factors_tbl
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
