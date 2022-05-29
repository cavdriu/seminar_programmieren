###############################
### userinterface dashboard ###
###############################
# source: https://shiny.rstudio.com/
# source: https://rstudio.github.io/shinydashboard/


# setup -------------------------------------------------------------------

library(shiny)
library(shinydashboard)

# ui ----------------------------------------------------------------------

header <- dashboardHeader(title = "Sozialversicherungen Schweiz")

# inputs (im server integrieren?)
service <- c("Total", "ALV", "IV", "SH")
gender <- c("f", "m")
age <- c("18-24", "25-39", "40-54", "55-65")

sidebar <- dashboardSidebar(
        
        sidebarMenu(id = "sidebar",
                    
            # menu            
            menuItem("Übersicht", tabName = "overview", icon = icon("overview")),
            menuItem("Geschlecht", tabName = "gender", icon = icon("gender")),
            menuItem("Bevölkerungsgruppe", tabName = "pop", icon = icon("population")),
            menuItem("Alter", tabName = "age", icon = icon("age")),
            
            #inputs
            selectInput("service", "Auwahl der Sozialversicherung", service),
            numericInput("start", "Von", value = 2010, min = 2010, max = 2019),
            numericInput("end", "Bis", value = 2019, min = 2010, max = 2019)
            
            )
        )

# overview ----------------------------------------------------------------

body <- dashboardBody(
        
        tabItems(
            tabItem(tabName = "overview",
                h2("Übersicht der Sozialversicherungen"),
                
                fluidPage(
                    
                    fluidRow(
                        box(title = "Configuration",
                            sliderInput("nobs",
                                        "Number of Observations",
                                        min = 100,
                                        max = 10000,
                                        value = 500),
                            sliderInput("mean_in", "Mean",
                                        min = 0,
                                        max = 10,
                                        value = 0),
                            sliderInput("sd_in", "SD",
                                        min = 1,
                                        max = 5,
                                        value =1),
                            width = 4),
                        
                        box(title = "Distribution",
                            plotOutput("dist"),
                            width = 8)
                        )
                    )
                ),

# gender ------------------------------------------------------------------

            tabItem(tabName = "gender",
                    
                    h2("Sozialversicherungen nach Geschlecht"),
                    
                    fluidPage(
                        
                        fluidRow(
                            # box(title = NULL,
                            #     checkboxGroupInput("gender", "Auswahl Geschlecht", gender)),
                            
                            box(title = "Überblick Verhältniss",
                                plotOutput("gender_ratio"),
                                width = 4),
                            
                            box(title = "Überblick Absolut",
                                plotOutput("gender_absolut"),
                                width = 4),
                            
                            box(title = "Zoom-In",
                                plotOutput("gender_zoomin"),
                                width = 4)
                            
                            ),
                        
                        fluidRow(
                            
                            box(title = "Daten",
                                dataTableOutput("gender_table"),
                                width = 12)
                            
                            )
                        )
                    ),

# population --------------------------------------------------------------

tabItem(tabName = "pop",
        
        h2("Sozialversicherungen nach Bevölkerungsgruppe"),

        fluidPage(
                
                fluidRow(

                        box(title = "Überblick Verhältniss",
                            plotOutput("pop_ratio"),
                            width = 4),
                        
                        box(title = "Überblick Absolut",
                            plotOutput("pop_absolut"),
                            width = 4),
                        
                        box(title = "Zoom-In",
                            plotOutput("pop_zoomin"),
                            width = 4)
                        
                ),
                
                fluidRow(
                        
                        box(title = "Daten",
                            dataTableOutput("pop_table"),
                            width = 12)
                        
                )
        )
),

# age -------------------------------------------------------------------------

tabItem(tabName = "age",
        
        h2("Sozialversicherungen nach Altersgruppe"),
        
        fluidPage(
                
                fluidRow(
                        
                        box(checkboxGroupInput("age", "Altersgruppe", age),
                            width = 1),
                        
                        box(title = "Überblick Verhältniss",
                            plotOutput("age_ratio"),
                            width = 3),
                        
                        box(title = "Überblick Absolut",
                            plotOutput("age_absolut"),
                            width = 3),
                        
                        box(title = "Zoom-In",
                            plotOutput("age_zoomin"),
                            width = 5),
                        
                ),
                
                fluidRow(

                        box(title = "Daten",
                            dataTableOutput("age_table"),
                            width = 12)
                        
                )

        )
)

))


# run app ---------------------------------------------------------------------

dashboardPage(header, sidebar, body)