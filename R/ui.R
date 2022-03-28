###############################
### userinterface dashboard ###
###############################
# source: https://shiny.rstudio.com/
# source: https://rstudio.github.io/shinydashboard/

#########
# setup #
#########
library(shiny)
library(shinydashboard)

###
dashboardPage(
    dashboardHeader(title = "Empty App"),
    dashboardSidebar(
        
        sidebarMenu(#id = "sidebar",
            menuItem("Item 1", tabName = "item_1", icon = icon("birthday-cake")),
            menuItem("Item 2", tabName = "item_2", icon = icon("birthday-cake"))
            )
        ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "item_1",
                h2("Dashboard 1"),
                
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
                
            tabItem(tabName = "item_2",
                    
                    h2("Dashboard 2"),
                    
                    fluidPage(
                        
                        fluidRow(
                            box("Tabelle",
                                dataTableOutput("tab"),
                                width = 12)
                            )
                        )
                    )
            )
        )
    )
