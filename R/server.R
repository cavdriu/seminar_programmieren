########################
### server dashboard ###
########################
# source: https://shiny.rstudio.com/
# source: https://rstudio.github.io/shinydashboard/


# setup -------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)


# server ------------------------------------------------------------------

shinyServer(function(input, output){
    
    # inputs selection
    # ...
    
    output$gender_ratio <- renderPlot({
        
        plot_gender_overview_ratio(
            data_socsec_gender, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$gender_absolut <- renderPlot({
        
        plot_gender_overview_absolute(
            data_socsec_gender, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$gender_zoomin <- renderPlot({
        
        plot_gender_zooming(
            data_socsec_gender, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$dist <- renderPlot({
        
        hist(
            rnorm(input$nobs,
                  mean = input$mean_in,
                  sd = input$sd_in),
            main = "",
            xlab = ""
        )
        
    })
    
    # output$gender_absolute
    # 
    # output$gender_zooming
    
    output$tab <- renderDataTable({
        mtcars
        
    })
    
})
