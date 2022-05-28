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
    
# output gender -----------------------------------------------------------

    
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
        
        plot_gender_zoomin(
            data_socsec_gender, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$gender_table <- renderDataTable({
        
        data_table_gender(
            input$start,
            input$end,
            input$service
            )
        
    })
    
# output population -------------------------------------------------------

    output$pop_ratio <- renderPlot({
        
        plot_pop_overview_ratio(
            data_socsec_pop, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$pop_absolut <- renderPlot({
        
        plot_pop_overview_absolute(
            data_socsec_pop, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$pop_zoomin <- renderPlot({
        
        plot_pop_zoomin(
            data_socsec_pop, 
            input$start,
            input$end,
            input$service
        )
        
    })
    
    output$pop_table <- renderDataTable({
        
        data_table_pop(
            input$start,
            input$end,
            input$service
        )
        
    })


# output age --------------------------------------------------------------

    # output$dist <- renderPlot({
    #     
    #     hist(
    #         rnorm(input$nobs,
    #               mean = input$mean_in,
    #               sd = input$sd_in),
    #         main = "",
    #         xlab = ""
    #     )
    #     
    # })

})
