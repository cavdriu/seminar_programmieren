########################
### server dashboard ###
########################
# source: https://shiny.rstudio.com/
# source: https://rstudio.github.io/shinydashboard/


#########
# setup #
#########
library(shiny)

###
shinyServer(function(input, output){
    
    output$dist <- renderPlot({
        
        hist(
            rnorm(input$nobs,
                  mean = input$mean_in,
                  sd = input$sd_in),
            main = "",
            xlab = ""
        )
        
    })
    
    output$tab <- renderDataTable({
        mtcars
        
    })
    
})
