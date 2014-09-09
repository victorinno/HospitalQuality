library(shiny)
source("rankall.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Return the requested dataset
    datasetInput <- reactive({
        switch(input$dataset,
               "best" = rankall(input$disease),
               "worst" = rankall(input$disease,"worst")
               )
    })
    
    # Show the first "n" observations
    output$table <- renderDataTable({
        datasetInput()
    })
    
 
})

