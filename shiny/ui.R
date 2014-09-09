library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Dados"),
    sidebarPanel(
        selectInput("dataset", "Choose a dataset:", 
                    choices = c("best", "worst")) ,
        selectInput("disease", "Choose a disease:", 
                    choices = c("heart attack","heart failure", "pneumonia")) ,
        submitButton("Update View")
    ),
    mainPanel(
        dataTableOutput(outputId="table")
    )   
))