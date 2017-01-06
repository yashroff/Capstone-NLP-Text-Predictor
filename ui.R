#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction Shiny App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("sentence","input phrase:",value="")
    ),
    
    # Show a table of top predictions
    mainPanel(
      h3("Predicted value:"),
      #textOutput("pred1")
      dataTableOutput("pred1")
    )
  )
))
