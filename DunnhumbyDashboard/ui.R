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
  navbarPage(
    title = "Airbnb Version 2",
    tabPanel("Recommendation System",
             tabPanel("Need Suggestion?",
                      
                      fluidRow(column(4),
                               column(
                                 6,
                                 textInput(inputId = "userID", label = "Insert Your ID Here"),
                                 fluidRow(column(
                                   3, actionButton(inputId = "proceed", label = "Recommend Me!")
                                 ),
                                 column(6, textOutput("errorMessage")),
                                 column(3))
                               ),
                               column(2)),
                      tags$hr(),
                      fluidRow(
                        column(1),
                        column(10,
                               dataTableOutput("recommenderTable")
                        ),
                        column(1)
                      )
             )
    )
    
  )
))
