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
    title = "Dunnhumby Dashboard",
    
    tabPanel("Recommendation System",
             tabPanel("Need Suggestion?",
                      
                      fluidRow(column(4),
                               column(3,
                                 textInput(inputId = "userID", label = "Insert Your ID Here")
                               ),
                               column(2, textOutput("errorMessage"))),
                      fluidRow(column(3),
                      column(4, radioButtons(inputId = "userSim", label = "Select Number of Similar User",
                                             choices = 2:5, inline=T)),
                      column(3, actionButton(inputId = "proceed", label = "Recommend Me!")),
                      column(2)),
                      tags$hr(),
                      fluidRow(
                        column(1),
                        column(5,
                               dataTableOutput("historicalTable")),
                        column(5,
                               dataTableOutput("recommenderTable")
                        ),
                        column(1)
                      )
             )
    )
    
  )
))
