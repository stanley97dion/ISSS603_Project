#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  navbarPage(
    title = "Dunnhumby Recommendation System",
    
    theme = shinytheme("cosmo"),
    
    tabPanel("System Overview",
             
             fluidRow(column(
               5,
               
               fluidRow(column(8,
                               textInput(inputId = "userID", label = "Insert Customer ID Here")),
                        column(2, textOutput("errorMessage"))), 
               
               fluidRow(column(12,
                          radioButtons(
                            inputId = "userSim",
                            label = "Select Number of Similar User",
                            choices = 2:5,
                            inline = T
                          )
                        ) ),
               
               fluidRow(column(12,
                               selectInput(
                                 inputId = "noOfRecommendation",
                                 label = "No of Recommendation Needed",
                                 choices = 5:10,
                                 selected = 5
                               )
               ) ),
               
               fluidRow(column(12,
                               actionButton(inputId = "proceed", label = "Recommend!")
                               ))
               
             ),
             
             column(7, textOutput("networkLabel"),plotOutput("networkGraph"))), 
                      
             
             tags$hr(),
             fluidRow(column(5, 
                             
                             fluidRow(column(12, textOutput("simUsersVerbose"))),
                             
                             dataTableOutput("recommenderTable")
                             ),
                      
                      column(6,
                             fluidRow(
                               column(1),
                               column(6,
                                      
                                      fluidRow(column(12, textOutput("userHistoricalLabel"))),
                                      dataTableOutput("userHistoricalTable")),
                               
                               column(5,
                                      fluidRow(column(12, textOutput("simUserHistoricalLabel"))),
                                      dataTableOutput("simHistoricalTable")
                                      )
                             )
                      )
             )
             
    )
    
  )
))
