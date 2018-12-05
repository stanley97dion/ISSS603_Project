#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#packages = c("shiny","sweep", "DT", "qgraph", "shinythemes") #"tidyverse"

# for(p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
#   eval(parse(text = paste0('library(', p, ')'))[1])
# }
# rm(packages, p)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  predictions <- reactiveValues(data = data.frame())
  message <- reactiveValues(data = "")
  similarUsers <- reactiveValues(data = list())
  currentUser <- reactiveValues(id = 0)
  
  top_n_item <- reactiveValues(num = 0)
  
  observeEvent (input$proceed, {
    if(input$userID %in% rownames(matrix_final)) {
      message$data <- ""
      top_n_item$num <- input$noOfRecommendation
      
      current_user <- input$userID
      currentUser$id <- input$userID
      
      similarities <-
        cor(t(matrix_final[rownames(matrix_final) != current_user, ]), t(matrix_final[current_user, ]), use = 'pairwise.complete.obs')
      
      sim <- as.vector(similarities)
      names(sim) <- rownames(similarities)
      sim <- sort(sim, decreasing = TRUE)
      
      
      similar_users <- names(sim[1:(as.numeric(input$userSim))])
      similarUsers$data <- similar_users
      selected_sim <- sim[1:(as.numeric(input$userSim))]
      
      similar_users_ratings <-
        data_frame(item = rep(colnames(matrix_final), length(similar_users)), 
                   rating = c(t(as.data.frame(matrix_final[similar_users,]))),
                   `Buy Quantity` = c(t(as.data.frame(customer_commodity_index_matrix[similar_users,])))) %>% filter(!is.na(rating)&!is.na(`Buy Quantity`))
      
      current_user_ratings <-
        data.frame(item = colnames(matrix_final), 
                   rating = c(t(customer_commodity_index_matrix[current_user,]))) %>% filter(!is.na(rating))
      
      # find the items that are not yet rated by current user but yet rated by the similar user.
      predictionTemp <-
        similar_users_ratings %>%
        filter(!(item %in% current_user_ratings$item)) %>%
        group_by(item) %>% summarise(`Predicted Quantity` = weighted_mean(`Buy Quantity`, selected_sim), 
                                     `Predicted Rating` = weighted_mean(rating, selected_sim), count = n())
        
      
      predictions$data <- predictionTemp %>%
        filter(count == length(similar_users)) %>% select(-count) %>%
        arrange(-`Predicted Rating`) %>% 
        top_n(as.numeric(input$noOfRecommendation), wt=`Predicted Rating`) %>%
        inner_join(commodity_index, by=c("item"= "commodity_index")) %>% select(SUB_COMMODITY_DESC, `Predicted Quantity`, `Predicted Rating`)
      
    } 
    else
      message$data = "ID Not Found"
    }  )
  
 
  output$recommenderTable <- renderDataTable({
    
    if(nrow(predictions$data) > 0) {
      predictions$data %>%
        datatable(
          class = "nowrap hover row-border",
          escape = FALSE,
          options = list(
            dom = 't',
            scrollX = TRUE,
            server = FALSE
          )
        )
    }
  })
  
  output$userHistoricalLabel <- renderText({
    if(currentUser$id !=0 )
      "User's Purchase Statistics"
  })
  
  output$userHistoricalTable <- renderDataTable({
    if(currentUser$id !=0 ) {
      household_commodity %>%
        filter(household_key == currentUser$id) %>%
        select(-household_key) %>%
        arrange(-total_quantity) %>%
        top_n(as.numeric(top_n_item$num), wt=total_quantity) %>%
        
        datatable(
          class = "nowrap hover row-border",
          escape = FALSE,
          options = list(
            dom = 't',
            scrollX = TRUE,
            server = FALSE
          )
        )
    }
  })
  
  output$simUserHistoricalLabel <- renderText({
    if(currentUser$id !=0 )
      paste("User's", similarUsers$data[1], "Purchase Statistics")
  })
  
  output$simHistoricalTable <- renderDataTable({
    if(currentUser$id !=0 ) {
      household_commodity %>%
        filter(household_key == as.numeric(similarUsers$data[1])) %>%
        select(-household_key) %>%
        arrange(-total_quantity) %>%
        top_n(as.numeric(top_n_item$num), wt=total_quantity) %>%
        
        datatable(
          class = "nowrap hover row-border",
          escape = FALSE,
          options = list(
            dom = 't',
            scrollX = TRUE,
            server = FALSE
          )
        )
    }
  })
  
  output$networkLabel <- renderText({
    if(currentUser$id !=0 )
      "Relationship of Inputted User with 10 Other Users"
  })
  
  output$networkGraph <- renderPlot({
    if(nrow(predictions$data) > 0) {
      message$data <- ""
      current_user <- currentUser$id
      random_users <- rownames(matrix_final)[sample(1:nrow(matrix_final), 10)]
      
      sim_mat <-
        cor(t(matrix_final[c(current_user, random_users),]), use = 'pairwise.complete.obs')
      

      qgraph(
        sim_mat,
        layout = "spring",
        vsize = 6.5,
        theme = "TeamFortress",
        labels = c(current_user, random_users)
      )
    }
  })

  
  
  output$errorMessage <- renderText({
    message$data
  })
  
  output$simUsersVerbose <- renderText({
    
    if(length(similarUsers$data) > 0) {
      text <- "This recommendation is based on users: "
      for (simUser in similarUsers$data)
        text <- paste(text, paste0(simUser, ", "))
      substr(text, 1, nchar(text)-2)
      
    }
  })
  
})
