#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

packages = c('shiny','tidyverse','sweep', 'DT', 'qgraph', 'shinythemes')

for(p in packages){library
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

rm(packages, p)

######## The code for data processing ############################
transactions = read_csv("data/transaction_data.csv")
product = read_csv("data/product.csv")

# removing dirty commodity description and joining the two tables
transaction_with_commodity_desc = product%>%
  select(`PRODUCT_ID`,`SUB_COMMODITY_DESC`)%>%
  filter(!is.na(`SUB_COMMODITY_DESC`))%>%
  mutate(`SUB_COMMODITY_DESC`=tolower(`SUB_COMMODITY_DESC`)) %>%
  
  inner_join(transactions, by = "PRODUCT_ID")

# grouping quantity based on household_key and sub_commodity
household_commodity = transaction_with_commodity_desc%>%
  select(`household_key`,`SUB_COMMODITY_DESC`,`QUANTITY`)%>%
  group_by(`household_key`,`SUB_COMMODITY_DESC`)%>%
  summarise(total_quantity = sum(`QUANTITY`))%>%
  ungroup()


# creating index for representation of the sub com index
commodity_index =  household_commodity %>%
  group_by(`SUB_COMMODITY_DESC`)%>%
  summarize(num = n())%>%
  select(`SUB_COMMODITY_DESC`)%>%
  mutate(`commodity_index` = as.character(row_number()),`SUB_COMMODITY_DESC`=tolower(`SUB_COMMODITY_DESC`))


# creating the utility matrix
household_commodity_index = inner_join(household_commodity,commodity_index,by = "SUB_COMMODITY_DESC")%>%
  select(`household_key`,`total_quantity`,`commodity_index`)

customer_commodity_index_matrix = household_commodity_index%>%
  spread(key = `commodity_index`,value = `total_quantity`) %>%
  select(-household_key)

rm(household_commodity_index)

columnMeans = colMeans(customer_commodity_index_matrix, na.rm = T)

matrix_final <- sweep(customer_commodity_index_matrix,2,columnMeans,"/")
matrix_final[is.na(matrix_final)] = 0


## function for calculating weighted mean
weighted_mean <- function(rating, sims) {
  (sum(rating * sims) / sum(sims))
}



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
        data.frame(item = rep(colnames(matrix_final), length(similar_users)), rating = c(t(as.data.frame(customer_commodity_index_matrix[similar_users,])))) %>% filter(!is.na(rating))
      
      current_user_ratings <-
        data.frame(item = colnames(matrix_final), rating = c(t(customer_commodity_index_matrix[current_user,]))) %>% filter(!is.na(rating))
      
      # find the items that are not yet rated by current user but yet rated by the similar user.
      predictionTemp <-
        similar_users_ratings %>%
        filter(!(item %in% current_user_ratings$item)) %>%
        group_by(item) %>% summarise(`Predicted Quantity` = weighted_mean(rating, selected_sim), count = n())
        
      
      predictions$data <- predictionTemp %>%
        filter(count == length(similar_users)) %>% select(-count) %>%
        arrange(-`Predicted Quantity`) %>%
        top_n(as.numeric(input$noOfRecommendation), wt=`Predicted Quantity`) %>%
        inner_join(commodity_index, by=c("item"= "commodity_index")) %>% select(SUB_COMMODITY_DESC, `Predicted Quantity`)
      
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
            server = FALSE,
            autoWidth = TRUE
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
            server = FALSE,
            autoWidth = TRUE
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
            server = FALSE,
            autoWidth = TRUE
          )
        )
    }
  })
  
  output$networkLabel <- renderText({
    if(currentUser$id !=0 )
      "Relationship of Inputted User with 7 Other Users"
  })
  
  output$networkGraph <- renderPlot({
    if(nrow(predictions$data) > 0) {
      message$data <- ""
      current_user <- currentUser$id
      random_users <- rownames(matrix_final)[sample(1:nrow(matrix_final), 7)]
      
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
      text <- "This recommendation is based on users"
      for (simUser in similarUsers$data)
        text <- paste(text, simUser)
      text
      
    }
  })
  
})
