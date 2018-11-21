#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

packages = c('shiny','tidyverse','sweep', 'DT')

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

# summarising quantity
household_commodity = transaction_with_commodity_desc%>%
  select(`household_key`,`SUB_COMMODITY_DESC`,`QUANTITY`)%>%
  group_by(`household_key`,`SUB_COMMODITY_DESC`)%>%
  summarise(total_quantity = sum(`QUANTITY`))%>%
  ungroup()

rm(transaction_with_commodity_desc)

# creating index for representation of the sub com index
commodity_index =  household_commodity %>%
  group_by(`SUB_COMMODITY_DESC`)%>%
  summarize(num = n())%>%
  select(`SUB_COMMODITY_DESC`)%>%
  mutate(`commodity_index` = as.character(row_number()),`SUB_COMMODITY_DESC`=tolower(`SUB_COMMODITY_DESC`))

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
  
  observeEvent (input$proceed, {
    if(input$userID %in% rownames(matrix_final)) {
      current_user <- input$userID
      
      similarities <-
        cor(t(matrix_final[rownames(matrix_final) != current_user, ]), t(matrix_final[current_user, ]), use = 'pairwise.complete.obs')
      
      sim <- as.vector(similarities)
      names(sim) <- rownames(similarities)
      sim <- sort(sim, decreasing = TRUE)
      
      
      similar_users <- names(sim[1:(as.numeric(input$userSim))])
      selected_sim <- sim[1:(as.numeric(input$userSim))]
      
      similar_users_ratings <-
        data.frame(item = rep(colnames(matrix_final), length(similar_users)), rating = c(t(as.data.frame(customer_commodity_index_matrix[similar_users,])))) %>% filter(!is.na(rating))
      
      current_user_ratings <-
        data.frame(item = colnames(matrix_final), rating = c(t(customer_commodity_index_matrix[current_user,]))) %>% filter(!is.na(rating))
      
      # find the items that are not yet rated by current user but yet rated by the similar user.
      predictionTemp <-
        similar_users_ratings %>%
        filter(!(item %in% current_user_ratings$item)) %>%
        group_by(item) %>% summarise(mean_rating = weighted_mean(rating, selected_sim), count = n())
        
      
      predictions$data <- predictionTemp %>%
        filter(count == length(similar_users)) %>% select(-count) %>%
        arrange(-mean_rating) %>%
        top_n(5, wt=mean_rating) %>%
        inner_join(commodity_index, by=c("item"= "commodity_index"))
      
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
  
  output$errorMessage <- renderText({
    message$data
  })
  
})
