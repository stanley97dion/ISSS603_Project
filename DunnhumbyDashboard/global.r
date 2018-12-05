library(shiny)
library(tidyverse)
library(sweep)
library(DT)
library(qgraph)
library(shinythemes)
library(rcmdcheck)

######## The code for data processing ############################
load(file = 'data/transaction_data.RData')
load(file = 'data/product.RData')

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