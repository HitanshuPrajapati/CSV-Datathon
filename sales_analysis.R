library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/THE NEW DATASET.csv")
library(data.table)
library(ggplot2)
library(plotly)


# sales data in 2017
sales_data_2017 <- sales_data_2017_2018 %>% 
  filter(year == 2017)
# View(sales_data_2017)

# sales data in 2018
sales_data_2018 <- sales_data_2017_2018 %>% 
  filter(year == 2018)

# View(sales_data_2018)

# items data (information distinct to each item)
items_data <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  select(item_code, item_name, main_category, sub_category, unit_buying_price,
         unit_selling_price, unit_price_margin) %>% 
  distinct()

# View(items_data)



# summary for both 2017 & 2018
# sales_count
sales_count_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sales_count = n())

# sales_quantity
sales_quantity_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(sum_quantity = sum(quantity))

# sum profit
sum_profit <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))
  
# summary
summary_2017_2018 <- sum_profit %>% 
  left_join(sales_count_2017_2018) %>% 
  left_join(sales_quantity_2017_2018) 


# summary for 2017
# sales_count
sales_count_2017 <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# sum_quantity
sales_quantity_2017 <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(sum_quantity = sum(quantity)) %>%
  arrange()

# sum_profit
sales_sum_profit_2017 <- sales_data_2017 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))

# summary
summary_2017 <- sales_sum_profit_2017 %>% 
  left_join(sales_count_2017) %>% 
  left_join(sales_quantity_2017)

# View(summary_2017)


# summary for 2018
# sales_count
sales_count_2018 <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# sum_quantity
sales_quantity_2018 <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(sum_quantity = sum(quantity)) %>%
  arrange()

# sum_profit
sales_sum_profit_2018 <- sales_data_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))

# summary
summary_2018 <- sales_sum_profit_2018 %>% 
  left_join(sales_count_2018) %>% 
  left_join(sales_quantity_2018) %>% 
  arrange(desc(sum_total_profit))

# View(summary_2018)

profit_2017 <- sum(summary_2017$sum_total_profit)
profit_2018 <- sum(summary_2018$sum_total_profit)
# print(profit_2017)
# print(profit_2018)




# sales per item dataset (distinct items, its information and its total profit)
sales_per_item <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(num_sales = n()) %>% 
  left_join(items_data, by='item_name') %>% 
  left_join(summary_2017_2018, by="item_name") %>% 
  select(item_code, item_name, num_sales, main_category, sub_category, 
         unit_buying_price, unit_price_margin, unit_selling_price, sum_total_profit)

View(sales_per_item)

quantile(sales_per_item$num_sales, 0.01)


# stop selling items with 1) quantity of 1 and 2) top 10 lowest 
stop_selling <- sales_per_item %>% 
  filter(num_sales == 1) %>% 
  arrange(sum_total_profit)

View(stop_selling)




# unit_selling_price - unit_price_margin = unit_buying_price


# The highest selling products by month and category
highest_selling <- sales_data_2017_2018 %>% 
  group_by(month_number, main_category, item_name) %>% 
  summarise(sales_count = n()) %>% 
  group_by(month_number, main_category) %>% 
  filter(sales_count == max(sales_count))

View(highest_selling)  

# The least selling products by month and category
lowest_selling <- sales_data_2017_2018 %>% 
  group_by(month_number, main_category, item_name) %>% 
  summarise(sales_count = n()) %>% 
  group_by(month_number, main_category) %>% 
  filter(sales_count == min(sales_count))

# because there is too many items with 1 sale, only shows the number of items
# in each category
lowest_selling <- lowest_selling %>% 
  group_by(month_number, main_category, sales_count) %>% 
  summarise(item_count = n())

View(lowest_selling)
  

  
  
  
  
