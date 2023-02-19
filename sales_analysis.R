library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/THE NEW DATASET.csv")
library(data.table)
library(ggplot2)
library(plotly)


# 2017 sales data
sales_data_2017 <- sales_data_2017_2018 %>% 
  filter(year == 2017)

# View(sales_data_2017)

# 2018 sales data
sales_data_2018 <- sales_data_2017_2018 %>% 
  filter(year == 2018)

# View(sales_data_2018)

# items data
items_data <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  select(item_code, item_name, main_category, sub_category, unit_buying_price,
         unit_selling_price, unit_price_margin) %>% 
  distinct()


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
  # mutate(average_sale = sum_quantity / sales_count) %>% 
  # mutate(average_profit = sum_total_profit / quantity)

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

View(summary_2018)

profit_2017 <- sum(summary_2017$sum_total_profit)
profit_2018 <- sum(summary_2018$sum_total_profit)
# print(profit_2017)
# print(profit_2018)

View(summary_2017)
View(summary_2018)

summary_2018 <- summary_2018 %>% 
  left_join(sales_data_2018, by="item_name")  %>% 
  select(item_name, sum_total_profit, sales_count, sum_quantity, unit_buying_price, 
         unit_selling_price, unit_price_margin) %>% 
  distinct(item_name)

View(summary_2018)


# # compare 2017 and 2018 (2018 - 2017)
# 
# df <- summary_2017 %>% 
#   left_join(summary_2018, by="item_name")
# df[is.na(df)] = 0
# 
# diff_2018_2017 = data.frame()
# diff_2018_2017 <- df %>% 
#   mutate(difference_total_profit = df$sum_total_profit.y - df$sum_total_profit.x) %>% 
#   mutate(difference_sales_count = df$sales_count.y - df$sales_count.x) %>% 
#   mutate(difference_quantity = df$quantity.y - df$quantity.x) %>% 
#   select(item_name,
#          difference_total_profit,
#          difference_sales_count,
#          difference_quantity)
# 
# View(diff_2018_2017)
# 
# item_decreased <- diff_2018_2017 %>% 
#   filter(difference_total_profit < 0)
# 
# quantile(item_decreased$difference_total_profit)
# 
# item_increased <- diff_2018_2017 %>% 
#   filter(difference_total_profit > 0)
# 
# quantile(item_increased$difference_total_profit)





# # by time? maybe?
# # in 2017 04/11/2017 
# 
# # in 2017
# sales_data_2017_time <- sales_data_2017 %>%
#   filter(year == 2017, month_number == 4, day_of_week_name == 'Tuesday', week_number == 15) %>%
#   group_by(hour) %>%
#   summarise(count = n())
# 
# View(sales_data_2017_time)
# 
# # in 2018
# sales_data_2018_time <- sales_data_2018 %>%
#   group_by(hour) %>%
#   summarise(count = n())
# 
# View(sales_data_2018_time)


sales_per_item <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(num_sales = n()) %>% 
  left_join(items_data, by='item_name') %>% 
  left_join(summary_2017_2018, by="item_name") %>% 
  select(item_code, item_name, num_sales, main_category, sub_category, 
         unit_buying_price, unit_price_margin, unit_selling_price, sum_total_profit)

View(sales_per_item)

quantile(sales_per_item$num_sales, 0.05)



# stop selling items with 1) quantity of 1 and 2) top 10 lowest 
stop_selling <- sales_per_item %>% 
  filter(num_sales == 1) %>% 
  arrange(sum_total_profit)

View(stop_selling)




# unit_selling_price - unit_price_margin = unit_buying_price
# unit_buying_price for cabbage = 3.75

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

View(lowest_selling)  

least_count <- lowest_selling %>% 
  group_by(month_number, main_category) %>% 
  summarise(item_count = n())

View(least_count)

df <- lowest_selling %>% 
  group_by(month_number, main_category, sales_count) %>% 
  summarise(item_count = n())

View(df)
  

  
  
  
  
