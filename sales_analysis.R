library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/new_dataset.csv")
library(data.table)
library(ggplot2)
# stop selling when 
# 1 sell 1-10 item per year?
# profit is less than a certain amount?

# store should stop selling? 

sales_data_2017 <- sales_data_2017_2018 %>% 
  filter(Year == 2017)

# View(sales_data_2017)

sales_data_2018 <- sales_data_2017_2018 %>% 
  filter(Year == 2018)

# View(sales_data_2018)

# data only sold in 2017 

sales_data_2017_ <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(sum_quantity = sum(quantity)) %>%
  arrange()

View(sales_data_2017_)

sales_data_2018_ <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(sum_quantity = sum(quantity)) %>%
  arrange()

View(sales_data_2018_)

# items that are only sold in 2017 -- not correct do not use
items_both <- sales_data_2017_ %>% 
  right_join(sales_data_2018_)
View(items_both)

only_2017 <- sales_data_2017 %>%
  filter(!(items_both$item_name %in% sales_data_2017_$item_name))




# total profit per item & num count for each
sales_count_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sales_count = n())
# View(sales_count_2017_2018)

sales_quantity_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(quantity = sum(quantity))

View(sales_quantity_2017_2018)

sum_profit <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit)) %>% 
  arrange(sum_total_profit)
# View(sum_profit)


joined <- sum_profit %>% 
  left_join(sales_count_2017_2018) %>% 
  left_join(sales_quantity_2017_2018) %>% 
  mutate(average_sale = quantity / sales_count) %>% 
  mutate(average_profit = sum_total_profit / quantity)
  

sum_quantity = sum(joined$quantity)

quantile(joined$quantity)

# Banana Cavendish increase 
#



# by time? maybe?
# in 2017 04/11/2017 
# df <- read.csv("Documents/datathon2023/sales_data_2017_2018_for_tableau_with_new_date_columns.csv")

# sales_data_2017 <- df %>% 
#   filter(year == 2017)
# 
# sales_data_2018 <- df %>% 
#   filter(year == 2018)
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

