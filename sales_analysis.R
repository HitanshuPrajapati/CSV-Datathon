library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/new_dataset.csv")

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

sales_data_2017 <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# View(sales_data_2017)

sales_data_2018 <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# View(sales_data_2018)

item_count_2017 <- sales_data_2017[sales_data_2017$sales_count <= 10,]
item_count_2018 <- sales_data_2018[sales_data_2018$sales_count <= 10,]

# View(item_count_2017)
# View(item_count_2018)

# items that are only sold in 2017 -- not correct do not use
items_both <- item_count_2017 %>% 
  right_join(item_count_2018)
# View(items_both)

# total profit per item & num count for each
sales_count_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sales_count = n())
# View(sales_count_2017_2018)

sum_profit <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit)) %>% 
  arrange(sum_total_profit)
# View(sum_profit)



joined <- sum_profit %>% 
  left_join(sales_count_2017_2018) %>% 
  mutate(average_profit = sum_total_profit / sales_count)

View(joined)


# by time? maybe?
# # in 2017 04/11/2017 
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

