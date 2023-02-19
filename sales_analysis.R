library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/THE NEW DATASET.csv")
library(data.table)
library(ggplot2)
library(plotly)
# stop selling when 
# 1 sell 1-10 item per year?
# profit is less than a certain amount?

# store should stop selling? 

sales_data_2017 <- sales_data_2017_2018 %>% 
  filter(year == 2017)

# View(sales_data_2017)

sales_data_2018 <- sales_data_2017_2018 %>% 
  filter(year == 2018)

# View(sales_data_2018)



# summary for both 2017 & 2018

# sales_count
sales_count_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sales_count = n())

# sales_quantity
sales_quantity_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(quantity = sum(quantity))

# sum profit
sum_profit <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))
  
# summary
summary_2017_2018 <- sum_profit %>% 
  left_join(sales_count_2017_2018) %>% 
  left_join(sales_quantity_2017_2018) %>% 
  mutate(average_sale = quantity / sales_count) %>% 
  mutate(average_profit = sum_total_profit / quantity)



# summary for 2017 only
# sales_count
sales_count_2017 <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# sum_quantity
sales_quantity_2017 <- sales_data_2017 %>%
  group_by(item_name) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange()

# sum_profit
sales_sum_profit_2017 <- sales_data_2017 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))

# summary
summary_2017 <- sales_sum_profit_2017 %>% 
  left_join(sales_count_2017) %>% 
  left_join(sales_quantity_2017) %>% 
  mutate(average_sale = quantity / sales_count) %>% 
  mutate(average_profit = sum_total_profit / quantity)

# View(summary_2017)

# summary for 2018 only
# sales_count
sales_count_2018 <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

# sum_quantity
sales_quantity_2018 <- sales_data_2018 %>%
  group_by(item_name) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange()

# sum_profit
sales_sum_profit_2018 <- sales_data_2018 %>% 
  group_by(item_name) %>% 
  summarise(sum_total_profit = sum(total_profit))

# summary
summary_2018 <- sales_sum_profit_2018 %>% 
  left_join(sales_count_2018) %>% 
  left_join(sales_quantity_2018) %>% 
  mutate(average_sale = quantity / sales_count) %>% 
  mutate(average_profit = sum_total_profit / quantity)

# View(summary_2018)



# items that are only sold in 2017 
s2017 <- sales_data_2017 %>% 
  group_by(item_name) %>% 
  summarise(count = n())

s2018 <- sales_data_2018 %>% 
  group_by(item_name) %>% 
  summarise(count = n())

joined <- s2017 %>% 
  left_join(s2018, by="item_name")

# View(joined)

# compare 2017 and 2018 (2018 - 2017)

df <- summary_2017 %>% 
  left_join(summary_2018, by="item_name")
df[is.na(df)] = 0

diff_2018_2017 = data.frame()
diff_2018_2017 <- df %>% 
  mutate(difference_total_profit = df$sum_total_profit.y - df$sum_total_profit.x) %>% 
  mutate(difference_sales_count = df$sales_count.y - df$sales_count.x) %>% 
  mutate(difference_quantity = df$quantity.y - df$quantity.x) %>% 
  select(item_name,
         difference_total_profit,
         difference_sales_count,
         difference_quantity)

View(diff_2018_2017)

item_decreased <- diff_2018_2017 %>% 
  filter(difference_total_profit < 0)

quantile(item_decreased$difference_total_profit)

item_increased <- diff_2018_2017 %>% 
  filter(difference_total_profit > 0)

quantile(item_increased$difference_total_profit)

# plot difference?

plott <- ggplot(data=item_increased,
                mapping=aes(x=))

# Plot "Change in Total Profit Over Time"

filtered <- sales_data_2017_2018 %>% 
  group_by(year, month_number) %>% 
  summarise(sum = sum(total_profit))

filtered$month_number <- as.factor(filtered$month_number)
filtered$year <- as.character(filtered$year)

filtered <- filtered %>% 
  group_by(year)

View(filtered)
line_plot <- ggplot(data=filtered,
                    aes(x=month_number, y=sum, group=year)) +
                    geom_line(aes(color=year)) +
                    geom_point() +
                    labs(title = "Change in Total Profit Over Time",
                         x="month",
                         y="total profit") +
                    theme(plot.title = element_text(hjust=0.5))
                


# by time? maybe?
# in 2017 04/11/2017 

# in 2017
sales_data_2017_time <- sales_data_2017 %>%
  filter(year == 2017, month_number == 4, day_of_week_name == 'Tuesday', week_number == 15) %>%
  group_by(hour) %>%
  summarise(count = n())

View(sales_data_2017_time)

# in 2018
sales_data_2018_time <- sales_data_2018 %>%
  group_by(hour) %>%
  summarise(count = n())

View(sales_data_2018_time)


# difference between is weekend / not weekend
weekend <- sales_data_2017_2018 %>% 
  filter(is_weekend == 1) %>%
  group_by(hour) %>% 
  summarise(count = n())

weekday <- sales_data_2017_2018 %>% 
  filter(is_weekday == 1) %>% 
  group_by(hour) %>% 
  summarise(count = n())

View(weekday)
View(weekend)

weekend_2018 <- sales_data_2017_2018 %>% 
  filter(year == 2018) %>% 
  filter(is_weekend == 1) %>%
  group_by(hour) %>% 
  summarise(count = n())

weekday_2018 <- sales_data_2017_2018 %>% 
  filter(year == 2018) %>% 
  filter(is_weekday == 1) %>%
  group_by(hour) %>% 
  summarise(count = n())

weekend_2017 <- sales_data_2017 %>% 
  filter(is_weekend == 1) %>%
  group_by(hour) %>% 
  summarise(count = n())

weekday_2017 <- sales_data_2017 %>% 
  filter(is_weekday == 1) %>%
  group_by(hour) %>% 
  summarise(count = n())

weekend_day <- weekend_2017 %>% 
  left_join(weekend_2018, by='hour') %>% 
  rename("2017" = "count.x",
         "2018" = "count.y") %>% 
  left_join(weekend, by='hour')

View(weekend_day)
  
diff_week <- weekend %>% 
  left_join(weekday, by='hour') %>% 
  rename("weekend" = "count.x",
         "weekday" = "count.y") 

diff_week$weekend = diff_week$weekend / 2
diff_week$weekday = diff_week$weekday / 5

diff_week_2018 <- weekend_2018 %>% 
  left_join(weekday_2018, by='hour') %>% 
  rename("weekend" = "count.x",
         "weekday" = "count.y")

diff_week_2017 <- weekend_2017 %>% 
  left_join(weekday_2017, by='hour') %>% 
  rename("weekend" = "count.x",
         "weekday" = "count.y")

# which day?
day_of_the_week <- sales_data_2017_2018 %>% 
  group_by(day_of_week_name) %>% 
  summarise(sum = n())

View(day_of_the_week)


  

View(diff_week)
View(diff_week_2018)
View(diff_week_2017)
View(sales_data_2017_2018)

# nrmal
transactions <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(transaction = n())

View(transactions)

quantities <- sales_data_2017_2018 %>% 
  group_by(item_name) %>% 
  summarize(quantity = sum(quantity))

View(quantities)

quantile(transactions$transaction, 0.05)
quantile(quantities$quantity, 0.05)

# transaction (count) == 2 & 

clear <- summary_2017_2018 %>% 
  filter((sales_count <= 2 & sum_total_profit < 0) | sales_count == 1)
View(clear)

newwww <- clear %>% 
  left_join(sales_data_2017_2018, by = 'item_name')
View(newwww) 

View(sales_data_2017_2018)

# unit_selling_price - unit_price_margin = unit_buying_price
# unit_buying_price for cabbage = 3.75