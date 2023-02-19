library(tidyverse)
library(dplyr)
library(lubridate)
library(gridExtra)

setwd("~/Desktop/UW Datathon 2023/CSV-Datathon")
df <- read_delim("sales_data_2017_2018.csv")


## Formatting the dataframe to contain separate columns for date,
# year, day, month in a separate dataset.
head(str_split_fixed(df$date, "/", 3)[,3], 5)
head(df$date, 5)
df$jDate <- parse_date_time(df$date, "%m-%d-%Y %H:%M:%S")
df$jDate <- as.Date(df$jDate)
df %>% 
  select(main_category) %>% 
  table()
str_split_fixed(df$jDate, "-", 3)[,1] # Year
str_split_fixed(df$jDate, "-", 3)[,2] # Month
str_split_fixed(df$jDate, "-", 3)[,3] # Day
df$Year <- str_split_fixed(df$jDate, "-", 3)[,1]
df$Month <- str_split_fixed(df$jDate, "-", 3)[,2]
df$Day <- str_split_fixed(df$jDate, "-", 3)[,3]



new_df <- read_delim("new_dataset.csv")
###################################################################
#### Task: Which items should the store increase/decrease the price
### of? By how much? Why? 
#### Optional Task: Which items should the store stop selling? Why?
###################################################################



# Worst 20 items by profit from sale (by indivudial sale)
new_df %>% 
  select(item_name, main_category, total_selling_price, total_profit) %>%
  mutate(cost <- total_selling_price - total_profit) %>% 
  arrange(rank(desc(total_profit))) %>% 
  tail(20)


## Top 20 Worst Profit Margin Items Sold in 2017 and 2018
worst_profit <- new_df %>% 
  group_by(item_name) %>% 
  summarise(avg_profit = mean(total_profit)) %>% 
  arrange(rank(avg_profit)) %>% 
  head(20) 

## Creates the top 20 worst profit margin items PNG
worst_profit$avg_profit <- round(worst_profit$avg_profit, 2)
png("Top20_Worst_Profits.png")
grid.table(worst_profit)
dev.off()


### The data from both what items have the worst profit margins and
# how many of each item is sold shows us that Cabbage Wombok and
# Avocado Hass Medium need a price increase, while grapes Calmeria
# needs to be removed to save money.

profit_margins <- new_df %>% 
  group_by(item_name) %>% 
  summarise(avg_profit = mean(total_profit)) %>% 
  arrange(rank(avg_profit)) 



# Function that allows for you to test the average profits from a
# specified item by adjusting the sale price (total_profits)
new_df %>% 
  group_by(item_name) %>% 
  filter(item_name == "Cabbage Wombok") %>% 
  mutate(new_price = total_profit + 3) %>% 
  summarise(avg_profit = mean(new_price))



## store's average profit per item (Any item)
mean(new_df$total_profit)


## Function that finds how many of each item was sold by year
new_df %>%
  group_by(item_name, Year) %>% 
  select(item_name, Year, total_profit) %>% 
  summarise(num_sold = length(item_name),
            total_sales = nrow(item_name),
            avg_profit = mean(total_profit)) %>% 
  arrange(desc(num_sold)) %>% 
  head(20)
  
# We CERTAINLY need to increase the price on the single most sold
# item at All Foods, the Banana Cavendish. We may want to increase
# the price of the second most sold item, the Field Tomatoes, if
# the owners wish to increase its below-average profit margins.
# The avergae profit margin for items at All Foods is $1.90


###################################################################

another_data <- read_delim("THE NEW DATASET.csv")
another_data$Year <- str_split_fixed(new_df$jDate, "-", 3)[,1]
another_data$Month <- str_split_fixed(new_df$jDate, "-", 3)[,2]
another_data$Day <- str_split_fixed(new_df$jDate, "-", 3)[,3]
another_data$jDate <- new_df$jDate


# Find the items that have high average profits
average_plot <- new_df %>%
  group_by(item_name, Year) %>% 
  select(item_name, Year, total_profit) %>% 
  summarise(num_sold = length(item_name),
            total_sales = nrow(item_name),
            avg_profit = mean(total_profit)) %>% 
  arrange(desc(avg_profit))

## Number of products sold IN TOTAL
number_sold <- another_data %>% 
  group_by(item_name) %>% 
  summarise(n = length(item_name))

# Quantiles of number of products sold
quantile(number_sold$n)
