# 2017/2018 difference

library(tidyverse)
library(ggplot2)

sales <- read.csv('~/Desktop/UW/CSV-Datathon/new_dataset.csv')
View(sales)

newsales <- read.csv('~/Desktop/UW/CSV-Datathon/THE NEW DATASET.csv')
View(newsales)

#monthly sales price and monthly purchases made
monthly_sales <- sales %>% group_by(Year, Month) %>% 
  summarize(sum_price = sum(total_selling_price), purchases = n_distinct(receipt_id))

#yearly sales
yearly_sales <- sales %>% group_by(Year) %>% summarize(sum_profit = sum(total_profit))

#average transaction price
receipt <- sales %>% group_by(Year, receipt_id) %>% 
  summarize(sum_price = sum(total_selling_price)) %>% summarize(avg_price = mean(sum_price))

#Sales-to-investment Ratio
profit <- sales %>% group_by(Year) %>% 
  summarize(total_invest = sum(total_buying_price, na.rm=T), total_sales = sum(total_selling_price, na.rm=T)) %>%
  mutate(ratio = total_invest / total_sales)

#basket size
basket_size <- newsales %>% group_by(year, day_of_week_name, receipt_id) %>%
  summarize(size = n()) %>% summarize(avg_size = mean(size)) #higher avg size on friday~sunday

basket_size2 <- newsales %>% group_by(year, month_number, receipt_id) %>%
  summarize(size=n()) %>% summarize(avg_size = mean(size)) #experienced increase in 2018

basket_size3 <- newsales %>% group_by(is_weekend, hour, receipt_id) %>%
  summarize(size=n()) %>% summarize(avg_size = mean(size))
  







