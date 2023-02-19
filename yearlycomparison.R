# 2017/2018 difference

library(tidyverse)
library(ggplot2)

sales <- read.csv('~/Desktop/UW/CSV-Datathon/new_dataset.csv')

sales_2017 <- sales %>%
  filter(Year == 2017)

sales_2018 <- sales %>%
  filter(Year == 2018)

#monthly sales price and monthly purchases made
monthly_2017 <- sales %>% filter(Year == 2017) %>%
  group_by(Month) %>% summarize(sum_price = sum(total_selling_price), purchases = n_distinct(receipt_id)) # 563609.3
monthly_2018 <- sales %>% filter(Year == 2018) %>%
  group_by(Month) %>% summarize(sum_price = sum(total_selling_price), purchases = n_distinct(receipt_id)) # 512427.3

#average transaction price
receipt_17 <- sales_2017 %>% group_by(receipt_id) %>% 
  summarize(sum_price = sum(total_selling_price)) %>% summarize(avg_price = mean(sum_price))

receipt_18 <- sales_2018 %>% group_by(receipt_id) %>% 
  summarize(sum_price = sum(total_selling_price)) %>% summarize(avg_price = mean(sum_price))

#average weekly volume per store
bags_17 <- sales_2017 %>% filter(main_category == 'Bag') %>% summarize(sum(quantity)) #514
bags_18 <- sales_2018 %>% filter(main_category == 'Bag') %>% summarize(sum(quantity)) #571

