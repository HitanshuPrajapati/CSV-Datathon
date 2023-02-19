# 2017/2018 difference

library(tidyverse)
library(ggplot2)

sales <- read.csv('~/Desktop/UW/CSV-Datathon/new_dataset.csv')
View(sales)

#monthly sales price and monthly purchases made
monthly_sales <- sales %>% group_by(Year, Month) %>% 
  summarize(sum_price = sum(total_selling_price), purchases = n_distinct(receipt_id))

#average transaction price
receipt <-  sales %>% group_by(Year, receipt_id) %>% 
  summarize(sum_price = sum(total_selling_price)) %>% summarize(avg_price = mean(sum_price))

#Sales-to-investment Ratio
profit <- sales %>% group_by(Year) %>% 
  summarize(total_invest = sum(total_buying_price, na.rm=T), total_sales = sum(total_selling_price, na.rm=T)) %>%
  mutate(ratio = total_invest / total_sales)



