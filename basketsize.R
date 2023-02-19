# 2017/2018 difference

library(tidyverse)
library(ggplot2)

newsales <- read.csv('~/Desktop/UW/CSV-Datathon/THE NEW DATASET.csv')
View(newsales)

#monthly sales price and monthly purchases made
#monthly_sales <- sales %>% group_by(Year, Month) %>% 
#  summarize(sum_price = sum(total_selling_price), purchases = n_distinct(receipt_id))

#yearly sales
#yearly_sales <- newsales %>% group_by(year) %>% summarize(sum_profit = sum(total_profit))

#Sales-to-investment Ratio 
#why is this so big??
profit <- newsales %>% group_by(year) %>% 
  summarize(total_invest = sum(total_buying_price, na.rm=T), total_sales = sum(total_selling_price, na.rm=T)) %>%
  mutate(ratio = total_sales / total_invest)

#average basket size per year & transaction price
b_yearly <- newsales %>% group_by(year, receipt_id) %>%
  summarize(size=n(), price = sum(total_selling_price)) %>% 
  summarize(avg_size = mean(size), avg_price = mean(price)) %>%
  mutate(ratio = avg_price / avg_size)
#2017: 10.31137
#2018: 11.54948
#2017: 3.606908
#2018: 3.957965
# ==> considering ratio, 1 item per year was a bit more expensive. 

#Examine by day of the week
b_dayofw <- newsales %>% group_by(year, day_of_week_name, receipt_id) %>%
  summarize(size = n(), price = sum(total_selling_price)) %>% 
  summarize(avg_size = mean(size), avg_price = mean(price)) %>%
  mutate(ratio = avg_price / avg_size)
#higher avg size on friday~sunday
#buy more expensive items in general on saturday?

#Examine by weekend or not
b_weekend <- newsales %>% group_by(year, is_weekend, receipt_id) %>%
  summarize(size = n(), price = sum(total_selling_price)) %>% 
  summarize(avg_size = mean(size), avg_price = mean(price)) %>%
  mutate(ratio = avg_price / avg_size)
#If it's a weekend, basket size IS larger.
#IF it's a weekend, item purchases tend to be higher as well. 

#What is the item that is purchased on a weekend

#Examine by month in each year
b_month <- newsales %>% group_by(year, month_number, receipt_id) %>%
  summarize(size = n(), price = sum(total_selling_price)) %>% 
  summarize(avg_size = mean(size), avg_price = mean(price)) %>%
  mutate(ratio = avg_price / avg_size)
#Months 9-12 has higher basket size
#

#Examine by month in each quarter
b_quarter <- newsales %>% group_by(year, quarter, receipt_id) %>%
  summarize(size = n(), price = sum(total_selling_price)) %>% 
  summarize(avg_size = mean(size), avg_price = mean(price)) %>%
  mutate(ratio = avg_price / avg_size)
#in 2017, 3rd and 4th quarter had higher avg size
#in 2017, they bought more expensive items in 3rd and 4th quarter as well
#in 2018, highest avg size was 2nd quarter?
#in 2018, the surge was also on the 2nd quarter







