library(dplyr)
library(readr)
sales_data_2017_2018 <- read_csv("Documents/datathon2023/CSV-Datathon/sales_data_2017_2018.csv")
View(sales_data_2017_2018)

# store should stop selling? 

sales_data <- sales_data_2017_2018 %>%
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

sales_data_2017_2018 <- sales_data_2017_2018 %>% 
  group_by(item_name) %>%
  summarise(sales_count = n()) %>%
  arrange()

one_sale <- sales_data[sales_data$sales_count == 1,]
one_sale2 <- sales_data_2017_2018[sales_data_2017_2018$sales_count == 1,]
View(one_sale)
View(one_sale2)

