

library(tidyverse)

transactions <- read_delim("new_dataset.csv")


main_category_df <- select(transactions, "main_category", "jDate")


main_category_trends <- ggplot(data = main_category_df, aes(x = as.Date(jDate), y = nrow(main_category), group = jDate, color = jDate)) + 
  geom_line() + xlab("Date(2021-01 to 2022-07)") + 
  ylab("People Vaccinated") + 
  ggtitle("People Vaccinated trends in US, California, Washington, and Marhshall Islands") + 
  theme(plot.title=element_text(family='', face='bold', colour='purple', size=12))


