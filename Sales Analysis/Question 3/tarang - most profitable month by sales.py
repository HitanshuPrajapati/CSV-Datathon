import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# question: Find the most profitable month by sales

# custom date 
date_parser = lambda x: pd.to_datetime(x, format='%m/%d/%Y %I:%M:%S %p')


df = pd.read_csv('sales.csv', parse_dates=['date'], date_parser=date_parser)

df['month'] = df['date'].dt.month
df['year'] = df['date'].dt.year

monthly_sales = df.groupby(['year', 'month'])['total_profit'].sum().reset_index()

monthly_sales = pd.pivot_table(monthly_sales, values='total_profit', index='month', columns='year', aggfunc=np.sum)

#plot 
monthly_sales.plot(kind='bar')
plt.title('Total Sales by Month')
plt.xlabel('Month')
plt.ylabel('Total Sales')
plt.show()



