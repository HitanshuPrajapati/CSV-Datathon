import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# custom date parser
date_parser = lambda x: pd.to_datetime(x, format='%m/%d/%Y %I:%M:%S %p')

# read csv n fix dates col
df = pd.read_csv('sales.csv', parse_dates=['date'], date_parser=date_parser)

df['month'] = df['date'].dt.month #new col

# replace main_category values
df['main_category'] = df['main_category'].replace(['Flowers', 'Breads & Bakery', 'Dairy, Cheese, and Eggs', 'Beverage', 'Beverages', 'Bag', 'Miscellaneous'], 'Other')


# group parameters for plot
monthly_category_sales = df.groupby(['month', 'main_category'])['total_profit'].sum().reset_index()


# create pivot table for bar stack to base off
monthly_category_sales_pivot = pd.pivot_table(monthly_category_sales, values='total_profit', index='month', columns='main_category', aggfunc=np.max)

# actually making bar stack
ax = monthly_category_sales_pivot.plot(kind='bar', stacked=True,figsize=(10, 6))  #10"x6"


# graph legend 
ax.legend(loc='center', bbox_to_anchor=(1.15, 0.5), fancybox=True, shadow=True)
plt.subplots_adjust(right=0.8) #increase space on right of graph


#plot and set chart/axis names
ax.set_title('Total Profit by Main Category for each Month') 
ax.set_xlabel('Month')
ax.set_ylabel('Profit ($)')
plt.show()