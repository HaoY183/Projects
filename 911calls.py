import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
df = pd.read_csv('../911call/911.csv')
df.info()
df['Date'] = df['timeStamp'].apply(lambda x: pd.to_datetime(x))
df['Hour'] = df['Date'].apply(lambda x : x.hour)
df['Month'] = df['Date'].apply(lambda x: x.month)
df['Year'] = df['Date'].apply(lambda x: x.year)
def time(x):
    if (x['Hour'] >= 6) & (x['Hour'] <= 11):
        return 'Morning'
    elif (x['Hour'] >= 12) & (x['Hour'] <= 17):
        return 'Afternoon'
    elif (x['Hour'] >= 18) & (x['Hour'] <= 23):
        return 'Night'
    elif (x['Hour'] >= 0) & (x['Hour'] <= 5):
        return 'Midnight'
df['Daytime'] = df.apply(time,axis = 1)
def months(x):
    if (x['Month'] >= 3) & (x['Month'] <= 5):
        return 'Spring'
    elif (x['Month'] >= 6) & (x['Month'] <= 8):
        return 'Summer'
    elif (x['Month'] >= 9) & (x['Month'] <= 11):
        return 'Autumn'
    elif (x['Month'] == 12) | (x['Month'] <= 2):
        return 'Winter'
df['Season'] = df.apply(months,axis = 1)
df['Issue'] = df['title'].apply(lambda x: x.split(":")[0])
df.info()
print(df.head())
sns.countplot(x = 'Issue', data = df, order = ['EMS','Traffic','Fire'])
plt.show()
df['twp'].value_counts()
df['twp'].nunique()
TownIssue = df.groupby(by=['twp','Issue']).count()['lat'].unstack()
plt.figure(figsize = (10,25))
sns.heatmap(TownIssue, cmap = 'coolwarm')
plt.show()
sns.countplot(x = 'Year', hue = 'Issue', data = df)
plt.show()
df2 = df.groupby('Year').count()
plt.pie(df2['lat'], labels = df2.index)
plt.show()
df.sort_values(by = 'Date')
print(df.head())
print(df.tail())
sns.countplot(x = 'Issue', hue = 'Daytime', hue_order = ['Morning', 'Afternoon', 'Night', 'Midnight'], data = df)
plt.show()
sns.countplot(x = 'Issue', data = df, hue = 'Season', hue_order = ['Spring', 'Summer', 'Autumn', 'Winter'])
plt.show()
df['Day'] = df['timeStamp'].apply(lambda x: x.split()[0])
df['Day'].value_counts()
df1 = df['Day'].value_counts().to_frame().reset_index()
df1.rename(columns = {'index':'date'}, inplace = True)
df1.rename(columns = {'Day':'calls per day'}, inplace = True)
df1['year'] = df1['date'].apply(lambda x: x.split('-')[0])
sns.stripplot(x = 'year', y ='calls per day', data = df1, jitter = True, size = 5, order = ['2015','2016','2017','2018','2019','2020'])
plt.show()
df3 = df.groupby(by = ['Daytime', 'Season']).count()['lat'].unstack()
plt.figure(figsize = (5,5))
sns.heatmap(data = df3, cmap = 'coolwarm')
plt.show()
df5 = df.groupby(by = ['Daytime', 'Year']).count()['lat'].unstack()
plt.figure(figsize = (5,5))
sns.heatmap(data = df5, cmap = 'coolwarm')
plt.show()

#Best Refer to https://www.kaggle.com/code/yinhao1120/911-calls-data-visualization/notebook
