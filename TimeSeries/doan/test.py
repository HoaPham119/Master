import yfinance as yf
import numpy as np
import re
import pandas as pd
import os
import datetime as dt
from meteostat import Point, Daily

path = os.getcwd()+"/TimeSeries/doan/data/"
all_data = pd.DataFrame()
for file_name in os.listdir(path):
    try:
        match = re.search(r'-(\d{4})-', file_name)
        year = match.group(1)
    except:
        match = re.search(r'-(\d{4})', file_name)
        year = match.group(1)

    df = pd.read_excel(f"{path}{file_name}")
    df.columns = df.iloc[1]
    df = df[df["Tên hàng"] == "Gạo"]
    nan_col = np.nan
    df = df[nan_col].reset_index(drop = True)
    df.columns = range(1,len(df.columns)+1)
    df = df.drop(df.columns[-1], axis=1)
    df.columns = [f'{year}-{col}' for col in df.columns]
    df = df.T
    all_data = pd.concat([all_data, df])#, ignore_index=True)
all_data.index = pd.to_datetime(all_data.index, format='%Y-%m')
all_data = all_data.apply(pd.to_numeric)
all_data["value"] = all_data[0].round(2) 
all_data = all_data.drop(0, axis = 1)
all_data.to_csv('gao.csv')

# all_data['year'] = all_data['date'].dt.year
# all_data['month'] = all_data['date'].dt.month
# all_data["value"] = all_data[0]
# timeseries_df = all_data.pivot_table(index='year', columns='month', values='value')
# # all_data.index = pd.to_datetime(all_data.index, format='%Y-%m')
print()


tickers = ['AAPL','IBM', 'GOOG', 'BP', 'XOM', 'COST', 'GS']
def fin():
    start = dt.datetime(2022,1,1)
    end = dt.datetime(2023,1,1)

    # Download closing prices
    data = yf.download(tickers, start,end, progress=False)['Close']
    data.tail()

def doam():
    location = Point(40.7128, -74.0060)  # New York
    data = Daily(location, start=dt.datetime(2020, 1, 1), end=dt.datetime(2023, 1, 1))
    data = data.fetch()
    print(data.head())

doam()
print()