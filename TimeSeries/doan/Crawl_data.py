import numpy as np
import re
import pandas as pd
import os
import datetime as dt
from meteostat import Point, Daily

def ex2():
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

def ex3():
    location = Point(40.7128, -74.0060)  # New York
    data = Daily(location, start=dt.datetime(2014, 11, 1), end=dt.datetime(2024, 10, 30))
    data = data.fetch()
    data = data[["tavg"]]
    data['date'] = pd.to_datetime(data.index)
    data['month-year'] = data['date'].dt.strftime('%Y-%m')
    monthly_avg_temp = pd.DataFrame(data.groupby('month-year')['tavg'].mean())
    monthly_avg_temp.index = pd.to_datetime(monthly_avg_temp.index, format='%Y-%m')
    monthly_avg_temp.to_csv("tavg.csv")
if __name__ == "__main__":
    ex2()
    ex3()
    print()