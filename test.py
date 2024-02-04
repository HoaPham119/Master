import pandas as pd

path = f'/Users/hoapham/Documents/Cá Nhân/Cao Học/Master/Thong_Ke_Nhieu_Chieu/Data/data_exo_chap4/mobile_3d_tasktime_multi.csv'
df = pd.read_csv(path)
df = df[['Trt11', 'Trt12', 'Trt13', 'Trt21', 'Trt22', 'Trt23']]
data = pd.DataFrame()
cols = df.columns
df.head()
for col in cols:
    _data = pd.DataFrame()
    l = list(col)[-1] #Giao diện
    k = list(col)[-2] # Kích thước
    _data["value"] = df[col]
    _data["Giao diện"] = l
    _data["Kích thước"] = k
    data = pd.concat([data, _data])
print