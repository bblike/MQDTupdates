import pandas as pd
import re
import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import interp1d

# 读取文件
file_path = "optimized parameters.txt"
with open(file_path, "r", encoding="utf-8") as file:
    lines = file.readlines()

# 解析数据
data = {}
current_title = None
delta_pattern = re.compile(r"delta_(\d+)\s*\(?\w*\)?:\s*([-\d\.]+)\s*\u00b1\s*([-\d\.]+)")

default_errors = {}
for line in lines:
    line = line.strip()
    if line.startswith("Results for"):
        current_title = float(re.search(r"Results for ([\d\.]+):", line).group(1))
        data[current_title] = {}
    else:
        match = delta_pattern.search(line)
        if match and current_title is not None:
            delta_key = f"delta_{match.group(1)}"
            value = float(match.group(2))
            error = float(match.group(3))  # 提取误差
            data[current_title][delta_key] = (value, error)
            default_errors.setdefault(delta_key, []).append(error)

# 创建 DataFrame
df = pd.DataFrame.from_dict(data, orient='index')
df.index.name = "Title"
df = df.map(lambda x: x[0] if isinstance(x, tuple) else x)  # 只保留值部分
df.sort_index(inplace=True)

# 存储表格
df.to_csv("rc_delta.txt", sep='\t')

# 绘制所有 delta_n 的曲线
plt.figure(figsize=(10, 6))

for column in df.columns[0:4]:
    if True:
        plt.plot(df.index[1:20], df[column].iloc[1:20], marker='o', linestyle='-', label=column)

plt.xlabel("value of Rc")
plt.ylabel("parameter values")
plt.title("Plot of quantum defect to Rc")
plt.legend()
plt.grid()
plt.savefig("rc_all_deltas.png")
plt.show()


# 插值函数用于反推 x，同时计算误差
def get_x_from_y(y_value, delta_key="delta_0"):
    if delta_key not in df.columns:
        raise ValueError(f"{delta_key} not found in data")

    interp_func = interp1d(df[delta_key], df.index, kind='cubic', fill_value="extrapolate")
    x_estimated = interp_func(y_value)

    error_series = pd.Series({k: v.get(delta_key, (0, 0))[1] for k, v in data.items()})
    error_interp = interp1d(df[delta_key], error_series, kind='linear', fill_value="extrapolate")
    x_error = error_interp(y_value)

    return x_estimated, x_error


# 显示表格
print(df)

y_input = 2.873  # from Aymer 1987 5s5p level "this work"
x, x_error = get_x_from_y(y_input, "delta_0")
print(f"convert {y_input} to {x}±{x_error}")

y_input = 2.872  # from Aymer 1987 5s5p level "experiment"
x, x_error = get_x_from_y(y_input, "delta_0")
print(f"convert {y_input} to {x}±{x_error}")