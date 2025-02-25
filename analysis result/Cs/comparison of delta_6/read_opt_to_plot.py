import pandas as pd
import re
import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import interp1d

# 读取文件
file_path = "optimized parameters_with.txt"
with open(file_path, "r", encoding="utf-8") as file:
    lines = file.readlines()

# 解析数据
data = {}
current_title = None
delta_pattern = re.compile(r"delta_(\d+)\s*\(?.*\)?:\s*([-\d\.]+)\s*\u00b1\s*([-\d\.]+)")

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

# 创建 DataFrame
df = pd.DataFrame.from_dict(data, orient='index')
df.index.name = "Title"
df = df.map(lambda x: x[0] if isinstance(x, tuple) else x)  # 只保留值部分
df.sort_index(inplace=True)

# 存储表格
df.to_csv("rc_delta0.txt", sep='\t')

# 绘制 delta_0 的曲线

temp_plot = np.log(df.index)
plt.figure()
#plt.plot(df.index, df["delta_0"], marker='o', linestyle='-')
plt.plot(temp_plot, df["delta_0"], marker='o', linestyle='-')

plt.xlabel("Log(A)")
plt.ylabel("delta_0")
plt.title("Plot of quantum defect to A")
plt.grid()
plt.savefig("A_delta0.png")
plt.show()


# 插值函数用于反推 x，同时计算误差
def get_x_from_y(y_value):
    interp_func = interp1d(df["delta_0"], df.index, kind='cubic', fill_value="extrapolate")
    x_estimated = interp_func(y_value)

    # 提取 delta_0 误差
    delta_errors = {k: v["delta_0"][1] if isinstance(v["delta_0"], tuple) else 0 for k, v in data.items()}
    error_series = pd.Series(delta_errors)

    # 误差插值
    error_interp = interp1d(df["delta_0"], error_series, kind='linear', fill_value="extrapolate")
    x_error = error_interp(y_value)

    return x_estimated, x_error


# 显示表格
print(df)
"""y_input = 2.873 # from Aymer 1987 5s5p level "this work"
x,x_error = get_x_from_y(y_input)
print("convert {} to {}±{}".format(y_input, x, x_error))

y_input = 2.872 # from Aymer 1987 5s5p level "experiment"
x,x_error = get_x_from_y(y_input)
print("convert {} to {}±{}".format(y_input, x, x_error))
"""