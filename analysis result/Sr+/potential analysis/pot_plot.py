import numpy as np
import matplotlib.pyplot as plt
import re
import glob

# 获取所有匹配的文件
file_names = glob.glob("potential_*.dat")
print(file_names)
plt.figure(figsize=(8, 6))

# 读取并绘制数据
for file_name in file_names:
    try:
        # 从文件名中提取数值部分
        match = re.search(r"potential_([\d\.]+)\.dat", file_name)
        num = match.group(1) if match else "unknown"
        data = np.loadtxt(file_name, comments="#")  # 跳过注释行
        r_values, potential_values = data[:, 0], data[:, 1]
        plt.plot(r_values, potential_values, label=f"{num}")
    except Exception as e:
        print(f"无法读取 {file_name}: {e}")

# 设置对数坐标
plt.xlim(10**-2, 1)
plt.xscale("log")
plt.xlabel("r")
plt.ylabel("r * V(r)")
plt.title("Potential Functions")
plt.legend()
plt.grid(True)
plt.show()
