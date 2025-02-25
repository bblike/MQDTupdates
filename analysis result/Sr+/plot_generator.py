import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import pandas as pd
import re

def cell_to_indices(cell):
    """将Excel单元格地址转换为0-based的行列索引"""
    cell = cell.upper()
    # 使用正则表达式分离列字母和行号
    match = re.match(r'^([A-Z]+)(\d+)$', cell)
    if not match:
        raise ValueError(f"无效的单元格格式: {cell}")

    col_str, row_str = match.groups()
    row = int(row_str)

    # 转换列字母为列索引（0-based）
    col = 0
    for i, char in enumerate(reversed(col_str)):  # 从右到左处理字母
        char_value = ord(char) - ord('A') + 1  # A=1, B=2,..., Z=26
        col += char_value * (26 ** i)
    col_index = col - 1  # 转换为0-based索引

    # 行号转换为0-based索引
    row_index = row - 1
    return row_index, col_index


def read_data_from_excel(file_path, sheet_name, start_cell, end_cell):
    # 读取Excel文件
    df = pd.read_excel(file_path, sheet_name=sheet_name, header=None)

    # 转换起止单元格地址
    start_row, start_col = cell_to_indices(start_cell)
    end_row, end_col = cell_to_indices(end_cell)

    # 检查是否为同行或同列
    if (start_row != end_row) and (start_col != end_col):
        raise ValueError("起始和结束单元格必须在同一行或同一列")

    # 确保范围有效性（交换起止位置保证start <= end）
    if start_row == end_row:
        start_col, end_col = sorted([start_col, end_col])
    else:
        start_row, end_row = sorted([start_row, end_row])

    # 提取数据
    if start_row == end_row:
        data = df.iloc[start_row, start_col:end_col + 1].values.flatten()
    else:
        data = df.iloc[start_row:end_row + 1, start_col].values.flatten()


    return data


def compute_asymptote_and_plot(begin_ns, arrays, labels):
    plt.figure(figsize=(10, 8))

    def asymptotic_func(x, delta_0, delta_2, delta_4, delta_6):
        return delta_0 + delta_2 / (x**2) + delta_4 / (x**4) + delta_6 / (x**6)

    for begin_n, array1, label in zip(begin_ns, arrays, labels):
        # Create array0 corresponding to array1
        array0 = np.arange(begin_n, len(array1) + begin_n)
        for i in range(len(array0)):
            if str(array1[i]) == 'nan':
                array0=array0[:i-1]
                array1=array1[:i-1]
                break
        # Plot raw data
        plt.plot(array0, array1, 'o-', label=f"Data ({label})")

        # Fit the curve
        try:
            popt, pcov = curve_fit(asymptotic_func, array0, array1, maxfev=10000)
            delta_0, delta_2, delta_4, delta_6 = popt
            perr = np.sqrt(np.diag(pcov))
            delta_0_err, delta_2_err, delta_4_err, delta_6_err = perr

            # Print results
            print(f"Results for {label}:")
            print(f"  delta_0 (asymptote): {delta_0:.10f} ± {delta_0_err:.10f}")
            print(f"  delta_2: {delta_2:.10f} ± {delta_2_err:.10f}")
            print(f"  delta_4: {delta_4:.10f} ± {delta_4_err:.10f}")
            print(f"  delta_6: {delta_6:.10f} ± {delta_6_err:.10f}")

            with open("optimized parameters.txt", "a", encoding="utf-8") as f:
                f.write(f"Results for {label}:\n")
                f.write(f"  delta_0 (asymptote): {delta_0:.10f} ± {delta_0_err:.10f}\n")
                f.write(f"  delta_2: {delta_2:.10f} ± {delta_2_err:.10f}\n")
                f.write(f"  delta_4: {delta_4:.10f} ± {delta_4_err:.10f}\n")
                f.write(f"  delta_6: {delta_6:.10f} ± {delta_6_err:.10f}\n")

        except Exception as e:
            print(f"Curve fitting failed for {label}. Error: {e}")

    plt.xlabel('Index (array0)')
    plt.ylabel('Values (array1)')
    plt.title('Sr+ Test')
    plt.legend()
    plt.grid()
    plt.show()

# Example usage
if __name__ == "__main__":
    # Specify the Excel file path and sheet name
    file_path = "plot.xlsx"
    sheet_name = "Sr+ test for rc l=0"

    # Define the cell ranges and corresponding labels
    """cell_ranges = ["C13:C66","E13:E66","G13:G66","I13:I66","K13:K66","M13:M66","O13:O66","Q13:Q66","S13:S66","U13:U66","W13:W66","Y13:Y66","AA13:AA66"]
    labels = ["0.6", "0.8", "1.0","1.2","1.4","0.5","0.4","0.3","0.2","0.1", "2.8", "3.0", "2.4"]
    begin_ns = [1,1,1,1,1,1,1,1,1,1,1,1,1]
    """


    def generate_cell_ranges(letters, begin, end):
        return [f"{letter}{begin}:{letter}{end}" for letter in letters]


    # 参数定义
    letters = ["C", "E", "G", "I", "K", "M", "O", "Q", "S", "U","W", "Y", "AA", "AC", "AE", "AG", "AI", "AK", "AM", "AO", "AQ", "AS", "AU", "AW", "AY", "BA", "BC", "BE", "BG", "BI", "BK", "BM", "BO", "BQ", "BS", "BU", "BW"]
    begin = 13
    end = 66

    # 生成 cell_ranges
    cell_ranges = generate_cell_ranges(letters, begin, end)

    # 其他参数
    labels = ["0.6", "0.8", "1.0", "1.2", "1.4", "0.5", "0.4","0.3","0.2", "0.1", "2.8", "3.0", "2.4", "2.0", "1.8", "1.10", "1.09", "1.08", "1.07", "1.06", "1.05", "0.22", "0.24", "0.26", "0.28", "0.32", "0.34", "0.36", "0.38", "0.42", "0.44", "0.46", "0.48", "0.52", "0.54", "0.56", "0.58"]
    print(len(letters))
    print(len(labels))
    begin_ns = [1] * len(labels)  # 统一初始化为1

    with open('optimized parameters.txt', 'w') as f:
        f.write("")
    # Read data from Excel and compute the asymptote
    try:
        arrays = []
        for cell_range in cell_ranges:
            start_cell, end_cell = cell_range.split(":")
            array = read_data_from_excel(file_path, sheet_name, start_cell, end_cell)
            arrays.append(array)

        compute_asymptote_and_plot(begin_ns, arrays, labels)
    except Exception as e:
        print("Error:", e)
