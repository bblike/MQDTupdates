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

    def asymptotic_func(x, delta_0, delta_2, delta_4):
        return delta_0 + delta_2 / (x**2) + delta_4 / (x**4)

    for begin_n, array1, label in zip(begin_ns, arrays, labels):
        # Create array0 corresponding to array1
        array0 = np.arange(begin_n, len(array1) + begin_n)
        for i in range(len(array0)):
            if str(array1[i]) == 'nan':
                array0=array0[:i-1]
                array1=array1[:i-1]
                break
        # Plot raw data
        plt.plot(array0, array1, 'o-', label=f"Data (A={label})")

        # Fit the curve
        try:
            popt, pcov = curve_fit(asymptotic_func, array0, array1, maxfev=10000)
            # measure of fit quality
            delta_0, delta_2, delta_4 = popt
            perr = np.sqrt(np.diag(pcov))
            delta_0_err, delta_2_err, delta_4_err = perr

            # Print results
            print(f"Results for {label}:")
            print(f"  delta_0 (asymptote): {delta_0:.10f} ± {delta_0_err:.10f}")
            print(f"  delta_2: {delta_2:.10f} ± {delta_2_err:.10f}")
            print(f"  delta_4: {delta_4:.10f} ± {delta_4_err:.10f}")


            with open("optimized parameters.txt", "a", encoding="utf-8") as f:
                f.write(f"Results for {label}:\n")
                f.write(f"  delta_0 (asymptote): {delta_0:.10f} ± {delta_0_err:.10f}\n")
                f.write(f"  delta_2: {delta_2:.10f} ± {delta_2_err:.10f}\n")
                f.write(f"  delta_4: {delta_4:.10f} ± {delta_4_err:.10f}\n")


        except Exception as e:
            print(f"Curve fitting failed for {label}. Error: {e}")

    plt.xlabel('Index (array0)')
    plt.ylabel('Values (array1)')
    plt.title('comparison of without delta_6')
    plt.legend()
    plt.grid()
    plt.show()


# Example usage
if __name__ == "__main__":
    # Specify the Excel file path and sheet name
    file_path = "plot.xlsx"
    sheet_name = "A param&Cs test"

    def generate_cell_ranges(letters, begin, end):
        return [f"{letter}{begin}:{letter}{end}" for letter in letters]

    letters = ["V","C","J","L", "AK"]
    begin = 19
    end = 33
    cell_ranges = generate_cell_ranges(letters, begin, end)

    # Define the cell ranges and corresponding labels
    # cell_ranges = [ "V16:V55", "C16:C55", "J16:J55", "L16:L55", "N16:N55", "P16:P55", "R16:R55", "T16:T55"]
    labels = ["0.001","0.01", "0.1", "1", "exp"]
    begin_ns = [8,8,8,8,8,8,8]

    # Read data from Excel and compute the asymptote
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
