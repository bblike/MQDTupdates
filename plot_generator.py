import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import pandas as pd

def read_data_from_excel(file_path, sheet_name, start_cell, end_cell):
    # Read the Excel file
    df = pd.read_excel(file_path, sheet_name=sheet_name, header=None)

    # Convert cell references to row and column indices
    start_row, start_col = int(start_cell[1:]) - 1, ord(start_cell[0].upper()) - ord('A')
    end_row, end_col = int(end_cell[1:]) - 1, ord(end_cell[0].upper()) - ord('A')

    # Ensure the range is either in the same row or column
    if start_row != end_row and start_col != end_col:
        raise ValueError("Start and end cells must be in the same row or column.")

    # Extract data from the specified range
    if start_row == end_row:
        data = df.iloc[start_row, start_col:end_col + 1].values
    else:
        data = df.iloc[start_row:end_row + 1, start_col].values

    return data

def compute_asymptote_and_plot(begin_n, arrays, labels):
    plt.figure(figsize=(10, 8))

    def asymptotic_func(x, delta_0, delta_2, delta_4, delta_6):
        return delta_0 + delta_2 / (x**2) + delta_4 / (x**4) + delta_6 / (x**6)

    for array1, label in zip(arrays, labels):
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
            delta_0, delta_2, delta_4, delta_6 = popt
            perr = np.sqrt(np.diag(pcov))
            delta_0_err, delta_2_err, delta_4_err, delta_6_err = perr

            # Plot the fitted curve
            fitted_curve = asymptotic_func(array0, *popt)
            #plt.plot(array0, fitted_curve, '--', label=f"Fit (A={label})")

            # Print results
            print(f"Results for A={label}:")
            print(f"  delta_0 (asymptote): {delta_0:.10f} ± {delta_0_err:.10f}")
            print(f"  delta_2: {delta_2:.10f} ± {delta_2_err:.10f}")
            print(f"  delta_4: {delta_4:.10f} ± {delta_4_err:.10f}")
            print(f"  delta_6: {delta_6:.10f} ± {delta_6_err:.10f}")
        except Exception as e:
            print(f"Curve fitting failed for A={label}. Error: {e}")

    plt.xlabel('Index (array0)')
    plt.ylabel('Values (array1)')
    plt.title('Influence of A selection')
    plt.legend()
    plt.grid()
    plt.show()

# Example usage
if __name__ == "__main__":
    # Specify the Excel file path and sheet name
    file_path = "plot.xlsx"
    sheet_name = "A param&Cs test"

    # Define the cell ranges and corresponding labels
    cell_ranges = [ "V16:V55", "C16:C55", "J16:J55", "L16:L55", "N16:N55", "P16:P55", "R16:R55", "T16:T55"]
    labels = ["0.001","0.01", "0.1", "1", "3", "2", "4", "5"]
    begin_n = 5

    # Read data from Excel and compute the asymptote
    try:
        arrays = []
        for cell_range in cell_ranges:
            start_cell, end_cell = cell_range.split(":")
            array = read_data_from_excel(file_path, sheet_name, start_cell, end_cell)
            arrays.append(array)

        compute_asymptote_and_plot(begin_n, arrays, labels)
    except Exception as e:
        print("Error:", e)
