# 生成 input.in 文件
def generate_input_file():
    # 文件名
    filename = "input.in"

    # 打开文件写入内容
    with open(filename, "w") as file:
        # 写入前四行
        eq = input("Equation No: ")
        file.write(f"{eq}\n")
        Z = input("Z: ")
        V0 = input("V0: ")
        A = input("A: ")
        file.write(f"{Z} {V0} {A}\n")
        file.write("n\n")

        # 获取循环范围
        begin = int(input("Begin Principle Quantum Number: "))
        end = int(input("End Principle Quantum Number: "))
        j = input("j: ")

        # 写入循环内容
        for i in range(begin, end + 1):
            file.write("1\n")
            file.write(f"{i} {j} 1E-15\n")

    print(f"文件 '{filename}' 已生成。")

# 执行函数
generate_input_file()
