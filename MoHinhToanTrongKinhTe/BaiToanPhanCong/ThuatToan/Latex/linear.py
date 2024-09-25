import numpy as np
import pulp

# Ma trận chi phí
cost_matrix = np.array([
    [8, 2, 5],
    [3, 2, 7],
    [4, 1, 6]
])

n = cost_matrix.shape[0]  # Số lượng người và công việc

# Bước 1: Khởi tạo bài toán Lập Trình Tuyến Tính
prob = pulp.LpProblem("Assignment_Problem", pulp.LpMinimize)

# Bước 2: Tạo các biến nhị phân x[i][j]
x = [[pulp.LpVariable(f"x_{i}_{j}", cat='Binary')
      for j in range(n)] for i in range(n)]

# Bước 3: Đặt hàm mục tiêu - tối thiểu hóa tổng chi phí
prob += pulp.lpSum(cost_matrix[i][j] * x[i][j] for i in range(n)
                    for j in range(n)), "Total_Cost"

# Bước 4: Ràng buộc - mỗi người chỉ được gán đúng một công việc
for i in range(n):
    prob += pulp.lpSum(x[i][j] 
                       for j in range(n)) == 1, f"Person_{i}_assignment"

# Bước 5: Ràng buộc - mỗi công việc chỉ được gán cho đúng một người
for j in range(n):
    prob += pulp.lpSum(x[i][j] 
                       for i in range(n)) == 1, f"Job_{j}_assignment"

# Bước 6: Giải bài toán
prob.solve()

# Bước 7: In ra kết quả
# print(f"Trạng thái giải: {pulp.LpStatus[prob.status]}")

# In tổng chi phí tối ưu
print(f"Tổng chi phí tối ưu: {pulp.value(prob.objective)}")

# In phân công công việc
for i in range(n):
    for j in range(n):
        if pulp.value(x[i][j]) == 1:
            print(f"Người {i+1} thực hiện công việc\
                  {j+1} với chi phí {cost_matrix[i][j]}")
