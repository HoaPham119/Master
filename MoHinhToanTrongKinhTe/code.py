import numpy as np

# Ma trận chi phí
cost_matrix = np.array([
    [8, 2, 5],
    [3, 2, 7],
    [4, 1, 6]
])

n = cost_matrix.shape[0]  # Số lượng người và công việc (3 trong ví dụ này)

# Hàm tính cận dưới bằng cách giảm hàng và cột
def calculate_lower_bound(matrix):
    # Giảm hàng
    row_min = np.min(matrix, axis=1)
    matrix = matrix - row_min[:, np.newaxis]
    lower_bound = np.sum(row_min)
    
    # # Giảm cột
    # col_min = np.min(matrix, axis=0)
    # matrix = matrix - col_min
    # lower_bound += np.sum(col_min)
    
    return lower_bound, matrix

# Hàm phân nhánh
def branch_and_bound(cost_matrix, assigned_jobs=None, current_cost=0, best_cost=np.inf, best_assignment=None):
    if assigned_jobs is None:
        assigned_jobs = []
    
    # Nếu tất cả công việc đã được gán
    if len(assigned_jobs) == n:
        if current_cost < best_cost:
            return current_cost, assigned_jobs
        else:
            return best_cost, best_assignment
    
    # Ma trận con tương ứng với những người/công việc chưa được gán
    remaining_indices = [i for i in range(n) if i not in [x[0] for x in assigned_jobs]]
    current_matrix = cost_matrix[np.ix_(remaining_indices, remaining_indices)]
    
    # Tính cận dưới cho ma trận hiện tại
    lower_bound, reduced_matrix = calculate_lower_bound(current_matrix.copy())
    
    if current_cost + lower_bound >= best_cost:
        return best_cost, best_assignment  # Cắt tỉa
    
    # Gán tiếp tục cho các công việc chưa gán
    for row_index, person_index in enumerate(remaining_indices):
        for col_index, job_index in enumerate(remaining_indices):
            if (person_index, job_index) not in assigned_jobs:
                new_assigned_jobs = assigned_jobs + [(person_index, job_index)]
                new_cost = current_cost + cost_matrix[person_index, job_index]
                best_cost, best_assignment = branch_and_bound(cost_matrix, new_assigned_jobs, new_cost, best_cost, best_assignment)
    
    return best_cost, best_assignment

# Chạy thuật toán Branch and Bound
optimal_cost, optimal_assignment = branch_and_bound(cost_matrix)

# Kết quả
print(f"Tổng chi phí tối ưu: {optimal_cost}")
print(f"Phân công tối ưu (Người, Công việc): {optimal_assignment}")
