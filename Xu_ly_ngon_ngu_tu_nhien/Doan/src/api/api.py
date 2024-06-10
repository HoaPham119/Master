import re

def chuan_hoa_dau_thanh(tu):
    # Định nghĩa các nguyên âm và dấu thanh
    nguyen_am = "aeiouyáàảãạâấầẩẫậăắằẳẵặéèẻẽẹêếềểễệíìỉĩịóòỏõọôốồổỗộơớờởỡợúùủũụưứừửữựýỳỷỹỵ"
    dau_thanh = "áàảãạấầẩẫậắằẳẵặéèẻẽẹếềểễệíìỉĩịóòỏõọôốồổỗộớờởỡợúùủũụứừửữựýỳỷỹỵ"
    khong_dau = "aaaaaaeeeeeiiiiiooooooooooouuuuuyyyy"

    # Tìm dấu thanh trong từ
    dau = ""
    for i, char in enumerate(tu):
        if char in dau_thanh:
            dau = char
            break

    if not dau:
        return tu  # Không có dấu thanh, trả về từ gốc

    # Xác định nguyên âm chính
    idx = -1
    for i, char in enumerate(tu):
        if char in nguyen_am:
            idx = i
            break

    # Tạo từ mới với dấu thanh đúng vị trí
    if idx != -1:
        tu = tu.replace(dau, khong_dau[dau_thanh.index(dau)])
        tu = tu[:idx] + dau + tu[idx+1:]
    
    return tu

# Ví dụ sử dụng
tieng_viet = ["hoá", "hóa", "hòa", "hoà"]
tieng_viet_chuan = [chuan_hoa_dau_thanh(tu) for tu in tieng_viet]
print(tieng_viet_chuan)
