# Read df function
read_csv_function <- function(base_path, file_name = "", header = TRUE) { # create a function with the name my_function
    # Đường dẫn lưu trữ dữ liệu
    path <- paste(base_path, glue("datasets/{file_name}"), sep = "/")
    # Gán dữ liệu vào biến df
    df <- read.csv(path, header = header, sep = ",")
}

EDA_function <- function(df) {
    print(glue("So dong cua du lieu: {nrow(df)}"))
    print(glue("So cot cua du lieu: {ncol(df)}"))
    print("Cau truc cua du lieu: ")
    print(str(df))
    # Kiểm tra dữ liệu unique của bộ dữ liệu
    print("unique cua tung cot")
    for (col in colnames(df)) {
        len_col <- length(unique(df[[col]]))
        print(glue("{col}: {len_col}"))
    }
    # Kiểm tra giá trị thiếu
    print("Gia tri bi thieu:")
    colSums(is.na(df))
}

check_missing_value_function <- function(df, list_col = c("")) {
    if (list_col[1] == "") {
        list_col <- names(df)
    }
    missing_df_all <- data.frame()
    for (col in list_col) {
        missing_rows <- which(is.na(df[[col]]))
        missing_df <- df[missing_rows, ]
        missing_df_all <- rbind(missing_df_all, missing_df)
    }
    missing_percent <- nrow(missing_df_all) / nrow(df) * 100
    print(glue("Du lieu co {nrow(missing_df_all)} dong bi thieu, chiem {missing_percent}%"))
    return(missing_df_all)
}

check_outlier_function <- function(df, list_col = c("")) {
    if (list_col[1] == "") {
        list_col <- names(df)
    }
    outlier_df <- data.frame()
    for (col in list_col) {
        Q1 <- quantile(df[[col]], 0.25)
        Q3 <- quantile(df[[col]], 0.75)
        IQR <- Q3 - Q1
        outliers <- df[df[[col]] < (Q1 - 1.5 * IQR) | df[[col]] > (Q3 + 1.5 * IQR), ]
        n_out <- nrow(outliers)
        print(paste("So outlier cua cot ", col, ": ", n_out))
        outlier_df <- rbind(outlier_df, outliers)
    }
    print(paste("Tong so outlier cua df: ", nrow(unique(outlier_df))))
    outlier_percent <- nrow(outlier_df) / nrow(df) * 100
    print(glue("Phan tram outlier: {outlier_percent}%"))
    return(outlier_df)
}

delete_nan_function = function(df, list_col = c("")){
    if (list_col[1] == "") {
        list_col <- names(df)
    }
    # Xoá bỏ dữ liệu na
    df <- na.omit(df, list_col)
    # Kiểm tra lại số dòng và số cột sau khi đã loại bỏ na
    dim(df)
    return(df)
}

box_plot_function = function(df, list_col = c(""), full_plot = TRUE){
    if (list_col[1] == "") {
        list_col <- names(df)
    }
    for (col in list_col){
        boxplot(df[[col]], col = "skyblue", main = col)
    }
    if (full_plot == TRUE){
        boxplot(df, col = "skyblue", main = "Boxplot")
    }
}
# Function bootstrap trung bình
boot_mu_fun <- function(data, ind){
    data_new <- data[ind]
    out <- mean(data_new)
    return(out)
}
# Function bootstrap với khoảng tin cậy
mean_boot_ktc_fun <- function(data, ind){
    n <- length(data)
    data <- data[ind]
    mu_est <- mean(data, na.rm = TRUE)
    mu_var <- var(data, na.rm = TRUE)/n
    return(c(mu_est, mu_var))
}

# Hãy tính khoảng tin cậy cho trimmed mean (trung bình bị cắt bớt) với mức 15%,
mean_trim_boot_ktc_fun <- function(data, ind){
    n <- length(data)
    data <- data[ind]
    mu_est <- mean(data, trim = 0.15)
    mu_var <- var(data)/n
    return(c(mu_est, mu_var))
}

# Bootstrap cho sự khác biệt hai khoảng thời gian trung bình
boot_diff_mean_fun <- function(data, ind){
    data_new <- data[ind, c("Basic", "Extended")]
    out_diff_mean <- mean(data_new[["Basic"]]) - mean(data_new[["Extended"]])
    n = nrow(data_new)
    # Tính Corr
    res_diff_mean_boot = numeric(200)
    for (j in 1:200){
        id_boot_cor = sample(1:n, size = n, replace = TRUE)
        diff_mean_boot_2 = data_new[id_boot_cor,]
        res_diff_mean_boot[j] = mean(diff_mean_boot_2[["Basic"]]) - mean(diff_mean_boot_2[["Extended"]])
        # res_diff_mean_boot[j] = cor(corr_boot_2)[1,2]
    }
    out_var = var(res_diff_mean_boot)
    return(c(out_diff_mean,out_var))
}
# Bootstrap cho sự khác biệt số lượng calo trung bình
boot_diff_mean_calo_fun <- function(data, ind){
    data_new <- data[ind, c("VanillaCalories", "ChocolateCalories")]
    out_diff_mean <- mean(data_new[["VanillaCalories"]]) - mean(data_new[["ChocolateCalories"]])
    n = nrow(data_new)
    # Tính Corr
    res_diff_mean_boot = numeric(200)
    for (j in 1:200){
        id_boot_cor = sample(1:n, size = n, replace = TRUE)
        diff_mean_boot_2 = data_new[id_boot_cor,]
        res_diff_mean_boot[j] = mean(diff_mean_boot_2[["VanillaCalories"]]) - mean(diff_mean_boot_2[["ChocolateCalories"]])

    }
    out_var = var(res_diff_mean_boot)
    return(c(out_diff_mean,out_var))
}

# Bootstrap cho ỷ lệ của trung bình giá sách của hai lĩnh vực
boot_diff_mean_price_fun <- function(data, ind){
    data_new <- data[ind, c("Math_science", "Social")]
    out_diff_mean <- mean(data_new[["Math_science"]], na.rm = TRUE) / mean(data_new[["Social"]], na.rm = TRUE)
    n = nrow(data_new)
    # Tính Corr
    res_diff_mean_boot = numeric(200)
    for (j in 1:200){
        id_boot_cor = sample(1:n, size = n, replace = TRUE)
        diff_mean_boot_2 = data_new[id_boot_cor,]
        res_diff_mean_boot[j] = mean(diff_mean_boot_2[["Math_science"]], na.rm = TRUE) / mean(diff_mean_boot_2[["Social"]], na.rm = TRUE)
    }
    out_var = var(res_diff_mean_boot)
    return(c(out_diff_mean,out_var))
}
