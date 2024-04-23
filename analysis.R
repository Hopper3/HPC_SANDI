library(oro.nifti)
library(car)
library(effsize)
# file path
base_path <- "/Users/hopperliu/Desktop/ISMRM/"
exp_path <- paste0(base_path, "AD") # experimental group
ctrl_path <- paste0(base_path, "B6") # control group

exp_samples <- list.dirs(exp_path, recursive = FALSE)
#print(exp_samples)
ctrl_samples <- list.dirs(ctrl_path, recursive = FALSE)
#print(crtl_samples)
# read all nii.gz file
#exp_nii_files <- list.files(exp_samples,pattern = "*.nii.gz",full.names = FALSE)
#print(exp_nii_files)
# read and process exp_data
extract_values_with_label <- function(sample_folder_path, 
                                      file_pattern,
                                      label_value) {
  #read named file
  nii_file <- list.files(sample_folder_path, 
                          pattern = file_pattern, 
                          full.names = TRUE)
  #read label file
  label_file_path <- list.files(sample_folder_path, 
                                pattern = "lbl2\\.nii\\.gz$", 
                                full.names = TRUE)[1]
  label_volume <- readNIfTI(label_file_path, reorient = FALSE)
  
  dwi_volume <- readNIfTI(nii_file, reorient = FALSE)
  masked_values <- dwi_volume[label_volume == label_value]
 
  return(masked_values)
}


#exp_ad vs ctrl_ad
result_exp_ad <- list()
result_ctrl_ad <- list()
for(i in 1:5){
  result1 = extract_values_with_label(exp_samples[i], "_ad\\.nii\\.gz$", 5)
  restlt2 = extract_values_with_label(ctrl_samples[i], "_ad\\.nii\\.gz$", 5)
  result_exp_ad[[i]] <- result1
  result_ctrl_ad[[i]] <- restlt2
}
result_exp_ad <- unlist(result_exp_ad)
result_ctrl_ad <- unlist(result_ctrl_ad)

# 可视化
hist(result_exp_ad, main="Histogram for Experiment Group (AD)", xlab="Value")
# QQ图
qqnorm(result_exp_ad)
qqline(result_exp_ad)

# Kolmogorov-Smirnov检验
ks_test_exp <- ks.test(result_exp_ad, "pnorm", mean=mean(result_exp_ad), sd=sd(result_exp_ad))
print(paste("Kolmogorov-Smirnov Test p-value for Experiment Group (AD):", ks_test_exp$p.value))

# 方差齐性检验（Levene's Test）
# 假设result_ctrl_ad是对照组的数据
levene_result <- leveneTest(c(result_exp_ad, result_ctrl_ad), factor(c(rep(1, length(result_exp_ad)), rep(2, length(result_ctrl_ad)))))
print(paste("Levene's Test p-value:", levene_result[1, "Pr(>F)"]))

# 可视化
hist(result_ctrl_ad, main="Histogram for Control Group (AD)", xlab="Value")
# QQ图
qqnorm(result_ctrl_ad)
qqline(result_ctrl_ad)

# Kolmogorov-Smirnov检验
ks_test_ctrl <- ks.test(result_ctrl_ad, "pnorm", mean=mean(result_ctrl_ad), sd=sd(result_ctrl_ad))
print(paste("Kolmogorov-Smirnov Test p-value for Contral Group (AD):", ks_test_ctrl$p.value))

t_result <- t.test(result_exp_ad, result_ctrl_ad)

# 输出t检验结果
print(t_result)

mean_exp = mean(result_exp_ad)
mean_ctrl = mean(result_ctrl_ad)
print(paste("Mean Value of Experiment Group: ", mean_exp))
print(paste("Mean Value of Control Group: ", mean_ctrl))
# 计算实验组的标准差
sd_exp = sd(result_exp_ad)

# 计算对照组的标准差
sd_ctrl = sd(result_ctrl_ad)

# 输出标准差
print(paste("Standard Deviation of Experiment Group: ", sd_exp))
print(paste("Standard Deviation of Control Group: ", sd_ctrl))
test_result <- wilcox.test(result_exp_ad, result_ctrl_ad, alternative = "two.sided")

# 输出测试结果
print(test_result)
# 假设 exp_data 和 ctrl_data 是您的实验组和对照组数据
cohen_d_result <- cohen.d(result_exp_ad, result_ctrl_ad, conf.level = 0.95)

# 输出Cohen's d和95%置信区间
print(cohen_d_result)

delta <- cliff.delta(result_exp_ad, result_ctrl_ad, conf.level = 0.95)
print(delta)