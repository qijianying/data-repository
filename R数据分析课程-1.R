#找到工作路径
getwd()
setwd()

?emmeans
data <- data.frame(Name = c("John", "Anna"), Age = c(25, 28))
data

#--------------------------------------------------------------------------------
#0.数据导入导出
# 1. 从CSV文件导入数据
data_csv <- read.csv("R-data.csv")  # 读取当前工作目录下的data.csv文件
data_csv
head(data_csv)
head(data_csv[, 1:10])

data_csv1 <- read.csv("data.csv")  # 读取当前工作目录下的data.csv文件
data_csv1
#--------------------------------------------------------------------------------
# 2. 从文本文件导入数据
data_txt <- read.table("data.txt", sep = "\t", header = TRUE)  # 使用制表符作为分隔符读取TXT文件
#--------------------------------------------------------------------------------
# 3. 从Excel文件导入数据
# 首先安装并加载readxl包
install.packages("readxl")  # 安装readxl包（如果未安装）
library(readxl)
data_excel <- read_excel("R-dataex.xlsx", sheet = 1)  # 读取Excel文件的第一个工作表
data_excel
###<chr>：代表该列的数据类型是字符型（character）。在你的数据里，它是 treatment1 列，意味着这列中的数据是文本（如 "CT"、"RT"、"NT" 等）。
###<dbl>：代表该列的数据类型是双精度浮点数（double），即数值型数据。在你的数据里，block 和其他数值列（如 antibiotic.efflux）使用的是这种数据类型。
###<dttm>：代表该列的数据类型是日期时间（datetime），通常格式为年-月-日 时:分:秒。在你的数据里，这个类型用于 season 列，表示具体的日期和时间。

# 4. 从RDS文件导入数据
data_rds <- readRDS("data.rds")  # 读取RDS文件，该文件保存了R对象



#--------------------------------------------------------------------------------
# 1. 将数据导出为CSV文件
write.csv(data_csv, "output.csv", row.names = FALSE)  # 导出为CSV文件，不包括行名

# 2. 将数据导出为文本文件
write.table(data_csv, "output.txt", sep = "\t", row.names = FALSE)  # 导出为制表符分隔的文本文件

# 3. 将数据导出为Excel文件
# 首先安装并加载openxlsx包
install.packages("openxlsx")  # 安装openxlsx包（如果未安装）
library(openxlsx)
write.xlsx(data_csv, "output.xlsx", sheetName = "Sheet1")  # 导出为Excel文件，并指定工作表名称





#--------------------------------------------------------------------------------
#1.异常值
# Load necessary libraries
library(ggplot2)

# Assume the data is already loaded into R as a data frame
# Select the column for analysis
data <- read.csv("R-data.csv")
efflux <- data$antibiotic.efflux

# Calculate Z-scores
z_scores <- (efflux - mean(efflux, na.rm = TRUE)) / sd(efflux, na.rm = TRUE)

# Identify outliers using a Z-score threshold of 3
outliers <- efflux[abs(z_scores) > 3]
print(outliers)

# Visualize the distribution and outliers using a boxplot
boxplot(efflux, main="Boxplot for antibiotic.efflux", ylab="Efflux Value")

# Handling outliers: Example by removing outliers
cleaned_efflux <- efflux[abs(z_scores) <= 3]
print("Data after removing outliers:")
print(cleaned_efflux)
boxplot(cleaned_efflux, main="Boxplot for antibiotic.efflux", ylab="Efflux Value")

#--------------------------------------------------------------------------------
#2.正态分布 生成QQ图。正态分布检验.组间？组内？
qqnorm(efflux, main="QQ Plot for antibiotic.efflux")
qqline(efflux, col = "red")
antibiotic.inactivation <- data$antibiotic.inactivation
qqnorm(antibiotic.inactivation, main="QQ Plot for antibiotic.efflux")
qqline(antibiotic.inactivation, col = "red")

# Shapiro-Wilk检验
shapiro_test <- shapiro.test(efflux)
print(shapiro_test)
shapiro_test1 <- shapiro.test(cleaned_efflux)
shapiro_test1
inactivation <- data$antibiotic.inactivation
shapiro_test2 <- shapiro.test(inactivation)
print(shapiro_test2)

# 不满足正态分布：对数转换
log_efflux <- log(efflux)
# Box-Cox转换
library(MASS)
boxcox_result <- boxcox(lm(efflux ~ 1), plotit = FALSE)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
transformed_efflux <- (efflux^lambda - 1) / lambda
shapiro_test3 <- shapiro.test(transformed_efflux)
print(shapiro_test3)# 仍然不满足，放弃参数检验

#--------------------------------------------------------------------------------
#3.方差分析和齐性检验 加载必要的库
library(car)  # 加载car包用于执行Levene's检验
# 加载数据集
data <- read.csv("R-data.csv")  # 读取CSV文件到R的数据框中
data
alteration <- data$antibiotic.target.alteration
# 执行方差分析（ANOVA）
# 将 'antibiotic.efflux' 作为因变量，将 'treatment1' 作为自变量
anova_model <- aov(alteration ~ treatment1, data = data)
# 提取残差
residuals <- residuals(anova_model)
# 绘制残差的QQ图
qqnorm(residuals, main="QQ Plot of Residuals")
qqline(residuals, col = "red")
# 使用Shapiro-Wilk检验检查残差的正态性
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)
# 显示ANOVA结果
summary(anova_model)  # 输出方差分析的摘要，查看各组之间的均值差异是否显著
# 执行Levene's检验（方差齐性检验）
# 检验各组间的方差是否相等，这是ANOVA的一个假设前提
leveneTest(alteration ~ treatment1, data = data)

# 解释：
# 如果Levene's检验的p值大于显著性水平（例如0.05），
# 则可以认为各组的方差齐性假设成立，方差齐性检验通过。
# 这个警告信息表示在进行Levene's检验时，group变量被强制转换为因子类型。这通常不是
# 一个错误，只是R在提醒你它已经进行了类型转换。如果你的group变量本来应该是因子
# （分类变量），那么这个警告可以忽略。

#修改后方差分析--------------------------------------------------------------------------------
# 加载数据集
data <- read.csv("R-data.csv")

# 提取分组变量和因变量
efflux <- data$antibiotic.efflux  # 因变量：抗生素外排
groups <- unique(data$treatment1)  # 获取所有组的名称

# 对每个组内的数据进行正态性检验
for (group in groups) {
  group_data <- efflux[data$treatment1 == group]  # 提取当前组的数据
  
  # 生成QQ图来检查组内数据的正态性
  qqnorm(group_data, main=paste("QQ Plot for group", group))
  qqline(group_data, col = "red")
  
  # 使用Shapiro-Wilk检验进行正态性检验
  shapiro_test <- shapiro.test(group_data)
  print(paste("Shapiro-Wilk test for group", group, "p-value:", shapiro_test$p.value))
  
  # 如果Shapiro-Wilk检验的p值小于0.05，则组内数据可能不符合正态分布
  if (shapiro_test$p.value < 0.05) {
    print(paste("Group", group, "data is likely not normally distributed."))
  } else {
    print(paste("Group", group, "data appears to be normally distributed."))
  }
}

# 如果某些组不满足正态分布，可以考虑数据转换
# 示例：对抗生素外排数据进行对数转换
log_efflux <- log(efflux)

# 还可以使用Box-Cox转换来进一步调整数据分布
library(MASS)
boxcox_result <- boxcox(lm(efflux ~ 1), plotit = FALSE)  # Box-Cox转换寻找最佳lambda值
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
transformed_efflux <- (efflux^lambda - 1) / lambda

# 对转换后的数据进行Shapiro-Wilk检验
for (group in groups) {
  group_data <- transformed_efflux[data$treatment1 == group]  # 提取当前组的数据
  
  # Shapiro-Wilk检验
  shapiro_test <- shapiro.test(group_data)
  print(paste("Shapiro-Wilk test for transformed data in group", group, "p-value:", shapiro_test$p.value))
}

# 解释：
# - 如果转换后的数据仍不满足正态性，可以考虑使用非参数检验方法，而不是依赖参数检验（如ANOVA）。



#相关分析--------------------------------------------------------------------------------
#相关分析--------------------------------------------------------------------------------
#相关分析--------------------------------------------------------------------------------
#相关分析--------------------------------------------------------------------------------
# 加载数据集
# 加载所需的库
library(ggplot2)
library(reshape2)

# 计算antibiotic.inactivation和antibiotic.target.alteration之间的相关性
correlation_matrix <- cor(data_csv[, c("antibiotic.inactivation", "antibiotic.target.alteration")])
correlation_matrix
# 将相关性矩阵转化为长格式，方便绘制热图
melted_correlation <- melt(correlation_matrix)
melted_correlation
# 绘制相关分析热图
ggplot(data = melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed()


# 加载所需的库
library(ggplot2)
library(reshape2)

# 选择所需的列进行相关分析
selected_columns <- data_csv[, c("antibiotic.efflux", "antibiotic.inactivation", "antibiotic.target.alteration",
                                 "antibiotic.target.protection", "antibiotic.target.replacement", 
                                 "reduced.permeability.to.antibiotic", "g__unclassified_p__Candidatus_Aenigmarchaeota")]
selected_data <- data_csv[, 5:9]
# 对每列进行Shapiro-Wilk正态性检验
for (i in 1:ncol(selected_data)) {
  column_name <- colnames(selected_data)[i]
  shapiro_test <- shapiro.test(selected_data[, i])
  
  # 输出每列的Shapiro-Wilk检验结果
  cat("Column:", column_name, "\n")
  print(shapiro_test)
  cat("\n")
}
# 代码说明：
# 读取数据：read.csv()函数用于读取文件R-data.csv。
# 选择列：data[, 5:9]选择第5到第9列。
# Shapiro-Wilk检验：shapiro.test()函数逐列计算Shapiro-Wilk检验的p值。
# 输出结果：cat()和print()函数用于打印每列的检验结果。
# 检验结果解释：
# 如果某列数据的p值 > 0.05，则表示该列数据符合正态分布。
# 如果p值 ≤ 0.05，则表示该列数据显著偏离正态分布。
# 提示：
# Shapiro-Wilk检验的样本数量不能超过5000个数据点，如果数据量过大，可以使用其他正态性检验（如Kolmogorov-Smirnov检验）。如果数据样本量较少，则Shapiro-Wilk检验是一个很好的选择。







library(Hmisc)  # 包含 rcorr 函数
# 使用 Hmisc 包中的 rcorr 函数计算相关性矩阵和 p 值矩阵
correlation_results <- rcorr(as.matrix(selected_columns), type = "pearson")

# 提取相关性矩阵和 p 值矩阵
correlation_matrix <- correlation_results$r
p_value_matrix <- correlation_results$P

# 输出相关性矩阵
print("Correlation Matrix:")
print(round(correlation_matrix, 3))  # 保留三位小数的相关性矩阵

# 输出 p 值矩阵
print("P-Value Matrix:")
print(round(p_value_matrix, 3))  # 保留三位小数的 p 值矩阵


# 计算相关性矩阵
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# 将相关性矩阵转化为长格式
melted_correlation <- melt(correlation_matrix)

# 绘制相关分析热图并美化
ggplot(data = melted_correlation, aes(Var1, Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +  # 添加白色的网格线
  scale_fill_gradient2(low = "#6D9EC1", high = "#E46726", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name="Correlation") +  # 调整颜色渐变
  theme_minimal(base_size = 15) +  # 设置基础字体大小
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # 调整X轴标签的角度和对齐
        axis.text.y = element_text(size = 12),  # 调整Y轴标签的大小
        panel.grid.major = element_blank(),  # 移除主要网格线
        panel.grid.minor = element_blank(),  # 移除次要网格线
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold")) +  # 添加标题样式
  geom_text(color = "black", size = 4) +  # 在每个方格上显示相关系数数值
  labs(title = "Correlation Heatmap", x = "", y = "") +  # 添加标题和去除XY轴标签
  coord_fixed()  # 保持方格为正方形



