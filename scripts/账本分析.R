# 输入：
# here("data", "raw", "Accounting-20240901.xlsx")
# here("data", "raw", "Budget.xlsx")
# 输出：
# 账目是否齐平
# 用处统计：本月花费、最近30天花费、预算、预算剩余
# 待实现：最近3月总收入总支出？、目前账目余额？

# 读取账单excel----
Accounting <- read_excel(here("data", "raw", "Accounting-20240901.xlsx"), sheet = 1) # 读取Excel文件中的第一个工作表到df1
Assets <- read_excel(here("data", "raw", "Accounting-20240901.xlsx"), sheet = 2) # 读取Excel文件中的第二个工作表到df2

# 核查账目----
Sum <- sum(Accounting$金额)  #计算账本收支总和
Personal_Assets <- sum(Assets$金额[Assets$所属 == "个人"], na.rm = TRUE) #计算个人总资产
epsilon <- 1e-10  # 设置一个很小的容差范围
Difference <- round(Personal_Assets - Sum, 2)
if (abs(Sum - Personal_Assets) < epsilon) {
  print("账本收支=个人总资产，账目齐平")
} else if (Sum > Personal_Assets) {
  print(paste("账本收支>个人总资产，账目不平，差值为", Difference))
} else {
  print(paste("账本收支<个人总资产，账目不平，差值为", Difference))
}

# 分类统计----
当前日期 <- Sys.Date() # 获取当前日期和30天前的日期
三十天前 <- 当前日期 - 30
天数差 <- as.numeric(当前日期 - as.Date("2024-09-01"))
月份差 <- 天数差 / 30.44
用处统计 <- Accounting %>% # 计算各项统计指标
  group_by(用处) %>%
  summarise(
    平均每月花费 = round(sum(金额) / 月份差, 2), # 平均每月金额总和
    本月花费 = sum(金额[floor_date(日期, "month") == floor_date(当前日期, "month")]), # 本月金额总和
    最近30天花费 = sum(金额[日期 >= 三十天前]), # 最近30天金额总和
    类型 = ifelse(sum(金额) < 0, "支出", "收入")  # 新增类型列
  ) %>%
  ungroup()
用处统计_支出 <- 用处统计 %>% 
  dplyr::filter(类型 == "支出") %>% 
  dplyr::select(-类型) %>%
  arrange(平均每月花费)
用处统计_收入 <- 用处统计 %>% 
  dplyr::filter(类型 == "收入") %>% 
  dplyr::select(-类型) %>%
  arrange(平均每月花费)

# 预算设置----
预算设置 <- read_excel(here("data", "raw", "Budget.xlsx"))
用处统计_支出 <- 用处统计_支出 %>% # 合并统计结果和预算设置
  left_join(预算设置, by = "用处") %>%
  mutate(
    预算剩余 = 每月预算 + 本月花费
  )
用处统计_收入 <- 用处统计_收入 %>% # 合并统计结果和预算设置
  left_join(预算设置, by = "用处") %>%
  mutate(
    预算剩余 = 每月预算 + 本月花费
  )

# 计算总和----
统计_支出 <- 用处统计_支出 %>%
  bind_rows(
    summarise(.,
      用处 = "支出总计",
      across(where(is.numeric), ~sum(., na.rm = TRUE)),
    )
  )
统计_收入 <- 用处统计_收入 %>%
  bind_rows(
    summarise(.,
      用处 = "收入总计",
      across(where(is.numeric), ~sum(., na.rm = TRUE)),
    )
  )

# 绘制饼图----
用处统计_支出 <- 用处统计_支出 %>% 
  mutate(绝对花费 = abs(平均每月花费)) %>% 
  arrange(desc(绝对花费)) %>%  # 按绝对花费降序排序
  mutate(用处 = factor(用处, levels = 用处))  # 固定因子顺序（按当前排序）
用处统计_收入 <- 用处统计_收入 %>% 
  mutate(绝对花费 = abs(平均每月花费)) %>% 
  arrange(desc(绝对花费)) %>%  # 按绝对花费降序排序
  mutate(用处 = factor(用处, levels = 用处))  # 固定因子顺序（按当前排序）
饼图_支出 <- ggplot(用处统计_支出, aes(x = "", y = 绝对花费, fill = 用处)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "支出分类占比（按绝对值降序）", fill = "用处") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(
    aes(label = paste0(round(绝对花费 / sum(绝对花费) * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5  # 可选：调整标签大小
  )
饼图_收入 <- ggplot(用处统计_收入, aes(x = "", y = 绝对花费, fill = 用处)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "收入分类占比（按绝对值降序）", fill = "用处") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(
    aes(label = paste0(round(绝对花费 / sum(绝对花费) * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5  # 可选：调整标签大小
  )
ggsave( # 导出为 JPG
  filename = here("output", "figures", "支出饼图.jpg"),  # 文件名
  plot = 饼图_支出,              # 要保存的图形对象（默认保存最后绘制的图）
  width = 8,                # 宽度（英寸）
  height = 6,               # 高度（英寸）
  dpi = 300,                # 分辨率（每英寸点数）
  device = "jpeg"           # 指定格式为 JPG
)
ggsave( # 导出为 JPG
  filename = here("output", "figures", "收入饼图.jpg"),  # 文件名
  plot = 饼图_收入,              # 要保存的图形对象（默认保存最后绘制的图）
  width = 8,                # 宽度（英寸）
  height = 6,               # 高度（英寸）
  dpi = 300,                # 分辨率（每英寸点数）
  device = "jpeg"           # 指定格式为 JPG
)
print(饼图_支出)
