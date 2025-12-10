总和行 <- data.frame(
  用处 = "总计",
  平均每月花费 = sum(用处统计$平均每月花费, na.rm = TRUE),
  本月花费 = sum(用处统计$本月花费, na.rm = TRUE),
  最近30天花费 = sum(用处统计$最近30天花费, na.rm = TRUE),
  每月预算 = sum(用处统计$每月预算, na.rm = TRUE),
  预算剩余 = sum(用处统计$预算剩余, na.rm = TRUE)
)
用处统计 <- bind_rows(用处统计, 总和行)