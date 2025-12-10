backup_workspace <- function() {
  # 创建备份目录
  backup_dir <- here::here("backups")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  
  # 生成新备份
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_dir, paste0("backup_", timestamp, ".RData"))
  save.image(file = backup_file)
  
  # 获取所有备份并按时间排序
  files <- list.files(backup_dir, "\\.RData$", full.names = TRUE)
  file_mtime <- file.info(files)$mtime
  sorted_files <- files[order(file_mtime, decreasing = TRUE)]  # 从新到旧排序
  
  # 计算保留范围
  seven_days_ago <- Sys.time() - 7 * 24 * 60 * 60
  is_recent <- file_mtime >= seven_days_ago  # 7天内标记
  keep_min <- max(10 - sum(is_recent), 0)    # 需补充的最小数量
  
  # 保留文件 = 7天内所有 + 必要的最新旧备份
  keep_files <- c(
    sorted_files[is_recent],                 # 所有7天内
    sorted_files[!is_recent][seq_len(keep_min)]  # 补充旧备份
  )
  
  # 删除其他文件
  delete_files <- setdiff(files, keep_files)
  if (length(delete_files) > 0) {
    removed <- file.remove(delete_files)
    message("清理备份：删除 ", sum(removed), " 个文件，保留 ", length(keep_files), " 个（7天内:", sum(is_recent), "+补充:", keep_min, "）")
  }
  
  message("备份完成：", backup_file)
}