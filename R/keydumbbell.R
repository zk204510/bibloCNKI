# 加载必要的库
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#' Generate Keyword Trend Dumbbell Plot
#'
#' Creates a dumbbell plot visualizing the temporal distribution of top keywords in academic publications.
#'
#' @param input Character. Path to the input Excel file containing publication records.
#'        The file must include columns named 'Keyword-关键词' (semicolon-separated keywords)
#'        and 'PubTime-发表时间' (publication dates in YYYY-MM-DD format).
#' @param output Character. Path for saving the output visualization image (supported formats:
#'        png, jpg, pdf, etc.) Recommended size: 15cm(width) × 18cm(height) at 200 DPI.
#'
#' @return Invisibly returns the ggplot object. Primarily used for saving the plot to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate plot for 1992-2025 period
#' keydumbbell("publications.xlsx", "keyword_trend.png")
#'
#' # Custom year range (modify start_year/end_year in function)
#' keydumbbell("data.xlsx", "trend_analysis.pdf")
#' }
#'
#' @details The function performs these operations:
#' 1. Extracts and cleans keywords from semicolon-separated values
#' 2. Filters top 45 most frequent keywords
#' 3. Calculates temporal statistics (Q1, Median, Q3) for each keyword
#' 4. Generates a dumbbell plot with:
#'    - Gray segments showing keyword lifespan (Q1-Q3)
#'    - Light green points for first quartile (Q1)
#'    - Dark red points for third quartile (Q3)
#'    - Blue-gray midpoint markers sized by keyword frequency
#'
#' @note
#' - Default temporal range: 1992-2025 (modify start_year/end_year in code for customization)
#' - Input date format must be compatible with as.Date()
#' - Requires readxl, dplyr, tidyr, ggplot2, and stringr packages
keydumbbell <- function(input, output) {
  # Function body remains unchanged
}
keydumbbell = function(input, output){
  # 读取 Excel 文件内容
  file_content <- read_excel(input)



  # 查看Excel文件的列名
  colnames(file_content)

  # 提取 Keyword 和 PubTime 列，并去重
  keywords_data <- file_content %>%
    select(`Keyword-关键词`) %>%
    filter(!is.na(`Keyword-关键词`)) %>%
    distinct(`Keyword-关键词`, .keep_all = TRUE)  # 去除重复的关键词行

  year_data <- file_content %>%
    select(`PubTime-发表时间`) %>%
    filter(!is.na(`PubTime-发表时间`))

  # 添加行号列到 keywords_data 和 year_data
  keywords_data <- keywords_data %>%
    mutate(row_number = row_number())

  year_data <- year_data %>%
    mutate(row_number = row_number())

  # 找出最大关键词数量
  max_keywords <- keywords_data %>%
    mutate(keyword_count = str_count(`Keyword-关键词`, ";") + 1) %>%
    pull(keyword_count) %>%
    max()

  # 将 'Keywords' 按照 ';' 分割为多个关键词列
  keywords_split <- keywords_data %>%
    separate(`Keyword-关键词`, into = paste("keyword", 1:max_keywords, sep = ""), sep = ";", remove = FALSE, fill = "right")

  # 合并 'Keywords' 和 'Year' 数据
  merged_data <- keywords_split %>%
    left_join(year_data, by = "row_number") %>%
    select(Year = `PubTime-发表时间`, starts_with("keyword"))

  # 将所有关键词合并为一列
  all_keywords <- merged_data %>%
    gather(key = "keyword_type", value = "keyword", starts_with("keyword")) %>%
    filter(!is.na(keyword)) %>%
    pull(keyword)

  # 统计每个关键词出现的频率
  keyword_counts <- tibble(keyword = all_keywords) %>%
    count(keyword, sort = TRUE)

  # 选择前45个最频繁的关键词
  top_50_keywords <- keyword_counts %>%
    slice_head(n = 45)

  # 过滤出前45个关键词的数据，确保所有关键词列在长格式下都有一个统一的"keyword"列
  df_top_keywords <- merged_data %>%
    gather(key = "keyword_type", value = "keyword", starts_with("keyword")) %>%
    filter(keyword %in% top_50_keywords$keyword)

  # 定义起始年份和结束年份
  start_year <- 1992
  end_year <- 2025

  # 将年份转换为数字格式，并筛选出在指定年份范围内的数据
  df_top_keywords$Year <- as.numeric(format(as.Date(df_top_keywords$Year, format = "%Y-%m-%d"), "%Y"))
  df_top_keywords <- df_top_keywords %>%
    filter(Year >= start_year & Year <= end_year)

  # 计算每个关键词的统计信息
  df_stats <- df_top_keywords %>%
    group_by(keyword) %>%
    summarise(
      Q1 = quantile(Year, 0.25),  # 计算四分之一位数
      Median = median(Year),      # 计算中位数
      Q3 = quantile(Year, 0.75),  # 计算四分之三位数
      Count = n(),                # 计算每个关键词出现的次数
      .groups = 'drop'
    ) %>%
    arrange(Q1)                   # 按照 Q1 进行排序，最早的年份出现在下方

  # 绘制哑铃图

  p1=ggplot(df_stats, aes(x = Q1, xend = Q3, y = factor(keyword, levels = keyword))) +
    # 绘制线段
    geom_segment(aes(x = Q1, xend = Q3), color = "gray", linewidth = 1) +
    # 绘制左端球
    geom_point(aes(x = Q1), color = "#90EE90", size = 2) +
    # 绘制右端球
    geom_point(aes(x = Q3), color = "#8B0000", size = 2) +
    # 绘制中端球，大小反映出现频率
    geom_point(aes(x = Median, size = Count/2), color = "#A3B9C4") +  # 中端球大小基于频率

    theme_minimal() +
    labs(
      x = "Year",
      y = "Keywords",
      title = paste("Keywords Trend Over Years (", start_year, "-", end_year, ")", sep = "")
    ) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.y = element_text(size = 6)
    ) +
    scale_color_brewer(palette = "Set3")
  ggsave(filename = output, plot = p1, height = 18, width = 15, units = "cm", dpi = 200)
}

