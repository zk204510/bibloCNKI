library(readxl)
library(ggplot2)


#' Generate Cumulative Article Count Plot Over Years
#'
#' This function reads an Excel file containing publication dates, calculates cumulative article counts per year,
#' and generates a line plot visualizing the trend. The resulting plot is saved as an image file.
#'
#' @param input Character. Path to the input Excel file. Must contain a column named 'PubTime-发表时间' with dates.
#' @param output Character. Path where the output image (e.g., .jpg/.png) should be saved.
#'
#' @return Invisibly returns the ggplot object of the cumulative count plot. Primarily used for saving the plot to disk.
#' @export
#'
#' @examples
#' \donttest{
#' # 使用包内置测试数据
#' example_file <- system.file("extdata", "test_data.xlsx", package = "wxjlst3")
#' if (file.exists(example_file)) {
#'   output_file <- tempfile(fileext = ".jpg")
#'   cumulative(example_file, output_file)
#' }
#' }
cumulative <- function(input, output) {
  # 检查输入文件是否存在
  if (!file.exists(input)) {
    stop("Input file does not exist: ", sQuote(input))
  }

  # 读取 Excel 文件内容
  file_content <- read_excel(input)

  # 检查是否存在 "PubTime-发表时间" 列
  if (!"PubTime-发表时间" %in% colnames(file_content)) {
    stop("Excel 文件中未找到 'PubTime-发表时间' 列。请确保年份存储在正确的列中。")
  }

  file_content$`PubTime-发表时间` <- as.integer(format(as.Date(file_content$`PubTime-发表时间`), "%Y"))
  years <- file_content$`PubTime-发表时间`

  # 确保年份是整数类型
  if (!is.numeric(years)) {
    stop("列 'PubTime-发表时间' 不是数值类型。请检查数据格式。")
  }

  start_year <- min(years, na.rm = TRUE)
  end_year <- max(years, na.rm = TRUE)
  years <- years[years >= start_year & years <= end_year]

  year_counts <- table(factor(years, levels = start_year:end_year))
  year_data <- data.frame(
    Year = as.integer(names(year_counts)),
    Count = as.integer(year_counts)
  )

  year_data <- year_data[order(year_data$Year), ]
  year_data$CumulativeCount <- cumsum(year_data$Count)

  p5 <- ggplot(year_data, aes(x = Year, y = CumulativeCount)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(
      title = paste("Cumulative Number of Articles from", start_year, "to", end_year),
      x = "Years",
      y = "Cumulative Number of Articles"
    ) +
    theme_minimal()

  if (!dir.exists(dirname(output))) {
    dir.create(dirname(output), recursive = TRUE)
  }

  ggsave(output, p5, width = 15, height = 8, units = "cm", dpi = 150)
  invisible(p5)
}
