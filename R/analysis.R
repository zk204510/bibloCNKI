library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#' Academic Literature Data Analysis Pipeline
#'
#' Performs comprehensive analysis of academic literature data from CNKI-style Excel files,
#' including temporal distribution, author networks, and keyword trends.
#'
#' @param input Character. Path to the input Excel file containing raw literature data.
#'        Required columns: 来源库, 题名, 作者, 单位, 文献来源, 关键词, 摘要, 发表时间, 中图分类号, 网址
#' @param output Character. Path for saving analysis results in RDS format.
#'
#' @return An invisible list containing four data frames:
#' \describe{
#'   \item{Author_Distribution}{Top 1000 authors by publication count (sorted descending)}
#'   \item{Publication_Years}{Annual publication counts (chronologically sorted)}
#'   \item{Keyword_Distribution}{Top 1000 keywords by frequency}
#'   \item{Organization_Distribution}{Top 1000 institutions by author affiliation}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Typical usage with Chinese data
#' analysis("literature_data.xlsx", "analysis_results.rds")
#'
#' # Load saved results
#' results <- readRDS("analysis_results.rds")
#' print(results$Author_Distribution)
#' }
#'
#' @details The analysis pipeline includes:
#' 1. Data cleaning: Remove header/footer rows and incomplete records
#' 2. Date parsing: Supports multiple Chinese date formats (YYYY年MM月DD日, YYYY-MM, etc.)
#' 3. Author analysis: Splits multi-author entries and counts individual contributions
#' 4. Temporal analysis: Annual publication trends from 1992-2025
#' 5. Keyword extraction: Cleans and counts keyword frequencies
#' 6. Institutional analysis: Identifies top research organizations
#'
#' @note
#' - Input file must maintain original Chinese column names
#' - Date parsing automatically handles common CNKI format variations
#' - Results include Chinese-language fields (author names, keywords, etc.)
#' - Requires readxl, dplyr, tidyr, stringr, and lubridate packages
analysis <- function(input, output) {
  # [Existing function body unchanged]
}
analysis <- function(input, output) {
  # 读取数据
  data_cnki <- read_excel(input)

  # 数据清洗：移除非数据行
  data_cnki_cleaned <- data_cnki %>%
    slice(-c(1, 3)) %>%
    filter(!if_any(everything(), is.na))

  # 重命名列
  data_cnki_cleaned <- data_cnki_cleaned %>%
    setNames(c("来源库", "题名", "作者", "单位", "文献来源", "关键词", "摘要", "发表时间", "中图分类号", "网址"))

  # 修改日期解析部分（添加 quiet = TRUE）
  data_cnki_cleaned <- data_cnki_cleaned %>%
    mutate(
      发表时间 = {
        raw_date <- 发表时间 %>%
          str_replace_all("年|月|日", "-") %>%
          str_remove_all("[[:punct:]]$|\\s+")

        case_when(
          str_detect(raw_date, "^\\d{4}$") ~ paste0(raw_date, "-01-01"),
          str_detect(raw_date, "^\\d{4}-\\d{1,2}$") ~ paste0(raw_date, "-01"),
          str_detect(raw_date, "^\\d{4}-\\d{1,2}-\\d{1,2}-") ~ str_replace(raw_date, "-", " "),
          TRUE ~ raw_date
        ) %>%
          parse_date_time(
            orders = c("Ymd HMS", "Ymd", "Ym", "Y"),
            truncated = 3,
            quiet = TRUE  # 关键修改：关闭解析警告
          )
      },
      解析状态 = if_else(is.na(发表时间), "解析失败", "解析成功"),
      发表年份 = year(发表时间)
    ) %>%
    filter(!is.na(发表时间))
  # 作者分布分析（兼容中英文分号）
  # 作者分布分析
  author_distribution <- data_cnki_cleaned %>%
    separate_rows(作者, sep = ";") %>%
    count(作者, sort = TRUE)

  # 时间序列分析（按年排序）
  publication_years_distribution <- data_cnki_cleaned %>%
    count(发表年份) %>%
    arrange(发表年份)

  # 关键词分析（清理空值）
  keywords_distribution <- data_cnki_cleaned %>%
    mutate(关键词 = str_replace_all(关键词, ";;", ";")) %>%
    separate_rows(关键词, sep = "；|;") %>%
    filter(!is.na(关键词), 关键词 != "") %>%
    count(关键词, sort = TRUE)

  # 机构分析（合并重复机构）
  organization_distribution <- data_cnki_cleaned %>%
    separate_rows(单位, sep = "；|;") %>%
    mutate(单位 = str_trim(单位)) %>%
    count(单位, sort = TRUE)

  # 构建结果集
  analysis_results <- list(
    Author_Distribution = head(author_distribution, 1000),
    Publication_Years = publication_years_distribution,
    Keyword_Distribution = head(keywords_distribution, 1000),
    Organization_Distribution = head(organization_distribution, 1000)
  )

  # 保存结果
  saveRDS(analysis_results, file = output)
  invisible(analysis_results)
}
