
# 加载必要包
library(readxl)
library(leaflet)
library(leaflet.extras)
library(httr)
library(jsonlite)
#' Generate Geographic Heatmap of Research Institutions
#'
#' Creates an interactive heatmap visualization showing geographic distribution of research institutions
#' using Gaode Map API for geocoding and Leaflet for mapping.
#'
#' @param input Character. Path to the input Excel file containing institution data. The file must have
#'        a column named 'Organ-单位' with institution names separated by semicolons.
#' @param API Character. Gaode Map API key for geocoding service authentication.
#' @param output Character. Path to save the output HTML file containing the interactive map.
#'
#' @return An invisible Leaflet object containing the generated heatmap visualization. The object includes:
#'         - Base satellite imagery layer from Geovis Earth
#'         - Heatmap layer showing institution density
#'         - Interactive map controls
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage (requires valid API key)
#' geoheatmap(
#'   input = "institutions.xlsx",
#'   API = "your_gaode_api_key",
#'   output = "heatmap.html"
#' )
#'
#' # Save output as HTML file
#' library(htmlwidgets)
#' saveWidget(heatmap_map, "heatmap.html")
#' }
#'
#' @details
#' This function performs the following operations:
#' 1. Reads institution data from Excel file
#' 2. Extracts and cleans institution names
#' 3. Converts addresses to coordinates using Gaode Map API
#' 4. Filters valid geographic coordinates
#' 5. Generates interactive heatmap visualization using Leaflet
#' 6. Returns/saves the visualization as HTML
#'
#' @note
#' - The Geovis Earth base map uses a default token for demonstration purposes. For production use,
#'   obtain your own token from https://www.geovisearth.com/
#' - Gaode Map API requires internet connection and valid API credentials
#' - Processing time depends on number of unique institutions and API response speed
geoheatmap <- function(input, API, output) {
  # Function body remains unchanged
}
geoheatmap = function(input,API, output){

  # 读取Excel数据
  df <- read_excel(input, sheet = "Sheet1")

  # 提取发表单位列并处理分隔符（示例数据用';'分隔）
  organs <- unlist(strsplit(df$`Organ-单位`, ";\\s*"))
  unique_organs <- unique(organs)  # 去重

  # 高德地图地理编码函数
  geocode_gaode <- function(address, key) {
    base_url <- "https://restapi.amap.com/v3/geocode/geo"
    response <- GET(base_url, query = list(key = key, address = address))
    content <- content(response, as = "parsed", type = "application/json")
    if (content$status == "1" && length(content$geocodes) > 0) {
      location <- content$geocodes[[1]]$location
      coords <- as.numeric(strsplit(location, ",")[[1]])
      data.frame(lng = coords[1], lat = coords[2])
    } else {
      data.frame(lng = NA, lat = NA)
    }
  }

  # 替换为你的高德API密钥
  gaode_key <- API

  # 获取经纬度（可能需要较长时间）
  coords_list <- lapply(unique_organs, function(organ) {
    geocode_gaode(organ, gaode_key)
  })
  coords_df <- do.call(rbind, coords_list)

  # 过滤无效坐标
  valid_coords <- coords_df[complete.cases(coords_df), ]

  # 创建地图底图（使用中科星图服务）
  geovis_token <- "6f6a548b30063f2aeeb149a5589f1362076fe636a113c690fd3864b14fe01d3e"
  geovis_satellite_url <- paste0("https://tiles1.geovisearth.com/base/v1/img/{z}/{x}/{y}?format=webp&tmsIds=w&token=", geovis_token)
  geovis_label_url <- paste0("https://tiles1.geovisearth.com/base/v1/cia/{z}/{x}/{y}?format=webp&tmsIds=w&token=", geovis_token)

  base_map <- leaflet() %>%
    addTiles(urlTemplate = geovis_satellite_url) %>%
    addTiles(urlTemplate = geovis_label_url)

  # 添加热力图层
  heatmap_map <- base_map %>%
    addHeatmap(
      data = valid_coords,
      lng = ~lng,
      lat = ~lat,
      radius = 15,
      blur = 20,
      max = 0.8,
      gradient = c("blue", "cyan", "yellow", "red"),
      cellSize = 10,
      minOpacity = 0.5
    )

  heatmap_map  # 显示地图

}


