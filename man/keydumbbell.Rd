% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keydumbbell.R
\name{keydumbbell}
\alias{keydumbbell}
\title{Generate Keyword Trend Dumbbell Plot}
\usage{
keydumbbell(input, output)
}
\arguments{
\item{input}{Character. Path to the input Excel file containing publication records.
The file must include columns named 'Keyword-关键词' (semicolon-separated keywords)
and 'PubTime-发表时间' (publication dates in YYYY-MM-DD format).}

\item{output}{Character. Path for saving the output visualization image (supported formats:
png, jpg, pdf, etc.) Recommended size: 15cm(width) × 18cm(height) at 200 DPI.}
}
\value{
Invisibly returns the ggplot object. Primarily used for saving the plot to disk.
}
\description{
Creates a dumbbell plot visualizing the temporal distribution of top keywords in academic publications.
}
\details{
The function performs these operations:
\enumerate{
\item Extracts and cleans keywords from semicolon-separated values
\item Filters top 45 most frequent keywords
\item Calculates temporal statistics (Q1, Median, Q3) for each keyword
\item Generates a dumbbell plot with:
\itemize{
\item Gray segments showing keyword lifespan (Q1-Q3)
\item Light green points for first quartile (Q1)
\item Dark red points for third quartile (Q3)
\item Blue-gray midpoint markers sized by keyword frequency
}
}
}
\note{
\itemize{
\item Default temporal range: 1992-2025 (modify start_year/end_year in code for customization)
\item Input date format must be compatible with as.Date()
\item Requires readxl, dplyr, tidyr, ggplot2, and stringr packages
}
}
\examples{
\dontrun{
# Generate plot for 1992-2025 period
keydumbbell("publications.xlsx", "keyword_trend.png")

# Custom year range (modify start_year/end_year in function)
keydumbbell("data.xlsx", "trend_analysis.pdf")
}

}
