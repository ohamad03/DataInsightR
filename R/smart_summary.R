#' Smart Data Summary with Automated Insights
#'
#' @description
#' Performs intelligent data profiling with automatic type detection, statistical
#' summaries, and actionable insights. Returns an S3 object with comprehensive
#' information about the dataset structure and quality.
#'
#' @param data A data frame or tibble to analyze
#' @param max_levels Integer, maximum unique values to treat as categorical (default: 20)
#' @param detailed Logical, include detailed statistics (default: TRUE)
#'
#' @return An S3 object of class 'data_profile' containing overview, numeric summary,
#' categorical summary, data quality metrics, and automated insights
#'
#' @examples
#' \dontrun{
#' profile = smart_summary(mtcars)
#' print(profile)
#' }
#'
#' @export
#' @importFrom stats sd median IQR quantile
smart_summary = function(data, max_levels = 20, detailed = TRUE) {
  
  # check inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("data cannot be empty")
  }
  
  # basic info
  n_rows = nrow(data)
  n_cols = ncol(data)
  total_missing = sum(is.na(data))
  
  # find numeric columns
  numeric_cols = names(data)[sapply(data, is.numeric)]
  
  # calculate stats for numeric columns
  numeric_stats = list()
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      x = data[[col]]
      numeric_stats[[col]] = list(
        count = length(x),
        missing = sum(is.na(x)),
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        q25 = quantile(x, 0.25, na.rm = TRUE),
        q75 = quantile(x, 0.75, na.rm = TRUE)
      )
    }
  }
  
  # find categorical columns
  cat_cols = names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # get frequencies for categorical
  cat_stats = list()
  if (length(cat_cols) > 0) {
    for (col in cat_cols) {
      freq_table = table(data[[col]], useNA = "always")
      cat_stats[[col]] = data.frame(
        value = names(freq_table),
        count = as.numeric(freq_table),
        percent = round(as.numeric(freq_table) / sum(freq_table) * 100, 1)
      )
    }
  }
  
  # data quality check
  missing_pct = round(total_missing / (n_rows * n_cols) * 100, 2)
  complete_rows = sum(complete.cases(data))
  
  # generate some insights
  insights = c()
  if (missing_pct > 5) {
    insights = c(insights, paste("Warning:", missing_pct, "% missing values detected"))
  }
  if (complete_rows < n_rows * 0.8) {
    insights = c(insights, "Many rows have missing data")
  }
  if (length(insights) == 0) {
    insights = "Data looks clean"
  }
  
  # put it all together
  result = list(
    overview = list(
      rows = n_rows,
      columns = n_cols,
      numeric_vars = length(numeric_cols),
      categorical_vars = length(cat_cols)
    ),
    numeric_summary = numeric_stats,
    categorical_summary = cat_stats,
    quality = list(
      total_missing = total_missing,
      missing_percent = missing_pct,
      complete_rows = complete_rows
    ),
    insights = insights
  )
  
  class(result) = "data_profile"
  return(result)
}

# print method
print.data_profile = function(x, ...) {
  cat("\n=== DATA SUMMARY ===\n\n")
  cat("Rows:", x$overview$rows, "\n")
  cat("Columns:", x$overview$columns, "\n")
  cat("Numeric variables:", x$overview$numeric_vars, "\n")
  cat("Categorical variables:", x$overview$categorical_vars, "\n\n")
  
  cat("Data Quality:\n")
  cat("Missing:", x$quality$total_missing, "values (", x$quality$missing_percent, "%)\n")
  cat("Complete rows:", x$quality$complete_rows, "\n\n")
  
  cat("Insights:\n")
  for (msg in x$insights) {
    cat("-", msg, "\n")
  }
  cat("\n")
}

