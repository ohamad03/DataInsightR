#' Analyze Missing Data Patterns
#'
#' @description
#' Checks where data is missing and gives you insights about the patterns.
#' Helps you decide what to do with missing values.
#'
#' @param data A data frame to analyze
#' @param visualize Should we make a plot? (default TRUE)
#'
#' @return An S3 object of class 'missing_analysis' with all the missing data info
#'
#' @examples
#' \dontrun{
#' missing_info = missing_analyzer(airquality)
#' print(missing_info)
#' }
#'
#' @export
missing_analyzer = function(data, visualize = TRUE) {
  
  # check input
  if (!is.data.frame(data)) {
    stop("need a data frame here")
  }
  
  if (nrow(data) == 0) {
    stop("data frame is empty")
  }
  
  # calculate missing for each column
  missing_counts = sapply(data, function(x) sum(is.na(x)))
  missing_percent = round(missing_counts / nrow(data) * 100, 2)
  
  # make a summary table
  missing_summary = data.frame(
    variable = names(data),
    missing_count = missing_counts,
    missing_percent = missing_percent,
    complete_count = nrow(data) - missing_counts,
    stringsAsFactors = FALSE
  )
  
  # sort by most missing
  missing_summary = missing_summary[order(-missing_summary$missing_count), ]
  
  # overall stats
  total_cells = nrow(data) * ncol(data)
  total_missing = sum(missing_counts)
  overall_missing_pct = round(total_missing / total_cells * 100, 2)
  
  # find patterns
  complete_rows = sum(complete.cases(data))
  complete_cols = sum(missing_counts == 0)
  
  # columns with any missing
  cols_with_missing = sum(missing_counts > 0)
  
  # generate recommendations
  recommendations = c()
  
  if (overall_missing_pct == 0) {
    recommendations = c("No missing data - you're all set!")
  } else if (overall_missing_pct < 5) {
    recommendations = c("Low missing data. You could remove rows with missing values.")
  } else if (overall_missing_pct < 20) {
    recommendations = c("Moderate missing data. Consider imputation methods.")
  } else {
    recommendations = c("High missing data. Investigate why so much is missing.")
    recommendations = c(recommendations, "You might need to collect more data or use advanced imputation.")
  }
  
  # check for columns that are mostly missing
  mostly_missing = missing_summary$variable[missing_summary$missing_percent > 50]
  if (length(mostly_missing) > 0) {
    recommendations = c(recommendations, 
                        paste("Consider dropping these columns:", paste(mostly_missing, collapse = ", ")))
  }
  
  # build result
  result = list(
    data = data,
    summary_table = missing_summary,
    overall = list(
      total_cells = total_cells,
      total_missing = total_missing,
      missing_percent = overall_missing_pct,
      complete_rows = complete_rows,
      complete_columns = complete_cols,
      columns_with_missing = cols_with_missing
    ),
    recommendations = recommendations
  )
  
  class(result) = "missing_analysis"
  return(result)
}

#' Print missing analysis nicely
#'
#' @param x A missing_analysis object
#' @param ... Other stuff
#' @export
print.missing_analysis = function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("      MISSING DATA ANALYSIS REPORT      \n")
  cat("========================================\n\n")
  
  cat("Overall Statistics:\n")
  cat("  Total cells:", x$overall$total_cells, "\n")
  cat("  Missing cells:", x$overall$total_missing, 
      paste0("(", x$overall$missing_percent, "%)"), "\n")
  cat("  Complete rows:", x$overall$complete_rows, "out of", nrow(x$data), "\n")
  cat("  Complete columns:", x$overall$complete_columns, "out of", ncol(x$data), "\n")
  cat("  Columns with missing:", x$overall$columns_with_missing, "\n\n")
  
  if (x$overall$total_missing > 0) {
    cat("Missing Data by Variable:\n")
    # only show variables with missing data
    to_show = x$summary_table[x$summary_table$missing_count > 0, ]
    print(to_show, row.names = FALSE)
    cat("\n")
  }
  
  cat("Recommendations:\n")
  for (rec in x$recommendations) {
    cat("  *", rec, "\n")
  }
  cat("\n")
}

