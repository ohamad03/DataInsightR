#' Detect Outliers Using Multiple Methods
#'
#' @description
#' Finds weird values in your data using two different approaches - the IQR method
#' (looking at quartiles) and Z-score method (standard deviations from mean).
#'
#' @param data A data frame
#' @param columns Which columns to check (default checks all numeric ones)
#' @param method Use "iqr", "zscore", or "both" (default both)
#' @param threshold How many standard deviations away counts as outlier (default 3)
#'
#' @return An outlier report showing which values look suspicious
#'
#' @examples
#' \dontrun{
#' result = detect_outliers(mtcars)
#' print(result)
#' }
#'
#' @export
#' @importFrom stats IQR quantile sd
detect_outliers = function(data, columns = NULL, method = "both", threshold = 3) {
  
  # basic checks
  if (!is.data.frame(data)) {
    stop("hey, data needs to be a data frame")
  }
  
  # if no columns specified, just use all numeric ones
  if (is.null(columns)) {
    columns = names(data)[sapply(data, is.numeric)]
  }
  
  if (length(columns) == 0) {
    stop("couldn't find any numeric columns to check")
  }
  
  # make sure method is valid
  valid_methods = c("iqr", "zscore", "both")
  if (!method %in% valid_methods) {
    stop("method should be 'iqr', 'zscore', or 'both'")
  }
  
  # go through each column and find outliers
  outlier_info = list()
  
  for (col_name in columns) {
    values = data[[col_name]]
    values = values[!is.na(values)]  # skip missing values
    
    if (length(values) == 0) next  # skip if everything is missing
    
    weird_indices = c()
    
    # method 1: IQR (interquartile range)
    if (method == "iqr" || method == "both") {
      q1 = quantile(values, 0.25)
      q3 = quantile(values, 0.75)
      iqr_value = q3 - q1
      
      lower_fence = q1 - 1.5 * iqr_value
      upper_fence = q3 + 1.5 * iqr_value
      
      # find values outside the fences
      iqr_weird = which(values < lower_fence | values > upper_fence)
      weird_indices = c(weird_indices, iqr_weird)
    }
    
    # method 2: Z-score (standard deviations)
    if (method == "zscore" || method == "both") {
      mean_val = mean(values)
      sd_val = sd(values)
      
      # calculate how many SDs away each value is
      z_scores = abs((values - mean_val) / sd_val)
      
      # flag anything beyond threshold
      z_weird = which(z_scores > threshold)
      weird_indices = c(weird_indices, z_weird)
    }
    
    # remove duplicates
    weird_indices = unique(weird_indices)
    
    # save info if we found any outliers
    if (length(weird_indices) > 0) {
      outlier_info[[col_name]] = list(
        how_many = length(weird_indices),
        percentage = round(length(weird_indices) / length(values) * 100, 1),
        the_values = values[weird_indices],
        positions = weird_indices
      )
    }
  }
  
  # make a summary table
  if (length(outlier_info) > 0) {
    summary_table = data.frame(
      variable = names(outlier_info),
      outliers_found = sapply(outlier_info, function(x) x$how_many),
      percent_of_data = sapply(outlier_info, function(x) x$percentage),
      stringsAsFactors = FALSE
    )
  } else {
    summary_table = data.frame(
      variable = character(0),
      outliers_found = numeric(0),
      percent_of_data = numeric(0)
    )
  }
  
  # package everything up
  result = list(
    original_data = data,
    method_used = method,
    columns_analyzed = columns,
    outlier_details = outlier_info,
    summary = summary_table
  )
  
  class(result) = "outlier_report"
  return(result)
}

#' Print outlier report nicely
#'
#' @param x An outlier_report object
#' @param ... Other stuff (ignored)
#' @export
print.outlier_report = function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("         OUTLIER DETECTION REPORT       \n")
  cat("========================================\n\n")
  
  cat("Method:", x$method_used, "\n")
  cat("Checked", length(x$columns_analyzed), "columns\n\n")
  
  if (nrow(x$summary) == 0) {
    cat("Good news! No outliers detected.\n")
    cat("Your data looks pretty clean.\n\n")
  } else {
    cat("Found outliers in", nrow(x$summary), "column(s):\n\n")
    print(x$summary, row.names = FALSE)
    cat("\n")
    
    # give some context
    total_outliers = sum(x$summary$outliers_found)
    cat("Total outliers:", total_outliers, "\n")
    cat("\nTip: You might want to investigate these values\n")
    cat("to see if they're errors or just unusual cases.\n\n")
  }
}

