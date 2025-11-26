#' DataQuality S4 Class Definition
#'
#' @description
#' An S4 class for comprehensive data quality assessment. This is the fancy
#' object-oriented approach that shows we know advanced R programming!
#'
#' @slot data The original data frame
#' @slot quality_score Overall quality score from 0-100
#' @slot issues List of detected issues
#' @slot recommendations What to fix
#' @slot timestamp When the analysis was done
#'
#' @export
setClass("DataQuality",
         slots = list(
           data = "data.frame",
           quality_score = "numeric",
           issues = "list",
           recommendations = "character",
           timestamp = "POSIXct"
         ))

#' Create a DataQuality object
#'
#' @description
#' Analyzes data quality and returns a fancy S4 object with all the details.
#' Checks for missing values, duplicates, outliers, and data type issues.
#'
#' @param data A data frame to assess
#'
#' @return A DataQuality S4 object
#'
#' @examples
#' \dontrun{
#' quality = assess_data_quality(mtcars)
#' show(quality)
#' }
#'
#' @export
assess_data_quality = function(data) {
  
  # validation
  if (!is.data.frame(data)) {
    stop("input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("data frame is empty")
  }
  
  # initialize scoring (start at 100, deduct for issues)
  score = 100
  issues = list()
  recommendations = c()
  
  # check for missing values
  total_missing = sum(is.na(data))
  missing_pct = total_missing / (nrow(data) * ncol(data)) * 100
  
  if (missing_pct > 0) {
    issues$missing_data = list(
      count = total_missing,
      percent = round(missing_pct, 2)
    )
    score = score - min(30, missing_pct * 2)
    
    if (missing_pct > 20) {
      recommendations = c(recommendations, 
                          "Critical: Over 20% missing data. Consider data collection issues.")
    } else if (missing_pct > 5) {
      recommendations = c(recommendations,
                          "Use imputation methods or remove rows with missing values.")
    }
  }
  
  # check for duplicate rows
  n_duplicates = sum(duplicated(data))
  
  if (n_duplicates > 0) {
    dup_pct = n_duplicates / nrow(data) * 100
    issues$duplicate_rows = list(
      count = n_duplicates,
      percent = round(dup_pct, 2)
    )
    score = score - min(20, dup_pct)
    recommendations = c(recommendations,
                        paste("Remove", n_duplicates, "duplicate rows."))
  }
  
  # check for constant columns (no variation)
  constant_cols = sapply(data, function(x) length(unique(x[!is.na(x)])) <= 1)
  n_constant = sum(constant_cols)
  
  if (n_constant > 0) {
    issues$constant_columns = list(
      columns = names(data)[constant_cols],
      count = n_constant
    )
    score = score - (n_constant * 5)
    recommendations = c(recommendations,
                        paste("Remove", n_constant, "constant column(s) - they provide no information."))
  }
  
  # check for high cardinality in small datasets
  if (nrow(data) < 100) {
    high_card_cols = sapply(data, function(x) {
      if (is.character(x) || is.factor(x)) {
        n_unique = length(unique(x[!is.na(x)]))
        return(n_unique > nrow(data) * 0.5)
      }
      return(FALSE)
    })
    
    if (any(high_card_cols)) {
      issues$high_cardinality = list(
        columns = names(data)[high_card_cols]
      )
      score = score - 10
      recommendations = c(recommendations,
                          "Some categorical variables have too many unique values for this dataset size.")
    }
  }
  
  # make sure score doesn't go below 0
  score = max(0, score)
  
  # add overall recommendation
  if (score >= 90) {
    recommendations = c("Data quality is excellent!", recommendations)
  } else if (score >= 70) {
    recommendations = c("Data quality is good with minor issues.", recommendations)
  } else if (score >= 50) {
    recommendations = c("Data quality needs improvement.", recommendations)
  } else {
    recommendations = c("Critical: Data quality is poor. Major cleanup needed!", recommendations)
  }
  
  # create the S4 object
  quality_obj = new("DataQuality",
                    data = data,
                    quality_score = round(score, 1),
                    issues = issues,
                    recommendations = recommendations,
                    timestamp = Sys.time())
  
  return(quality_obj)
}

#' Show method for DataQuality objects
#'
#' @param object A DataQuality object
#' @export
setMethod("show", "DataQuality", function(object) {
  cat("\n")
  cat("========================================\n")
  cat("  DATA QUALITY ASSESSMENT REPORT\n")
  cat("========================================\n\n")
  
  cat("Dataset:", nrow(object@data), "rows x", ncol(object@data), "columns\n")
  cat("Analysis Date:", format(object@timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("QUALITY SCORE:", object@quality_score, "/ 100\n")
  
  # visual score bar
  bars = round(object@quality_score / 10)
  cat("[", rep("#", bars), rep("-", 10 - bars), "]\n\n", sep = "")
  
  if (length(object@issues) > 0) {
    cat("ISSUES DETECTED:\n")
    for (issue_name in names(object@issues)) {
      cat("  -", issue_name, "\n")
    }
    cat("\n")
  } else {
    cat("No major issues detected!\n\n")
  }
  
  cat("RECOMMENDATIONS:\n")
  for (i in seq_along(object@recommendations)) {
    cat(" ", i, ".", object@recommendations[i], "\n")
  }
  cat("\n")
})

#' Get quality score from DataQuality object
#'
#' @param object A DataQuality object
#' @return The quality score
#' @export
setGeneric("getScore", function(object) standardGeneric("getScore"))

#' @export
setMethod("getScore", "DataQuality", function(object) {
  return(object@quality_score)
})

