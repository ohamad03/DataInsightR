#' Calculate Correlation Matrix with Significance Tests
#'
#' @description
#' Computes correlations between numeric variables and tests if they're
#' statistically significant. Helps you find relationships in your data.
#'
#' @param data A data frame
#' @param method Correlation method: "pearson", "spearman", or "kendall" (default "pearson")
#' @param min_correlation Minimum correlation to highlight (default 0.3)
#'
#' @return A list with correlation matrix, p-values, and significant pairs
#'
#' @examples
#' \dontrun{
#' corr_result = correlation_matrix(mtcars)
#' print(corr_result)
#' }
#'
#' @export
#' @importFrom stats cor cor.test
correlation_matrix = function(data, method = "pearson", min_correlation = 0.3) {
  
  # input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  # get only numeric columns
  numeric_data = data[, sapply(data, is.numeric), drop = FALSE]
  
  if (ncol(numeric_data) < 2) {
    stop("need at least 2 numeric variables to compute correlations")
  }
  
  # check method is valid
  valid_methods = c("pearson", "spearman", "kendall")
  if (!method %in% valid_methods) {
    stop("method must be 'pearson', 'spearman', or 'kendall'")
  }
  
  # calculate correlation matrix
  cor_matrix = cor(numeric_data, use = "pairwise.complete.obs", method = method)
  
  # calculate p-values for each pair
  n_vars = ncol(numeric_data)
  p_matrix = matrix(1, nrow = n_vars, ncol = n_vars)
  rownames(p_matrix) = colnames(numeric_data)
  colnames(p_matrix) = colnames(numeric_data)
  
  # test each pair
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      # get the two variables
      x = numeric_data[, i]
      y = numeric_data[, j]
      
      # remove missing values
      valid = complete.cases(x, y)
      
      if (sum(valid) > 3) {  # need at least 4 points
        test_result = cor.test(x[valid], y[valid], method = method)
        p_matrix[i, j] = test_result$p.value
        p_matrix[j, i] = test_result$p.value
      }
    }
  }
  
  # find significant correlations
  significant_pairs = data.frame(
    var1 = character(0),
    var2 = character(0),
    correlation = numeric(0),
    p_value = numeric(0),
    significant = logical(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      cor_val = cor_matrix[i, j]
      p_val = p_matrix[i, j]
      
      # check if meets minimum threshold
      if (abs(cor_val) >= min_correlation) {
        significant_pairs = rbind(significant_pairs, data.frame(
          var1 = rownames(cor_matrix)[i],
          var2 = colnames(cor_matrix)[j],
          correlation = round(cor_val, 3),
          p_value = round(p_val, 4),
          significant = p_val < 0.05,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # sort by absolute correlation
  if (nrow(significant_pairs) > 0) {
    significant_pairs = significant_pairs[order(-abs(significant_pairs$correlation)), ]
  }
  
  # generate insights
  insights = c()
  
  strong_corrs = sum(abs(significant_pairs$correlation) > 0.7)
  if (strong_corrs > 0) {
    insights = c(insights, paste("Found", strong_corrs, "strong correlation(s) (|r| > 0.7)"))
  }
  
  sig_corrs = sum(significant_pairs$significant)
  if (sig_corrs > 0) {
    insights = c(insights, paste(sig_corrs, "correlation(s) are statistically significant (p < 0.05)"))
  }
  
  if (length(insights) == 0) {
    insights = "No strong correlations found. Variables appear mostly independent."
  }
  
  # build result
  result = list(
    correlation_matrix = cor_matrix,
    p_values = p_matrix,
    significant_pairs = significant_pairs,
    method = method,
    insights = insights,
    n_variables = n_vars
  )
  
  class(result) = "correlation_result"
  return(result)
}

#' Print correlation results
#'
#' @param x A correlation_result object
#' @param ... Other parameters
#' @export
print.correlation_result = function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("      CORRELATION ANALYSIS RESULTS      \n")
  cat("========================================\n\n")
  
  cat("Method:", x$method, "\n")
  cat("Variables analyzed:", x$n_variables, "\n\n")
  
  if (nrow(x$significant_pairs) == 0) {
    cat("No correlations found above the minimum threshold.\n\n")
  } else {
    cat("Significant Variable Pairs:\n")
    print(x$significant_pairs, row.names = FALSE)
    cat("\n")
  }
  
  cat("Insights:\n")
  for (insight in x$insights) {
    cat("  •", insight, "\n")
  }
  cat("\n")
}