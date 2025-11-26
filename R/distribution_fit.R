#' Fit Multiple Distributions to Data
#'
#' @description
#' Tests how well your data fits different statistical distributions
#' (normal, uniform, exponential). Helps you understand your data better.
#'
#' @param x A numeric vector
#' @param distributions Which distributions to test (default: all)
#'
#' @return A data frame ranking the best fitting distributions
#'
#' @examples
#' \dontrun{
#' results = distribution_fit(rnorm(100))
#' print(results)
#' }
#'
#' @export
#' @importFrom stats shapiro.test ks.test
distribution_fit = function(x, distributions = c("normal", "uniform", "exponential")) {
  
  # input checks
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  # remove missing values
  x = x[!is.na(x)]
  
  if (length(x) < 3) {
    stop("need at least 3 non-missing values")
  }
  
  if (length(unique(x)) < 2) {
    stop("x has no variation - all values are the same")
  }
  
  # results storage
  results = data.frame(
    distribution = character(),
    test_statistic = numeric(),
    p_value = numeric(),
    fits_well = character(),
    stringsAsFactors = FALSE
  )
  
  # test normal distribution
  if ("normal" %in% distributions) {
    if (length(x) >= 3 && length(x) <= 5000) {
      # use shapiro test for small to medium samples
      test = shapiro.test(x)
      results = rbind(results, data.frame(
        distribution = "Normal",
        test_statistic = round(test$statistic, 4),
        p_value = round(test$p.value, 4),
        fits_well = ifelse(test$p.value > 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      ))
    } else {
      # use KS test for large samples
      test = ks.test(x, "pnorm", mean(x), sd(x))
      results = rbind(results, data.frame(
        distribution = "Normal",
        test_statistic = round(test$statistic, 4),
        p_value = round(test$p.value, 4),
        fits_well = ifelse(test$p.value > 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # test uniform distribution
  if ("uniform" %in% distributions) {
    test = ks.test(x, "punif", min(x), max(x))
    results = rbind(results, data.frame(
      distribution = "Uniform",
      test_statistic = round(test$statistic, 4),
      p_value = round(test$p.value, 4),
      fits_well = ifelse(test$p.value > 0.05, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
  
  # test exponential distribution (only for positive values)
  if ("exponential" %in% distributions && all(x > 0)) {
    rate_est = 1 / mean(x)
    test = ks.test(x, "pexp", rate_est)
    results = rbind(results, data.frame(
      distribution = "Exponential",
      test_statistic = round(test$statistic, 4),
      p_value = round(test$p.value, 4),
      fits_well = ifelse(test$p.value > 0.05, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
  
  # sort by p-value (higher is better)
  results = results[order(-results$p_value), ]
  
  # add interpretation
  best_fit = results$distribution[1]
  best_p = results$p_value[1]
  
  if (best_p > 0.05) {
    interpretation = paste("Data fits", best_fit, "distribution reasonably well (p =", best_p, ")")
  } else {
    interpretation = "Data doesn't fit any of the tested distributions well. Might need transformation."
  }
  
  attr(results, "interpretation") = interpretation
  attr(results, "sample_size") = length(x)
  
  class(results) = c("distribution_fit_result", "data.frame")
  
  return(results)
}

#' Print distribution fit results
#'
#' @param x A distribution_fit_result object
#' @param ... Other parameters
#' @export
print.distribution_fit_result = function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("    DISTRIBUTION FITTING RESULTS\n")
  cat("========================================\n\n")
  
  cat("Sample size:", attr(x, "sample_size"), "\n\n")
  
  cat("Test Results (sorted by fit quality):\n")
  print.data.frame(x, row.names = FALSE)
  cat("\n")
  
  cat("Interpretation:\n")
  cat(" ", attr(x, "interpretation"), "\n\n")
  
  cat("Note: p-value > 0.05 suggests good fit\n")
  cat("      p-value < 0.05 suggests poor fit\n\n")
}