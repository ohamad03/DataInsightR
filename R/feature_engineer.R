#' Automated Feature Engineering
#'
#' @description
#' Creates new features from existing ones automatically. Makes polynomial features,
#' interactions between variables, and bins for numeric data.
#'
#' @param data A data frame
#' @param interactions Should we create interaction terms? (default TRUE)
#' @param polynomials Should we create squared terms? (default TRUE)
#' @param n_bins Number of bins for numeric variables (default 5)
#'
#' @return Data frame with original columns plus new engineered features
#'
#' @examples
#' \dontrun{
#' new_data = feature_engineer(mtcars)
#' head(new_data)
#' }
#'
#' @export
feature_engineer = function(data, interactions = TRUE, polynomials = TRUE, n_bins = 5) {
  
  # check input
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("data is empty")
  }
  
  # start with original data
  result = data
  
  # get numeric columns
  numeric_cols = names(data)[sapply(data, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    warning("no numeric columns found, returning original data")
    return(data)
  }
  
  # create polynomial features (squared terms)
  if (polynomials && length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      new_col_name = paste0(col, "_squared")
      result[[new_col_name]] = data[[col]]^2
    }
  }
  
  # create interaction features (products of pairs)
  if (interactions && length(numeric_cols) >= 2) {
    # only do top combinations to avoid explosion
    max_interactions = min(10, choose(length(numeric_cols), 2))
    interaction_count = 0
    
    for (i in 1:(length(numeric_cols) - 1)) {
      for (j in (i + 1):length(numeric_cols)) {
        if (interaction_count >= max_interactions) break
        
        col1 = numeric_cols[i]
        col2 = numeric_cols[j]
        new_col_name = paste0(col1, "_x_", col2)
        
        result[[new_col_name]] = data[[col1]] * data[[col2]]
        interaction_count = interaction_count + 1
      }
      if (interaction_count >= max_interactions) break
    }
  }
  
  # create binned versions of numeric variables
  for (col in numeric_cols) {
    x = data[[col]]
    
    # skip if all values are the same
    if (length(unique(x[!is.na(x)])) <= 1) next
    
    # create bins
    breaks = quantile(x, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
    breaks = unique(breaks)  # remove duplicates
    
    if (length(breaks) > 1) {
      binned_col = cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
      new_col_name = paste0(col, "_binned")
      result[[new_col_name]] = binned_col
    }
  }
  
  # add some summary info as attributes
  attr(result, "original_features") = ncol(data)
  attr(result, "new_features") = ncol(result) - ncol(data)
  attr(result, "total_features") = ncol(result)
  
  return(result)
}