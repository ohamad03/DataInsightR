#' Automatic Plot Generation Based on Data Types
#'
#' @description
#' Smart function that looks at your data and automatically creates
#' the right type of plot. Saves you from deciding what plot to use!
#'
#' @param data A data frame
#' @param x Column name for x-axis
#' @param y Column name for y-axis (optional for single variable plots)
#' @param plot_type Force a specific plot type or let it auto-detect (default "auto")
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' plot1 = auto_plot(mtcars, "mpg")
#' plot2 = auto_plot(mtcars, "cyl", "mpg")
#' print(plot1)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_boxplot geom_bar geom_point geom_smooth
#' @importFrom ggplot2 labs theme_minimal theme element_text
auto_plot = function(data, x, y = NULL, plot_type = "auto") {
  
  # check inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!x %in% names(data)) {
    stop(paste("column", x, "not found in data"))
  }
  
  if (!is.null(y) && !y %in% names(data)) {
    stop(paste("column", y, "not found in data"))
  }
  
  # load ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed")
  }
  
  # determine what kind of plot to make
  x_type = if (is.numeric(data[[x]])) "numeric" else "categorical"
  y_type = if (!is.null(y)) {
    if (is.numeric(data[[y]])) "numeric" else "categorical"
  } else {
    NULL
  }
  
  # single variable plots
  if (is.null(y)) {
    if (x_type == "numeric") {
      # histogram for numeric
      p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]])) +
        ggplot2::geom_histogram(fill = "steelblue", color = "white", bins = 30) +
        ggplot2::labs(
          title = paste("Distribution of", x),
          x = x,
          y = "Count"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
      
      return(p)
    } else {
      # bar chart for categorical
      p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]])) +
        ggplot2::geom_bar(fill = "coral", color = "white") +
        ggplot2::labs(
          title = paste("Frequency of", x),
          x = x,
          y = "Count"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      return(p)
    }
  }
  
  # two variable plots
  if (x_type == "numeric" && y_type == "numeric") {
    # scatter plot with trend line
    p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(color = "darkblue", alpha = 0.6, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
      ggplot2::labs(
        title = paste(y, "vs", x),
        x = x,
        y = y
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
    
    return(p)
    
  } else if (x_type == "categorical" && y_type == "numeric") {
    # box plot
    p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      ggplot2::labs(
        title = paste(y, "by", x),
        x = x,
        y = y
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    
    return(p)
    
  } else {
    # grouped bar chart for two categorical
    p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], fill = .data[[y]])) +
      ggplot2::geom_bar(position = "dodge") +
      ggplot2::labs(
        title = paste(x, "grouped by", y),
        x = x,
        y = "Count",
        fill = y
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    
    return(p)
  }
}