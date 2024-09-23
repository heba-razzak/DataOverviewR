#' Create Text Histogram
#'
#' @importFrom graphics hist
#' @noRd
#' @export
#'
#' @examples
#' create_hist(iris$Sepal.Length)
#' cat(create_hist(iris$Sepal.Length))
create_hist <- function(data) {
  # Remove missing/infinite values from data
  data <- data[!is.na(data) & is.finite(data)]

  # Check if data has enough values for a histogram
  num_values <- length(data)
  if (num_values < 5) {
    return("")
  }

  # Use default histogram bins
  hist_data <- hist(data, plot = FALSE)
  breaks <- hist_data$breaks
  n_breaks <- length(breaks) - 1
  counts <- hist_data$counts

  # Scale histogram counts
  hist_height <- 10
  counts_scaled <- round(counts / max(counts) * hist_height)

  # Create histogram text representation
  hist_output <- ""
  for (i in 1:(hist_height/2)) {
    row_output <- ""
    for (c in 1:n_breaks) {
      symbol <- ifelse(counts_scaled[c] < (2*i)-1," ",ifelse(counts_scaled[c] < 2*i,".",":"))
      row_output <- paste0(row_output, symbol)
    }
    hist_output <- paste0(row_output, "\n", hist_output)
  }
  return(hist_output)

  # # Print histograms for each variable
  # for (i in 1:nrow(histogram_df)) {
  #   cat(histogram_df$variable[i], "\n")
  #   cat(histogram_df$histogram[i], "\n\n")
  # }
}
