#' Create Histogram
#'
#' @noRd
#' @export
create_hist <- function(data) {
  # Remove missing/infinite values from data
  data <- data[!is.na(data) & is.finite(data)]

  # Check if data has enough values for a histogram
  num_values <- length(data)
  if (num_values < 5) {
    return("")
  }

  # Determine the number of breaks for the histogram (between 1 and 10)
  n_breaks <- pmax(1, pmin(nclass.Sturges(data), 10))

  # Round break values if bin width > 1
  if ((diff(range(data)/n_breaks)) < 1) {
    breaks <- seq(min(data), max(data), length.out = n_breaks+1)
  } else {
    breaks <- round(seq(floor(min(data)), ceiling(max(data)), length.out = n_breaks+1))
  }

  # Get histogram counts
  hist_height <- 10
  counts <- hist(data, breaks = breaks, plot = FALSE)$counts
  counts <- round(counts / max(counts) * hist_height)

  # Create histogram representation
  hist_output <- ""
  for (i in 1:(hist_height/2)) {
    row_output <- ""
    for (c in 1:n_breaks) {
      symbol <- ifelse(counts[c] < (2*i)-1," ",ifelse(counts[c] < 2*i,".",":"))
      row_output <- paste0(row_output, symbol)
    }
    hist_output <- paste0(row_output, "\n", hist_output)
  }
  return(hist_output)
  # # Run the txthist function on the example data
  # data <- rnorm(100, mean = 50, sd = 10)
  # txthist(data)
  # cat(txthist(data))
}
