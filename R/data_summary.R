#' data_summary
#'
#' Generate Summary for Different Variable Types
#'
#' This function generates summary statistics for variables in a dataset, categorized by their types. It allows for custom selection of variable types and summary statistics to include. The function can handle numeric, character, and other variable types and compute appropriate summary statistics for each.
#'
#' @param data A dataframe containing the variables for which summary statistics will be generated.
#' @param var_types A named list of variable types. If not provided, all types will be used.
#' @param include_stats A list of statistics to include for each variable type (e.g., mean, median, sd). If not provided, default summary statistics will be computed based on the variable type.
#' @param data_title A title for the dataset. If not provided it will be empty.
#'
#' @return A dataframe or printed output summarizing the statistics for each variable, depending on the variable type.
#' @importFrom stats median quantile sd
#' @importFrom utils head modifyList
#' @export
#'
#' @examples
#' # Example usage with the iris dataset
#' summary_stats <- data_summary(iris)
#' print(summary_stats)
#'
#' # Specify variable types manually and include custom statistics
#' var_types <- list(Sepal.Length = "numeric", Species = "character")
#' include_stats <- list(numeric = c("mean", "median", "sd"), character = c("count"))
#' summary_stats <- data_summary(iris, var_types = var_types, include_stats = include_stats, data_title = "Iris Summary")
#' print(summary_stats)
#'
data_summary <- function(data, data_title = NULL, var_types = NULL, include_stats = NULL) {
  # If no title is provided use dataframe name
  if (is.null(data_title)) {
    data_title <- deparse(substitute(data))
  }

  # Ensure it's a data.frame
  data <- as.data.frame(data)

  # Define default included statistics for each column type
  default_include_stats <- list(
    numeric = c("mean", "sd", "min", "p25", "median", "p75", "max", "mode", "hist"),
    character = c("n_unique", "top_counts", "min_char", "max_char"),
    factor = c("levels", "n_unique", "top_counts"),
    posixct = c("min", "max", "median", "n_unique"),
    logical = c("mean", "top_counts"),
    date = c("min", "max", "median", "n_unique")
  )

  # Merge provided include_stats with defaults
  if (is.null(include_stats)) {
    include_stats <- default_include_stats
  } else {
    include_stats <- modifyList(default_include_stats, include_stats)
  }

  # Classify variables if var_types is provided
  if (!is.null(var_types)) {
    exception_vars <- names(var_types)
    default_vars <- setdiff(names(data), exception_vars)

    # Classify default variables first
    generated_types <- list(
      numeric = default_vars[sapply(data[default_vars], is.numeric)],
      character = default_vars[sapply(data[default_vars], is.character)],
      factor = default_vars[sapply(data[default_vars], is.factor)],
      posixct = default_vars[sapply(data[default_vars], inherits, "POSIXct")],
      logical = default_vars[sapply(data[default_vars], is.logical)],
      date = default_vars[sapply(data[default_vars], inherits, "Date")]
    )

    # Classify others
    for (v in exception_vars) {
      generated_types[[var_types[[v]]]] <- c(generated_types[[var_types[[v]]]], v)
    }
  } else {
    # Classify all variables if var_types is not provided
    generated_types <- list(
      numeric = names(data)[sapply(data, is.numeric)],
      character = names(data)[sapply(data, is.character)],
      posixct = names(data)[sapply(data, inherits, "POSIXct")],
      logical = names(data)[sapply(data, is.logical)],
      date = names(data)[sapply(data, inherits, "Date")],
      factor = names(data)[sapply(data, is.factor)]
    )
  }

  # Initialize result list
  result <- list()
  hist_list <- list()
  # Numeric
  if (!is.null(generated_types$numeric)) {
    numeric_stats <- lapply(generated_types$numeric, function(var) {
      x <- data[[var]]
      stats <- list(variable = var)
      if ("mean" %in% include_stats$numeric) stats$mean <- mean(x, na.rm = TRUE)
      if ("sd" %in% include_stats$numeric) stats$sd <- sd(x, na.rm = TRUE)
      if ("min" %in% include_stats$numeric) stats$min <- min(x, na.rm = TRUE)
      if ("p25" %in% include_stats$numeric) stats$p25 <- quantile(x, 0.25, na.rm = TRUE)
      if ("median" %in% include_stats$numeric) stats$median <- median(x, na.rm = TRUE)
      if ("p75" %in% include_stats$numeric) stats$p75 <- quantile(x, 0.75, na.rm = TRUE)
      if ("max" %in% include_stats$numeric) stats$max <- max(x, na.rm = TRUE)
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$numeric <- do.call(rbind, numeric_stats)

    # # Histogram
    # if ("hist" %in% include_stats$numeric) {
    #   histogram_df <- data.frame(
    #     variable = generated_types$numeric,
    #     histogram = sapply(generated_types$numeric, function(var) {
    #       create_hist(data[[var]])
    #     }), stringsAsFactors = FALSE, row.names = NULL)
    # }
  }

  # Character
  if (!is.null(generated_types$character)) {
    character_stats <- lapply(generated_types$character, function(var) {
      x <- data[[var]]
      x <- as.character(x)
      stats <- list(variable = var)
      if ("n_unique" %in% include_stats$character) stats$n_unique <- length(unique(x))
      if ("min_char" %in% include_stats$character) stats$min_char <- min(nchar(x), na.rm = TRUE)
      if ("max_char" %in% include_stats$character) stats$max_char <- max(nchar(x), na.rm = TRUE)
      if ("top_counts" %in% include_stats$character) {
        n <- 3  # Number of top values to extract
        top_vals <- head(sort(table(x), decreasing = TRUE), n)
        stats$top_counts <- paste(names(top_vals), top_vals, sep = ": ", collapse = ", ")
      }
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$character <- do.call(rbind, character_stats)
  }

  # Factor
  if (!is.null(generated_types$factor)) {
    factor_stats <- lapply(generated_types$factor, function(var) {
      x <- data[[var]]
      stats <- list(variable = var)
      if ("levels" %in% include_stats$factor) stats$levels <- paste(levels(x), collapse = ", ")
      if ("n_unique" %in% include_stats$factor) stats$n_unique <- length(unique(x))
      if ("min_char" %in% include_stats$factor) stats$min_char <- min(nchar(x), na.rm = TRUE)
      if ("max_char" %in% include_stats$factor) stats$max_char <- max(nchar(x), na.rm = TRUE)
      if ("top_counts" %in% include_stats$factor) {
        n <- 3  # Number of top values to extract (can be changed)
        top_vals <- head(sort(table(x), decreasing = TRUE), n)
        stats$top_counts <- paste(names(top_vals), top_vals, sep = ": ", collapse = ", ")
      }
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$factor <- do.call(rbind, factor_stats)
  }

  # Logical
  if (!is.null(generated_types$logical)) {
    logical_stats <- lapply(generated_types$logical, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("mean" %in% include_stats$logical) stats$mean <- mean(x, na.rm = TRUE)
      if ("top_counts" %in% include_stats$logical) {
        top_counts <- sort(table(x), decreasing = TRUE)
        stats$top_counts <- paste(names(top_counts), top_counts, sep = ": ", collapse = ", ")
      }
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$logical <- do.call(rbind, logical_stats)
  }

  # POSIXct
  if (!is.null(generated_types$posixct)) {
    posixct_stats <- lapply(generated_types$posixct, function(var) {
      x <- data[[var]]
      stats <- list(variable = var)
      if ("min" %in% include_stats$posixct) stats$min <- min(x, na.rm = TRUE)
      if ("max" %in% include_stats$posixct) stats$max <- max(x, na.rm = TRUE)
      if ("median" %in% include_stats$posixct) stats$median <- median(x, na.rm = TRUE)
      if ("n_unique" %in% include_stats$posixct) stats$n_unique <- length(unique(x))
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$posixct <- do.call(rbind, posixct_stats)
  }

  # Date
  if (!is.null(generated_types$date)) {
    date_stats <- lapply(generated_types$date, function(var) {
      x <- data[[var]]
      x <- x <- as.Date(x)
      stats <- list(variable = var)
      if ("min" %in% include_stats$date) stats$min <- min(x, na.rm = TRUE)
      if ("max" %in% include_stats$date) stats$max <- max(x, na.rm = TRUE)
      if ("median" %in% include_stats$date) stats$median <- median(x, na.rm = TRUE)
      if ("n_unique" %in% include_stats$date) stats$n_unique <- length(unique(x))
      return(as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL))
    })
    result$date <- do.call(rbind, date_stats)
  }

  # Number of rows, and NA rows
  rows_text <- format(nrow(data), big.mark = ",", scientific = F)
  rows_na_text <- format(sum(rowSums(is.na(data)) > 0), big.mark = ",", scientific = F)

  # Initialize an empty list to store the formatted tables
  formatted_tables <- list()
  formatted_tables_console <- list()

  # Format all tables in result and store them in formatted_table
  for (var_type in names(result)) {
    var_stats <- result[[var_type]]  # Get the dataframe for each variable type

    # Generate the knitr::kable output and store it in the list
    formatted_table <- knitr::kable(var_stats,
                                    align = "c",
                                    row.names = FALSE,
                                    format = "markdown",
                                    caption = var_type,
                                    digits = 2,
                                    format.args = list(big.mark = ",",
                                                       scientific = FALSE))

    formatted_tables[[var_type]] <- paste(formatted_table, collapse = "\n")
    formatted_tables_console[[var_type]] <- formatted_table
  }

  # Print output (markdown for knitting or console text otherwise)
  if (isTRUE(getOption('knitr.in.progress'))) {
    # Create the markdown output
    markdown_output <- paste0(
      "## **", data_title, "**\n\n\n",
      "`", rows_text, "` rows\n\n",
      "`", rows_na_text, "` rows with missing values\n\n",
      paste((formatted_tables), collapse = "\n\n")
    )

    # Use knitr::asis_output to directly output the markdown
    knitr::asis_output(markdown_output)
  } else {
    # Console output
    cat(data_title, "\n\n")
    cat(rows_text, "\trows\n")
    cat(rows_na_text, "\trows with missing values\n\n")

    # Print each formatted table
    for (table in formatted_tables_console) {
      print(table)
    }
  }
}
