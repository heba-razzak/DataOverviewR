generate_summary_stats <- function(data, var_types = NULL, include_stats = NULL, data_title="") {
  # Ensure data is a data.frame
  data <- as.data.frame(data)

  # Define default included statistics
  default_include_stats <- list(
    numeric = c("mean", "sd", "p0", "p25", "median", "p75", "p100", "mode", "hist"),
    categorical = c("n_unique", "top_counts"),
    posixct = c("min", "max", "median", "n_unique"),
    logical = c("mean", "top_counts"),
    character = c("min_char", "max_char", "n_unique", "n_empty", "n_whitespace"),
    date = c("min", "max", "median", "n_unique"),
    factor = c("n_unique", "top_counts")
  )

  # Merge provided include_stats with defaults
  if (is.null(include_stats)) {
    include_stats <- default_include_stats
  } else {
    include_stats <- modifyList(default_include_stats, include_stats)
  }

  # Classify variables if var_types is not fully provided
  if (!is.null(var_types)) {
    all_var_names <- unlist(var_types)
    remaining_vars <- setdiff(names(data), all_var_names)

    # Classify remaining variables
    auto_classified_vars <- list(
      numeric = remaining_vars[sapply(data[remaining_vars], is.numeric)],
      categorical = remaining_vars[sapply(data[remaining_vars], is.factor) | sapply(data[remaining_vars], is.character)],
      posixct = remaining_vars[sapply(data[remaining_vars], inherits, "POSIXct")],
      logical = remaining_vars[sapply(data[remaining_vars], is.logical)],
      character = remaining_vars[sapply(data[remaining_vars], is.character)],
      date = remaining_vars[sapply(data[remaining_vars], inherits, "Date")],
      factor = remaining_vars[sapply(data[remaining_vars], is.factor)]
    )

    # Merge user-provided var_types with auto classified variable types
    var_types <- modifyList(auto_classified_vars, var_types)
  } else {
    # Classify all variables if var_types is not provided
    var_types <- list(
      numeric = names(data)[sapply(data, is.numeric)],
      categorical = names(data)[sapply(data, is.factor) | sapply(data, is.character)],
      posixct = names(data)[sapply(data, inherits, "POSIXct")],
      logical = names(data)[sapply(data, is.logical)],
      character = names(data)[sapply(data, is.character)],
      date = names(data)[sapply(data, inherits, "Date")],
      factor = names(data)[sapply(data, is.factor)]
    )
  }

  # Initialize result list
  result <- list()

  # Generate summary statistics for numeric variables
  if (!is.null(var_types$numeric)) {
    numeric_stats <- lapply(var_types$numeric, function(var) {
      x <- data[[var]]
      stats <- list(variable = var)
      if ("mean" %in% include_stats$numeric) stats$mean <- mean(x, na.rm = TRUE)
      if ("sd" %in% include_stats$numeric) stats$sd <- sd(x, na.rm = TRUE)
      if ("min" %in% include_stats$numeric) stats$p0 <- min(x, na.rm = TRUE)
      if ("p25" %in% include_stats$numeric) stats$p25 <- quantile(x, 0.25, na.rm = TRUE)
      if ("median" %in% include_stats$numeric) stats$median <- median(x, na.rm = TRUE)
      if ("p75" %in% include_stats$numeric) stats$p75 <- quantile(x, 0.75, na.rm = TRUE)
      if ("max" %in% include_stats$numeric) stats$p100 <- max(x, na.rm = TRUE)
      if ("hist" %in% include_stats$numeric) stats$hist <- create_hist(x)
      return(as.data.frame(stats, stringsAsFactors = FALSE))
    })
    result$numeric <- do.call(rbind, numeric_stats)
  }

  # Generate summary statistics for categorical variables
  if (!is.null(var_types$categorical)) {
    categorical_stats <- lapply(var_types$categorical, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("n_unique" %in% include_stats$categorical) stats$n_unique <- length(unique(x))
      if ("top_counts" %in% include_stats$categorical) {
        top_counts <- sort(table(x), decreasing = TRUE)[1:3]
        top_counts_perc <- top_counts / sum(top_counts) * 100
        stats$top_counts <- paste(names(top_counts), sprintf("%.2f%%", top_counts_perc), sep = ": ", collapse = ", ")
      }
    })
    result$categorical <- do.call(rbind, lapply(categorical_stats, as.data.frame))
  }

  # Generate summary statistics for POSIXct variables
  if (!is.null(var_types$posixct)) {
    posixct_stats <- lapply(var_types$posixct, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("min" %in% include_stats$posixct) stats$min <- min(x, na.rm = TRUE)
      if ("max" %in% include_stats$posixct) stats$max <- max(x, na.rm = TRUE)
      if ("median" %in% include_stats$posixct) stats$median <- median(x, na.rm = TRUE)
      if ("n_unique" %in% include_stats$posixct) stats$n_unique <- length(unique(x))
    })
    result$posixct <- do.call(rbind, lapply(posixct_stats, as.data.frame))
  }

  # Generate summary statistics for logical variables
  if (!is.null(var_types$logical)) {
    logical_stats <- lapply(var_types$logical, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("mean" %in% include_stats$logical) stats$mean <- mean(x, na.rm = TRUE)
      if ("top_counts" %in% include_stats$logical) {
        top_counts <- sort(table(x), decreasing = TRUE)
        stats$top_counts <- paste(names(top_counts), top_counts, sep = ": ", collapse = ", ")
      }
    })
    result$logical <- do.call(rbind, lapply(logical_stats, as.data.frame))
  }

  # Generate summary statistics for character variables
  if (!is.null(var_types$character)) {
    character_stats <- lapply(var_types$character, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("min_char" %in% include_stats$character) stats$min_char <- min(nchar(x), na.rm = TRUE)
      if ("max_char" %in% include_stats$character) stats$max_char <- max(nchar(x), na.rm = TRUE)
      if ("n_unique" %in% include_stats$character) stats$n_unique <- length(unique(x))
      if ("n_empty" %in% include_stats$character) stats$n_empty <- sum(nchar(x) == 0, na.rm = TRUE)
      if ("n_whitespace" %in% include_stats$character) stats$n_whitespace <- sum(nchar(trimws(x)) == 0, na.rm = TRUE)
    })
    result$character <- do.call(rbind, lapply(character_stats, as.data.frame))
  }

  # Generate summary statistics for date variables
  if (!is.null(var_types$date)) {
    date_stats <- lapply(var_types$date, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("min" %in% include_stats$date) stats$min <- min(x, na.rm = TRUE)
      if ("max" %in% include_stats$date) stats$max <- max(x, na.rm = TRUE)
      if ("median" %in% include_stats$date) stats$median <- median(x, na.rm = TRUE)
      if ("n_unique" %in% include_stats$date) stats$n_unique <- length(unique(x))
    })
    result$date <- do.call(rbind, lapply(date_stats, as.data.frame))
  }

  # Generate summary statistics for factor variables
  if (!is.null(var_types$factor)) {
    factor_stats <- lapply(var_types$factor, function(var) {
      x <- data[[var]]
      stats <- list(
        variable = var
      )
      if ("ordered" %in% include_stats$factor) stats$ordered <- is.ordered(x)
      if ("n_unique" %in% include_stats$factor) stats$n_unique <- length(unique(x))
      if ("top_counts" %in% include_stats$factor) {
        top_counts <- sort(table(x), decreasing = TRUE)[1:3]
        stats$top_counts <- paste(names(top_counts), top_counts, sep = ": ", collapse = ", ")
      }
    })
    result$factor <- do.call(rbind, lapply(factor_stats, as.data.frame))
  }

  # Title for summary statistics
  title <- paste0("Summary Statistics",data_title)

  if (data_title!="")  {
    title <- paste0("Summary Statistics for ",data_title)
  }

  # Format for markdown
  title <- paste0("## **", title, "**\n")

  output <- title

  for (var_type in names(result)) {
    var_stats <- result[[var_type]]

    # Create the markdown table for each variable type
    md_table <- knitr::kable(var_stats,
                             align = "lrrrrrrrr",
                             row.names = FALSE,
                             format = "markdown",
                             digits = 2,
                             format.args = list(big.mark = ",",
                                                scientific = FALSE))
    md_table <- paste(md_table, collapse = "\n")

    # Add the table to the output
    output <- paste0(output, "\n### ", tools::toTitleCase(var_type), "\n\n", md_table, "\n")
  }

  # Print the combined output as is in markdown format
  knitr::asis_output(output)
}
