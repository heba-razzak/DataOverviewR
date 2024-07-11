#' Generate Summary Statistics for Different Variable Types
#'
#' Generates summary statistics for different variable types including numeric,
#' categorical, POSIXct, logical, character, date, and factor variables.
#' Users can define which statistics to include for each variable type.
#'
#' @param data The dataset for which the summary statistics are to be generated.
#' @param var_types A named list where names are variable types (e.g., 'numeric', 'categorical')
#' and values are vectors of variable names of that type. If not provided, the function
#' will attempt to classify variables automatically.
#' @param include_stats A named list where names are variable types and values are vectors
#' of statistics to include for that type. If not provided, default statistics will be used.
#' Default statistics are:
#' \itemize{
#'   \item{numeric: c("mean", "sd", "p0", "p25", "median", "p75", "p100", "hist")}
#'   \item{categorical: c("n_unique", "top_counts")}
#'   \item{posixct: c("min", "max", "median", "n_unique")}
#'   \item{logical: c("mean", "top_counts")}
#'   \item{character: c("min_char", "max_char", "n_unique", "n_empty", "n_whitespace")}
#'   \item{date: c("min", "max", "median", "n_unique")}
#'   \item{factor: c("n_unique", "top_counts")}
#' }
#'
#' @return A list of data frames containing summary statistics for each variable type.
#' @export
#'
#' @examples
#' summary_stats <- generate_summary_stats(
#'   data = mtcars,
#'   var_types = list(
#'     numeric = c("mpg", "hp"),
#'     categorical = c("cyl"),
#'     posixct = c("time")
#'   ),
#'   include_stats = list(
#'     numeric = c("mean", "median"),
#'     categorical = c("n_missing", "n_unique", "top_counts"),
#'     posixct = c("min", "max", "median")
#'   )
#' )
#' print(summary_stats)
generate_summary_stats <- function(data, var_types = NULL, include_stats = NULL, data_title="") {
  # Ensure data is a data.frame
  data <- as.data.frame(data)

  # Helper function to create histogram representation
  create_hist <- function(x) {
    hist_info <- hist(x, plot = FALSE)
    hist_counts <- hist_info$counts
    max_count <- max(hist_counts)
    scaled_counts <- round(hist_counts / max_count * 5)
    paste(rep("▇", scaled_counts), collapse = "")
  }

  # Define default included statistics
  default_include_stats <- list(
    numeric = c("mean", "sd", "p0", "p25", "median", "p75", "p100", "hist"),
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
      if ("p0" %in% include_stats$numeric) stats$p0 <- min(x, na.rm = TRUE)
      if ("p25" %in% include_stats$numeric) stats$p25 <- quantile(x, 0.25, na.rm = TRUE)
      if ("median" %in% include_stats$numeric) stats$median <- median(x, na.rm = TRUE)
      if ("p75" %in% include_stats$numeric) stats$p75 <- quantile(x, 0.75, na.rm = TRUE)
      if ("p100" %in% include_stats$numeric) stats$p100 <- max(x, na.rm = TRUE)
      if ("hist" %in% include_stats$numeric) stats$hist <- create_hist(x)
    })
    result$numeric <- do.call(rbind, lapply(numeric_stats, as.data.frame))
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
  if (missing(data_title))  {
    data_title <- deparse(substitute(data))
  }

  title <- paste0("Summary Statistics for ",data_title)
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


#' Generate Dataframe Skeleton for Variable Descriptions
#'
#' Generates a dataframe that can be used as the descriptions argument
#' for \code{\link{print_data_dict}} function.
#'
#' It has placeholders for the actual descriptions that need to be changed.
#' It is designed to simplify the process of creating input for
#' \code{\link{print_data_dict}} function by providing a template for describing
#' variables in a dataset.
#'
#' @param data The dataset for which the data dictionary is to be printed.
#'
#' @return dataframe
#' @export
#'
#' @examples
#' descriptions = descriptions_df(mtcars)
descriptions_df <- function(data) {
  # Get variable names
  variables <- names(data)
  descriptions = data.frame(Variable = names(data),
                            Description = "")

  # Return descriptions dataframe
  return(descriptions)
}


#' Update Descriptions for Variables
#'
#' @param descriptions A dataframe containing variable descriptions.
#' @param variable The name of the variable for which the description is to be updated.
#' @param description The new description for the variable.
#'
#' @return The updated descriptions dataframe.
#' @export
#'
#' @examples
#' descriptions <- update_description(descriptions,
#'                                    "osm_id",
#'                                    "Unique Identifier from OpenStreetMap")
#' descriptions <- update_description(descriptions,
#'                                    c("osm_id", "name"),
#'                                    c("Unique Identifier from OpenStreetMap",
#'                                      "Name of the entity"))
update_description <- function(descriptions, variable, description) {
  for (i in seq_along(variable)) {
    descriptions$Description[descriptions$Variable == variable[i]] <- description[i]
  }
  return(descriptions)
}

#' Print Data Dictionary
#'
#' Creates a data dictionary for the dataset using attributes from the data
#' and description if provided.
#' Adapted from the data_dict_md function from the explore package.
#' Source: https://rdrr.io/cran/explore/src/R/tools.R
#'
#' @param data The dataset for which the data dictionary is to be printed.
#' @param data_title The title for the data dictionary (optional).
#' @param descriptions A data frame providing detailed descriptions for
#' variables (optional).
#' @param show_na Logical value indicating whether to show the NA and %NA columns (default: TRUE).
#'
#' @return None
#' @export
#'
#' @examples
#' descriptions = descriptions_df(mtcars)
#' print_data_dict(mtcars, data_title = "Cars", descriptions = descriptions)
print_data_dict <- function(data, data_title = "", descriptions = NULL, show_na = TRUE) {
  # Load packages
  suppressPackageStartupMessages({
    if (!require("dplyr")) {
      install.packages("dplyr")
      library(dplyr)
    }
  })
  ##################
  # Get Table Data #
  ##################

  num_rows <- nrow(data)
  variables <- names(data)
  types <- sapply(data, function(x) class(x)[1])
  nas <- colSums(is.na(data))
  nas_pct <- sprintf("%.2f%%", (round(nas / num_rows * 100, 2)))
  unique_vals <- sapply(data, function(x) length(unique(x)))

  # Create the data dictionary table
  dict_table <- data.frame(
    "Variable" = variables,
    "Type" = types,
    "NA" = nas,
    `%NA` = nas_pct,
    "Unique" = unique_vals,
    check.names = FALSE
  )

  ####################
  # Add Descriptions #
  ####################

  # Join detailed descriptions
  if (!is.null(descriptions)) {
    descriptions$Variable <- as.character(descriptions$Variable)
    descriptions$Description <- as.character(descriptions$Description)
    dict_table <- dict_table %>% dplyr::left_join(descriptions, by = "Variable")
  } else {
    dict_table$Description <- ""
  }

  # Reorder columns
  dict_table <- dict_table %>% select(Variable, Description, Type, everything())

  # Exclude NA columns if show_na is FALSE
  if (!show_na) {
    dict_table <- dict_table %>% select(-"NA", -"%NA")
  }

  ####################
  # Construct Output #
  ####################

  # Title for data dictionary

  # if data_title is missing use dataframe name
  if (missing(data_title))  {
    data_title <- deparse(substitute(data))
  }
  # Format for markdown
  data_title <- paste0("## **",data_title,"**\n")

  # Number of rows
  rows_text = paste0("**Number of rows:** `", format(num_rows, big.mark = ",", scientific = F),"` \n\n")

  # Number of rows with at least one NA
  rows_with_na = sum(rowSums(is.na(data)) > 0)
  rows_with_na_text = paste0("**Number of rows with NA:** `", format(rows_with_na, big.mark = ",", scientific = F),"` \n\n")

  # Data dictionary in markdown format
  md_dict <- knitr::kable(dict_table,
                          align = "llrrrl",
                          row.names = FALSE,
                          format = "markdown",
                          format.args = list(big.mark = ",",
                                             scientific = FALSE))

  md_dict <- paste(md_dict, collapse = "\n")

  # Combine all components into a single string
  output <- paste0(data_title, rows_text, rows_with_na_text, md_dict)

  # Print the combined output as is in markdown format
  knitr::asis_output(output)
}
