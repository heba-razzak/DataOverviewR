#' #' data_dictionary
#'
#' Prints a data dictionary for a given dataset in markdown format.
#'
#' This function generates a data dictionary for the provided dataset. Optionally, it includes column descriptions, counts of missing values, and the top unique values for each column.
#'
#' @param data A dataframe for which the data dictionary will be printed.
#' @param data_title A title for the data dictionary.
#' @param descriptions A dataframe containing descriptions of the variables. It should have at least two columns: "Variable" and "Description".
#' @param show_na Logical; whether to display NA counts and percentages in the data dictionary. Default is `TRUE`.
#' @param top_n Integer; the number of top unique values to display for each column. Default is 5. top_n = 0 will remove column.
#'
#' @return None. Prints the data dictionary.
#' @importFrom rlang expr_label
#' @export
#'
#' @seealso \code{\link[explore]{data_dict_md}} for the original function in the `explore` package.
#'
#' @examples
#' # Create a description dataframe for iris and update one of the descriptions
#' desc <- data_description(iris, variable = c("Species"), description = c("Species of Flower"))
#'
#' # Print the data dictionary for the iris dataset
#' data_dictionary(iris, descriptions = desc, top_n = 1)

data_dictionary <- function(data, data_title = NULL, descriptions = NULL, top_n = 5, hide = NULL) {
  # hide = c("Type","NA_Count","NA_Percentage","N_Unique", "top_123")

  # Lowercase columns to hide
  hide <- tolower(hide)

  # Anything that starts with "top_" will be considered "top_n" (top_5...)
  if (any(grepl("top_", hide))) {
    hide <- c(hide,"top_n")
  }

  # If no title is provided use dataframe name
  if (is.null(data_title)) {
    data_title <- deparse(substitute(data))
  }

  # Number of rows
  num_rows <- nrow(data)

  # Number of rows that have missing values
  rows_with_na = sum(rowSums(is.na(data)) > 0)

  # Create the data dictionary table
  dict_table <- data.frame(
    "Column" = names(data),
    row.names = NULL
  )

  # Add Type column
  if (!"type" %in% hide) {
    dict_table$Type = sapply(data, function(x) class(x)[1])
  }

  # If descriptions are provided, match them to column names
  if (!is.null(descriptions) && (!"description" %in% hide)) {
    dict_table$Description <- ""
    for (col in descriptions$Variable) {
      dict_table$Description[dict_table$Column == col] <- descriptions[descriptions$Variable == col, "Description"]
    }
  }

  # Add NA_Count column
  if (!"na_count" %in% hide) {
    nas <- colSums(is.na(data))
    dict_table$NA_Count <- nas
  }

  # Add NA_Percentage column
  if (!"na_percentage" %in% hide) {
    nas_pct <- sapply(colSums(is.na(data)) / num_rows * 100, function(x) {
      if (x == 0) {
        return("")  # Leave empty for 0%
      } else {
        return(sprintf("%.0f%%", x))
      }
    })
    dict_table$NA_Percentage <- nas_pct
  }

  # Add N_Unique column
  if (!"n_unique" %in% hide) {
    num_unique <- sapply(data, function(x) length(unique(x)))
    dict_table$N_Unique <- num_unique
  }

  # Add Top_n column
  if ((top_n > 0) && (!"top_n" %in% hide)) {
    # Get top n unique values for each column
    top_n_values <- sapply(data, function(col) {
      # empty string for geometry columns
      if (any(grepl("sfc", class(col)))) {
        return("")
      }
      unique_vals <- unique(col)
      top_vals <- head(sort(table(col), decreasing = TRUE), top_n)
      paste(names(top_vals), collapse = ", ")
    })

    # Add the top n unique values as a new column
    dict_table[[paste0("Top_", top_n)]] <- top_n_values
  }

  # Format row numbers with commas
  rows_text <- format(num_rows, big.mark = ",", scientific = F)
  rows_na_text <- format(rows_with_na, big.mark = ",", scientific = F)

  # Data dictionary table in better format
  formatted_table <- knitr::kable(dict_table,
                                  align = "c",
                                  row.names = FALSE,
                                  format = "markdown",
                                  format.args = list(big.mark = ",",
                                                     scientific = FALSE))

  # Print output (markdown for knitting or console text otherwise)
  if (isTRUE(getOption('knitr.in.progress'))) {
    markdown_output <- paste0(
      "## **", data_title, "**\n\n\n",
      "`", rows_text, "`\trows\n\n",
      "`", rows_na_text, "`\trows with missing values\n\n",
      paste(formatted_table, collapse = "\n")
    )
    knitr::asis_output(markdown_output)
  } else {
    cat(data_title, "\n\n")
    cat(rows_text, "\trows\n")
    cat(rows_na_text, "\trows with missing values\n")
    print(formatted_table)
  }
}
