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
#' @param top_n Integer; the number of top unique values to display for each column. Default is 5.
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

data_dictionary <- function(data, data_title = NULL, descriptions = NULL, show_na = TRUE, top_n = 5) {
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
    "Type" = sapply(data, class),
    row.names = NULL
  )

  # If descriptions are provided, match them to column names
  if (!is.null(descriptions)) {
    dict_table$Description <- ""
    for (col in descriptions$Variable) {
      dict_table$Description[dict_table$Column == col] <- descriptions[descriptions$Variable == col, "Description"]
    }
  }

  # NA count and percentage
  if (show_na) {
    nas <- colSums(is.na(data))
    nas_pct <- sprintf("%.2f%%", nas / num_rows * 100)
    dict_table$NA_Count <- nas
    dict_table$NA_Percentage <- nas_pct
  }

  # Number of unique values
  num_unique <- sapply(data, function(x) length(unique(x)))
  dict_table$N_Unique <- num_unique

  # Get top n unique values for each column
  top_n_values <- sapply(data, function(col) {
    unique_vals <- unique(col)
    top_vals <- head(sort(table(col), decreasing = TRUE), top_n)
    paste(names(top_vals), collapse = ", ")
  })

  # Add the top n unique values as a new column
  dict_table[[paste0("Top_", top_n)]] <- top_n_values

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
