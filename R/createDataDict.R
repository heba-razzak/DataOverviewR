#' Print Skeleton for Variable Descriptions
#'
#' Generates a skeleton code to create a dataframe containing variable names
#' and their corresponding descriptions.
#' It is designed to simplify the process of creating input for
#' \code{\link{print_data_dict}} function by providing a template for describing
#' variables in a dataset.
#'
#' @param data The dataset for which the data dictionary is to be printed.
#'
#' @return None
#' @export
#'
#' @examples
#' print_descriptions_df(mtcars)
print_descriptions_df <- function(data) {
  # Get variable names
  variables <- names(data)

  # Initialize vectors to store variable names and descriptions
  variables_txt <- paste0("'", variables, "'", collapse =
                            ",\n                                       ")
  desc_txt <- paste0("'", variables, "_description'", collapse =
                       ",\n                                         ")

  # Construct Output Text
  txt1 <- "descriptions = data.frame(Variable = c("
  txt2 <- "),\n                          Description = c("
  txt3 <- "))\n\n"

  txt <- paste0(txt1, variables_txt, txt2, desc_txt, txt3)

  # Print the output
  cat(txt)
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
#'
#' @return None
#' @export
#'
#' @examples
#' descriptions = descriptions_df(mtcars)
#' print_data_dict(mtcars, data_title = "Cars", descriptions = descriptions)
print_data_dict <- function(data, data_title = "", descriptions = NULL) {
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

  ####################
  # Construct Output #
  ####################

  # if data_title is missing use dataframe name
  if (missing(data_title))  {
    data_title <- deparse(substitute(data))
  }

  data_title <- paste0("## **",data_title,"**\n")

  # create table that shows table name and number of rows
  rows_text = paste0("**Number of rows:** `", format(num_rows,
                                                big.mark = ",",
                                                scientific = F),"`\n")

  # Print Table name & Number of Rows
  knitr::knit_print(knitr::asis_output(data_title))
  knitr::knit_print(knitr::asis_output(rows_text))

  # Print Data Dictionary
  knitr::kable(dict_table,
                     align = "llrrrl",
                     row.names = FALSE,
                     format = "markdown",
                     format.args = list(big.mark = ",",
                                        scientific = FALSE))

}
