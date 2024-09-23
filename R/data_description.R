#' data_description
#'
#'Create a data description dataframe and optionally update the descriptions
#'
#'#' This function generates a dataframe of variables from the dataset and their descriptions.
#' If `description` is provided without `variable`, it updates the descriptions sequentially for the first N columns.
#' If both `variable` and `description` are provided, it updates the specified variables with the corresponding descriptions.
#'
#' @param data A dataframe whose variables are to be described.
#' @param variable A vector of variable names from the dataframe. If provided, descriptions will be updated for these variables.
#' @param description A vector of descriptions. If `variable` is not provided, descriptions will be applied sequentially to the first N columns.
#'                   If `variable` is provided, descriptions will be applied to the corresponding variables.
#'
#' @return A dataframe with the variables and their descriptions.
#' @export
#'
#' @examples
#' # Create a description dataframe without updating any descriptions
#' descriptions <- data_description(iris)
#' print(descriptions)
#'
#' # Update descriptions for the first two columns
#' descriptions <- data_description(iris, description = c("Length of sepals", "Width of sepals"))
#' print(descriptions)
#'
#' # Update descriptions for specific variables
#' descriptions <- data_description(iris,
#'                                  variable = c("Sepal.Length", "Species"),
#'                                  description = c("Length of sepals", "Species of the flower"))
#' print(descriptions)
data_description <- function(data, variable = NULL, description = NULL) {
  # Create initial descriptions dataframe
  descriptions <- data.frame(Variable = names(data), Description = "", stringsAsFactors = FALSE)

  # If description is provided without variable, update first N columns
  if (!is.null(description) && is.null(variable)) {
    n <- length(description)
    descriptions$Description[1:n] <- description
  }

  # If variable and description are provided, match variable name
  if (!is.null(variable) && !is.null(description)) {
    if (length(variable) != length(description)) {
      stop("The lengths of 'variable' and 'description' do not match.")
    }

    # Update the descriptions for the specified variables
    for (i in seq_along(variable)) {
      descriptions$Description[descriptions$Variable == variable[i]] <- description[i]
    }
  }
  return(descriptions)
}
