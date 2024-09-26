#' data_description
#'
#'Create a data description dataframe and optionally update the descriptions
#'
#'#' This function generates a dataframe of variables from the dataset and their descriptions.
#' If `description` is provided without `variable`, it updates the descriptions sequentially for the first N columns.
#' If both `variable` and `description` are provided, it updates the specified variables with the corresponding descriptions.
#'
#' @param data A dataframe whose variables are to be described.
#' @param var_desc A vector of variable names from the dataframe. If provided, descriptions will be updated for these variables.
#'              ##### edit docs #### description A vector of descriptions. If `variable` is not provided, descriptions will be applied sequentially to the first N columns.
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
#' descriptions <- data_description(iris, var_desc = c("Length of sepals", "Width of sepals"))
#' print(descriptions)
#'
#' # Update descriptions for specific variables
#' descriptions <- data_description(iris,
#'                                  var_desc = c("Sepal.Length" = "Length of sepals",
#'                                               "Species" = "Species of the flower"))
#' print(descriptions)
data_description <- function(data, var_desc = NULL) {
  # Create initial descriptions dataframe
  descriptions <- data.frame(Variable = names(data), Description = "", stringsAsFactors = FALSE)

  # if var_desc doesn't have names, use descriptions in order
  if (is.null(names(var_desc))) {
    n <- length(var_desc)
    descriptions$Description[1:n] <- var_desc
  } else {
    for (v in names(var_desc)) {
      descriptions$Description[descriptions$Variable == v] <- var_desc[[v]]
    }
  }
  return(descriptions)
}
