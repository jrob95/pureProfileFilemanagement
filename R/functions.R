# notes
# modify as you need for your dataset.
# use your attributes instead of the field `youratts`

# packages
# read, tidy, display
library(tidyverse)
library(conflicted)
library(readxl)

# conflicts
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::starts_with)

# functions

## data dictionary - label fields and levels
label_factors_with_variable_names <- function(data, data_dict, dict_nm) {
  # Label factors based on data_dict
  for (i in 1:nrow(data_dict)) {
    field <- data_dict$field[i]
    value <- data_dict$value[i]
    label <- data_dict$label[i]

    if (field %in% names(data)) {
      if (is.factor(data[[field]])) {
        # If it's already a factor, just label its levels
        levels(data[[field]])[levels(data[[field]]) == value] <- label
      } else {
        # If it's not a factor, convert it to factor and then label its levels
        data[[field]] <- factor(data[[field]])
        levels(data[[field]])[levels(data[[field]]) == value] <- label
      }
    }
  }

  # Label fields based on dict_nm
  for (i in 1:nrow(dict_nm)) {
    variable <- dict_nm$Variable[i]
    label <- dict_nm$Label[i]

    if (variable %in% names(data)) {
      attr(data[[variable]], "label") <- label
    }
  }

  return(data)
}
