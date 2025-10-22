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

# data - can also import from excel or whatever.
data <- read_csv("your.csv")

dict_lvl <- read_xlsx(
  "your_datadictionary.xlsx",
  sheet = "Variable Values",
  .name_repair = "unique_quiet"
) %>% # level labels
  rename("field" = "Variable Values", "value" = "...2", "label" = "...3") %>%
  slice(2:n()) %>%
  fill(field) %>%
  glimpse()

dict_nm <- read_xlsx(
  "your_datadictionary.xlsx",
  sheet = "Variable Information",
  skip = 1
)

# use above defined functions to label the data
data_lbl <- label_factors_with_variable_names(data, dict_lvl, dict_nm)


# need as long for mlogit
# can join on relevent demo/ person data adhoc.
# dce data nd - no demographics
dce_nd <- data_lbl %>%
  select(
    RECORD,
    starts_with("DCE_"),
    starts_with("HQ"),
    -starts_with("HQCHOICE_SITUATION_")
  ) %>%
  pivot_longer(
    cols = matches("HQ....._[1-9]|1[012]_."),
    names_sep = "_",
    names_to = c("attr", "task", "alt"),
    values_to = "level"
  ) %>%
  pivot_longer(
    cols = starts_with("DCE_"),
    names_prefix = "DCE_",
    names_to = "dce_task",
    values_to = "choice"
  ) %>%
  filter(task == dce_task) %>%
  select(-c(starts_with("HQSET_"), "dce_task")) %>%
  pivot_wider(
    id_cols = c("RECORD", "HQBLOCK", "alt", "choice", "task"),
    names_from = "attr",
    values_from = "level"
  ) %>%
  mutate(
    choice = case_when(
      alt == "1" & choice == "Option A" ~ TRUE,
      alt == "2" & choice == "Option B" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  rename(id = "RECORD", question = "task") %>%
  rename_all(~ stringr::str_replace(., "^HQ", "")) %>%
  rename_with(tolower)

dce_not_dummy <- dce_nd %>%
  mutate(
    choice = as.numeric(choice),
    # need question per id.
    str = as.numeric(paste0(
      1,
      str_pad(as.character(id), 4, pad = "0"),
      str_pad(as.character(question), 2, pad = "0")
    ))
  )
# note: choice 11 is the dominant task

find_repeat <- dce_not_dummy %>%
  pivot_wider(
    id_cols = c("id", "block", "question"),
    names_from = "alt",
    values_from = c("youratts", "choice")
  ) %>%
  mutate(
    choice = case_when(choice_1 == 1 ~ 1, choice_2 == 1 ~ 2, TRUE ~ NA)
  ) %>%
  select(-c(choice_1, choice_2)) %>%
  group_by(id, block, youratts) %>%
  mutate(dupe = n() > 1) %>%
  filter(dupe == TRUE) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(same = n_distinct(choice) == 1)


cLogitMnl <- survival::clogit(
  choice ~ alt + youratts + strata(str),
  data = dce_cleaned
)


# use randPars for MIXL see ?logitr
LogitrMnl <- logitr(
  data = dce_cleaned,
  outcome = "choice",
  obsID = "str",
  pars = c("alt", "youratts"),
  panelID = "id",
  numCores = parallel::detectCores() - 1
)
