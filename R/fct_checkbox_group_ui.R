library(dplyr)
library(data.table)
library(shinyBS)

# functions for extended checkbox group in analysis page

extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions <- length(extensions)
  nChoices <- length(cbg$children[[2]]$children[[1]])

  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

# place the button on the right and not directly after the text
bsButtonRight <- function(...) {
  # Bootstrap component button from package shinyBS
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}


data <- fread("www/data/geofresh_environmental_variables.csv")[1:104, ] %>%
  select("Category", "Variable type", "Variable", "Description")


data_list <- data %>%
  group_split(Category) %>%
  setNames(sort(unique(data$Category)))


data_list_inputData <- lapply(data_list, function(x) {
  # make IDs unique for each category
  cbind(x, "ids" = paste0(tolower(substr(x[1, 1], 1, 4)), sprintf("%02d", rep(1:nrow(x)))))
})

# order the ids in a lexicographic way
data_list_inputData <- lapply(data_list_inputData, function(df) {
  df[order(df$ids), ]
})

checkBoxHelpList <- function(id, text) {
  extensionsList <- tipify(bsButtonRight(id, "?",
    trigger = "hover",
    size = "extra-small",
    placement = "right"
  ), text)

  return(extensionsList)
}

helpList <- lapply(data_list_inputData, function(x) {
  split(x, x$ids)
})

checkboxExtensions <- lapply(helpList, function(x) {
  lapply(x, function(x) {
    checkBoxHelpList(id = x[[5]], text = x[[4]])
  })
})
