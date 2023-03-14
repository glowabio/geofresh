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
choiceNames <- data$Variable

txt <- data$Description

ids <- paste0("pB", rep(1:length(data$Variable)))

inputData <- data.frame(cbid = ids, helpInfoText = txt)

inputData$cbid <- sapply(inputData$cbid, as.character)

inputData$helpInfoText <- sapply(inputData$helpInfoText, as.character)

checkBoxHelpList <- function(id, Text) {
  extensionsList <- tipify(bsButtonRight(id, "?",
    trigger = "hover",
    size = "extra-small"
  ), Text)

  return(extensionsList)
}


helpList <- split(inputData, f = rownames(inputData))

checkboxExtensions <- lapply(helpList, function(x) {
  checkBoxHelpList(
    x[1],
    as.character(x[2])
  )
})
