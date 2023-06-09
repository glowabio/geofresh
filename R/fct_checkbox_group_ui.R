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


data <- fread("www/data/geofresh_environmental_variables.csv")[1:102, ] %>%
  select("Category", "Variable type", "Variable", "Description", "Table")


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

# create named lists for CheckboxGroup choices
env_var_topo <- setNames(
  as.character(data_list_inputData$`Topography`$Table),
  data_list_inputData$`Topography`$Variable
)

env_var_clim <- setNames(
  as.character(data_list_inputData$`Climate`$Table),
  data_list_inputData$`Climate`$Variable
)

env_var_soil <- setNames(
  as.character(data_list_inputData$`Soil`$Table),
  data_list_inputData$`Soil`$Variable
)

env_var_land <- setNames(
  as.character(data_list_inputData$`Land cover`$Table),
  data_list_inputData$`Land cover`$Variable
)

# set single value topography columns (without statistics)
topo_without_stats <- c(
  "strahler", "shreve", "horton", "hack", "topo_dim", "length", "stright",
  "sinusoid", "cum_length", "flow_accum", "out_dist", "source_elev",
  "outlet_elev", "elev_drop", "out_drop", "gradient"
)

# set categorical value topography columns
topo_categorical <- c("strahler", "shreve", "horton", "hack", "topo_dim")

# set topography columns that are only valid for local sub-catchment
topo_local <- c(
  "cum_length", "flow_accum", "source_elev",
  "outlet_elev", "out_drop"
)

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
