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
  select("Category", "Variable type", "Variable", "Description", "Column")


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
  as.character(data_list_inputData$`Topography`$Column),
  data_list_inputData$`Topography`$Variable
)

env_var_clim <- setNames(
  as.character(data_list_inputData$`Climate`$Column),
  data_list_inputData$`Climate`$Variable
)

env_var_soil <- setNames(
  as.character(data_list_inputData$`Soil`$Column),
  data_list_inputData$`Soil`$Variable
)

env_var_land <- setNames(
  as.character(data_list_inputData$`Land cover`$Column),
  data_list_inputData$`Land cover`$Variable
)

# set single value topography columns (without statistics)
topo_without_stats <- c(
  "strahler", "shreve", "horton", "hack", "topo_dim", "length", "stright",
  "sinusoid", "cum_length", "out_dist", "source_elev", "outlet_elev",
  "elev_drop", "out_drop", "gradient"
)

# set categorical value topography columns
topo_categorical <- c("strahler", "shreve", "horton", "hack", "topo_dim")

# set topography columns that are only valid for local sub-catchment
topo_local <- c("cum_length", "source_elev", "outlet_elev", "out_drop")

# create info button for displaying extra information for env variables on hover
checkBoxHelpList <- function(id, text) {
  extensionsList <- tipify(bsButtonRight(id, "",
    trigger = "hover",
    size = "extra-small",
    placement = "right",
    icon = icon("info")
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

# function for creating checkboxGroup labels
# including initially disabled "select/deselect all" checkbox with tooltip
set_checkbox_group_label <- function(title, subtitle, select_all_id, tooltip_title) {
  label <- tagList(
    h4(title),
    p(subtitle),
    shinyjs::disabled(
      checkboxInput(
        select_all_id,
        em("Select/Deselect all")
      )
    ),
    bsTooltip(
      select_all_id,
      tooltip_title,
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
  )
  return(label)
}

# function for adding custom tooltip to checkbox inputs or action buttons
add_custom_tooltip <- function(session, input_id, tooltip_title) {
  addTooltip(
    session,
    input_id,
    tooltip_title,
    placement = "right",
    trigger = "hover",
    options = list(container = "body")
  )
}
