# This module allows selecting environmental variables
# and querying the GeoFRESH database


# Module UI function
envVarAnalysisUI <- function(id) {
  ns <- NS(id)

  # add elements to fluidRow() in module analysis page
  column(
    12,
    h3("Select environmental variables", align = "center"),
    p("Activate the checkboxes to select the required environmental
      information that should be summarized within the upstream
      catchment of each point.
      Please see the source and the citation for each category
      under the 'Documentation' tab."),
    fluidRow(
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxTopography"),
          label = "Topography",
          choices = data_list_inputData$`Topography`$Variable,
          extensions = checkboxExtensions$`Topography`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxClimate"),
          label = "Climate",
          choices = data_list_inputData$`Climate`$Variable,
          extensions = checkboxExtensions$`Climate`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxSoil"),
          label = "Soil",
          choices = data_list_inputData$`Soil`$Variable,
          extensions = checkboxExtensions$`Soil`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxLandcover"),
          label = "Landcover",
          choices = data_list_inputData$`Land cover`$Variable,
          extensions = checkboxExtensions$`Land cover`
        )
      )
    ),
    fluidRow(
      column(
        12,
        wellPanel(
          # button for starting query
          actionButton(
            ns("env_button"),
            "Start query",
            icon = icon("play"),
            class = "btn-primary"
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        sidebarLayout(
          sidebarPanel(
            textOutput(ns("topo_txt")),
            textOutput(ns("clim_txt")),
            textOutput(ns("soil_txt")),
            textOutput(ns("land_txt"))
          ),
          mainPanel(
            # show the queried environmental variables as tables in a tabsetPanel
            tabsetPanel(
              type = "tabs",
              tabPanel("Topography", tableOutput(ns("topo_table"))),
              tabPanel("Climate", tableOutput(ns("clim_table"))),
              tabPanel("Soil", tableOutput(ns("soil_table"))),
              tabPanel("Landcover", tableOutput(ns("land_table")))
            )
          )
        )
      )
    )
  )
}


# Module server function
envVarAnalysisServer <- function(id, point) {
  moduleServer(
    id,
    function(input, output, session) {
      stopifnot(is.reactive(point$user_table))

      # non-reactive data frame for displaying an empty table
      empty_df <- matrix(ncol = 3, nrow = 10) %>% as.data.frame()
      # column names for empty table
      column_names <- c("ID", "sub-catchment ID", "")

      column_defs <- list(
        list(orderable = FALSE, targets = "_all")
      )

      # Empty table, before query result
      tableServer("topo_table", empty_df, column_names, column_defs, searching = FALSE)

      # when user has uploaded CSV show ID of user points in tables
      observe({
        req(point$user_points())
        tableServer(
          "topo_table",
          point$user_points()[1] %>% cbind(empty_column1 = NA, empty_column2 = NA),
          column_names,
          column_defs = list(
            list(orderable = FALSE, targets = c(1, 2))
          )
        )
      })

      # when points are snapped show ID and sub-catchment ID
      observe({
        req(point$snap_points())
        tableServer(
          "topo_table",
          point$snap_points() %>% select("id", "subc_id") %>% cbind(empty_column = NA),
          column_names,
          column_defs = list(
            list(orderable = FALSE, targets = 2)
          )
        )
      })

      # render selected variables as text (just for testing)
      # later used to create database queries
      observe({
        output$topo_txt <- renderText({
          topo <- paste0(input$envCheckboxTopography, collapse = ", ")
          paste("Topography: ", topo)
        })
      })

      observe({
        output$clim_txt <- renderText({
          clim <- paste0(input$envCheckboxClimate, collapse = ", ")
          paste("Climate: ", clim)
        })
      })

      observe({
        output$soil_txt <- renderText({
          soil <- paste0(input$envCheckboxSoil, collapse = ", ")
          paste("Soil: ", soil)
        })
      })

      observe({
        output$land_txt <- renderText({
          land <- paste0(input$envCheckboxLandcover, collapse = ", ")
          paste("Land cover: ", land)
        })
      })

      # query environmental variables tables on button click
      query_result <- eventReactive(input$env_button,
        {
          # TODO: check if points are snapped first, display error message if not

          # set user input points table name
          points_table <- tbl(pool, in_schema("shiny_user", point$user_table()))

          # example query for table stats_topo, to be replaced by user selection
          points_table %>%
            left_join(stats_topo_tbl, by = c("reg_id", "subc_id")) %>%
            select(id, subc_id, shreve, scheidegger, length, stright, sinosoid, cum_length) %>%
            collect()
        },
        ignoreInit = TRUE
      )

      # show query result in a table
      observeEvent(query_result(), {
        # set column names (TODO: change to checkbox input)
        result_column_names <- c(
          "ID", "sub-catchment ID", "shreve", "scheidegger", "length",
          "stright", "sinosoid", "cum_length"
        )
        # call table module to render query result data
        tableServer("topo_table", query_result(), result_column_names)
      })
    }
  )
}
