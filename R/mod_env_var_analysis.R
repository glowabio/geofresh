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
          choices = env_var_topo,
          extensions = checkboxExtensions$`Topography`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxClimate"),
          label = "Climate",
          choices = env_var_clim,
          extensions = checkboxExtensions$`Climate`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxSoil"),
          label = "Soil",
          choices = env_var_soil,
          extensions = checkboxExtensions$`Soil`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxLandcover"),
          label = "Landcover",
          choices = env_var_land,
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
      tableServer("clim_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("soil_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("land_table", empty_df, column_names, column_defs, searching = FALSE)

      # when user has uploaded CSV show ID of user points in tables
      observe({
        req(point$user_points())

        user_df <- point$user_points()[1] %>% cbind(empty_column1 = NA, empty_column2 = NA)

        column_defs <- list(list(orderable = FALSE, targets = c(1, 2)))

        tableServer("topo_table", user_df, column_names, column_defs)
        tableServer("clim_table", user_df, column_names, column_defs)
        tableServer("soil_table", user_df, column_names, column_defs)
        tableServer("land_table", user_df, column_names, column_defs)
      })

      # when points are snapped show ID and sub-catchment ID
      observe({
        req(point$snap_points())

        snap_df <- point$snap_points() %>%
          select("id", "subc_id") %>%
          cbind(empty_column = NA)

        column_defs <- list(list(orderable = FALSE, targets = 2))

        tableServer("topo_table", snap_df, column_names, column_defs)
        tableServer("clim_table", snap_df, column_names, column_defs)
        tableServer("soil_table", snap_df, column_names, column_defs)
        tableServer("land_table", snap_df, column_names, column_defs)
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

      # create empty dplyr connection for user input points table
      points_table <- reactive(NULL)
      # set user input points database table name
      observe({
        req(point$user_table())
        points_table <<- reactive(tbl(pool, in_schema("shiny_user", point$user_table())))
      })

      # create empty vectors for result column headers
      result_columns_topo <- c("")
      result_columns_clim <- c("")
      result_columns_soil <- c("")
      result_columns_land <- c("")

      ## query environmental variables tables on button click

      # get topography result for local sub-catchment
      query_result_topo <- eventReactive(input$env_button, {
        # TODO: check if points are snapped, display error message if not

        # check if database table with user input points exists
        req(points_table())

        # check if any topography variables are selected
        req(input$envCheckboxTopography)

        # create list of selected topography variable columns with statistics suffix
        topo_input <- sapply(input$envCheckboxTopography, function(x) {
          # check if input element is in topo_without_stats
          if (x %in% topo_without_stats) {
            x
          } else {
            stats <- c("_min", "_max", "_mean", "_sd")
            sapply(stats, function(y) {
              paste0(x, y)
            }, USE.NAMES = FALSE)
          }
        }, USE.NAMES = FALSE)

        # convert list to vector and add columns "id" and "subc_id" to the query
        topo_columns <- append(c(unlist(topo_input)), c("id", "subc_id"), after = 0)

        # set vector of resulting columns for table header
        result_columns_topo <<- topo_columns

        # query selected topography variables
        points_table() %>%
          left_join(stats_topo_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(topo_columns)) %>%
          collect()
      })

      # get climate result for local sub-catchment
      query_result_clim <- eventReactive(input$env_button, {
        # TODO: check if points are snapped first, display error message if not

        # check if database table with user input points exists
        req(points_table())
        # check if any climate variables are selected
        req(input$envCheckboxClimate)

        # create matrix of selected climate variable columns with statistics suffix
        clim_input <- sapply(input$envCheckboxClimate, function(x) {
          stats <- c("_min", "_max", "_mean", "_sd")
          sapply(stats, function(y) {
            paste0(x, y)
          })
        })

        # convert matrix to vector and add columns "id" and "subc_id" to the query
        clim_columns <- append(c(clim_input), c("id", "subc_id"), after = 0)

        # set vector of resulting columns for table header
        result_columns_clim <<- clim_columns

        # query selected climate variables
        points_table() %>%
          left_join(stats_clim_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(clim_columns)) %>%
          collect()
      })

      # get soil result for local sub-catchment
      query_result_soil <- eventReactive(input$env_button, {
        # TODO: check if points are snapped first, display error message if not

        req(points_table())
        req(input$envCheckboxSoil)

        # create matrix of selected soil variable columns with statistics suffix
        soil_input <- sapply(input$envCheckboxSoil, function(x) {
          stats <- c("_min", "_max", "_mean", "_sd")
          sapply(stats, function(y) {
            paste0(x, y)
          })
        })

        # convert matrix to vector and add columns "id" and "subc_id" to the query
        soil_columns <- append(c(soil_input), c("id", "subc_id"), after = 0)

        # set vector of resulting columns for table header
        result_columns_soil <<- soil_columns

        # example query for table stats_topo, to be replaced by user selection
        points_table() %>%
          left_join(stats_soil_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(soil_columns)) %>%
          collect()
      })
      # get land cover result for local sub-catchment
      query_result_land <- eventReactive(input$env_button, {
        # TODO: check if points are snapped first, display error message if not

        req(points_table())
        req(input$envCheckboxLandcover)

        # add columns "id" and "subc_id" to the query
        land_columns <- append(input$envCheckboxLandcover, c("id", "subc_id"), after = 0)

        # set vector of resulting columns for table header
        result_columns_land <<- land_columns

        # example query for table stats_topo, to be replaced by user selection
        points_table() %>%
          left_join(stats_land_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(land_columns)) %>%
          collect()
      })

      # show query result in the tables
      observeEvent(query_result_topo(), {
        # call table module to render query result data
        tableServer("topo_table", query_result_topo(), result_columns_topo)
      })

      observeEvent(query_result_clim(), {
        # call table module to render query result data for climate
        tableServer("clim_table", query_result_clim(), result_columns_clim)
      })

      observeEvent(query_result_soil(), {
        # call table module to render query result data for soil
        tableServer("soil_table", query_result_soil(), result_columns_soil)
      })

      observeEvent(query_result_land(), {
        # call table module to render query result data for land cover
        tableServer("land_table", query_result_land(), result_columns_land)
      })
    }
  )
}
