# This module allows selecting environmental variables
# and querying the GeoFRESH database

library(bslib)
library(bsicons)
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
          label = tagList(
            h4("Topography"),
            p("Hydrography90m stream topology")
          ),
          choices = env_var_topo,
          extensions = checkboxExtensions$`Topography`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxClimate"),
          label = tagList(
            h4("Climate"),
            p("Bioclimatic variables for 1981-2010")
          ),
          choices = env_var_clim,
          extensions = checkboxExtensions$`Climate`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxSoil"),
          label = tagList(
            h4("Soil"),
            p("Soil data for 2016")
          ),
          choices = env_var_soil,
          extensions = checkboxExtensions$`Soil`
        )
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxLandcover"),
          label = tagList(
            h4("Land cover"),
            p("Annual land cover for 2020")
          ),
          choices = env_var_land,
          extensions = checkboxExtensions$`Land cover`
        )
      )
    ),
    fluidRow(
      column(
        12,
        wellPanel(
          fluidRow(
            column(
              2,
              # button for starting query
              shinyjs::disabled(
                actionButton(ns("env_button_local"),
                             "Start query",
                             icon = icon("play"),
                             class = "btn-primary"
                             )
                ),
                bsTooltip(ns("env_button_local"),
                        "Please, snap points first",
                        "right",
                        options = list(container = "body")
                        ),
              shinyjs::hidden(p(id = ns("text1"), "Processing..."))
            ),
            column(
              10,
              p("Query selected environmental variables for the local sub-catchment (min, max, mean, sd)")
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        sidebarLayout(
          sidebarPanel(
            h4("Selected variables:"),
            textOutput(ns("topo_txt")),
            textOutput(ns("clim_txt")),
            textOutput(ns("soil_txt")),
            textOutput(ns("land_txt"))
          ),
          mainPanel(
            # show the queried environmental variables as tables in a tabsetPanel
            h4("Local sub-catchment"),
            tabsetPanel(
              id = "env_var_subcatchment",
              type = "tabs",
              tabPanel(
                "Topography",
                tableOutput(ns("topo_table")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("topo_download"))
              ),
              tabPanel(
                "Climate",
                tableOutput(ns("clim_table")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("clim_download"))
              ),
              tabPanel(
                "Soil",
                tableOutput(ns("soil_table")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("soil_download"))
              ),
              tabPanel(
                "Landcover",
                tableOutput(ns("land_table")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("land_download"))
              ),
            ),
            br()
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        wellPanel(
          fluidRow(
            column(
              2,
              # button for starting upstream query
              shinyjs::disabled(actionButton(
                ns("env_button_upstr"),
                "Start query",
                icon = icon("play"),
                class = "btn-primary"
              )),
              bsTooltip(ns("env_button_upstr"),
                        "Please, snap points first",
                        "right",
                        options = list(container = "body")
              ),
              shinyjs::hidden(p(id = ns("text2"), "Processing..."))
            ),
            column(
              10,
              p("Query selected environmental variables for the upstream catchment of each point (mean of sub-catchment means)")
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        sidebarLayout(
          sidebarPanel(
            h4("Selected variables:"),
            textOutput(ns("topo_txt_upstr")),
            textOutput(ns("clim_txt_upstr")),
            textOutput(ns("soil_txt_upstr")),
            textOutput(ns("land_txt_upstr"))
          ),
          mainPanel(
            # show the queried environmental variables as tables in a tabsetPanel
            h4("Upstream catchment"),
            tabsetPanel(
              id = "env_var_upstream",
              type = "tabs",
              tabPanel(
                "Topography",
                tableOutput(ns("topo_table_upstr")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("topo_download_upstr"))
              ),
              tabPanel(
                "Climate",
                tableOutput(ns("clim_table_upstr")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("clim_download_upstr"))
              ),
              tabPanel(
                "Soil",
                tableOutput(ns("soil_table_upstr")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("soil_download_upstr"))
              ),
              tabPanel(
                "Landcover",
                tableOutput(ns("land_table_upstr")) %>% withSpinner(hide.ui = FALSE),
                uiOutput(ns("land_download_upstr"))
              )
            ),
            br()
          )
        )
      )
    ),
    fluidRow(
      # download button for zipped results
      uiOutput(ns("download_zipped"))
    )
  )
}


# Module server function
envVarAnalysisServer <- function(id, point) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      stopifnot(is.reactive(point$user_table))

      # non-reactive data frame for displaying an empty table
      empty_df <- matrix(ncol = 3, nrow = 10) %>% as.data.frame()
      # column names for empty table
      column_names <- c("ID", "sub-catchment_ID", "")

      column_defs <- list(
        list(orderable = FALSE, targets = "_all")
      )

      # Empty table, before query result
      tableServer("topo_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("clim_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("soil_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("land_table", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("topo_table_upstr", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("clim_table_upstr", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("soil_table_upstr", empty_df, column_names, column_defs, searching = FALSE)
      tableServer("land_table_upstr", empty_df, column_names, column_defs, searching = FALSE)

      # when user has uploaded CSV show ID of user points in tables
      observe({
        req(point$user_points())

        user_df <- point$user_points()[1] %>% cbind(empty_column1 = NA, empty_column2 = NA)

        column_defs <- list(list(orderable = FALSE, targets = c(1, 2)))

        tableServer("topo_table", user_df, column_names, column_defs)
        tableServer("clim_table", user_df, column_names, column_defs)
        tableServer("soil_table", user_df, column_names, column_defs)
        tableServer("land_table", user_df, column_names, column_defs)
        tableServer("topo_table_upstr", user_df, column_names, column_defs)
        tableServer("clim_table_upstr", user_df, column_names, column_defs)
        tableServer("soil_table_upstr", user_df, column_names, column_defs)
        tableServer("land_table_upstr", user_df, column_names, column_defs)
      })

      # create reactive dataset list for collecting resulting CSVs in download zip file
      datasets <- reactiveValues()

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
        tableServer("topo_table_upstr", snap_df, column_names, column_defs)
        tableServer("clim_table_upstr", snap_df, column_names, column_defs)
        tableServer("soil_table_upstr", snap_df, column_names, column_defs)
        tableServer("land_table_upstr", snap_df, column_names, column_defs)

        # add snapped coordinate data frame as first dataset in ZIP file
        datasets$snapped <- list("-snapped-method-sub-catchment" = point$snap_points())
      })

      # render selected variables as text
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

      # render selected variables as text for upstream catchment
      # Todo: categorical variables, excluded variables
      observe({
        output$topo_txt_upstr <- renderText({
          topo <- paste0(input$envCheckboxTopography, collapse = ", ")
          paste("Topography: ", topo)
        })
      })

      observe({
        output$clim_txt_upstr <- renderText({
          clim <- paste0(input$envCheckboxClimate, collapse = ", ")
          paste("Climate: ", clim)
        })
      })

      observe({
        output$soil_txt_upstr <- renderText({
          soil <- paste0(input$envCheckboxSoil, collapse = ", ")
          paste("Soil: ", soil)
        })
      })

      observe({
        output$land_txt_upstr <- renderText({
          land <- paste0(input$envCheckboxLandcover, collapse = ", ")
          paste("Land cover: ", land)
        })
      })

      # create empty dplyr connection for user input points table
      points_table <- reactive({
        req(point$user_table())
        # set user input points database table name
        tbl(pool, in_schema("shiny_user", point$user_table()))
      })

      # create reactive values with empty vectors for result column headers
      result_columns <- reactiveValues(
        topo = c(""),
        clim = c(""),
        soil = c(""),
        land = c(""),
        topo_upstr = c(""),
        clim_upstr = c(""),
        soil_upstr = c(""),
        land_upstr = c("")
      )

      # activate query buttons after snapped coordinate data frame is created
      # and remove tooltips
      observeEvent(datasets$snapped, {
        toggleState("env_button_local")
        removeTooltip(session, ns("env_button_local"))
      })
      observeEvent(datasets$snapped, {
        toggleState("env_button_upstr")
        removeTooltip(session, ns("env_button_upstr"))
      })



      # check if checked box exist when clicking query button for local
      # sub-catchment
      observeEvent(input$env_button_local, {
        if (all(is.null(input$envCheckboxTopography), is.null(input$envCheckboxClimate), is.null(input$envCheckboxSoil), is.null(input$envCheckboxLandcover))) {
          print("No box is checked!!")
          validate(showModal(modalDialog(
            title = "Cannot continue...",
            "Please select some environmental variable (by checking one or several boxes above) before you can query for a variable summary!",
            easyClose = TRUE
          )))
        }
      })

      # check if checked box exist when clicking query button for upstream catchment
      observeEvent(input$env_button_upstr, {
        if (all(is.null(input$envCheckboxTopography), is.null(input$envCheckboxClimate), is.null(input$envCheckboxSoil), is.null(input$envCheckboxLandcover))) {
          print("No box is checked!!")
          validate(showModal(modalDialog(
            title = "Cannot continue...",
            "Please select some environmental variable (by checking one or several boxes above) before you can query for a variable summary!",
            easyClose = TRUE
          )))
        }
      })

      ## query environmental variables tables on button click

      # reactive values object to save query results
      query_results <- reactiveValues(
        topo = NULL,
        clim = NULL,
        soil = NULL,
        land = NULL,
        topo_upstr = NULL,
        clim_upstr = NULL,
        soil_upstr = NULL,
        land_upstr = NULL
      )

      # get topography result for local sub-catchment
      observeEvent(input$env_button_local, {
        # TODO: check if points are snapped, display error message if not

        # check if database table with user input points exists
        req(points_table())

        # check if any topography variables are selected
        req(input$envCheckboxTopography)

        # disable query button until calculation is done
        shinyjs::disable("env_button_local")
        shinyjs::show("text1")

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
        result_columns$topo <- topo_columns

        # query selected topography variables
        query_results$topo <- points_table() %>%
          left_join(stats_topo_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(topo_columns)) %>%
          collect()

        # enable button
        shinyjs::enable("env_button_local")
        shinyjs::hide("text1")
      })


      # get climate result for local sub-catchment
      observeEvent(input$env_button_local, {
        # TODO: check if points are snapped first, display error message if not

        # check if database table with user input points exists
        req(points_table())
        # check if any climate variables are selected
        req(input$envCheckboxClimate)

        # disable query button until calculation is done
        shinyjs::disable("env_button_local")

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
        result_columns$clim <- clim_columns

        # query selected climate variables
        query_results$clim <- points_table() %>%
          left_join(stats_clim_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(clim_columns)) %>%
          collect()
        # enable button
        shinyjs::enable("env_button_local")
      })


      # get soil result for local sub-catchment
      observeEvent(input$env_button_local, {
        # TODO: check if points are snapped first, display error message if not

        req(points_table())
        req(input$envCheckboxSoil)

        # disable query button until calculation is done
        shinyjs::disable("env_button_local")

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
        result_columns$soil <- soil_columns

        # example query for table stats_topo, to be replaced by user selection
        query_results$soil <- points_table() %>%
          left_join(stats_soil_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(soil_columns)) %>%
          collect()
        # enable button
        shinyjs::enable("env_button_local")
      })
      # get land cover result for local sub-catchment
      observeEvent(input$env_button_local, {
        # TODO: check if points are snapped first, display error message if not

        req(points_table())
        req(input$envCheckboxLandcover)

        # disable query button until calculation is done
        shinyjs::disable("env_button_local")

        # add columns "id" and "subc_id" to the query
        land_columns <- append(input$envCheckboxLandcover, c("id", "subc_id"), after = 0)

        # set vector of resulting columns for table header
        result_columns$land <- land_columns

        # example query for table stats_topo, to be replaced by user selection
        query_results$land <- points_table() %>%
          left_join(stats_land_tbl, by = c("reg_id", "subc_id")) %>%
          select(all_of(land_columns)) %>%
          collect()
        # enable button
        shinyjs::enable("env_button_local")
      })

      # calculate upstream catchment for each user point when user selects
      # the first environmental variable; run only once
      # return TRUE when finished
      upstream_done <- reactive(NULL)

      observeEvent(
        list(
          input$envCheckboxTopography,
          input$envCheckboxClimate,
          input$envCheckboxSoil,
          input$envCheckboxLandcover
        ),
        {
          req(points_table())
          req(point$snap_points())

          print("calculating upstream catchment")

          # update user point table calculate upstream catchment IDs
          sql <- sqlInterpolate(pool,
            "UPDATE ?point_table poi SET
            upstream = sub.nodes
              FROM (SELECT upstr.subc_id, upstr.nodes FROM ?point_table poi,
              hydro.pgr_upstreamcomponent(poi.subc_id, poi.reg_id, poi.basin_id) upstr
              WHERE poi.strahler_order != 1) AS sub
            WHERE sub.subc_id = poi.subc_id",
            point_table = dbQuoteIdentifier(pool, Id(schema = "shiny_user", table = point$user_table()))
          )
          dbExecute(pool, sql)

          print("calculating upstream catchment done")
          upstream_done <<- reactive(TRUE)
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        once = TRUE
      )


      ## upstream catchment aggregates

      # get topography result for upstream catchment
      observeEvent(input$env_button_upstr, {
        # TODO: check if points are snapped, display error message if not
        req(point$snap_points())
        req(points_table())
        req(input$envCheckboxTopography)

        # disable query button until calculation is done
        shinyjs::disable("env_button_upstr")
        shinyjs::show("text2")

        # get upstream catchment with component analysis
        # TODO: move to module upload_csv and check here if done
        # TODO: display error message or timer if upstream catchment calculation not done
        req(upstream_done())

        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_segments")

        # create list of selected topography variable columns with '_mean' suffix
        # only for non-categorical and variables that not only meaningful for the local sub-catchment
        # TODO: add function for categorical
        # TODO: add min, max, sd
        topo_input_upstr <- sapply(input$envCheckboxTopography, function(x) {
          # exclude input elements if in topo_local or topo_categorical
          # if in topo_without_stats return variable name as is
          if (x %in% c(topo_local, topo_categorical)) {
            NULL
          } else {
            if (x %in% topo_without_stats) x else paste0(x, "_mean")
          }
        }, USE.NAMES = FALSE)

        topo_columns_upstr_query <- sapply(topo_input_upstr, function(x) {
          # add query text to column names
          if (!is.null(x)) {
            paste0("round(avg(", x, ")::numeric, 4) AS ", x)
          }
        }, USE.NAMES = FALSE)

        # convert list to vector and add columns "id" and "subc_id"
        # for table header
        topo_columns_upstr <- append(c(unlist(topo_input_upstr)),
          c("id", "subc_id"),
          after = 0
        )

        # set vector of resulting columns for table header
        result_columns$topo_upstr <- topo_columns_upstr

        # aggregate query for non-categorical values
        sql_string <- paste(
          "WITH upstream AS (
            SELECT poi.id, poi.reg_id, poi.subc_id,
            unnest(poi.subc_id || upstream) AS upstr_id
            FROM ?point_table poi
            WHERE poi.strahler_order != 1
          )
          SELECT up.id, min(up.subc_id) AS subc_id,",
          paste0(topo_columns_upstr_query, collapse = ", "),
          "FROM upstream up LEFT JOIN ?topo_table topo
          ON up.upstr_id = topo.subc_id
          GROUP BY up.id"
        )

        sql <- sqlInterpolate(pool,
          sql_string,
          point_table = dbQuoteIdentifier(pool, Id(schema = "shiny_user", table = point$user_table())),
          topo_table = dbQuoteIdentifier(pool, Id(schema = "hydro", table = "stats_topo"))
        )

        # return resulting dataframe
        query_results$topo_upstr <- dbGetQuery(pool, sql)

        # enable button
        shinyjs::enable("env_button_upstr")
        shinyjs::hide("text2")
      })

      # get climate result for upstream catchment
      observeEvent(input$env_button_upstr, {
        # TODO: check if points are snapped, display error message if not
        req(point$snap_points())
        req(points_table())
        req(input$envCheckboxClimate)

        # disable query button until calculation is done
        shinyjs::disable("env_button_upstr")
        shinyjs::show("text2")

        # get upstream catchment with component analysis
        # TODO: move to module upload_csv and check here if done
        # TODO: display error message or timer if upstream catchment calculation not done
        req(upstream_done())

        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_segments")

        # create list of selected climate variable columns with '_mean' suffix
        # TODO: add min, max, sd
        clim_input_upstr <- sapply(input$envCheckboxClimate, function(x) {
          paste0(x, "_mean")
        }, USE.NAMES = FALSE)

        clim_columns_upstr_query <- sapply(clim_input_upstr, function(x) {
          # add query text to column names
          paste0("round(avg(", x, ")::numeric, 4) AS ", x)
        }, USE.NAMES = FALSE)

        # convert list to vector and add columns "id" and "subc_id"
        # for table header
        clim_columns_upstr <- append(c(unlist(clim_input_upstr)),
          c("id", "subc_id"),
          after = 0
        )

        # set vector of resulting columns for table header
        result_columns$clim_upstr <- clim_columns_upstr

        # aggregate query for non-categorical values
        sql_string <- paste(
          "WITH upstream AS (
            SELECT poi.id, poi.reg_id, poi.subc_id,
            unnest(poi.subc_id || upstream) AS upstr_id
            FROM ?point_table poi
            WHERE poi.strahler_order != 1
          )
          SELECT up.id, min(up.subc_id) AS subc_id,",
          paste0(clim_columns_upstr_query, collapse = ", "),
          "FROM upstream up LEFT JOIN ?clim_table clim
          ON up.upstr_id = clim.subc_id
          GROUP BY up.id"
        )

        sql <- sqlInterpolate(pool,
          sql_string,
          point_table = dbQuoteIdentifier(pool, Id(schema = "shiny_user", table = point$user_table())),
          clim_table = dbQuoteIdentifier(pool, Id(schema = "hydro", table = "stats_climate"))
        )

        # return resulting dataframe
        query_results$clim_upstr <- dbGetQuery(pool, sql)

        # enable button
        shinyjs::enable("env_button_upstr")
        shinyjs::hide("text2")
      })

      # get soil result for upstream catchment
      observeEvent(input$env_button_upstr, {
        # TODO: check if points are snapped, display error message if not
        req(point$snap_points())
        req(points_table())
        req(input$envCheckboxSoil)

        # disable query button until calculation is done
        shinyjs::disable("env_button_upstr")
        shinyjs::show("text2")

        # get upstream catchment with component analysis
        # TODO: move to module upload_csv and check here if done
        # TODO: display error message or timer if upstream catchment calculation not done
        req(upstream_done())

        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_segments")

        # create list of selected climate variable columns with '_mean' suffix
        # TODO: add min, max, sd
        soil_input_upstr <- sapply(input$envCheckboxSoil, function(x) {
          paste0(x, "_mean")
        }, USE.NAMES = FALSE)

        soil_columns_upstr_query <- sapply(soil_input_upstr, function(x) {
          # add query text to column names
          paste0("round(avg(", x, ")::numeric, 4) AS ", x)
        }, USE.NAMES = FALSE)

        # convert list to vector and add columns "id" and "subc_id"
        # for table header
        soil_columns_upstr <- append(c(unlist(soil_input_upstr)),
          c("id", "subc_id"),
          after = 0
        )

        # set vector of resulting columns for table header
        result_columns$soil_upstr <- soil_columns_upstr

        # aggregate query for non-categorical values
        sql_string <- paste(
          "WITH upstream AS (
            SELECT poi.id, poi.reg_id, poi.subc_id,
            unnest(poi.subc_id || upstream) AS upstr_id
            FROM ?point_table poi
            WHERE poi.strahler_order != 1
          )
          SELECT up.id, min(up.subc_id) AS subc_id,",
          paste0(soil_columns_upstr_query, collapse = ", "),
          "FROM upstream up LEFT JOIN ?stats_table stats
          ON up.upstr_id = stats.subc_id
          GROUP BY up.id"
        )

        sql <- sqlInterpolate(pool,
          sql_string,
          point_table = dbQuoteIdentifier(pool, Id(schema = "shiny_user", table = point$user_table())),
          stats_table = dbQuoteIdentifier(pool, Id(schema = "hydro", table = "stats_soil"))
        )

        # return resulting dataframe
        query_results$soil_upstr <- dbGetQuery(pool, sql)

        # enable button
        shinyjs::enable("env_button_upstr")
        shinyjs::hide("text2")
      })

      # get landcover result for upstream catchment
      observeEvent(input$env_button_upstr, {
        # TODO: check if points are snapped, display error message if not
        req(point$snap_points())
        req(points_table())
        req(input$envCheckboxLandcover)

        # disable query button until calculation is done
        shinyjs::disable("env_button_upstr")
        shinyjs::show("text2")

        # get upstream catchment with component analysis
        # TODO: move to module upload_csv and check here if done
        # TODO: display error message or timer if upstream catchment calculation not done
        req(upstream_done())

        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_segments")

        # create list of selected climate variable columns with '_mean' suffix
        # TODO: add min, max, sd
        land_input_upstr <- sapply(input$envCheckboxLandcover, function(x) {
          paste0(x, "_mean")
        }, USE.NAMES = FALSE)

        # create query text for selected columns
        land_columns_upstr_query <- sapply(input$envCheckboxLandcover, function(x) {
          # add query text to column names
          paste0("round(avg(", x, ")::numeric, 4) AS ", x)
        }, USE.NAMES = FALSE)

        # convert list to vector and add columns "id" and "subc_id"
        # for table header
        land_columns_upstr <- append(c(unlist(land_input_upstr)),
          c("id", "subc_id"),
          after = 0
        )

        # set vector of resulting columns for table header
        result_columns$land_upstr <- land_columns_upstr

        # aggregate query for non-categorical values
        sql_string <- paste(
          "WITH upstream AS (
            SELECT poi.id, poi.reg_id, poi.subc_id,
            unnest(poi.subc_id || upstream) AS upstr_id
            FROM ?point_table poi
            WHERE poi.strahler_order != 1
          )
          SELECT up.id, min(up.subc_id) AS subc_id,",
          paste0(land_columns_upstr_query, collapse = ", "),
          "FROM upstream up LEFT JOIN ?stats_table stats
          ON up.upstr_id = stats.subc_id
          GROUP BY up.id"
        )

        sql <- sqlInterpolate(pool,
          sql_string,
          point_table = dbQuoteIdentifier(pool, Id(schema = "shiny_user", table = point$user_table())),
          stats_table = dbQuoteIdentifier(pool, Id(schema = "hydro", table = "stats_landuse"))
        )

        # return resulting dataframe
        query_results$land_upstr <- dbGetQuery(pool, sql)

        # enable button
        shinyjs::enable("env_button_upstr")
        shinyjs::hide("text2")
      })

      ## Show query result in the tables
      # local sub-catchment
      observeEvent(query_results$topo, {
        # call table module to render query result data for topography
        tableServer("topo_table", query_results$topo, result_columns$topo)
        # call download module to render single download button for table
        output$topo_download <- renderUI({
          tagList(
            downloadDataUI(ns("topo_download"),
              label = "Download topography data for local catchment"
            )
          )
        })
        downloadDataServer("topo_download",
          dataset = query_results$topo,
          file_name = "-env-var-topography-local"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$topo <- list("-env-var-topography-local" = query_results$topo)
      })


      observeEvent(query_results$clim, {
        # call table module to render query result data for climate
        tableServer("clim_table", query_results$clim, result_columns$clim)
        # call download module to render single download button for table
        output$clim_download <- renderUI({
          tagList(
            downloadDataUI(ns("clim_download"),
              label = "Download climate data for local catchment"
            )
          )
        })
        downloadDataServer("clim_download",
          dataset = query_results$clim,
          file_name = "-env-var-climate-local"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$clim <- list("-env-var-climate-local" = query_results$clim)
      })

      observeEvent(query_results$soil, {
        # call table module to render query result data for soil
        tableServer("soil_table", query_results$soil, result_columns$soil)
        # call download module to render single download button for table
        output$soil_download <- renderUI({
          tagList(
            downloadDataUI(ns("soil_download"),
              label = "Download soil data for local catchment"
            )
          )
        })
        downloadDataServer("soil_download",
          dataset = query_results$soil,
          file_name = "-env-var-soil-local"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$soil <- list("-env-var-soil-local" = query_results$soil)
      })

      observeEvent(query_results$land, {
        # call table module to render query result data for land cover
        tableServer("land_table", query_results$land, result_columns$land)
        # call download module to render single download button for table
        output$land_download <- renderUI({
          tagList(
            downloadDataUI(ns("land_download"),
              label = "Download land cover data for local catchment"
            )
          )
        })
        downloadDataServer("land_download",
          dataset = query_results$land,
          file_name = "-env-var-land-cover-local"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$land <- list("-env-var-land-cover-local" = query_results$land)
      })

      # upstream catchment
      observeEvent(query_results$topo_upstr, {
        tableServer("topo_table_upstr", query_results$topo_upstr, result_columns$topo_upstr)
        output$topo_download_upstr <- renderUI({
          tagList(
            downloadDataUI(ns("topo_download_upstr"),
              label = "Download topography data for upstream catchment"
            )
          )
        })
        downloadDataServer("topo_download_upstr",
          dataset = query_results$topo_upstr,
          file_name = "-env-var-topography-upstream"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$topo_upstr <- list("-env-var-topography-upstream" = query_results$topo_upstr)
      })

      observeEvent(query_results$clim_upstr, {
        tableServer("clim_table_upstr", query_results$clim_upstr, result_columns$clim_upstr)
        output$clim_download_upstr <- renderUI({
          tagList(
            downloadDataUI(ns("clim_download_upstr"),
              label = "Download climate data for upstream catchment"
            )
          )
        })
        downloadDataServer("clim_download_upstr",
          dataset = query_results$clim_upstr,
          file_name = "-env-var-climate-upstream"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$clim_upstr <- list("-env-var-climate-upstream" = query_results$clim_upstr)
      })

      observeEvent(query_results$soil_upstr, {
        tableServer("soil_table_upstr", query_results$soil_upstr, result_columns$soil_upstr)
        output$soil_download_upstr <- renderUI({
          tagList(
            downloadDataUI(ns("soil_download_upstr"),
              label = "Download soil data for upstream catchment"
            )
          )
        })
        downloadDataServer("soil_download_upstr",
          dataset = query_results$soil_upstr,
          file_name = "-env-var-soil-upstream"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$soil_upstr <- list("-env-var-soil-upstream" = query_results$soil_upstr)
      })

      observeEvent(query_results$land_upstr, {
        tableServer("land_table_upstr", query_results$land_upstr, result_columns$land_upstr)
        output$land_download_upstr <- renderUI({
          tagList(
            downloadDataUI(ns("land_download_upstr"),
              label = "Download land cover data for upstream catchment"
            )
          )
        })
        downloadDataServer("land_download_upstr",
          dataset = query_results$land_upstr,
          file_name = "-env-var-land-cover-upstream"
        )
        # add to dataset reactiveValues object for zipped download
        datasets$land_upstr <- list("-env-var-land-cover-upstream" = query_results$land_upstr)
      })

      # When list of datasets is created, show download button for zipped files
      observeEvent(datasets$snapped, {
        # UI for download button zipped results
        output$download_zipped <- renderUI({
          column(
            12,
            wellPanel(
              fluidRow(
                column(
                  2,
                  # download button for zipped results
                  downloadDataUI(ns("download_zipped"), label = "Download ZIP")
                ),
                column(
                  10,
                  p("Download resulting CSVs in a ZIP file")
                )
              )
            )
          )
        })

        downloadDataServer("download_zipped",
          dataset = datasets,
          zipped = TRUE,
          file_name = "-results"
        )
      })
    }
  )
}
