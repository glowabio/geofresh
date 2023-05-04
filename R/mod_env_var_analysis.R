# This module allows the selecting environmental variables
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
        ),
        textOutput(ns("topo_txt"))
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxClimate"),
          label = "Climate",
          choices = data_list_inputData$`Climate`$Variable,
          extensions = checkboxExtensions$`Climate`
        ),
        textOutput(ns("clim_txt"))
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxSoil"),
          label = "Soil",
          choices = data_list_inputData$`Soil`$Variable,
          extensions = checkboxExtensions$`Soil`
        ),
        textOutput(ns("soil_txt"))
      ),
      column(
        3,
        extendedCheckboxGroup(
          inputId = ns("envCheckboxLandcover"),
          label = "Landcover",
          choices = data_list_inputData$`Land cover`$Variable,
          extensions = checkboxExtensions$`Land cover`
        ),
        textOutput(ns("land_txt"))
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          # button for starting query
          actionButton(
            ns("env_button"),
            "Start query",
            icon = icon("play"),
            class = "btn-primary"
          )
        ),
        mainPanel(
          # show the queried environmental variables as a table
          DTOutput(ns("env_table")),
          uiOutput(ns("env_download"))
        )
      )
    )
  )
}


# Module server function
envVarAnalysisServer <- function(id, point) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      stopifnot(is.reactive(point$user_table))

      # output, non-reactive, to show an empty table
      df_non_reactive <- matrix(ncol = 3, nrow = 10) %>%
        as.data.frame()

      # Empty table, before query result
      empty_table <- renderDT({
        datatable(
          df_non_reactive,
          options = list(
            deferRender = TRUE,
            scrollX = TRUE,
            scrollY = "150px"
          ),
          rownames = FALSE,
          colnames = c("id", "sub-catchment id")
        )
      })
      output$env_table <- empty_table

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
        output$env_table <- renderDT({
          datatable(
            query_result(),
            options = list(
              deferRender = TRUE,
              scrollX = TRUE,
              scrollY = "150px"
            ),
            rownames = FALSE,
            colnames = c(
              "id", "sub-catchment id", "shreve", "scheidegger", "length",
              "stright", "sinosoid", "cum_length"
            )
          )
        })
      })

      # download the query result
      observeEvent(query_result(), {
        output$download <- renderUI({
          tagList(
            hr(),
            downloadDataUI(ns("env_download"))
          )
        })

        downloadDataServer("env_download", data = query_result())
      })
    }
  )
}
