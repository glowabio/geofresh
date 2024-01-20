# This module allows the user to plot the results of the environmental
# variables queries as interactive histograms or boxplots

# Module UI function
plotResultsUI <- function(id, label = "Plot results") {
  ns <- NS(id)
  tagList(
    p("Plot the results of environmental variables queries as histograms or boxplots."),
    tabsetPanel(
      tabPanel(
        "Topography",
        sidebarLayout(
          sidebarPanel(
            br(),
            selectInput(
              ns("topo_variable"),
              "Select variable to plot:",
              choices = NULL
            ),
            br(),
            sliderInput(
              ns("topo_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("topo_plot_local"))
          )
        )
      ),
      tabPanel(
        "Climate",
        sidebarLayout(
          sidebarPanel(
            br(),
            selectInput(
              ns("clim_variable"),
              "Select variable to plot:",
              choices = NULL
            ),
            br(),
            sliderInput(
              ns("clim_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("clim_plot_local"))
          )
        )
      ),
      tabPanel(
        "Soil",
        sidebarLayout(
          sidebarPanel(
            br(),
            selectInput(
              ns("soil_variable"),
              "Select variable to plot:",
              choices = NULL
            ),
            br(),
            sliderInput(
              ns("soil_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("soil_plot_local"))
          )
        )
      ),
      tabPanel(
        "Land cover",
        sidebarLayout(
          sidebarPanel(
            br()
          ),
          mainPanel(
            plotOutput(ns("land_plot_local"))
          )
        )
      )
    )
  )
}
# Module server function
# query results passed as reactive list of datasets by envVarAnalysisServer()
plotResultsServer <- function(id, datasets) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      # check if datasets is reactiveValues
      stopifnot(is.reactivevalues(datasets))

      # TODO: check for error in plot.window(need finite 'ylim' values)

      # update selectInput using topography variables from query result
      observe({
        df_topo <- datasets$topo[[1]]
        # get column names as list
        colnames_topo <- as.list(names(df_topo))
        # remove first two elements (id, subc_id)
        colnames_topo <- colnames_topo[-c(1, 2)]
        # subset list to get only mean values
        colnames_topo <- colnames_topo[grep("_mean", colnames_topo)]


        updateSelectInput(
          session,
          "topo_variable",
          choices = colnames_topo
        )
      })

      # update selectInput using climate variables from query result
      observe({
        df_clim <- datasets$clim[[1]]
        # get column names as list
        colnames_clim <- as.list(names(df_clim))
        # remove first two elements (id, subc_id)
        colnames_clim <- colnames_clim[-c(1, 2)]
        # subset list to get only mean values
        colnames_clim <- colnames_clim[grep("_mean", colnames_clim)]


        updateSelectInput(
          session,
          "clim_variable",
          choices = colnames_clim
        )
      })

      # update selectInput using soil variables from query result
      observe({
        df_soil <- datasets$soil[[1]]
        # get column names as list
        colnames_soil <- as.list(names(df_soil))
        # remove first two elements (id, subc_id)
        colnames_soil <- colnames_soil[-c(1, 2)]
        # subset list to get only mean values
        colnames_soil <- colnames_soil[grep("_mean", colnames_soil)]


        updateSelectInput(
          session,
          "soil_variable",
          choices = colnames_soil
        )
      })

      ## Plot histograms for topography, climate and soil
      # histogram for local topography
      observe({
        output$topo_plot_local <- renderPlot({
          df_topo <- datasets$topo[[1]]
          x <- df_topo[[input$topo_variable]]
          x <- na.omit(x)
          bins <- seq(min(x), max(x), length.out = input$topo_bins + 1)

          hist(x,
            breaks = bins,
            col = "#66a8d4",
            border = "black",
            xlab = input$topo_variable,
            main = paste(
              "Histogram of",
              input$topo_variable,
              # names(named_topography)[named_topography == input$topo_variable],
              "(local)"
            )
          )
        })
      })

      # histogram for local climate
      observe({
        output$clim_plot_local <- renderPlot({
          df_clim <- datasets$clim[[1]]
          x <- df_clim[[input$clim_variable]]
          x <- na.omit(x)
          bins <- seq(min(x), max(x), length.out = input$clim_bins + 1)

          hist(x,
            breaks = bins,
            col = "#66a8d4",
            border = "black",
            xlab = input$clim_variable,
            main = paste(
              "Histogram of",
              input$clim_variable,
              "(local)"
            )
          )
        })
      })

      # histogram for local soil
      observe({
        output$soil_plot_local <- renderPlot({
          df_soil <- datasets$soil[[1]]
          x <- df_soil[[input$soil_variable]]
          x <- na.omit(x)
          bins <- seq(min(x), max(x), length.out = input$soil_bins + 1)

          hist(x,
            breaks = bins,
            col = "#66a8d4",
            border = "black",
            xlab = input$soil_variable,
            main = paste(
              "Histogram of",
              input$soil_variable,
              "(local)"
            )
          )
        })
      })
    }
  )
}
