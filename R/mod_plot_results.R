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

      # create empty reactiveValues lists for named environmental variable subsets
      env_var_subset <- reactiveValues(topo = NULL, clim = NULL, soil = NULL, land = NULL)

      # create empty reactiveValues lists for mean values column names
      colnames_split <- reactiveValues(topo = NULL, clim = NULL, soil = NULL)

      # filter column name lists to get single value topography column names
      # and mean value column names
      observeEvent(datasets$topo, {
        colnames_split$topo <- get_col_names_for_plots(datasets$topo)
      })
      observeEvent(datasets$clim, {
        colnames_split$clim <- get_col_names_for_plots(datasets$clim)
      })
      observeEvent(datasets$soil, {
        colnames_split$soil <- get_col_names_for_plots(datasets$soil)
      })


      observeEvent(colnames_split$topo, {
        # filter named environmental variables list
        env_var_subset$topo <- env_var_topo[env_var_topo %in% names(colnames_split$topo)]
        # update selectInput for topography
        updateSelectInput(
          session,
          "topo_variable",
          choices = env_var_subset$topo
        )
      })

      observeEvent(colnames_split$clim, {
        # filter named environmental variables list
        env_var_subset$clim <- env_var_clim[env_var_clim %in% names(colnames_split$clim)]
        # update selectInput for climate
        updateSelectInput(
          session,
          "clim_variable",
          choices = env_var_subset$clim
        )
      })

      observeEvent(colnames_split$soil, {
        # filter named environmental variables list
        env_var_subset$soil <- env_var_soil[env_var_soil %in% names(colnames_split$soil)]
        # update selectInput for soil
        updateSelectInput(
          session,
          "soil_variable",
          choices = env_var_subset$soil
        )
      })

      ## Plot histograms for topography, climate and soil
      # histogram for local topography
      output$topo_plot_local <- renderPlot({
        req(datasets$topo)

        selected_column <- colnames_split$topo[[input$topo_variable]]

        x <- datasets$topo[[1]] %>%
          pull(selected_column) %>%
          na.omit()

        bins <- seq(min(x), max(x), length.out = input$topo_bins + 1)

        hist(x,
          breaks = bins,
          col = "#66a8d4",
          border = "black",
          xlab = selected_column,
          main = paste0(
            'Histogram of "',
            names(env_var_subset$topo[env_var_subset$topo %in% input$topo_variable]),
            '" for local sub-catchment'
          )
        )
      })

      # histogram for local climate
      output$clim_plot_local <- renderPlot({
        req(datasets$clim)

        selected_column <- colnames_split$clim[[input$clim_variable]]

        x <- datasets$clim[[1]] %>%
          pull(selected_column) %>%
          na.omit()

        bins <- seq(min(x), max(x), length.out = input$clim_bins + 1)

        hist(x,
          breaks = bins,
          col = "#66a8d4",
          border = "black",
          xlab = selected_column,
          main = paste0(
            'Histogram of "',
            names(env_var_subset$clim[env_var_subset$clim %in% input$clim_variable]),
            '" for local sub-catchment'
          )
        )
      })

      # histogram for local soil
      output$soil_plot_local <- renderPlot({
        req(datasets$soil)

        selected_column <- colnames_split$soil[[input$soil_variable]]

        x <- datasets$soil[[1]] %>%
          pull(selected_column) %>%
          na.omit()

        bins <- seq(min(x), max(x), length.out = input$soil_bins + 1)

        hist(x,
          breaks = bins,
          col = "#66a8d4",
          border = "black",
          xlab = selected_column,
          main = paste0(
            'Histogram of "',
            names(env_var_subset$soil[env_var_subset$soil %in% input$soil_variable]),
            '" for local sub-catchment'
          )
        )
      })
    }
  )
}
