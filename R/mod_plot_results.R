# This module allows the user to plot the results of the environmental
# variables queries as interactive histograms or boxplots

# Module UI function
plotResultsUI <- function(id, label = "Plot results") {
  ns <- NS(id)
  tagList(
    p("Plot results of environmental variables queries as histograms for
      topography, climate and soil or boxplots for land cover."),
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
            fluidRow(
              column(plotOutput(ns("topo_plot_local")), width = 6),
              column(plotOutput(ns("topo_plot_upstr")), width = 6)
            )
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
            fluidRow(
              column(plotOutput(ns("clim_plot_local")), width = 6),
              column(plotOutput(ns("clim_plot_upstr")), width = 6)
            )
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
            fluidRow(
              column(plotOutput(ns("soil_plot_local")), width = 6),
              column(plotOutput(ns("soil_plot_upstr")), width = 6)
            )
          )
        )
      ),
      tabPanel(
        "Land cover",
        sidebarLayout(
          sidebarPanel(
            br(),
            strong("Select variables to plot:"),
            checkboxInput(
              ns("select_all"),
              em("Select/Deselect all")
            ),
            checkboxGroupInput(
              ns("land_variables"),
              label = NULL,
              choices = NULL
            ),
          ),
          mainPanel(
            plotOutput(ns("land_plot_local")),
            plotOutput(ns("land_plot_upstr"))
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
      colnames_split <- reactiveValues(topo = NULL, clim = NULL, soil = NULL, land = NULL)

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

      # filter column name list for land cover
      observeEvent(datasets$land, {
        colnames_split$land <- get_col_names_for_plots(datasets$land, landcover = TRUE)
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

      observeEvent(colnames_split$land, {
        # filter named environmental variables list
        env_var_subset$land <- env_var_land[env_var_land %in% names(colnames_split$land)]
        # update CheckboxGroupInput for land cover
        updateCheckboxGroupInput(
          session,
          "land_variables",
          choices = env_var_subset$land
        )
      })

      # update CheckboxGroupInput for land cover to select or deselect all options
      observeEvent(input$select_all, {
        req(env_var_subset$land)
        updateCheckboxGroupInput(
          session,
          "land_variables",
          selected = if (input$select_all) env_var_subset$land else character(0)
        )
      })

      ## Plot histograms for topography, climate and soil
      # histogram for local topography
      output$topo_plot_local <- renderPlot({
        req(datasets$topo, colnames_split$topo, input$topo_variable)

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
            '"\n for local sub-catchments'
          )
        )
      })

      # histogram for upstream topography
      output$topo_plot_upstr <- renderPlot({
        req(datasets$topo_upstr, colnames_split$topo, input$topo_variable)

        selected_column <- colnames_split$topo[[input$topo_variable]]

        # require selected column to be present in upstream data set
        # do not plot topography columns that are only valid for
        # local sub-catchment (defined in topo_local)
        req(selected_column %in% colnames(datasets$topo_upstr[[1]]))

        x <- datasets$topo_upstr[[1]] %>%
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
            '"\n for upstream catchments'
          )
        )
      })

      # histogram for local climate
      output$clim_plot_local <- renderPlot({
        req(datasets$clim, colnames_split$clim, input$clim_variable)

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
            '"\n for local sub-catchments'
          )
        )
      })

      # histogram for upstream climate
      output$clim_plot_upstr <- renderPlot({
        req(datasets$clim_upstr, colnames_split$clim, input$clim_variable)

        selected_column <- colnames_split$clim[[input$clim_variable]]

        x <- datasets$clim_upstr[[1]] %>%
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
            '"\n for upstream catchments'
          )
        )
      })

      # histogram for local soil
      output$soil_plot_local <- renderPlot({
        req(datasets$soil, colnames_split$soil, input$soil_variable)

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
            '"\n for local sub-catchments'
          )
        )
      })

      # histogram for upstream soil
      output$soil_plot_upstr <- renderPlot({
        req(datasets$soil_upstr, colnames_split$soil, input$soil_variable)

        selected_column <- colnames_split$soil[[input$soil_variable]]

        x <- datasets$soil_upstr[[1]] %>%
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
            '"\n for upstream catchments'
          )
        )
      })

      # boxplots for local land cover
      output$land_plot_local <- renderPlot({
        req(datasets$land, colnames_split$land, input$land_variables)

        selected_columns <- colnames_split$land[input$land_variables]

        x <- datasets$land[[1]] %>%
          select(all_of(unlist(selected_columns, use.names = FALSE)))

        boxplot(x,
          main = "Land cover in local sub-catchments"
        )
      })

      # boxplots for upstream land cover
      output$land_plot_upstr <- renderPlot({
        req(datasets$land_upstr, colnames_split$land, input$land_variables)

        selected_columns <- colnames_split$land[input$land_variables]

        x <- datasets$land_upstr[[1]] %>%
          select(all_of(unlist(selected_columns, use.names = FALSE)))

        boxplot(x,
          main = "Land cover in upstream catchments"
        )
      })
    }
  )
}
