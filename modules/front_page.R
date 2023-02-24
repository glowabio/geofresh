# This module configures the content of the GeoFRESH app front page

# front page module UI function
frontPageUI <- function(id, label = "front_page") {
  ns <- NS(id)

  # configure front page tabPanel
  tabPanel(
    title = "Home",
    value = "panel1",
    fluidPage(
      # Grid layout for start page with navigation boxes
      # TODO: add css for border around boxes in external file
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the application
        column(
          12,
          h3("Welcome to the GeoFRESH platform", align = "center"),
          p("GeoFRESH is a platform that helps freshwater researchers to process
           point data across the global river network by providing a set of
            spatial tools."),
          p("Follow the demo or simply upload your csv-table that contains
          geographic coordinates. GeoFRESH allows you to
          >>>> CREATE BULLET POINTS <<<<
          - map your points,
          - move points to the nearest stream network segment,
          - delineate upstream catchments of each point,
          - extract a suite of environmental attributes across the catchment,
          and
          - download the data for further analyses"),
          p(
            "GeoFRESH uses the Hydrography90m stream network. For more
            information, please see the ",
            a("publication",
              href = "https://essd.copernicus.org/articles/14/4525/2022/",
              target = "_blank"
            ), "and ",
            a("hydrography.org",
              href = "https://hydrography.org/hydrography90m/hydrography90m_layers/",
              target = "_blank"
            ), "regarding the single data layers."
          ),
          img(src = "../img/nfdi4earth_logo.png", width = 300),
          img(src = "../img/igb_logo.png", width = 300)
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          6,
          h3("Upload", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink(ns("tablink2"), "Go to upload!")
        ),
        column(
          6,
          h3("Analysis", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink(ns("tablink3"), "Go to analysis workflow!")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          6,
          h3("Demo", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink(ns("tablink4"), "Go to Demo!")
        ),
        column(
          6,
          h3("Documentation", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink(ns("tablink5"), "Go to Documentation!")
        )
      )
    )
  )
}

# front page module server function
frontPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # create reactive value for selected action link on front page
      selected_panel <- reactiveVal(value = 1, label = "selected_panel")

      # tab 2 "Upload" selected
      observeEvent(input$tablink2, {
        selected_panel(2)
      })

      # tab 3 "Analysis" selected
      observeEvent(input$tablink3, {
        selected_panel(3)
      })

      # tab 4 "Demo" selected
      observeEvent(input$tablink4, {
        selected_panel(4)
      })

      # tab 5 "Documentation" selected
      observeEvent(input$tablink5, {
        selected_panel(5)
      })

      return(selected_panel)
    }
  )
}
