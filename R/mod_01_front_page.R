# This module configures the content of the GeoFRESH app front page

# front page module UI function
frontPageUI <- function(id, label = "front_page") {
  ns <- NS(id)

  # configure front page tabPanel
  tabPanel(
    title = "Home",
    value = "panel1",
    div(
      style = "margin: auto; max-width: 1500px;",
      fluidPage(
        # Grid layout for start page with navigation boxes
        # TODO: add css for border around boxes in external file
        fluidRow(
          # General information on the application
          column(
            12,
            h3("Welcome to the GeoFRESH platform", align = "center"),
            br(),
            column(
              8,
              p("GeoFRESH is a platform that helps freshwater researchers to process
           point data across the global river network by providing a set of
            spatial tools."),
              p("Follow the", tags$b("demo"), "or upload your csv-table that contains
          geographic coordinates. GeoFRESH allows you to"),
              tags$ul(
                tags$li("map your points,"),
                tags$li("move points to the nearest stream network segment,"),
                tags$li("delineate upstream catchments of each point,"),
                tags$li("extract a suite of environmental attributes across the catchment,"),
                tags$li("identify intersection points between stream network and lakes,"),
                tags$li("and download the data for further analyses.")
              ),
              p(
                "GeoFRESH is based on the", tags$b("Hydrography90m stream network"), ". For more
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
              p(
                "For further analyses of your freshwater data, you can use the",
                tags$b(tags$i("hydrographr")), tags$b("R package"),
                "that facilitates the download and data processing of the Hydrography90m data (",
                a("publication",
                  href = "https://doi.org/10.1111/2041-210X.14226",
                  target = "_blank"
                ), "in",
                tags$i("Methods in Ecology and Evolution"), ",",
                a("website",
                  href = "https://glowabio.github.io/hydrographr/",
                  target = "_blank"
                ), "with details and examples, source code on",
                a("GitHub",
                  href = "https://github.com/glowabio/hydrographr/",
                  target = "_blank"
                ), ")."
              ),
              p(
                "For a detailed description of the platform and the workflow, please refer to the ",
                tags$b(
                  a("GeoFRESH publication",
                    href = "https://doi.org/10.1080/17538947.2024.2391033",
                    target = "_blank"
                  )
                ),
                ":"
              ),
              tags$ul(
                "Domisch, S., Bremerich, V., Buurman, M., Kaminke, B., Tomiczek,
                T., Torres-Cambas, Y., Grigoropoulou, A., Garcia Marquez, J. R.,
                Amatulli, G., Grossart, H. P., Gessner, M. O., Mehner, T.,
                Adrian, R. & De Meester, L. (2024).
                GeoFRESH – an online platform for freshwater geospatial data processing.",
                tags$i("International Journal of Digital Earth, 17,"),
                "(1).",
                a("https://doi.org/10.1080/17538947.2024.2391033",
                  href = "https://doi.org/10.1080/17538947.2024.2391033",
                  target = "_blank"
                ),
                "."
              )
            ),
            column(
              4,
              img(src = "./img/geofresh_logo.png", width = 330)
            )
          )
        ),
        #   #   h3(actionLink(ns("tablink2"), "Upload"), align = "center"),
        #   #   p("Upload your point data and start mapping.",
        #   #     align = "center"),
        fluidRow(
          h3(actionLink(ns("tablink3"), "Analysis"), align = "center"),
          p("Check out the different analysis steps.",
            align = "center"
          ),
          h3(actionLink(ns("tablink4"), "Tutorial"), align = "center"),
          p("Check out the full functionality of the platform.",
            align = "center"
          ),
          h3(actionLink(ns("tablink5"), "Documentation"), align = "center"),
          p("Learn about the project background.",
            align = "center"
          )
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
      # observeEvent(input$tablink2, {
      #   selected_panel(2)
      # })

      # tab 3 "Analysis" selected
      observeEvent(input$tablink3, {
        selected_panel(3)
      })

      # tab 4 "Tutorial" selected
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
