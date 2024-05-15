#
# This is a Shiny web application for the NFDI4Earth pilot "GeoFRESH".
#

library(shiny)

# Define UI for GeoFresh application start page
# using Navbar layout

navbarpage <- navbarPage(
  title = "GeoFRESH",
  id = "navbar",

  # link to GeoFRESH CSS file
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")),

  # Panel 1: front page (module: front_page)
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
              panel(
                p("The platform is currently under maintenance, please visit again in 24h.",
                  style = "color:#A94442"
                ),
                heading = "Maintenance",
                status = "danger"
              ),
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
              )
            ),
            column(
              4,
              img(src = "./img/geofresh_logo.png", width = 330)
            )
          )
        )
      )
    )
  ),

  # Panel 2: Upload page
  # uploadPageUI("panel2"),

  # Panel 3: Analysis page
  # analysisPageUI("panel3"),

  # Panel 4: Demo page
  # demoPageUI("panel4"),

  # Panel 5: start page
  tabPanel(
    title = "Documentation",
    value = "panel5",
    div(
      style = "margin: auto; padding:0px 11px; max-width: 1500px;",
      mainPanel(
        div(
          includeMarkdown("documentation.md")
        ),
        width = 100
      )
    )
  ),

  # add common footer to all sub-pages
  footer = column(
    12,
    div(
      style = "margin: auto; padding: 6px 22px; max-width: 1500px;",
      br(),
      hr(),
      a(img(src = "./img/nfdi4earth_logo.png", width = 200, align = "left"), href = "https://www.nfdi4earth.de/", target = "_blank"),
      a(img(src = "./img/igb_logo.png", width = 200, align = "right"), href = "https://www.igb-berlin.de/", target = "_blank"),
      p("GeoFRESH was funded by NFDI4Earth and the Leibniz Institute
      of Freshwater Ecology and Inland Fisheries (IGB).",
        align = "center",
        style = "font-size:0.9em;"
      ),
      p(modalDialogUI("privacy"),
        align = "center"
      )
    )
  )
)

# user interface. useShinyjs() needed for button enable/disable in other modules
# must be called here
ui <- tagList(shinyjs::useShinyjs(), navbarpage)


# add the privacy_policy.md" to the footer


# Define server logic for GeoFRESH application
server <- function(input, output, session) {
  # get selected navbar page from front_page module server function as
  # reactive value
  selected_panel <- frontPageServer("panel1")

  # activate navbar page when front page action link is selected
  observe({
    updateNavbarPage(
      session,
      inputId = "navbar",
      selected = paste0("panel", selected_panel())
    )
  })

  # server function of the analysis page module
  # analysisPageServer("panel3")

  # server function of the modal dialogue module
  modalDialogServer("privacy")
}

# Run the application
shinyApp(ui = ui, server = server)
