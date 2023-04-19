#
# This is a Shiny web application for the NFDI4Earth pilot "GeoFRESH".
#

library(shiny)
library(DBI)
library(pool)


# set up database connection pool ("dev" or "prod")
# pool <- get_pool("dev")

# Define UI for GeoFresh application start page
# using Navbar layout

ui <- navbarPage(
  title = "GeoFRESH",
  id = "navbar",

  # Panel 1: front page (module: front_page)
  frontPageUI("panel1"),

  # Panel 2: Upload page
  uploadPageUI("panel2"),

  # Panel 3: Analysis page
  analysisPageUI("panel3"),

  # Panel 4: Demo page
  demoPageUI("panel4"),

  # Panel 5: start page
  tabPanel(
    title = "Documentation",
    value = "panel5",
    includeMarkdown("documentation.md")
  ),

  # add common footer to all sub-pages
  footer = column(
    12,
    p("GeoFRESH was funded by NFDI4Earth and the Leibniz Institute
      of Freshwater Ecology and Inland Fisheries (IGB).",
      align = "center",
      style = "font-size:0.9em;"),
    p(modalDialogUI("privacy"),
    align = "center")
    )
)

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

  # server function of the upload page module, "point" is a list with two data
  # frames, one with users' coordinates and other with newly generated
  # coordinates after snapping. Use "points" as input for the analysis page
  point <- uploadPageServer("panel2")

  # server function of the analysis page module
  analysisPageServer("panel3", point)

  # server function of the modal dialogue module
  modalDialogServer("privacy")
}

# Run the application
shinyApp(ui = ui, server = server)
