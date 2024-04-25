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
  frontPageUI("panel1"),

  # Panel 2: Upload page
  # uploadPageUI("panel2"),

  # Panel 3: Analysis page
  analysisPageUI("panel3"),

  # Panel 4: Demo page
  demoPageUI("panel4"),

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
      img(src = "./img/nfdi4earth_logo.png", width = 200, align = "left"),
      img(src = "./img/igb_logo.png", width = 200, align = "right"),
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
  analysisPageServer("panel3")

  # server function of the modal dialogue module
  modalDialogServer("privacy")
}

# Run the application
shinyApp(ui = ui, server = server)
