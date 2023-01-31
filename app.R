#
# This is a Shiny web application for the NFDI4Earth pilot "GeoFRESH".
#
# First outline version 31.01.2023
#

library(shiny)

# Define UI for GeoFresh application start page
# using Navbar layout

ui <- navbarPage(
  title = "GeoFRESH",
  id = "navbar",

  # Panel 1: start page
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
          h3("Information on the GeoFRESH app", align = "center"),
          p("TODO: add some general text here..."),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua. At vero eos et accusam
             et justo duo dolores et ea rebum. Stet clita kasd gubergren,
             no sea takimata sanctus est Lorem ipsum dolor sit amet.
             Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua. At vero eos et accusam
             et justo duo dolores et ea rebum. Stet clita kasd gubergren,
             no sea takimata sanctus est Lorem ipsum dolor sit amet."),
          p(
            "TODO: add link to publication",
            a("https://essd.copernicus.org/articles/14/4525/2022/")
          ),
          img(src = "/img/nfdi4earth_logo.png", width = 300),
          img(src = "/img/igb_logo.png", width = 300)
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
          actionLink("tablink2", "Go to upload!")
        ),
        column(
          6,
          h3("Environmental Variables", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink("tablink3", "Go to environmental variables!")
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
          actionLink("tablink4", "Go to Demo!")
        ),
        column(
          6,
          h3("Documentation", align = "center"),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua."),
          actionLink("tablink5",  "Go to Documentation!")
        )
      )
    )
  ),

  # Panel 2: Upload page
  tabPanel(
    title = "Upload",
    value = "panel2",
    fluidPage(
      # Page title
      titlePanel("Upload", windowTitle = "GeoFRESH")
    )
  ),

  # Panel 3: Environmental variables page
  tabPanel(
    title = "Environmental variables",
    value = "panel3",
    fluidPage(
      # Page title
      titlePanel("Environmental variables", windowTitle = "GeoFRESH")
    )
  ),
  
  # Panel 4: Demo page
  tabPanel(
    title = "Demo",
    value = "panel4",
    fluidPage(
      # Page title
      titlePanel("Demo", windowTitle = "GeoFRESH")
    )
  ),
  
  # Panel 5: start page
  tabPanel(
    title = "Documentation",
    value = "panel5",
    fluidPage(
      # Page title
      titlePanel("Documentation", windowTitle = "GeoFRESH")
    )
  ),
  
  # add common footer to all sub-pages
  footer = p("Footer: GeoFRESH was funded by... (TODO)")
)

# Define server logic for GeoFRESH application
server <- function(input, output, session) {
# TODO...
# activate tab 2 "Upload"
  observeEvent(input$tablink2, {
    updateTabsetPanel(session, "navbar",
                      selected = "panel2"
    )
  })
  
  # activate tab 3 "Environmental variables"
  observeEvent(input$tablink3, {
    updateTabsetPanel(session, "navbar",
                      selected = "panel3"
    )
  })
  
  # activate tab 4 "Demo"
  observeEvent(input$tablink4, {
    updateTabsetPanel(session, "navbar",
                      selected = "panel4"
    )
  })
  
  # activate tab 5 "Documentation"
  observeEvent(input$tablink5, {
    updateTabsetPanel(session, "navbar",
                      selected = "panel5"
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
