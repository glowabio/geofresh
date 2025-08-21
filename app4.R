library(shiny)
library(bslib)

# Content for the sidebar
side_bar_content <- accordion(
  accordion_panel(
    title = "Point data",
    icon = bsicons::bs_icon("pin-map-fill"),
    # UI upload data module
    uploadDataUI("upload_data"),
    # UI snap points module
    snapPointsUI("snap_point")
  ),
  accordion_panel(
    title = "Lakes",
    icon = bsicons::bs_icon("water"),
    # UI lake analysis module
    lakeAnalysisUI("lake_analysis")
  ),
  accordion_panel(
    title = "Environmental variables",
    icon = bsicons::bs_icon("moisture"),
    # UI topography module
    varsUI("topography", trigger_label = "Topography"),
    # UI climate module
    varsUI("climate", trigger_label = "Climate"),
    # UI soil
    varsUI("soil", trigger_label = "Soil"),
    # UI landcover module
    varsUI("landcover", trigger_label = "Landcover")
  ),
  accordion_panel(
    title = "Routing info",
    icon = bsicons::bs_icon("bezier2"),
    # UI routing module
    routingUI("routing")
  ),
  accordion_panel(
    title = "Catchment delineation tool",
    icon = bsicons::bs_icon("cursor"),
    # UI linkt to catchment delineation module
    linkCatchtoolUI("link_catch_tool")
  ),

  id = "acc",
  open = "Point data"
)

# CSS for the close button
app_css <- "
.custom-close-btn {
  position: absolute;
  top: 10px;
  right: 10px;
  background-color: transparent;
  border: none;
  font-size: 20px;
  color: #333;
  cursor: pointer;
}
.custom-close-btn:hover {
  background-color: #e81123;  /* Windows red */
  color: white;
  border-radius: 3px;
}
"

# Define UI for GeoFresh application start page
ui <- page_navbar(
  title = "GeoFRESH",
  id = "main",
  header = tagList(
    # Link to GeoFRESH CSS file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    # GitHub icon link (floated top right)
    tags$div(
      style = "position: absolute; right: 20px; top: 10px;",
      a(
        href = "https://github.com/glowabio/geofresh",
        target = "_blank",
        bsicons::bs_icon("github", size = "1.5em")
      )
    ),
    # Make modal dialogues in the app draggable
    # Load jQuery UI
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),
    tags$script(HTML("
      $(document).on('shown.bs.modal', function() {
        if ($('.modal-dialog').length > 0 && typeof $('.modal-dialog').draggable === 'function') {
          $('.modal-dialog').draggable({
            handle: '.modal-header'
          });
        } else {
          console.warn('Modal dialog found but .draggable() is not defined.');
        }
      });
    ")),
    # CSS to HTML
    tags$style(HTML(app_css))
  ),

  # Analysis page(main)
  nav_panel(
    "Analysis",
    page_sidebar(
      sidebar = sidebar(side_bar_content),
      navset_tab(
        # Map tab
        nav_panel("MAP",
                  # UI point editor
                  pointEditorUI("point_edit"),
                  # UI map viewer module
                  mapViewerUI("mapviewer"),
                  icon = bsicons::bs_icon("globe-americas")),
        # Table tab
        nav_panel("TABLE", tableUI("main_table"),
                  icon = bsicons::bs_icon("table")),
        # Plot tab
        nav_panel("Plot", DTOutput("filtered_points"),
                  icon = bsicons::bs_icon("bar-chart-fill"))
      )
    )
  ),
  # Demo page
  nav_panel("Tutorial",
            div(
              style = "margin: auto; padding:0px 11px; max-width: 1500px;",
              mainPanel(
                div(
                  includeMarkdown("www/tutorial.md")
                ),
                width = 100
              )
            )
  ),
  # Documentation page
  nav_panel("Documentation",
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
  # R packge page
  nav_panel("R package hydrographr",
            div(
              style = "margin: auto; padding:0px 11px; max-width: 1500px;",
              mainPanel(
                div(
                  includeMarkdown("hydrographr.md")
                ),
                width = 100
              )
            )
  ),
  # Add common footer to all sub-pages
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

# # Define server logic for GeoFRESH application
server <- function(input, output, session) {

  # Show modal dialog first time app is opened. This the welcome page
  observeEvent(input$main, {
      showModal(modalDialog(
        title = NULL,
        easyClose = TRUE,
        footer = NULL,  # Disable default footer
        size = "l",
        HTML('
    <div class="geofresh-modal">
      <div class="modal-header">
        Welcome to GeoFRESH!
      </div>
      <div class="modal-body">
        <div style="display: flex; gap: 30px;">
          <div style="flex: 2;">
            <p>GeoFRESH is a platform that helps freshwater researchers to process
            point data across the global river network by providing a set of
            spatial tools.</p>

            <p>Follow the <b>tutorial</b> or upload your csv table with
            the geographical coordinates and carry out the <b>analysis</b> steps.</p>

            <p>GeoFRESH allows you to:</p>
            <ul>
              <li>map your points,</li>
              <li>move points to the nearest stream network segment,</li>
              <li>delineate upstream catchments of each point,</li>
              <li>extract a suite of environmental attributes across the catchment,</li>
              <li>identify intersection points between stream network and lakes,</li>
              <li>and download the data for further analyses.</li>
            </ul>

            <p>
              GeoFRESH is based on the <b>Hydrography90m stream network</b>. For more
              information, please see the
              <a href="https://essd.copernicus.org/articles/14/4525/2022/" target="_blank">publication</a>
              and <a href="https://hydrography.org/hydrography90m/hydrography90m_layers/" target="_blank">hydrography.org</a>.
            </p>

            <p>
              For further analyses of your freshwater data, you can use the
              <b><i>hydrographr</i> R package</b> (
              <a href="https://doi.org/10.1111/2041-210X.14226" target="_blank">publication</a>,
              <a href="https://glowabio.github.io/hydrographr/" target="_blank">website</a>,
              <a href="https://github.com/glowabio/hydrographr/" target="_blank">source code</a>).
            </p>

            <p>
              For a detailed description of the platform and the workflow, see the
              <b><a href="https://doi.org/10.1080/17538947.2024.2391033" target="_blank">GeoFRESH publication</a></b>:
            </p>

            <ul>
              Domisch, S., et al. (2024). GeoFRESH – an online platform for freshwater geospatial data processing.
              <i>International Journal of Digital Earth, 17(1)</i>.
              <a href="https://doi.org/10.1080/17538947.2024.2391033" target="_blank">
              https://doi.org/10.1080/17538947.2024.2391033</a>.
            </ul>
          </div>
          <div style="flex: 1;">
            <img src="img/geofresh_logo.png" />
          </div>
        </div>

        <!-- Custom footer block -->
        <div style="margin: auto; padding: 6px 22px; max-width: 1500px;">
          <br />
          <hr />
          <div style="display: flex; justify-content: space-between; align-items: center;">
            <a href="https://www.nfdi4earth.de/" target="_blank">
              <img src="img/nfdi4earth_logo.png" width="200" />
            </a>
            <a href="https://www.igb-berlin.de/" target="_blank">
              <img src="img/igb_logo.png" width="200" />
            </a>
          </div>
          <p style="text-align: center; font-size: 0.9em;">
            GeoFRESH was funded by NFDI4Earth and the Leibniz Institute
            of Freshwater Ecology and Inland Fisheries (IGB).
          </p>
          <div style="text-align: center; font-size: 0.9em;">
  ', as.character(modalDialogUI("privacy")), '
</div>
        </div>
      </div>
    </div>
  ')
      ))

  }, once = TRUE)

  # 1. Central reactiveVal to store point data
  points <- reactiveVal()

  # server function of the modal dialogue module. It shows privacy police
  modalDialogServer("privacy")

  # 2. INPUT MODULES
  # server function of the upload data module
  input_points <- uploadDataServer("upload_data") # returns reactive
  observe({
    req(input_points())
    points(input_points())
  })

  # 3. DISPLAY MODULES (read-only)
  # server function map viewer module. This is the map in MAP tab
  mapViewerServer("mapviewer", points)

  # server function table module. This is the table in TABLE tab
  tableServer("main_table", points)

  # 4. EDITING MODULES (can update points)
  # server function of the snap point module
  # updated_points_snap <- snapPointsServer("snap_point", point_user = points)

  # server function of the point editor module
  updated_points_editor <- pointEditorServer("point_edit", point_user = points)

  # 5. Merge updates from both editing modules
  observeEvent(updated_points_editor(), {
    points(updated_points_editor())
  })

  # observeEvent(updated_points_snap(), {
  #   points(updated_points_snap())
  # })

  # server function of the lake analysis module
  lakeAnalysisServer("lake_analysis")

  # ENVITONMENTAL VARIABLES. Load list with variable's name
  source("./R/env_var_list.R")

  # server function of the pick var module customized for
  # topography
  varsServer("topography", title = "Hydrography90m stream topology",
             choices = Variable_groups$Topography$choices,
             desc    = Variable_groups$Topography$desc)

  # server function of the pick var module customized for climate variables
  varsServer("climate", title = "Bioclimatic variables (1981–2010)",
             choices = Variable_groups$Climate$choices,
             desc    = Variable_groups$Climate$desc)

  # server function of the pick var module customized for soil variables
  varsServer("soil", title = "Soil data for 2016",
             choices = Variable_groups$Soil$choices,
             desc    = Variable_groups$Soil$desc)

  # server function of the pick var module customized for land cover variables
  varsServer("landcover", title = "Annual land cover for 2020",
             choices = Variable_groups$Landcover$choices,
             desc    = Variable_groups$Landcover$desc)

  # server function routing module
  routingServer("routing")


  # server function link to point-and-click catchment delineation tool
  linkCatchtoolServer("link_catch_tool")

}

shinyApp(ui = ui, server = server)
