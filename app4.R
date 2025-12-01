library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)


# Land mask
#land_mask <- readRDS("./www/data/land_mask_50m.rds")
#options(myapp.land_mask = land_mask)

# Content for the sidebar
side_bar_content <- accordion(
  accordion_panel(
    title = "Point data",
    icon = bsicons::bs_icon("pin-map-fill"),
    div(
      class = "alert alert-info",
      HTML("<b>Upload, snap & edit</b> — 1. Upload point data. 2. Snap points to stream network. 3. Edit directly on the map with the <em>Point Editor</em>.")
    ),
    # UI upload data module
    uploadDataUI("upload_data"),
    # UI snap points module
    useShinyjs(),
    snapPointsUI("snap_point"),
    # UI Point editor module
    pointEditorUI("point_edit")
  ),
  accordion_panel(
    title = "Lakes",
    icon = bsicons::bs_icon("water"),
    div(
      class = "alert alert-info",
      HTML("<b>Lake analysis</b> — For points inside a HydroLAKES polygon, we return the lake outlet (from the intersection with the Hydrography90m stream having the highest discharge), plus lake name and area.")
    ),
    # UI lake analysis module
    lakeAnalysisUI("lake_analysis")
  ),
  accordion_panel(
    title = "Catchment delineation and routing",
    icon = bsicons::bs_icon("bezier2"),
    div(
      class = "alert alert-info",
      HTML("<b>Upstream catchment & routing information</b> — Some text here describing functionalities.")
    ),
    # UI catchment delineation and routing module
    catchmentRoutingUI("catchdelrout")
  ),
  accordion_panel(
    title = "Barriers",
    icon = bsicons::bs_icon("bricks"),
    div(
      class = "alert alert-info",
      HTML("<b>Text here</b> — Text here.")
    ),
    # UI interactive spatial barrier filtering
  ),
  accordion_panel(
    title = "Environmental variables",
    icon = bsicons::bs_icon("moisture"),
    div(
      class = "alert alert-info",
      HTML("<b>Select environmental variables</b> — Click a class of environmental variables to view local values and upstream-catchment summaries for each point.")
    ),
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
    title = "Download",
    icon = bsicons::bs_icon("download"),
    div(
      class = "alert alert-info",
      HTML("<b>Download</b> — Choose a dataset to download (only if available/created): snapped points, lakes, and environmental variables summarized at local sub-catchments and upstream catchments.")
    ),
    # UI linkt to catchment delineation module
    downloadDataUI("download")
  ),

  id = "acc",
  open = "Point data"
)


# CSS for button
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

/* --- Header buttons on dark navbar ------------------------------------ */
/* Pill-style action button (e.g., Glossary) */
.header-chip {
  background: #ffffff;            /* white pill for contrast */
  color: #0d6efd;                 /* bootstrap primary */
  border: 1px solid rgba(0,0,0,.12);
  padding: 4px 10px;
  border-radius: 9999px;
  font-weight: 500;
  box-shadow: 0 1px 2px rgba(0,0,0,.12);
}
.header-chip:hover,
.header-chip:focus {
  background: #ffffff;
  color: #0a58ca;                 /* darker primary on hover */
  text-decoration: none;
}

/* Circular icon button (e.g., GitHub) */
.header-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 34px; height: 34px;
  background: #ffffff;            /* high contrast on dark blue */
  color: #0d6efd;
  border: 1px solid rgba(0,0,0,.12);
  border-radius: 50%;
  box-shadow: 0 1px 2px rgba(0,0,0,.12);
}
.header-icon:hover,
.header-icon:focus {
  background: #ffffff;
  color: #0a58ca;
  text-decoration: none;
}
/* Ensure bsicons inherit the text color */
.header-icon svg { fill: currentColor !important; }
"


# linked badges helper
badgeLink <- function(text, url) {
  tags$a(class = "badge bg-info", href = url, target = "_blank", rel = "noopener", text)
}


# Define UI for GeoFresh application start page
ui <- page_navbar(
  title = "GeoFRESH",
  id = "main",
  fillable = FALSE,
  header = tagList(
    # Link to GeoFRESH CSS file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    # Add the Glossary button (70px from the right, same top offset)
    tags$div(
      style = "position: absolute; right: 70px; top: 10px;",
      actionButton(
        "open_glossary",
        label = "Glossary",
        icon  = icon("book"),   # or icon("book-open")
        class = "btn btn-sm header-chip"
      )
    ),
    # GitHub icon link (floated top right)
    tags$div(
      style = "position: absolute; right: 20px; top: 10px;",
      a(
        href = "https://github.com/glowabio/geofresh",
        target = "_blank",
        class="header-icon",
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
      sidebar = sidebar(side_bar_content, width = 500),
      navset_tab(
        id = "analysis_tabs",   # << add an id
        nav_panel("MAP",
                  br(),
                  div(
                    class = "alert alert-info",
                    tags$strong("Analysis workflow"),
                    p("GeoFRESH lets you upload points, snap them to the stream network, manually edit points, query local and upstream environmental variables, and download results."),

                    # --- two compact columns ---------------------------------------------------
                    div(
                      style = "display:flex; gap:24px; flex-wrap:wrap; align-items:flex-start;",
                      # Column A: actions (inline badges)
                      div(
                        style = "flex:1 1 320px; min-width:280px;",
                        tags$div(tags$b("Actions:"), style = "margin-bottom:6px;"),
                        div(
                          style = "display:flex; flex-wrap:wrap; gap:6px;",
                          span(class = "badge bg-secondary", "Upload points"),
                          span(class = "badge bg-secondary", "Snap to streams"),
                          span(class = "badge bg-secondary", "Edit points"),
                          span(class = "badge bg-secondary", "Local sub-catchment queries"),
                          span(class = "badge bg-secondary", "Upstream queries"),
                          span(class = "badge bg-secondary", "Download tables")
                        )
                      ),
                      # Column B: variables catalog (compact badges)
                      div(
                        style = "flex:1 1 320px; min-width:280px;",
                        tags$div(tags$b("Variables:"), style = "margin-bottom:6px;"),
                        div(
                          style = "display:flex; flex-wrap:wrap; gap:6px;",
                          badgeLink("Topography & Hydrography · 48", "https://hydrography.org/hydrography90m/hydrography90m_layers"),
                          badgeLink("Climate (bioclim) · 19",        "https://chelsa-climate.org/"),
                          badgeLink("Soils · 15",                    "https://soilgrids.org/"),
                          badgeLink("Land cover · 22",               "http://maps.elie.ucl.ac.be/CCI/viewer/index.php")
                        )
                      )
                    ),

                    # --- compact deliverables line --------------------------------------------
                    div(
                      style = "margin-top:10px;",
                      tags$b("Outputs:"),
                      HTML("&nbsp;(i)&nbsp;Per-variable table per point with sub-catchment ID and stats"),
                      HTML("&nbsp;|&nbsp;(ii)&nbsp;Upstream-catchment summary per point"),
                      HTML("&nbsp;|&nbsp;(iii)&nbsp;Lake points table (HydroLAKES ID, name, area, outlet sub-catchment ID, outlet coords).")
                    ),

                    # --- small print -----------------------------------------------------------
                    tags$small(
                      class = "text-muted",
                      "If any variables were scaled in the raster layers, they are rescaled back to original units in the output tables."
                    )
                  ),
                  br(),
                  mapViewerUI("mapviewer", height = 700),
                  icon = bsicons::bs_icon("globe-americas")),
        nav_panel("TABLE",
                  tableUI("main_table"),
                  icon = bsicons::bs_icon("table"))
      )
    )
  ),
  # Demo page
  nav_panel(
    "Tutorial",
    page_fixed(
      div(
        style = "margin: auto; padding:0px 11px; max-width: 1500px;",
        includeMarkdown("www/tutorial.md")
      )
    )
  ),
  # Documentation page
  nav_panel(
    "Documentation",
    page_fixed(
      div(
        style = "margin: auto; padding:0px 11px; max-width: 1500px;",
        includeMarkdown("documentation.md")
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
  footer = div(
    id = "app-footer",
    div(
      style = "margin: auto; padding: 6px 22px; max-width: 1500px;",
      br(),
      hr(),
      a(
        img(src = "./img/nfdi4earth_logo.png", width = 200, align = "left"),
        href   = "https://www.nfdi4earth.de/",
        target = "_blank"
      ),
      a(
        img(src = "./img/igb_logo.png", width = 200, align = "right"),
        href   = "https://www.igb-berlin.de/",
        target = "_blank"
      ),
      p(
        "GeoFRESH was funded by NFDI4Earth and the Leibniz Institute
      of Freshwater Ecology and Inland Fisheries (IGB).",
        align = "center",
        style = "font-size:0.9em;"
      ),
      p(
        modalDialogUI("privacy"),
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

  # Helper to render a glossary item (term + short definition + optional link)
  glossary_item <- function(term, def, link = NULL, link_text = "Learn more") {
    tags$div(
      style = "margin-bottom:10px;",
      tags$b(term), tags$br(),
      span(def),
      if (!is.null(link)) tags$span(HTML("&nbsp;")) else NULL,
      if (!is.null(link)) tags$a(href = link, target = "_blank", rel = "noopener", link_text) else NULL
    )
  }

  # Server: open the modal
  observeEvent(input$open_glossary, {
    showModal(modalDialog(
      title = "Glossary",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      div(
        style = "column-count: 2; column-gap: 32px; max-height: 65vh; overflow:auto; padding-right:8px;",
        glossary_item("Catchment (Drainage basin)", "Any area of land where precipitation collects and drains into a common outlet.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("GeoPackage (.gpkg)", "Vector data format used for exporting points and layers.", "https://www.geopackage.org/"),
        glossary_item("Hydrography90m", "Global 90 m stream network used by GeoFRESH.", "https://hydrography.org/hydrography90m/hydrography90m_layers"),
        glossary_item("HydroLAKES ID", "Stable identifier for lakes in HydroLAKES.", "https://www.hydrosheds.org/products/hydrolakes"),
        glossary_item("Lake outlet", "Intersection of HydroLAKES polygon and the Hydrography90m stream with the highest discharge."),
        glossary_item("Local (sub-catchment) stats", "Statistics computed within the sub-catchment that contains the point."),
        glossary_item("Snapped point", "An input point moved to a stream segment of Hydrography90m to align analyses."),
        glossary_item("Stream channel", "Part of the hydrographic network, as extracted from the DEM. A stream channel consists of many stream segments.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("Stream segment", "The stream channel between two segment nodes (or from initialisation to the first confluence) of the network where the stream order is unchanged.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("Sub-catchment", "Land area between two segment nodes that contributes to the local flow accumulation of a given stream segment.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("Upstream catchment", "Area draining to a point along the stream network; used for upstream summaries."),
        glossary_item("Upstream stats", "Statistics computed over the full upstream area draining to the point."),
        glossary_item("WGS84 (EPSG:4326)", "Coordinate system expected for input coordinates (latitude/longitude).")

      )
    ))
  })


  # 1. Central reactiveVal to store point data
  points <- reactiveVal()

  # server function of the modal dialogue module. It shows privacy police
  modalDialogServer("privacy")

  # 2. INPUT MODULES

  # server function of the upload data module

  input_points <- uploadDataServer("upload_data") # returns reactive
  observeEvent(input_points$uploaded_data(), {
    req(input_points$uploaded_data())
    points(input_points$uploaded_data())
  }, ignoreInit = TRUE)

  # 3. DISPLAY MODULES (read-only)
  # server function map viewer module. This is the map in MAP tab
  mapViewerServer("mapviewer", points)

  # server function table module. This is the table in TABLE tab
  tableServer("main_table", points)

  # 4. EDITING MODULES (can update points)
  # server function of the snap point module
  updated_points_snap <- snapPointsServer("snap_point",
                                          input_point_table = points,
                                          input_point_table_name = input_points$db_table_name)


  # server function of the point editor module
  updated_points_editor <- pointEditorServer("point_edit", point_user = points)


  # 5. Merge updates from both editing modules

  observeEvent(updated_points_snap$snapped_data(), {
    points(updated_points_snap$snapped_data())
  })

  observeEvent(updated_points_editor(), {
    points(updated_points_editor())
  })


  # server function of the lake analysis module
  lakeAnalysisServer("lake_analysis")

  # 6. ENVITONMENTAL VARIABLES.

  ## Analysis of environmental variables only possible after snapping

  ## #  Load list with variable's name
  load("./www/data/env_var_list.rda")

  # server function of the pick var module customized for
  # topography
  topo_r <- varsServer("topography", title = "Hydrography90m stream topology",
             choices = Variable_groups$Topography$choices,
             desc    = Variable_groups$Topography$desc,
             var_class = "topography",
             var_groups = Variable_groups,
             user_table_name = input_points$db_table_name,
             snap_status = updated_points_snap$snapped_data)

  # server function of the pick var module customized for climate variables
  clim_r<- varsServer("climate", title = "Bioclimatic variables (1981–2010)",
             choices = Variable_groups$Climate$choices,
             desc    = Variable_groups$Climate$desc,
             var_class = "climate",
             var_groups = Variable_groups,
             user_table_name = input_points$db_table_name,
             snap_status = updated_points_snap$snapped_data)

  # server function of the pick var module customized for soil variables
  soil_r <- varsServer("soil", title = "Soil data for 2016",
             choices = Variable_groups$Soil$choices,
             desc    = Variable_groups$Soil$desc,
             var_class = "soil",
             var_groups = Variable_groups,
             user_table_name = input_points$db_table_name,
             snap_status = updated_points_snap$snapped_data)

  # server function of the pick var module customized for land cover variables
  land_r<- varsServer("landcover", title = "Annual land cover for 2020",
             choices = Variable_groups$Landcover$choices,
             desc    = Variable_groups$Landcover$desc,
             var_class = "landcover",
             var_groups = Variable_groups,
             user_table_name = input_points$db_table_name,
             snap_status = updated_points_snap$snapped_data)


  # server function point-and-click catchment delineation and routing tool
  catchmentRoutingServer("catchdelrout")

  # 7. Download
  # Server function of the download module
  downloadDataServer("download",
                     r_points = points,
                     r_topo_loc = topo_r$local,
                     r_topo_up = topo_r$upstream,
                     r_clim_loc  = clim_r$local,
                     r_clim_up   = clim_r$upstream,
                     r_soil_loc  = soil_r$local,
                     r_soil_up   = soil_r$upstream,
                     r_land_loc  = land_r$local,
                     r_land_up   = land_r$upstream)

}

shinyApp(ui = ui, server = server)
