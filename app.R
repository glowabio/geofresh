library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
# For asynchronous:
library(promises)
library(future)
# Enable async execution
#plan(multicore)
future::plan(multisession, workers=4)


# Land mask
#land_mask <- readRDS("./www/data/land_mask_50m.rds")
#options(myapp.land_mask = land_mask)
source("R/db_points_helpers.R")
# Helpers to request processing and retrieve outcome from OGC processing API (pygeoapi)
source("R/pygeoapi_helpers.R")


# Content for the sidebar
side_bar_content <- accordion(
  accordion_panel(
    title = "Point data",
    icon = bsicons::bs_icon("pin-map-fill"),
    div(
      class = "alert alert-info",
      HTML("<b>Upload, snap & edit</b> — 1. First, upload point data. 2. Then, snap points to stream network. 3. Finally, edit directly on the map with the <em>Point Editor</em>.")
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
    title = "Routing",
    icon = bsicons::bs_icon("bezier2"),
    div(
      class = "alert alert-info",
      HTML("<b>Downstream paths</b> — Compute and display the paths from each input point to the sea, along the river network.")
    ),
    # UI catchment delineation and routing module
    routingUI("routing_outlets")
  ),
  accordion_panel(
    title = "Catchment delineation",
    icon = bsicons::bs_icon("bezier2"),
    div(
      class = "alert alert-info",
      HTML("<b>Upstream catchments</b> — Compute and display the upstream catchment of each input point.")
    ),
    # UI catchment delineation and routing module
    catchmentUI("upstr_catchments")
  ),
  accordion_panel(
    title = "Barriers",
    icon = bsicons::bs_icon("bricks"),
    div(
      class = "alert alert-info",
      # Note: Currently, we only display barriers. No snapping, no interaction, no filtering.
      HTML("<b>Barrier</b> — Display barriers that hinder the free flow of the rivers.")
      #tagList(
      #  tags$b("Barriers"),
      #  tags$br(),
      #  div(
      #    style = "display:flex; gap:.5rem; align-items:flex-start;",
      #    bsicons::bs_icon("cone-striped", size = "2em"),
      #    HTML("This functionality is currently under development and is temporarily disabled.<br>
      #     Please use the available tools in the sidebar while we finish implementation.")
      #  )
      #)
    ),
    barrierUI("barrier_display")
    # TODO: Finish implementing the barrier editing.
    # UI interactive spatial barrier filtering
    # pointEditorUI("barrier_edit")
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
    # UI download module
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

# Linked badges helper
badgeLink <- function(text, url) {
  tags$a(class = "badge bg-info", href = url, target = "_blank", rel = "noopener", text)
}

# Define UI for GeoFresh application start page
ui <- page_navbar(
  title = "GeoFRESH",
  id = "main",
  fillable = FALSE,
  navbar_options = navbar_options(
    bg = "#003d72",
    fg = "#ffffff",   # text/icons color
    underline = TRUE
  ),
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
    tags$style(HTML(app_css)),

    # Clean up any modal closes
    tags$script(HTML("
  // ---- Bootstrap modal cleanup (prevents stuck 'modal-open' / backdrops) ----
  $(document).on('hidden.bs.modal', '.modal', function () {
    // Wait a tick for Bootstrap to finish transitions
    setTimeout(function () {
      // If no modals are currently shown, unlock body scroll
      if ($('.modal.show').length === 0) {
        $('body').removeClass('modal-open');
        $('body').css({
          'overflow': '',
          'padding-right': ''
        });
        $('.modal-backdrop').remove();
      }
    }, 50);
  });
"))
  ),

  # Analysis page (main)
  nav_panel(
    "Analysis",
    page_sidebar(
      sidebar = sidebar(side_bar_content, width = 500),
      navset_tab(
        id = "analysis_tabs",   # << add an id
        nav_panel("MAP",
                  br(),
                  accordion(accordion_panel(
                    title = "Analysis workflow",
                    icon = bsicons::bs_icon("info-square"),
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
                    )
                  ),
                  id = "desc_workflow",
                  open = "Analysis workflow"),
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

  # Show modal dialog first time app is opened. This is the welcome page
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
        glossary_item("Strahler order (stream order)", "A numeric ranking of stream size based on how tributaries join. Headwater channels with no tributaries are order 1. When two streams of the same order meet, the downstream segment increases by one order (e.g., 1 + 1 → 2; 2 + 2 → 3). When streams of different order meet, the downstream segment keeps the higher order (e.g., 1 + 2 → 2)."),
        glossary_item("Stream segment", "The stream channel between two segment nodes (or from initialisation to the first confluence) of the network where the stream order is unchanged.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("Sub-catchment", "Land area between two segment nodes that contributes to the local flow accumulation of a given stream segment.", "https://essd.copernicus.org/articles/14/4525/2022/"),
        glossary_item("Upstream catchment", "Area draining to a point along the stream network; used for upstream summaries."),
        glossary_item("Upstream stats", "Statistics computed over the full upstream area draining to the point."),
        glossary_item("WGS84 (EPSG:4326)", "Coordinate system expected for input coordinates (latitude/longitude).")
      )
    ))
  })

  # Server function of the modal dialogue module. It shows privacy policy
  modalDialogServer("privacy")

  # Create dataset manager
  ds <- dataset_manager(pool, session)
  ds$ensure() # create table in database to store user points

  # DB-backed reactive reader
  # What does this do?
  # Whenever the version changes, it reads the new data...
  # The version is changed by: ds$bump_version() in mod_upload_data.R
  points_db <- reactive({
    ds$version()   # reactive dependency trigger
    ds$ensure()    # safe, creates table if missing
    with_pool_connection(pool, function(conn) {
      read_points_db(conn, ds$table_id())
    })
  })

  ## 1. INPUT MODULE
  # Server function of the upload data module
  uploadDataServer("upload_data", ds = ds)

  ## 2. DISPLAY MODULES (read-only)
  # Server function of the map viewer module. This is the map in MAP tab.
  mapViewerServer("mapviewer", points_db, last_path_to_outlet, last_upstream_catchment, barrier_points)

  # Server function for the table module. This is the table in TABLE tab.
  tableServer("main_table", points_db)

  ## 3. EDITING MODULES (can update points)
  # Server function of the snap points module
  updated_points_snap <- snapPointsServer(
    "snap_point",
    input_point_table_name = ds$table_name,
    db_version = ds$version,
    on_db_changed = ds$bump_version
  )

  # Server function of the point editor module
  pointEditorServer(
    "point_edit",
    points_table_name = ds$table_name,
    on_db_changed = ds$bump_version
  )

  ## 4. LAKE ANALYSIS MODULE
  # Server function of the lake analysis module
  lake_r <- lakeAnalysisServer(
    "lake_analysis",
    pool = pool,
    points_table_name = ds$table_name,
    db_version = ds$version
  )

  ## 5. ENVIRONMENTAL VARIABLES

  # Analysis of environmental variables only possible after snapping

  # Load list with variable names
  load("./www/data/env_var_list.rda")

  # Server function of the pick var module customized for topography variables
  topo_r <- varsServer("topography", title = "Hydrography90m stream topology",
             choices = Variable_groups$Topography$choices,
             desc    = Variable_groups$Topography$desc,
             var_class = "topography",
             var_groups = Variable_groups,
             user_table_name = ds$table_name,
             snap_status = updated_points_snap$snapped_data)

  # Server function of the pick var module customized for climate variables
  clim_r<- varsServer("climate", title = "Bioclimatic variables (1981–2010)",
             choices = Variable_groups$Climate$choices,
             desc    = Variable_groups$Climate$desc,
             var_class = "climate",
             var_groups = Variable_groups,
             user_table_name = ds$table_name,
             snap_status = updated_points_snap$snapped_data)

  # Server function of the pick var module customized for soil variables
  soil_r <- varsServer("soil", title = "Soil data for 2016",
             choices = Variable_groups$Soil$choices,
             desc    = Variable_groups$Soil$desc,
             var_class = "soil",
             var_groups = Variable_groups,
             user_table_name = ds$table_name,
             snap_status = updated_points_snap$snapped_data)

  # Server function of the pick var module customized for land cover variables
  land_r<- varsServer("landcover", title = "Annual land cover for 2020",
             choices = Variable_groups$Landcover$choices,
             desc    = Variable_groups$Landcover$desc,
             var_class = "landcover",
             var_groups = Variable_groups,
             user_table_name = ds$table_name,
             snap_status = updated_points_snap$snapped_data)


  ## 6. ROUTING MODULE
  # Server function for point-and-click catchment delineation and routing tools.

  # We will retrieve the paths to outlet and store them in reactive "paths_to_outlet"
  # which should be observed by the map...
  # Either we store a list of paths, or one by one, as they come back from pygeoapi:
  #paths_to_outlet <- reactive(list())
  barrier_points <- reactiveVal()
  last_path_to_outlet <- reactiveVal()
  last_upstream_catchment <- reactiveVal()

  barrierServer("barrier_display", points_db, barrier_points)
  routingServer("routing_outlets", points_db, last_path_to_outlet)
  catchmentServer("upstr_catchments", points_db, last_upstream_catchment)


  # X. Server function of the point editor module for barriers
  # (TODO: define which data table should be edited here)
  # pointEditorServer(
  #     "barrier_edit",
  #     points_table_name = ds$table_name,
  #     on_db_changed = ds$bump_version
  #   )

  ## 7. DOWNLOAD MODULE
  # Server function of the download module
  downloadDataServer("download",
                     r_points = points_db,
                     r_lakes    = lake_r$lakes_data,
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
