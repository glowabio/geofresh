# Point editor module
library(leaflet)
library(DT)
library(sf)
library(terra)
library(dplyr)
library(htmlwidgets)
library(leaflet.extras)
library(bsicons)

# =========================
# UI
# =========================
barrierEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionLink(ns("open_modal"), "Open Barrier Editor")
  )
}

barrierEditorServer <- function(id, point_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Working state inside the modal ---
    working_points <- reactiveVal(NULL)   # data.frame: id, latitude, longitude, (optional *_snap)
    sel_geom       <- reactiveVal(NULL)   # sf polygon(s) for current selection
    saved_points   <- reactiveVal(NULL)   # last saved to app (for parent)

    # ---------- helper: (re)draw points ----------
    draw_points <- function(df) {
      if (is.null(df) || !nrow(df)) return(invisible())

      has_snapped <- all(c("latitude_snap", "longitude_snap") %in% names(df)) &&
        any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))

      lbl_input <- lapply(paste0("id: ", df$id), htmltools::HTML)
      if (has_snapped) lbl_snap <- lapply(paste0("id: ", df$id, " (snapped)"), htmltools::HTML)

      icon_input <- icons(
        iconUrl = "./www/img/marker-icon-violet.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41,
        shadowUrl = "./www/img/marker-shadow.png",
        shadowWidth = 41, shadowHeight = 41,
        shadowAnchorX = 12, shadowAnchorY = 41
      )
      icon_snap <- icons(
        iconUrl = "./www/img/marker-icon-yellow.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41,
        shadowUrl = "./www/img/marker-shadow.png",
        shadowWidth = 41, shadowHeight = 41,
        shadowAnchorX = 12, shadowAnchorY = 41
      )

      proxy <- leafletProxy("map", data = df, session = session) %>%
        clearGroup("Input points") %>%
        clearGroup("Snapped points") %>%
        addMarkers(
          lat = ~latitude, lng = ~longitude,
          label = lbl_input,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px", direction = "bottom", opacity = 0.9
          ),
          options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
          layerId = ~as.character(id),  # explicit (needed for drag events)
          icon = icon_input,
          group = "Input points"
        ) %>%
        showGroup("Input points")

      if (has_snapped) {
        proxy <- proxy %>%
          addMarkers(
            lat = ~latitude_snap, lng = ~longitude_snap,
            label = lbl_snap,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px", direction = "bottom", opacity = 0.9
            ),
            options = markerOptions(draggable = TRUE, riseOnHover = TRUE),   # draggable snapped
            layerId = ~paste0(id, "_snap"),                                  # unique id for snapped
            icon = icon_snap,
            group = "Snapped points"
          ) %>%
          showGroup("Snapped points")
      } else {
        proxy <- proxy %>% hideGroup("Snapped points")
      }

      proxy <- proxy %>%
        removeControl("points-legend") %>%
        addLegend(
          position = "topright",
          colors = c("#9C2BCB", "#ffd456"),
          labels = c("Input points", "Snapped points"),
          opacity = 1,
          layerId = "points-legend"
        )

      # Fit bounds
      if (has_snapped) {
        lats <- df$latitude_snap[is.finite(df$latitude_snap)]
        lngs <- df$longitude_snap[is.finite(df$longitude_snap)]
      } else {
        lats <- df$latitude[is.finite(df$latitude)]
        lngs <- df$longitude[is.finite(df$longitude)]
      }
      if (length(lats) == 1) {
        leafletProxy("map", session = session) %>% setView(lng = lngs[1], lat = lats[1], zoom = 10)
      } else if (length(lats) > 1) {
        leafletProxy("map", session = session) %>% fitBounds(min(lngs), min(lats), max(lngs), max(lats))
      }
    }

    # helper to keep icons nicely aligned with text
    ui_icon <- function(name) bsicons::bs_icon(name, class = "me-1", style = "vertical-align:-2px;")


    # ---------- helper: open (or reopen) the main editor modal ----------
    open_editor_modal <- function() {
      showModal(
        modalDialog(
          size = "xl",
          easyClose = FALSE,
          title = "Point editor",
          footer = tagList(
            actionButton(ns("save_changes"), "Save changes", icon = icon("save"), class = "btn btn-primary"),
            actionButton(ns("save_as"),      "Save as…",     icon = icon("file-export"), class = "btn btn-outline-primary"),
            modalButton("Close without saving")
          ),
          div(
            style = "position: relative; padding: 20px;",
            page_fluid(
              navset_tab(
                nav_panel(
                  "MAP",
                  div(
                    class = "alert alert-info",
                    tags$strong("How to edit points"),
                    tags$ul(
                      tags$li(tags$b("Move points:"), " Drag any ", ui_icon("geo-alt-fill"), "marker to reposition it."),
                      tags$li(tags$b("Insert new points:"), " Click ", ui_icon("geo-alt-fill"), "on the toolbar, then click on the map."),
                      tags$li(
                        tags$b("Select points (four ways):"),
                        tags$ol(
                          tags$li(tags$b("Polygon tool (toolbar):"),
                                  " Click ", ui_icon("pentagon-fill"), "on the toolbar, then draw a polygon.", "Selection includes points",
                                  tags$em("within"), " the polygon (", tags$code("st_within"), ")."),
                          tags$li(tags$b("Bounding box (manual):"),
                                  " Enter ", tags$code("xmin, ymin, xmax, ymax"), " below."),
                          tags$li(tags$b("GeoPackage (GPKG):"),
                                  " Upload polygons; selection includes points ",
                                  tags$em("within"), " those polygons."),
                          tags$li(
                            tags$b("Catchment (click-to-delineate):"),
                            " Click on the map to choose a location; the upstream catchment for that point is delineated and used to select points ",
                            tags$em("within"), "."
                          )
                        )
                      ),
                      tags$li(tags$b("Actions:"),
                              " Use ", tags$strong("Keep selected"),
                              " or ", tags$strong("Delete selected"),
                              " to act on the current selection.")
                    )
                  ),
                  leafletOutput(ns("map"), height = 600),
                  div(
                    class = "d-flex justify-content-end gap-2",
                    actionButton(ns("keep"),   "Keep selected",  icon = icon("check")),
                    actionButton(ns("delete"), "Delete selected", icon = icon("trash"), class = "btn btn-danger")
                  ),
                  br(),
                  page_fluid(
                    accordion(
                      accordion_panel(
                        title = "Enter a bounding box",
                        p("Type bounding box coordinates"),
                        fluidRow(
                          column(3, numericInput(ns("xmin"), "xmin:", value = NA)),
                          column(3, numericInput(ns("ymin"), "ymin:", value = NA)),
                          column(3, numericInput(ns("xmax"), "xmax:", value = NA)),
                          column(3, numericInput(ns("ymax"), "ymax:", value = NA))
                        )
                      ),
                      accordion_panel(
                        title = "Upload a polygon layer",
                        fileInput(ns("sf_file"), "Upload a *.gpkg file", accept = c(".gpkg"))
                      ),
                      accordion_panel(
                        title = "Delineate catchment",
                        div(
                          class = "d-flex align-items-center gap-2",
                          checkboxInput(ns("catchment_mode"), "Click to delineate catchment", value = FALSE),
                          tags$small(class = "text-muted", "When enabled, click the map to outline the upstream catchment and use it to select points.")
                        )
                      ),
                      open = FALSE
                    )
                  ),
                  br(),
                  div(
                    class = "alert alert-info",
                    tags$strong("Save options"),
                    tags$ul(
                      tags$li(
                        tags$strong("Save changes:"), " Commits the current edits to the app’s dataset.",
                        " Runs validations; on success you’ll see a confirmation message."
                      ),
                      tags$li(
                        tags$strong("Save as…:"), " Opens an export dialog to download the edited points as ",
                        tags$code("CSV"), ", ", tags$code("GeoJSON"), " or ", tags$code(".gpkg"),
                        ". Lets you choose file name and whether to use original or snapped coordinates.",
                        " This does ", tags$em("not"), " modify the dataset in the app."
                      ),
                      tags$li(
                        tags$strong("Close without saving:"), " Closes the editor and discards any unsaved changes."
                      )
                    )
                  )
                ),
                nav_panel(
                  "TABLE",
                  DTOutput(ns("coord_table")),
                  icon = bsicons::bs_icon("table")
                )
              )
            )
          )
        )
      )

      # build (or rebuild) the map every time the modal opens
      output$map <- renderLeaflet({
        s2mapsAttribution <- paste0(
          '<a xmlns:dct="http://purl.org/dc/terms/"',
          'href="https://s2maps.eu" property="dct:title">Sentinel-2 cloudless - ',
          'https://s2maps.eu</a> by <a xmlns:cc="http://creativecommons.org/ns#"',
          'href="https://eox.at" property="cc:attributionName" rel="cc:attributionURL">',
          "EOX IT Services GmbH</a> (Contains modified Copernicus Sentinel data 2016 &amp; 2017)"
        )
        leaflet() %>%
          setView(0, 10, 2.5) %>%
          addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
          addTiles("https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless_3857/default/g/{z}/{y}/{x}.jpg",
                   s2mapsAttribution, group = "Sentinel-2 cloudless") %>%
          addTiles(group = "OpenStreetMap") %>%
          addWMSTiles("https://geo.igb-berlin.de/geoserver/ows?",
                      layers = "hydrography90m_v1_sub_catchment_cog",
                      group = "Sub-catchments",
                      options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.35)) %>%
          addWMSTiles("https://geo.igb-berlin.de/geoserver/ows?",
                      layers = "hydrography90m_v1_stream_order_strahler_cog",
                      group = "Stream segments",
                      options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 1.0)) %>%
          hideGroup(c("Stream segments", "Input points", "Snapped points")) %>%
          addLayersControl(
            baseGroups    = c("Sentinel-2 cloudless", "OpenStreetMap"),
            overlayGroups = c("Input points", "Snapped points", "Stream segments", "Sub-catchments", "AMBER", "AMBER-snapped"),
            options       = layersControlOptions(collapsed = FALSE)
          ) %>%
          addDrawToolbar(
            targetGroup        = "draw",
            polygonOptions     = drawPolygonOptions(),
            rectangleOptions   = FALSE,
            circleOptions      = FALSE,
            markerOptions      = drawMarkerOptions(repeatMode = TRUE),
            polylineOptions    = FALSE,
            circleMarkerOptions= FALSE,
            editOptions        = editToolbarOptions(edit = FALSE)
          ) %>%
          # Signal when the map exists; also auto-disable draw tool after create
          htmlwidgets::onRender(
            sprintf("
              function(el, x) {
                var map = this;
                setTimeout(function() {
                  var mk = document.querySelector('.leaflet-draw-draw-marker');
                  if (mk) mk.title = 'Insert point';
                  var pg = document.querySelector('.leaflet-draw-draw-polygon');
                  if (pg) pg.title = 'Draw a polygon';
                  if (HTMLWidgets.shinyMode) {
                    Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
                  }
                }, 0);
                map.on('draw:created', function() {
                  if (map.drawControl && map.drawControl._toolbars && map.drawControl._toolbars.draw) {
                    map.drawControl._toolbars.draw.disable();
                  } else {
                    var active = document.querySelector('.leaflet-draw-toolbar .leaflet-draw-toolbar-button-enabled');
                    if (active) active.click();
                  }
                });
              }
            ", ns('map_ready'))
          )
      })
      outputOptions(output, "map", suspendWhenHidden = FALSE)

      # if we already have points, redraw them
      isolate({
        df <- working_points()
        if (!is.null(df) && nrow(df)) draw_points(df)
      })
    }

    # ---------- modal launcher (initial open) ----------
    observeEvent(input$open_modal, {
      # fresh working copy from parent-provided data (only on first open)
      if (!is.null(point_user()) && nrow(point_user())) {
        working_points(point_user())
      } else {
        working_points(NULL)
      }
      sel_geom(NULL)
      open_editor_modal()
    }, ignoreInit = TRUE)

    # ---------- "Save changes" -> persist staged edits to parent ----------
    observeEvent(input$save_changes, {
      pts <- working_points()
      if (is.null(pts) || !nrow(pts)) {
        showNotification("Nothing to save.", type = "warning"); return()
      }
      saved_points(pts)
      showNotification("Changes saved to the app.", type = "message")
    })

    # ---------- "Save as…" -> open mini modal for export -----------------
    observeEvent(input$save_as, {
      default_name <- paste0("points_", format(Sys.time(), "%Y%m%d_%H%M"))
      showModal(modalDialog(
        title = "Export edited points",
        easyClose = TRUE,
        footer = tagList(
          downloadButton(ns("download_export"), "Download"),
          actionButton(ns("exp_done"), "Done"),   # will close mini and reopen main
          modalButton("Cancel")
        ),
        fluidRow(
          column(7, textInput(ns("exp_name"), "File name (no extension)", value = default_name)),
          column(5, selectInput(ns("exp_format"), "Format", choices = c("CSV", "GeoJSON", "GeoPackage (.gpkg)")))
        ),
        fluidRow(
          column(6, radioButtons(
            ns("coord_choice"), "Coordinates to export",
            choices = c("Original (latitude/longitude)" = "orig",
                        "Snapped (latitude_snap/longitude_snap, fallback to original when missing)" = "snap"),
            selected = "orig"
          )),
          column(6, checkboxGroupInput(
            ns("include_cols"), "Include extra columns",
            choices = c("Include *_snap columns" = "snapcols"),
            selected = NULL
          ))
        ),
        helpText("CRS: EPSG:4326 (WGS 84).")
      ))
    })

    # ---------- Close mini modal and immediately reopen main editor -------
    observeEvent(input$exp_done, {
      removeModal()        # closes the mini modal
      open_editor_modal()  # reopens the main editor modal (state preserved)
    })

    # ---------- download handler (adapts to chosen format) ---------------
    output$download_export <- downloadHandler(
      filename = function() {
        nm <- input$exp_name
        ext <- switch(input$exp_format,
                      "CSV" = ".csv",
                      "GeoJSON" = ".geojson",
                      "GeoPackage (.gpkg)" = ".gpkg",
                      ".dat"
        )
        paste0(ifelse(isTruthy(nm), nm, "points_export"), ext)
      },
      content = function(file) {
        df <- working_points()
        validate(need(!is.null(df) && nrow(df), "No points to export."))

        # choose coordinate source
        lat_use <- df$latitude
        lon_use <- df$longitude
        if (identical(input$coord_choice, "snap") &&
            all(c("latitude_snap","longitude_snap") %in% names(df))) {
          # fallback to original when snapped missing
          lat_use <- ifelse(is.finite(df$latitude_snap), df$latitude_snap, df$latitude)
          lon_use <- ifelse(is.finite(df$longitude_snap), df$longitude_snap, df$longitude)
        }

        # assemble export frame
        out <- df
        out$latitude  <- lat_use
        out$longitude <- lon_use

        # optionally include *_snap columns
        if (!("snapcols" %in% (input$include_cols %||% character(0)))) {
          out <- out[, setdiff(names(out), c("latitude_snap","longitude_snap")), drop = FALSE]
        }

        fmt <- input$exp_format %||% "CSV"
        if (fmt == "CSV") {
          utils::write.csv(out, file, row.names = FALSE, na = "")
        } else {
          # to sf points
          sf_pts <- sf::st_as_sf(out, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
          if (fmt == "GeoJSON") {
            tmp <- tempfile(fileext = ".geojson")
            sf::st_write(sf_pts, tmp, driver = "GeoJSON", quiet = TRUE)
            file.copy(tmp, file, overwrite = TRUE)
          } else if (fmt == "GeoPackage (.gpkg)") {
            tmp <- tempfile(fileext = ".gpkg")
            sf::st_write(sf_pts, tmp, driver = "GPKG", layer = "points", quiet = TRUE)
            file.copy(tmp, file, overwrite = TRUE)
          }
        }
      }
    )

    # ---------- table (shows working copy) ----------
    output$coord_table <- DT::renderDT({
      req(working_points())
      working_points()
    }, rownames = FALSE)

    # ---------- when map is ready, draw points ----------
    observeEvent(input$map_ready, {
      df <- isolate(working_points())
      if (!is.null(df) && nrow(df)) draw_points(df)
    }, ignoreInit = TRUE)

    # ---------- external points changed while modal is open ----------
    observeEvent(point_user(), {
      if (isTruthy(input$open_modal)) {
        working_points(point_user())
        draw_points(point_user())
      }
    }, ignoreInit = FALSE)

    # ---------- draw toolbar: new features ----------
    observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      type <- feat$geometry$type

      if (type == "Point") {
        cur <- working_points()
        lat <- feat$geometry$coordinates[[2]]
        lng <- feat$geometry$coordinates[[1]]
        new_id <- if (is.null(cur) || !nrow(cur)) 1 else max(cur$id, na.rm = TRUE) + 1
        new_row <- data.frame(id = new_id, latitude = lat, longitude = lng)
        working_points(dplyr::bind_rows(cur, new_row))
        draw_points(working_points())
      }

      if (type == "Polygon") {
        coords <- feat$geometry$coordinates[[1]]
        shp <- sf::st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))) |>
          sf::st_sfc(crs = 4326) |>
          sf::st_sf()
        sel_geom(shp)
      }
    })

    # ---------- render selection polygon(s) ----------
    observe({
      req(sel_geom())
      leafletProxy("map", session = session) %>%
        clearShapes() %>%
        addPolygons(data = sel_geom(), color = "blue", fillOpacity = 0.35)
    })

    # ---------- bbox -> selection ----------
    observe({
      req(!is.na(input$xmin), !is.na(input$ymin), !is.na(input$xmax), !is.na(input$ymax))
      validate(
        need(input$xmin < input$xmax, "xmin must be < xmax"),
        need(input$ymin < input$ymax, "ymin must be < ymax")
      )
      bb <- matrix(c(
        input$xmin, input$ymin,
        input$xmax, input$ymin,
        input$xmax, input$ymax,
        input$xmin, input$ymax,
        input$xmin, input$ymin
      ), ncol = 2, byrow = TRUE)
      sel_geom(sf::st_sf(sf::st_sfc(sf::st_polygon(list(bb)), crs = 4326)))
    })

    # ---------- GPKG upload -> selection ----------
    observeEvent(input$sf_file, {
      ext <- tools::file_ext(input$sf_file$name)
      if (tolower(ext) != "gpkg") {
        showNotification("Unsupported file format (use .gpkg).", type = "error")
        return()
      }
      shp <- tryCatch(sf::st_read(input$sf_file$datapath, quiet = TRUE), error = function(e) NULL)
      if (is.null(shp)) {
        showNotification("Failed to read GPKG.", type = "error")
      } else {
        shp <- sf::st_make_valid(shp)
        if (sf::st_crs(shp) != sf::st_crs(4326)) shp <- sf::st_transform(shp, 4326)
        sel_geom(shp)
      }
    })

    # ---------- DRAG handler (updates original vs snapped columns) ----------
    observeEvent(input$map_marker_dragend, {
      req(working_points())
      drag <- input$map_marker_dragend
      cur  <- working_points()

      drag_id <- as.character(drag$id)

      if (grepl("_snap$", drag_id)) {
        # dragged a SNAPPED marker -> update *_snap
        base_id <- sub("_snap$", "", drag_id)
        idx <- which(as.character(cur$id) == base_id)
        if (length(idx) == 1) {
          cur[idx, c("latitude_snap", "longitude_snap")] <- c(round(drag$lat, 6), round(drag$lng, 6))
        }
      } else {
        # dragged an INPUT marker -> update original
        idx <- which(as.character(cur$id) == drag_id)
        if (length(idx) == 1) {
          cur[idx, c("latitude", "longitude")] <- c(round(drag$lat, 6), round(drag$lng, 6))
        }
      }

      working_points(cur)
      draw_points(cur)
    })

    # ---------- KEEP selected ----------
    observeEvent(input$keep, {
      pts <- working_points()
      shp <- sel_geom()
      if (is.null(pts) || nrow(pts) == 0) { showNotification("No points to filter.", type = "warning"); return() }
      if (is.null(shp) || sf::st_is_empty(shp)) { showNotification("No selection geometry.", type = "warning"); return() }
      pts_sf  <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
      inside  <- lengths(sf::st_within(pts_sf, shp)) > 0  # robust for multi-polygons
      kept_df <- pts[inside, , drop = FALSE]
      if (!nrow(kept_df)) { showNotification("Selection contains no points.", type = "message"); return() }
      working_points(kept_df)
      draw_points(kept_df)
    })

    # ---------- DELETE selected ----------
    observeEvent(input$delete, {
      pts <- working_points()
      shp <- sel_geom()
      if (is.null(pts) || nrow(pts) == 0) { showNotification("No points to filter.", type = "warning"); return() }
      if (is.null(shp) || sf::st_is_empty(shp)) { showNotification("No selection geometry.", type = "warning"); return() }
      pts_sf  <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
      inside  <- lengths(sf::st_within(pts_sf, shp)) > 0
      kept_df <- pts[!inside, , drop = FALSE]
      working_points(kept_df)
      draw_points(kept_df)
    })

    # --- outputs for parent
    return(saved_points)
  })
}
