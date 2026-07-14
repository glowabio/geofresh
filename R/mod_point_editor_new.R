# Point editor module
library(leaflet)
library(DT)
library(sf)
library(terra)
library(dplyr)
library(htmlwidgets)
library(leaflet.extras)
library(bsicons)
library(later)
# for asynchronous:
library(promises)
library(httr2)

# =========================
# UI
# =========================
pointEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionLink(ns("open_modal"), "3. Open Point Editor")
  )
}

pointEditorServer <- pointEditorServer <- function(id,
                                                   points_table_name,
                                                   on_db_changed = NULL,
                                                   on_snap = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    baseline_points <- reactiveVal(NULL)   # snapshot from DB at open
    upstream_sf     <- reactiveVal(NULL)   # pygeoapi result (upstream catchment as filtering geometry)

    # --- Working state inside the modal ---
    working_points <- reactiveVal(NULL)   # data.frame: id, latitude, longitude, (optional *_snap)
    sel_geom       <- reactiveVal(NULL)   # sf polygon(s) for current selection
    saved_points   <- reactiveVal(NULL)   # last saved to app (for parent)

    # ---------- helper: (re)draw points ----------
    draw_points <- function(df) {

      if (is.null(df) || !nrow(df)) {
        leafletProxy("map", session = session) %>%
          clearGroup("Input points") %>%
          clearGroup("Snapped points") %>%
          removeControl("points-legend")
        return(invisible())
      }

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

    } # end of function definition draw_points()

    # helper to keep icons nicely aligned with text
    ui_icon <- function(name) bsicons::bs_icon(name, class = "me-1", style = "vertical-align:-2px;")


    # helper for change detection
    round6 <- function(x) ifelse(is.finite(x), round(x, 6), NA_real_)


    # ---------- Snap-after-save modal (Sub-catchment only, then return to editor) ----------
    show_snap_after_save_modal <- function() {
      showModal(
        modalDialog(
          title = "Points need snapping",
          easyClose = FALSE,
          footer = tagList(
            actionButton(
              ns("snap_after_save_run"),
              "Snap now",
              icon = icon("magnet"),
              class = "btn btn-primary"
            ),
            actionButton(
              ns("snap_after_save_close"),
              "Close without snapping",
              class = "btn btn-outline-secondary"
            )
          ),
          tagList(
            div(
              class = "alert alert-warning",
              tags$b("Points were modified. They must be snapped before analysis."),
              tags$p("Even if you dragged a point onto a stream, it may not lie exactly on the stream line. Click “Snap now” to align points to the stream network and save the snapped locations to the app."),
              tags$small(class = "text-muted", "Snapping method: sub-catchment.")
            ),
            br(),
            shinyWidgets::progressBar(
              id = ns("progress_after_save"),
              value = 0,
              title = " ",
              display_pct = TRUE
            ),
            shinyjs::hidden(tags$p(id = ns("text_after_save"), "Processing..."))
          )
        )
      )
    }

    update_after_save_progress <- function(p, sleep = 0.05) {
      shinyWidgets::updateProgressBar(session, id = ns("progress_after_save"), value = p)
      Sys.sleep(sleep)
    }

    # Close snap modal and return to main editor modal (same pattern as Export as…)
    close_snap_modal_return_to_editor <- function() {
      safe_swap_to_editor()
    }


    run_snap_after_save <- function() {
      tn <- points_table_name()
      req(tn)

      table_id <- DBI::Id(schema = "shiny_user", table = tn)

      shinyjs::show(ns("text_after_save"))
      update_after_save_progress(0)

      # Optional: prevent double-click while snapping
      shinyjs::disable(ns("snap_after_save_run"))
      shinyjs::disable(ns("snap_after_save_close"))

      tryCatch({
        pool::poolWithTransaction(pool, function(conn) {
          snap_points_subcatchment_db(
            conn         = conn,
            points_table = table_id,
            progress     = function(p) update_after_save_progress(p)
          )
        })

        if (is.function(on_db_changed)) on_db_changed()

        # Refresh editor state so when we reopen it, it's already updated
        df_new <- with_pool_connection(pool, function(conn) read_points_db(conn, table_id))
        working_points(df_new)
        baseline_points(df_new)
        draw_points(df_new)

        shinyjs::hide(ns("text_after_save"))
        update_after_save_progress(0, sleep = 0.1)

        showNotification("Snapping finished. Snapped locations saved.", type = "message", duration = 5)

        # Return to main editor modal
        close_snap_modal_return_to_editor()

      }, error = function(e) {
        shinyjs::hide(ns("text_after_save"))
        update_after_save_progress(0, sleep = 0.1)

        # Re-enable buttons (defensive)
        shinyjs::enable(ns("snap_after_save_run"))
        shinyjs::enable(ns("snap_after_save_close"))

        showNotification(
          paste("Snapping failed:", conditionMessage(e)),
          type = "error",
          duration = 6
        )

        # Return to main editor modal even on error
        close_snap_modal_return_to_editor()
      })
    }

    # Snap now
    observeEvent(input$snap_after_save_run, {
      run_snap_after_save()
    })

    # Close without snapping -> return to main editor modal
    observeEvent(input$snap_after_save_close, {
      close_snap_modal_return_to_editor()
    })

    # Re-open editor modal
    safe_swap_to_editor <- function(delay = 0.2) {
      removeModal(session = session)

      later::later(function() {
        shiny::withReactiveDomain(session, {
          open_editor_modal()
        })
      }, delay)
    }



    # ---------- helper: open the main editor modal ----------
    open_editor_modal <- function() {
      showModal(
        modalDialog(
          size = "xl",
          easyClose = FALSE,
          title = "Point editor",
          footer = tagList(
            actionButton(ns("save_changes"), "Save changes", icon = icon("save"), class = "btn btn-primary"),
            actionButton(ns("export_as"),      "Export as…",     icon = icon("file-export"), class = "btn btn-outline-primary"),
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
                      tags$li(tags$b("Move points:"), " Drag any ", tags$em("purple"), ui_icon("geo-alt-fill"), "marker to reposition it."),
                      tags$li(tags$b("Insert new points:"), " Click ", ui_icon("geo-alt-fill"), "on the toolbar, then click on the map."),
                      tags$li(
                        tags$b("Select points"), " to keep or to discard:",
                        tags$ol(
                          tags$li("by ", tags$b("drawing a polygon"), "on the map"),
                          tags$li("by ", tags$b("specifying a bounding box")),
                          tags$li("by ", tags$b("uploading a file"), " containing polygons (GeoPackage or GeoJSON)"),
                          tags$li("by ", tags$b("copying and pasting GeoJSON")),
                          tags$li("by ", tags$b("delineating an upstream catchment"))
                        )
                      )
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
                        title = "Draw a polygon",
                        p("Click on the icon ", ui_icon("pentagon-fill"), " and draw a polygon. Close it by clicking ", tags$code("Finish"), "  or by clicking on the polygon's first point. The selection includes points", tags$em("within"), " the polygon (", tags$code("st_within"), ")."),
                      ),
                      accordion_panel(
                        title = "Enter a bounding box",
                        p("Type bounding box coordinates:"),
                        fluidRow(
                          column(3, numericInput(ns("xmin"), "min lon:", value = NA)),
                          column(3, numericInput(ns("ymin"), "min lat:", value = NA)),
                          column(3, numericInput(ns("xmax"), "max lon:", value = NA)),
                          column(3, numericInput(ns("ymax"), "max lat:", value = NA))
                        )
                      ),
                      accordion_panel(
                        title = "Upload a polygon layer",
                        p("Upload a GeoPackage or GeoJSON file (", tags$code(".gpkg, .json, .geojson"), "). The selection includes points ", tags$em("within"), " those polygons."),
                        fileInput(ns("sf_file"), "Upload a file:", accept = c(".gpkg", ".geojson", ".json")),
                        textInput(ns("sf_url"), "Or provide a URL", placeholder = "https://example.com/data.geojson"),
                        actionButton(ns("read_uploaded_polygons"), "Load polygons to map")
                      ),
                      accordion_panel(
                        title = "Paste GeoJSON directly",
                        p("Paste GeoJSON polygons into the text field below:"),
                        textAreaInput(ns("sf_geojson_text"), "Paste here:", rows = 10, placeholder = '{"type": "FeatureCollection", "name": "geofresh_test_polygons", "features": [{"type": "Feature", "properties": {"name": "testpoly1"}, "geometry": {"type": "Polygon", "coordinates": [[[9.58573412496881, 53.70333661212113], [10.7492189194642, 52.87426660885793], [11.2508411063125, 53.39678972015825], [10.8606905165416, 54.05168535298799], [9.58573412496881, 53.70333661212113]]]}}]}'),
                        actionButton(ns("read_pasted_geojson"), "Load polygons to map")
                      ),
                      accordion_panel(
                        title = "Upstream catchment",
                        div(
                          class = "d-flex align-items-center gap-2",
                          checkboxInput(ns("catchment_mode"), "Click to delineate catchment", value = FALSE),
                          tags$small(class = "text-muted", "When enabled, click the map and wait for the server's reply.")
                        ),
                        numericInput(
                          inputId = ns("target_strahler"),
                          label   = "Minimum Strahler order",
                          value   = 3,
                          min     = 1,
                          step    = 1
                        ),
                        p("Click on the map to calculate the upstream catchment of that location, which you can then use to select or deselect points.")
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
                        tags$strong("Export as…:"), " Opens an export dialog to download the edited points as ",
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
            overlayGroups = c("Input points", "Snapped points", "Stream segments", "Sub-catchments"), #, "AMBER", "AMBER-snapped"),
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

      map.on('draw:created', function(e) {
        // If the user drew a marker (point), remove the blue marker layer immediately
        if (e.layerType === 'marker') {
          map.removeLayer(e.layer);
        }

        // Your existing logic to auto-disable the draw tool
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
      req(points_table_name())
      table_id <- DBI::Id(schema = "shiny_user", table = points_table_name())

      df <- with_pool_connection(pool, function(conn) {
        read_points_db(conn, table_id)
      })

      df$id <- as.numeric(df$id)

      working_points(df)
      baseline_points(df)
      sel_geom(NULL)

      open_editor_modal()
    }, ignoreInit = TRUE)

    ##########################
    ### upstream catchment ###
    ### (for selection)    ###
    ##########################

    # What? Let users click on map to retrieve one upstream catchment
    # of a random point, to be used as filtering geometry.
    # The geometry is calculated by / requested from pygeoapi.
 
    # TODO: The click location is shown only after the result comes
    # back from pygeoapi. Working on this.

    # define asynchronous extended task here, to be invoked below:
    upstr_task <- ExtendedTask$new(function(lon, lat, strahler=NULL) {

      # If no min strahler value was provided:
      # TODO: This never happens, as strahler order is always some integer (by default 3),
      # so we always run snapping first. Is this desired? Should we prevent snapping is strahler=1?
      # Should we let users pick between no-snapping and snapping (as strahler=1 is not the same as
      # strahler=NULL, in terms of what happens during snapping)
      if (is.null(strahler)) {
        #showNotification(paste("DEBUG: INVOKED upstream calculation for point: lon=", lon, ", lat=", lat, "..."))
        future_promise({
          upstr_res <- fetch_from_pygeoapi_upstream(lon=lon, lat=lat)
          upstr_res
        })
      } else {
        # TODO: this calls pygeoapi twice, not super efficient!
        #showNotification(paste("DEBUG: INVOKED upstream calculation strahler for point: lon=", lon, ", lat=", lat, ", strahler=", strahler, "..."))
        future_promise({
          snapped_subc_id <- fetch_from_pygeoapi_strahler_snap_singular(lon, lat, strahler)
          upstr_res <- fetch_from_pygeoapi_upstream(subc_id=snapped_subc_id)
          upstr_res
        })
      }
    })


    # Observer only for debugging the upstream task status and error situation:
    observe({
      req(FALSE) # this deactivates this observer, for when we are not debugging!
      req(upstr_task)

      # Write status to /tmp:
      writeLines(capture.output(upstr_task$status()), "/tmp/upstream_status.txt")

      # Write error to /tmp:
      if (upstr_task$status() == "error") {

      # this re-throws the error (calling upstr_task$result()):
        err <- tryCatch(
          upstr_task$result(),
          error = function(e) e
        )

        # get the message and write it to /tmp:
        writeLines(capture.output(conditionMessage(err)), "/tmp/upstream_error.txt")

        # show message in notification (always goes away automatically):
        showNotification(
          paste("DEBUG: Upstream task failed:", conditionMessage(err)),
          type = "error"
        )

        # show message in modal dialog (has to be acknowledged by user):
        showModal(modalDialog(
          title = "Upstream task failed",
          conditionMessage(err),
          easyClose = FALSE,
          footer = modalButton("Got it!")
        ))
      }
    })

    # Reacting to completion or failure of asynchronous extended tasks:
    observeEvent(upstr_task$status(), {

      status <- upstr_task$status()

      if (status == "success") {
        showNotification("Upstream task finished!")
        res <- upstr_task$result()
        req(res)
        # res is your sf object
        upstream_sf(res) # reactive

      } else if (status == "error") {

        # calling upstr_task$result() re-throws error, so we wrap it
        # in tryCatch and extract the error message:
        err <- tryCatch(
          upstr_task$result(),
          error = function(e) e
        )
        full_msg <- conditionMessage(err)

        # show full error message in notification (always goes away automatically):
        #showNotification(
        #  paste("Upstream task failed:", full_msg),
        #  type = "error"
        #)

        # try to make a cleaner message for users, by matching the expected ocean-message and rewriting:
        # TODO: Move this code to pygeoapi_helpers, as the other processes may return the same message!
        if (grepl("No reg_id found for lon .* lat .* Is this in the ocean\\?", full_msg)) {
          match <- regexec("No reg_id found for lon ([^,]+), lat ([^!]+)!", full_msg)
          coords <- regmatches(full_msg, match)[[1]]
          if (length(coords) == 3) {
            lon <- coords[2]
            lat <- coords[3]
            cleaned_msg <- paste0("Point (lon=", lon, ", lat=", lat, ") is outside the valid region (probably in the ocean).")
          } else {
            cleaned_msg <- "Point is outside the valid region (probably in the ocean)."
          }
        } else {
          # if we could not match the expected message, something else may have gone wrong:
          cleaned_msg <- full_msg
        }

        # show cleaned message in modal dialog (has to be acknowledged by user):
        # TODO: this closes the entire point editor, prevent that!
        showModal(modalDialog(
          title = "Computing upstream catchment failed",
          cleaned_msg,
          easyClose = FALSE,
          footer = modalButton("Got it!")
        ))

      }
    }, ignoreInit = TRUE)

    # Display the delineated upstream catchment on the map, once it was
    # returned by pygeoapi server:
    observe({
      req(upstream_sf())
      #showNotification("DEBUG: display upstream polygons...")
      # Extract polygons from FeatureCollection, otherwise "addPolygons()" fails:
      upstream_polys <- sf::st_collection_extract(upstream_sf(), "POLYGON")
      # Display the catchment on the map (just the most recent one):
      # Note: This polygon gets drawn, but then sel_geom (which is the same polygon)
      # gets drawn on top. So this polygon is only visible if we create a new
      # sel_geom after this, which is not a upstream polygon!
      leafletProxy("map") %>%
        clearGroup("upstream_polys") %>%
        addPolygons(
          data = upstream_polys,
          group = "upstream_polys",
          fillColor = "green",
          fillOpacity = 0.5,
          stroke = FALSE,
          color = NA,
          weight = 0,
          opacity = 0
        )

      # Convert to WGS84 if applicable:
      if (sf::st_crs(upstream_polys) != sf::st_crs(4326)) {
        upstream_polys <- sf::st_transform(upstream_polys, 4326)
      }
      # Now we also need to set it as filtering geometry
      # TODO: Do we allow several upstream catchments?
      # TODO: Should we add strahler snapping, because here the upstream catchments are so small
      sel_geom(upstream_polys)
    })

    # Variable to store map click info (if catchment mode is enabled),
    # also used to observe/trigger the upstream computation:
    clicked_point_for_upstream <- reactiveVal(NULL)

    # Variable to store min strahler (need to store in variable,
    # because the asynchronous task cannot access any input$...):
    min_strahler_for_upstream <- reactiveVal(NULL)

    # When user clicks on map AND catchment-click-mode is enabled,
    # display the click on the map and store the relevant info we
    # need for computing the upstream catchment. (This will trigger
    # the second observer further below).
    observeEvent(input$map_click, {

      # Check if we are in catchment delineation mode?
      req(isTRUE(input$catchment_mode))

      # Disable catchment mode - otherwise any subsequent click
      # will trigger upstream computation!
      # We also disable it when the user has finished drawing/editing
      # a polygon feature.
      # TODO: Rather disable this mode when another tool is started or
      # opened, but it seems a bit complicated to catch those events,
      # as leaflet does not expose them.
      updateCheckboxInput(session, "catchment_mode", value = FALSE)

      # Store click for longer task
      clicked_point_for_upstream(input$map_click)

      # Also store min_strahler value provided by user:
      min_strahler_for_upstream(as.integer(input$target_strahler %||% 3L))

      # display the click on the map (just the most recent one):
      mylabel <- paste0("upstream of here (min. strahler order: ", min_strahler_for_upstream(),")")
      leafletProxy("map") %>%
        clearGroup("upstream_click") %>%
        addCircleMarkers(
          lng = input$map_click$lng,
          lat = input$map_click$lat,
          color = "blue",
          label = mylabel,
          group = "upstream_click"
        )

      # TODO (not urgent): The point should be displayed immediately,
      # but for some reason it takes a little while. Figure out why.
      # Maybe have to re-render somehow.
      #showNotification("DEBUG: DONE: displayed the click on the map")
    })

    # Second observer to do the expensive work:
    observeEvent(clicked_point_for_upstream(), {
      click <- clicked_point_for_upstream()
      #showNotification("DEBUG: Now calculating upstream catchment (asynchronously)")
      upstr_task$invoke(click$lng, click$lat, min_strahler_for_upstream())
      #showNotification(paste("DEBUG: Calculating upstream catchment was requested for point lon=", click$lng, ", lat=", click$lat, "..." ))
    })

    # Emergency-disable catchment mode when a user has finished
    # drawing or editing a polygon - otherwise any subsequent
    # click will trigger upstream computation! Already all the
    # polygon creation clicks all triggered one... But catching
    # earlier events is tricky.
    observeEvent(input$map_draw_new_feature, {
      #showNotification("DEBUG: A new feature was drawn")
      updateCheckboxInput(session, "catchment_mode", value = FALSE)
    })
    observeEvent(input$map_draw_edited_features, {
      #showNotification("Features were edited")
      updateCheckboxInput(session, "catchment_mode", value = FALSE)
    })
    observeEvent(input$map_draw_deleted_features, {
      #showNotification("Features were deleted")
      updateCheckboxInput(session, "catchment_mode", value = FALSE)
    })


    ##################################
    ### end of: upstream catchment ###
    ##################################

    # ---------- "Save changes" -> persist staged edits to parent ----------
    observeEvent(input$save_changes, {
      req(points_table_name())
      cur <- working_points()
      base <- baseline_points()

      if (is.null(cur) || !nrow(cur)) {
        # If baseline had rows, this means "user deleted everything" -> persist empty DB table
        if (!is.null(base) && nrow(base) > 0) {

          table_id <- DBI::Id(schema = "shiny_user", table = points_table_name())

          tryCatch({
            pool::poolWithTransaction(pool, function(conn) {
              wipe_points_db(conn, table_id)
            })

            if (is.function(on_db_changed)) on_db_changed()

            # refresh local state from DB (will be empty)
            df_new <- with_pool_connection(pool, function(conn) read_points_db(conn, table_id))
            working_points(df_new)
            baseline_points(df_new)
            draw_points(df_new)

            showNotification("Saved: all points deleted.", type = "message", duration = 5)


          }, error = function(e) {
            showModal(modalDialog(
              title = "Save failed",
              paste("Database error:", conditionMessage(e)),
              easyClose = TRUE
            ))
          })

          return()
        }

        # base was also empty -> truly nothing to save
        showNotification("No changes detected.", type = "message", duration = 4)
        return()
      }

      if (is.null(base)) base <- cur[0, , drop = FALSE]  # safety

      # Normalize
      cur$id  <- as.character(cur$id)
      base$id <- as.character(base$id)

      # Detect adds/deletes
      added   <- setdiff(cur$id,  base$id)
      removed <- setdiff(base$id, cur$id)

      # Join for comparisons
      merged <- merge(
        cur[, intersect(c("id","latitude","longitude","latitude_snap","longitude_snap"), names(cur)), drop=FALSE],
        base[, intersect(c("id","latitude","longitude","latitude_snap","longitude_snap"), names(base)), drop=FALSE],
        by = "id", all = FALSE, suffixes = c(".cur", ".base")
      )

      # Detect original coordinate edits
      orig_changed <- FALSE
      if (nrow(merged)) {
        orig_changed <- any(
          round6(merged$latitude.cur)  != round6(merged$latitude.base) |
            round6(merged$longitude.cur) != round6(merged$longitude.base),
          na.rm = TRUE
        )
      }

      # Detect snapped marker edits (hint edits)
      snap_changed_ids <- character(0)
      if (nrow(merged) && all(c("latitude_snap.cur","longitude_snap.cur","latitude_snap.base","longitude_snap.base") %in% names(merged))) {
        ch <- (
          round6(merged$latitude_snap.cur)  != round6(merged$latitude_snap.base) |
            round6(merged$longitude_snap.cur) != round6(merged$longitude_snap.base)
        )
        snap_changed_ids <- merged$id[which(ch)]
      }

      # Decide save mode:
      # - If ids added/removed or original coords changed => ORIGINAL SAVE
      # - Else if only snapped coords changed => HINT SAVE
      do_original <- (length(added) > 0) || (length(removed) > 0) || isTRUE(orig_changed)
      do_hint     <- (!do_original) && (length(snap_changed_ids) > 0)

      table_id <- DBI::Id(schema = "shiny_user", table = points_table_name())

      tryCatch({
        pool::poolWithTransaction(pool, function(conn) {
          if (do_original) {
            save_original_edits_db(conn, table_id, cur)
          } else if (do_hint) {
            df_hint <- cur[cur$id %in% snap_changed_ids, c("id","latitude_snap","longitude_snap"), drop = FALSE]
            save_snap_hints_db(conn, table_id, df_hint)
          } else {
            # nothing changed
            NULL
          }
        })

        # bump version so DB readers refresh
        if (is.function(on_db_changed)) on_db_changed()

        # refresh baseline from DB (so consecutive saves work)
        df_new <- with_pool_connection(pool, function(conn) read_points_db(conn, table_id))
        working_points(df_new)
        baseline_points(df_new)
        draw_points(df_new)

        # Call snapping modal if required
        if (do_original) {
          showNotification(
            "Saved. Points now need snapping before analysis.",
            type = "message", duration = 6
          )
          show_snap_after_save_modal()

        } else if (do_hint) {
          showNotification(
            "Saved manual snap hints. Snapping is required to finalize.",
            type = "message", duration = 6
          )
          show_snap_after_save_modal()

        } else {
          showNotification("No changes detected.", type = "message", duration = 4)
        }

      }, error = function(e) {
        showModal(modalDialog(
          title = "Save failed",
          paste("Database error:", conditionMessage(e)),
          easyClose = TRUE
        ))
      })
    })

    # ---------- "Export as…" -> open mini modal for export -----------------
    observeEvent(input$export_as, {
      default_name <- paste0("points_", format(Sys.time(), "%Y%m%d_%H%M"))
      showModal(modalDialog(
        title = "Export edited points",
        easyClose = TRUE,
        footer = tagList(
          downloadButton(ns("download_export"), "Download"),
          # Test users did not understand this option, so I removed it:
          #actionButton(ns("exp_done"), "Done"),   # will close mini and reopen main
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
      safe_swap_to_editor()
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
        if (is.null(df) || nrow(df) == 0) {
          writeLines("No points to export.", con = file)
          return()
        }

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


    # ---------- draw toolbar: new features ----------
    observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      type <- feat$geometry$type

      if (type == "Point") {
        # 1) ID of the marker created by the draw toolbar
        draw_id <- feat$properties$`_leaflet_id`

        # 2) Remove blue marker from the map
        # leafletProxy("map", session = session) %>%
        #   removeMarker(layerId = as.character(draw_id))

        # 3) Add it to working_points and draw violet marker(s)
        cur <- working_points()
        if (!is.null(cur) && nrow(cur)) cur$id <- as.numeric(cur$id)
        lat <- feat$geometry$coordinates[[2]]
        lng <- feat$geometry$coordinates[[1]]
        new_id <- if (is.null(cur) || !nrow(cur)) 1L else max(cur$id, na.rm = TRUE) + 1L
        new_row <- data.frame(id = new_id, latitude = lat, longitude = lng)
        working_points(dplyr::bind_rows(cur, new_row))
        draw_points(working_points())
      }

      # User drew a polygon. We add it to the selection geometries:
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
      # TODO: Figure out why clearGroup does not clear the map / why several polygons are visible instead of just the most recently drawn!
      # The variable sel_geom() always contains just the most recent selection geometry.
      # But on the map, drawn polygons are being added up, despite calling clearGroup().
      # This only happens for drawn polygons, so apparently they are not (only) in the
      # group "selection_geom", but also somewhere else.
      leafletProxy("map", session = session) %>%
        clearGroup("selection_geom") %>%
        addPolygons(
          data = sel_geom(),
          color = "blue",
          fillOpacity = 0.35,
          group = "selection_geom"
        )
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

    # ---------- GPKG and GeoJSON upload/download -> selection ----------
    observeEvent(input$read_uploaded_polygons, {
      filepath <- NULL
      # If user provided a file from their disk, using upload dialogue:
      if (!is.null(input$sf_file)) {
        ext <- tools::file_ext(input$sf_file$name)
        if (!(tolower(ext) %in% c("gpkg", "json", "geojson"))) {
          showNotification("Unsupported file format (use .gpkg, .json, or .geojson).", type = "error")
          return()
        }
        filepath <- input$sf_file$datapath

      # If a user provided a URL from where to read the file:
      } else {
          url <- trimws(input$sf_url)
          if (nzchar(url)) {
            ext <- tolower(tools::file_ext(url))
            if (!(tolower(ext) %in% c("gpkg", "json", "geojson"))) {
              showNotification("Unsupported file format (use .gpkg, .json, or .geojson).", type = "error")
              return()
            }
            filepath <- tempfile(fileext = paste0(".", ext))
            tryCatch({download.file(url, filepath, mode = "wb", quiet = FALSE)}, error = function(e) {
              showNotification(paste("Failed to load URL:", e$message), type = "error")
            })
          }
      }
      # Read and validate uploaded/downloaded file:
      shp <- tryCatch(sf::st_read(filepath, quiet = TRUE), error = function(e) NULL)
      if (is.null(shp)) {
        showNotification("Failed to read GeoPackage or GeoJSON.", type = "error")
      } else {
        shp <- sf::st_make_valid(shp)
        if (sf::st_crs(shp) != sf::st_crs(4326)) shp <- sf::st_transform(shp, 4326)
        sel_geom(shp)
      }
    })

    # ---------- GeoJSON pasted -> selection ----------
    observeEvent(input$read_pasted_geojson, {
      pasted_txt <- trimws(input$sf_geojson_text)
      if (nzchar(pasted_txt)) {
        # write to temp file because st_read() expects a datasource
        geojson_tmp <- tempfile(fileext = ".geojson")
        writeLines(pasted_txt, geojson_tmp)
        shp <- tryCatch(sf::st_read(geojson_tmp, quiet = TRUE), error = function(e) NULL)
        if (is.null(shp)) {
          showNotification("Failed to read pasted GeoJSON polygons.", type = "error")
        } else {
          shp <- sf::st_make_valid(shp)
          if (sf::st_crs(shp) != sf::st_crs(4326)) {
            shp <- sf::st_transform(shp, 4326)
          }
          sel_geom(shp)
        }
      } else {
        showNotification("Failed to read pasted GeoJSON data.", type = "error")
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

      # Zoom into the dragged point
      leafletProxy("map", session = session) %>%
        setView(lng = drag$lng, lat = drag$lat, zoom = 20)

      working_points(cur)
      draw_points(cur)
    })

    # ---------- KEEP selected ----------
    observeEvent(input$keep, {
      pts <- working_points()
      shp <- sel_geom()
      if (is.null(pts) || nrow(pts) == 0) { showNotification("No points to filter.", type = "warning"); return() }
      if (is.null(shp) || all(sf::st_is_empty(shp))) { showNotification("No selection geometry.", type = "warning"); return() }
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
