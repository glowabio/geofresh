
####################################################
### Request upstream subcatchments from pygeoapi ###
####################################################

# First, define the actual task, not asynchronous:
# Make HTTP POST request to pygeoapi and get the result!
# This is done in several sub-functions.

# define function to calculate upstream catchment
# this function will run in an extended task, i.e. in a different R process/session
run_upstream_computation <- function(lon, lat) {
  sf_obj <- fetch_from_pygeoapi_upstream(lon=lon, lat=lat)
  return(sf_obj)
}


# Main function to retrieve upstream catchments for one pair of coordinates or one subc_id:
fetch_from_pygeoapi_upstream <- function(lon=NULL, lat=NULL, subc_id=NULL) {
  process_id <- "get-upstream-subcatchments"
  url <- get_pygeoapi_url(process_id)
  inputs <- make_payload_upstream(lon=lon, lat=lat, subc_id=subc_id)
  job_url   <- pygeoapiSubmitJob(url=url, inputs=inputs)
  json_link <- pygeoapiPollForResultLink(job_url)
  # WIP if json_link .....

  # Fetch the result from the server, convert, validate and return it:
  geojson_obj <- pygeoapiFetchActualResult(json_link)
  sf_obj <- convert_to_sf_object(geojson_obj)
  return(sf_obj)
}

# Construct input JSON snippet to be sent to pygeoapi as HTTP POST payload
make_payload_upstream <- function(lon=NULL, lat=NULL, subc_id=NULL) {
  if (!(is.null(subc_id))) {
    inputs <- list(
      comment = "UPSTR-geofresh-newfrontend-subcid",
      geometry_only = TRUE,
      add_upstream_ids = FALSE,
      subc_id = subc_id
    )
  } else if (!(is.null(lon) && is.null(lat))) {
    inputs <- list(
      comment = "UPSTR-geofresh-newfrontend-coords",
      geometry_only = TRUE,
      add_upstream_ids = FALSE,
      point = list(
        type = "Point",
        coordinates = c(lon, lat)
      )
    )
  } else {
    stop('Missing parameter when submitting pygeoapi job.')
    # TODO Handle this more gracefully!
  }
  return(inputs)
}

############################################
### Request path to outlet from upstream ###
############################################

# define function to calculate upstream catchment
# this function will run in an extended task, i.e. in a different R process/session
#run_upstream_computation_outlet <- function(lon, lat) {
#  sf_obj <- fetch_from_pygeoapi(lon=lon, lat=lat)
#  return(sf_obj)
#}

# Main function to retrieve path to outlet for one pair of coordinates or one subc_id:
fetch_from_pygeoapi_outlet <- function(lon=NULL, lat=NULL, subc_id=NULL) {
  process_id <- "get-shortest-path-to-outlet"
  url <- get_pygeoapi_url(process_id)
  inputs <- make_payload_routing(lon=lon, lat=lat, subc_id=subc_id)
  job_url   <- pygeoapiSubmitJob(url=url, inputs=inputs)
  json_link <- pygeoapiPollForResultLink(job_url)
  # WIP if json_link .....

  # Fetch the result from the server, convert, validate and return it:
  geojson_obj <- pygeoapiFetchActualResult(json_link)
  sf_obj <- convert_to_sf_object(geojson_obj)
  return(sf_obj)
}

# Construct input JSON snippet to be sent to pygeoapi as HTTP POST payload
make_payload_routing <- function(lon=NULL, lat=NULL, subc_id=NULL) {
  if (!(is.null(subc_id))) {
    inputs <- list(
      comment = "DOWNSTR-geofresh-newfrontend-subcid",
      geometry_only = FALSE,
      subc_id = subc_id
    )
  } else if (!(is.null(lon) && is.null(lat))) {
    inputs <- list(
      comment = "DOWNSTR-geofresh-newfrontend-coords",
      geometry_only = FALSE,
      point = list(
        type = "Point",
        coordinates = c(lon, lat)
      )
    )
  } else {
    stop('Missing parameter when submitting pygeoapi job.')
    # TODO Handle this more gracefully!
  }
  return(inputs)
}


####################################
### generic pygeoapi interaction ###
### independent of process       ###
####################################

# Construct URL
get_pygeoapi_url <- function(process_id) {
  base <- "https://aqua.igb-berlin.de/pygeoapi-dev/"
  url <- paste0(base, "processes/", process_id, "/execution")
  return(url)
}

# Submit the job to pygeoapi via HTTP, receive
# the URL where to poll for the job's status:
pygeoapiSubmitJob <- function(url, inputs) {
  resp <- request(url) |>
    req_method("POST") |>
    req_body_json(list(
      inputs = inputs
    )) |>
    req_headers(
      Prefer = "respond-async",
      Accept = "application/json"
    ) |>
    req_perform()
  # Extract the job url from the response:
  #body <- resp_body_json(resp)
  hdrs <- resp_headers(resp)
  job_url <- hdrs$location
  return(job_url)
}


# Poll until successful and return the link to the result:
# With Sys.sleep():
pygeoapiPollForResultLink <- function(job_url) {

  # this will be filled and returned:
  json_link <- "nothing-yet"

  # now poll for status:
  attempts <- 0
  repeat {
    # TODO: Important, remove sys.sleep, as it blocks the entire worker.
    # Instead, use recursive promises
    sleep_seconds = 1
    Sys.sleep(sleep_seconds)

    # Limit number of attempts we make:
    attempts <- attempts + 1
    max_seconds <- 3*60
    max_attempts <- max_seconds / sleep_seconds
    if (attempts > max_attempts) {
      stop(paste0("Polling timeout at processing server (waited more than ", max_seconds, " seconds)!"))
    }

    # Poll once:
    out <- pygeoapiPollOnce(job_url)
    # For debugging, write the response to tmp:
    #writeLines(
    #  jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE), "/tmp/pygeoapi_polling_response.json"
    #)
    if (out$ok && out$state == "running") {
      # do nothing, continue polling...
    } else if (out$ok && out$state == "done") {
      if (is.null(out$result_url)) {
        # This should not happen, as non-existing links are handled already in the polling function:
        stop("No result link was returned from processing server, although it claims completion.")
      }
      json_link = out$result_url
      break
    } else if (!out$ok) {
      stop(out$error)
      break
    }
  }

  if (is.null(json_link) || json_link == "") {
    stop("No result link was returned from processing server.")
  }
  return(json_link)
}


# Poll once for current status:
pygeoapiPollOnce <- function(job_url) {
  # Request for job status...
  res <- request(job_url) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)

  # If job failed:
  if (res$status %in% c("failed", "error")) {
    err_msg <- res$message %||% res$error %||% "Unknown error at processing server"
    return(list(
      ok = FALSE,
      state = "error",
      error = err_msg,
      raw = res
    ))
  }

  # If job was successful, go for the result extraction...
  if (res$status == "successful") {
    json_link <- NULL
    for (link in res$links) {
      if (!is.null(link$type) &&
          link$type == "application/json") {
        json_link <- link$href
        break
      }
    }
    if (is.null(json_link)) {
      return(list(
        ok = FALSE,
        state = "error",
        error = "No result link was returned from processing server",
        raw = res
      ))
    }

    # Return JSON link if found
    return(list(
      ok = TRUE,
      state = "done",
      result_url = json_link,
      raw = res
    ))
  }

  # Not successful yet: Return to continue polling...
  return(list(
    ok = TRUE,
    state = "running",
    raw = res
  ))
}


# Fetch GeoJSON result from server:
pygeoapiFetchActualResult <- function(json_link) {

  # Make HTTP request
  geojson_extended <- request(json_link) |>
    req_perform() |>
    resp_body_json()
  return(geojson_extended)

  # Clean GeoJSON: Strip any additional properties that may confuse the parser:
  #geojson_clean <- list(
  #  type = "FeatureCollection",
  #  features = geojson_extended$features
  #)
  #return(geojson_clean)
}


# Converto to sf:
convert_to_sf_object <- function(geojson_obj) {

  # Parse GeoJSON to sf
  json_txt <- jsonlite::toJSON(geojson_obj, auto_unbox = TRUE)
  sf_obj <- sf::st_read(
    dsn = json_txt,
    quiet = TRUE
  )

  # Check if valid:
  if (is.null(sf_obj) || !inherits(sf_obj, "sf")) {
    stop("Invalid spatial result returned from processing server")
  }

  return(sf_obj)
}


