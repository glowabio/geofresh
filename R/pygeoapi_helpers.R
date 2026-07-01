
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
  sf_obj    <- pygeoapiFetchActualResult(json_link)
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
  sf_obj    <- pygeoapiFetchActualResult(json_link)
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
    Sys.sleep(1)
    attempts <- attempts + 1
    if (attempts > 60) {
      stop("Polling timeout")
    }
    json_link <- pygeoapiPollOnce(job_url)
    # todo better handling here merret
    if (!isFALSE(json_link)) {
      break
    }
  }
  return(json_link)
}


# Poll once for current status:
pygeoapiPollOnce <- function(job_url) {
  # Request for job status...
  res <- request(job_url) |>
    req_perform() |>
    resp_body_json()

  # If job failed:
  if (res$status %in% c("failed", "error")) {
    #stop("Job failed")
    err_msg <- res$message %||% res$error %||% "Unknown backend error"
    #stop(sprintf("Upstream processing failed: %s", err_msg))
    showNotification(sprintf("Upstream processing failed: %s", err_msg), type="error")
    # TODO return structured thing here merret
    #return(list(
    #  ok = FALSE,
    #  error = err_msg,
    #  raw = res
    #))
    return(FALSE)
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
      stop("No JSON result link found")
    }
    return(json_link)
  }

  # No result yet: Return FALSE to continue polling...
  return(FALSE)
}


# Fetch the actual result from server as GeoJSON
# and parse to spatial object:
pygeoapiFetchActualResult <- function(json_link) {

  # Make HTTP request
  geojson_extended <- request(json_link) |>
    req_perform() |>
    resp_body_json()

  # Clean GeoJSON: Strip any additional properties that may confuse the parser:
  #geojson_clean <- list(
  #  type = "FeatureCollection",
  #  features = geojson_extended$features
  #)

  # Parse cleaned GeoJSON to sf
  #json_txt <- jsonlite::toJSON(geojson_clean, auto_unbox = TRUE)
  json_txt <- jsonlite::toJSON(geojson_extended, auto_unbox = TRUE)
  sf_obj <- sf::st_read(
    dsn = json_txt,
    quiet = TRUE
  )
  return(sf_obj)
}


