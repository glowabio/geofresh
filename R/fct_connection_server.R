# function for connecting to the PostgreSQL database used from the server side

# set up custom connection pools for dev and prod database
get_db_options <- function(dev_or_prod="prod") {
  # define connection options for dev and prod server
  db_options <- switch(dev_or_prod,
    "dev" = list(host = "localhost", port = "5433", user = "shiny_usr", password = "...", minSize = 1, idleTimeout = 180),
    "prod" = list(host = "...", port = "...", user = "...", password = "...", minSize = 1, idleTimeout = 500)
  )
  return(db_options)
}


# set up custom connection pools for dev and prod database
get_pool <- function(dev_or_prod) {
  # create connection pool
  db_options <- get_db_options(dev_or_prod)
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "geofresh_data",
    application_name = "new_frontend_geofresh",
    host = db_options$host,
    port = db_options$port,
    user = db_options$user,
    password = db_options$password,
    minSize = db_options$minSize,
    idleTimeout = db_options$idleTimeout
  )
  return(pool)
}


# set up a single connection to the database, to be used to connect
# to database in separate thread, where the pool is not available:
#
# (Note: Currently not being used, as we do the asynchronous calls not
# directly to database, but via HTTP to pygeoapi. But we will switch
# snapping and computing upstream to async, so this will be needed).
connect_to_db <- function(dev_or_prod="prod") {
  db_options <- get_db_options(dev_or_prod)
  connection <- dbConnect(
    RPostgres::Postgres(),
    dbname = "geofresh_data",
    application_name = "new_frontend_async_geofresh",
    host = db_options$host,
    port = db_options$port,
    user = db_options$user,
    password = db_options$password
  )
  return(connection)
}

