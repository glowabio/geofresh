# function for connecting to the PostgreSQL database used from the server side

# set up custom connection pools for dev and prod database
get_db_options <- function(dev_or_prod="prod") {
  # define connection options for dev and prod server
  db_options <- switch(dev_or_prod,
    "dev" = list(host = "localhost", port = "5433", user = "shiny_usr", minSize = 1, idleTimeout = 180)
    # "prod" = list(host = "...", port = "...", minSize = 5, idleTimeout = 600),
  )
  return(db_options)
}


# set up custom connection pools for dev and prod database
get_pool <- function(db_server) {
  # create connection pool
  db_options <- get_db_options(db_server)
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "geofresh_data",
    host = db_options$host,
    port = db_options$port,
    user = db_options$user,
    minSize = db_options$minSize,
    idleTimeout = db_options$idleTimeout
  )
  return(pool)
}

# set up a single connection to the database, to be used to connect to db in other thread:
connect_to_db <- function(db_server="prod") {
  db_options <- get_db_options(db_server)
  connection <- dbConnect(
    RPostgres::Postgres(),
    dbname = "geofresh_data",
    host = db_options$host,
    port = db_options$port,
    user = db_options$user
  )
  return(connection)
}
