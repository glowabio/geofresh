# function for connecting to the PostgreSQL database used from the server side

# set up custom connection pools for dev and prod database
get_pool <- function(db_server) {
  # define connection options for dev and prod server
  db_options <- switch(db_server,
    "dev" = list(host = "localhost", port = "5433", user = "shiny_usr", minSize = 1, idleTimeout = 180)
    # "prod" = list(host = "...", port = "...", minSize = 5, idleTimeout = 600),
  )

  # create connection pool
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
