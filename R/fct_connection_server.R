# function for connecting to the PostgreSQL database used from the server side

# set up custom connection pools for dev and prod database
get_pool <- function(dev_or_prod) {
  # define connection options for dev and prod server
  db_options <- switch(dev_or_prod,
    "dev" = list(host = "localhost", port = "5433", user = "shiny_usr", password = "...", minSize = 1, idleTimeout = 180),
    "prod" = list(host = "...", port = "...", user = "...", password = "...", minSize = 1, idleTimeout = 500)
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
