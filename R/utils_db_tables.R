# Utilities for database connection

library(pool)
library(DBI)
library(dplyr)
library(dbplyr)

# set up database connection pool ("dev" or "prod")
# pool <- get_pool("dev")
pool <- get_pool("prod")

# set up references to the database tables

## hydropgrahy tables
# stream_segments
stream_segments_tbl <- tbl(pool, in_schema("hydro", "stream_segments"))

# stream_nodes (temporarily not available)
# stream_nodes_tbl <- tbl(pool, in_schema("hydro", "stream_nodes"))

# sub_catchments
sub_catchments_tbl <- tbl(pool, in_schema("hydro", "sub_catchments"))

# regional_units
regional_units_tbl <- tbl(pool, in_schema("hydro", "regional_units"))

## environmental variables tables
# stats_topo
stats_topo_tbl <- tbl(pool, in_schema("hydro", "stats_topo"))

# stats_climate
stats_clim_tbl <- tbl(pool, in_schema("hydro", "stats_climate"))

# stats_soil
stats_soil_tbl <- tbl(pool, in_schema("hydro", "stats_soil"))

# stats_landuse
stats_land_tbl <- tbl(pool, in_schema("hydro", "stats_landuse"))
