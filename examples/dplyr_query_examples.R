# install.packages("dplyr")
# install.packages("dbplyr")

library(pool)
library(DBI)
library(dplyr)
library(dbplyr)

pool <- dbPool(
  RPostgres::Postgres(),
  dbname = "geofresh_data",
  host = "localhost",
  port = "5433",
  user = "shiny_usr"
)

# https://shiny.rstudio.com/articles/overview.html
# https://shiny.rstudio.com/articles/pool-dplyr.html


# list all the tables of the database
dbListTables(pool)

# get the first 5 rows of a table using dplyr:

# stream_segments
pool %>%
  tbl("stream_segments") %>%
  head(5)

# stream_nodes
pool %>%
  tbl("stream_nodes") %>%
  head(5)

# sub_catchments
pool %>%
  tbl("sub_catchments") %>%
  head(5)

# regional_units
pool %>%
  tbl("regional_units") %>%
  head(5)

# stats_climate
pool %>%
  tbl("stats_climate") %>%
  head(5)

# stats_soil
pool %>%
  tbl("stats_soil") %>%
  head(5)

# stats_topography
pool %>%
  tbl("stats_topography") %>%
  head(5)

# stats_landuse
pool %>%
  tbl("stats_topography") %>%
  head(5)

# same query using DBI:
dbGetQuery(pool, "SELECT * FROM stream_segments LIMIT 5;")

#------------------------------------------------------------
# set the table to query (--> input from Shiny app)
# set the number of lines to fetch (--> input from Shiny app)
# and create query using sqlInterpolate()
# to prevent SQL injection

table_id <- Id(schema = "public", table = "stream_segments")
lines_str <- "5"

sql <- sqlInterpolate(pool,
  "SELECT * FROM ?table LIMIT ?numlines;",
  table = dbQuoteIdentifier(pool, table_id),
  numlines = lines_str
)
dbGetQuery(pool, sql)

# same query using dplyr:
table <- "stream_segments"
lines_int <- 5

pool %>%
  tbl(table) %>%
  head(lines_int) %>%
  collect()

# or including the schema:
pool %>%
  tbl(in_schema("public", table)) %>%
  head(lines_int) %>%
  collect()

#--------------------------------------------------------------
# filter by sub-catchment ID
table <- "stream_segments"
id <- "450409563"
pool %>%
  tbl(table) %>%
  filter(subc_id == id)

# filter by regional unit ID and get first 5 lines
table <- "stream_segments"
reg <- "3"
pool %>%
  tbl(table) %>%
  filter(reg_id == reg) %>%
  head(5)

# filter by multiple sub-catchment ids
ids <- c("511475167", "511475852", "511476223", "511476224", "511476225",
"511476934", "511477296", "511477297", "511477298", "511477645", "511477646",
"511477647", "511477648", "511477973", "511477974", "511478309", "511478310",
"511478311", "511478312", "511478313", "511478314", "511478975", "511478976",
"511478977", "511478978", "511479303", "511479304", "511479305", "511479306",
"511479642", "511479643", "511479644", "511479977", "511479978", "511479979",
"511479980", "511480615")
reg <- 59
table <- "stats_climate"

clim <- pool %>%
  tbl(table) %>%
  filter(reg_id == reg) %>%
  filter(subc_id %in% ids) %>%
  collect()
#-----------------------------------------------------------------

# https://dbplyr.tidyverse.org/articles/dbplyr.html

# set regional unit to query only single partition table
reg <- 1

# reference the database table:
stream_segments <- tbl(pool, in_schema("public", "stream_segments"))

# select columns
stream_segments %>%
  filter(reg_id == reg) %>%
  select(subc_id, length, flow_accum:strahler, reg_id)

# filter lines
stream_segments %>%
  filter(reg_id == reg) %>%
  select(subc_id, flow_accum, reg_id) %>%
  filter(flow_accum > 50)

# summarise
# e.g. get sum of river segments length per strahler order
sum_length <- stream_segments %>%
  filter(reg_id == reg) %>%
  group_by(strahler) %>%
  summarise(length = sum(length))

# show the generated SQL query
sum_length %>% show_query()

# use collect() to run the full query and
# pull down the data into a local dataframe
sum_length_df <- sum_length %>% collect()

#--------------------------------------------------------------------------

# https://dplyr.tidyverse.org/articles/dplyr.html

## dplyr single table verbs:

# Rows:
# - filter() chooses rows based on column values.
# - slice() chooses rows based on location.
# - arrange() changes the order of the rows.
# Columns:
# - select() changes whether or not a column is included.
# - rename() changes the name of columns.
# - mutate() changes the values of columns and creates new columns.
# - relocate() changes the order of the columns.
# Groups of rows:
# - summarise() collapses a group into a single row.

# see also:

# Grouped data (group_by)
# https://dplyr.tidyverse.org/articles/grouping.html

# Two table verbs (joins)
# https://dplyr.tidyverse.org/articles/two-table.html
