# Helper functions to query database
# Per-session dataset manager for a user points table in Postgres

library(DBI)
library(pool)
library(uuid)
library(shiny)

# ---- helper: run code with a checked-out pool connection ----
with_pool_connection <- function(pool, fun) {
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn), add = TRUE)
  fun(conn)
}

# ---- base empty table ----
create_empty_points_table <- function(conn, table_id) {
  tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)
  DBI::dbExecute(conn, paste0(
    "CREATE TABLE ", tbl_q, " (
       id text PRIMARY KEY,
       latitude double precision,
       longitude double precision
     )"
  ))
}

# ---- ensure extra columns exist  ----
ensure_points_schema <- function(conn, table_id) {
  tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)

  DBI::dbExecute(conn, paste0(
    "ALTER TABLE ", tbl_q, "
      ADD COLUMN IF NOT EXISTS geom_orig geometry(POINT,4326),
      ADD COLUMN IF NOT EXISTS geom_hint geometry(POINT,4326),
      ADD COLUMN IF NOT EXISTS geom_snap geometry(POINT,4326),
      ADD COLUMN IF NOT EXISTS snap_state text DEFAULT 'needs_snap',
      ADD COLUMN IF NOT EXISTS snap_fail_reason text"
  ))

  invisible(TRUE)
}

# ---- read points from DB for editor/map/table (lat/lon + optional snap) ----
read_points_db <- function(conn, table_id) {
  tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)

  DBI::dbGetQuery(conn, paste0(
    "SELECT
       id,
       latitude,
       longitude,
       CASE WHEN geom_snap IS NULL THEN NULL ELSE round(ST_Y(geom_snap)::numeric, 6) END AS latitude_snap,
       CASE WHEN geom_snap IS NULL THEN NULL ELSE round(ST_X(geom_snap)::numeric, 6) END AS longitude_snap
     FROM ", tbl_q, "
     ORDER BY id"
  ))
}

# Write points into DB
write_points_base_db <- function(conn, table_id, df_base) {
  ensure_points_schema(conn, table_id)
  tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)

  DBI::dbExecute(conn, paste0("TRUNCATE TABLE ", tbl_q))

  DBI::dbWriteTable(conn, table_id, df_base, append = TRUE, row.names = FALSE)

  DBI::dbExecute(conn, paste0(
    "UPDATE ", tbl_q, "
     SET geom_orig = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326),
         geom_hint = NULL,
         geom_snap = NULL,
         snap_state = 'needs_snap',
         snap_fail_reason = NULL"
  ))

  DBI::dbExecute(conn, paste0("ANALYZE ", tbl_q))
  invisible(TRUE)
}


# ---- dataset manager ----
dataset_manager <- function(pool, session, schema = "shiny_user", prefix = "points_") {

  # ---- internal state stored in session$userData ----
  if (is.null(session$userData$points_table_name)) session$userData$points_table_name <- NULL
  if (is.null(session$userData$points_version))    session$userData$points_version    <- 0L

  # NEW: reactive trigger (this is what Shiny can depend on)
  if (is.null(session$userData$points_version_rv)) {
    session$userData$points_version_rv <- shiny::reactiveVal(0L)
  }

  ensure <- function() {
    tn <- session$userData$points_table_name
    if (!is.null(tn)) return(tn)

    uuid_str   <- uuid::UUIDgenerate(use.time = TRUE)
    table_name <- paste0(prefix, uuid_str)
    table_id   <- DBI::Id(schema = schema, table = table_name)

    with_pool_connection(pool, function(conn) {
      create_empty_points_table(conn, table_id)
      ensure_points_schema(conn, table_id)
    })

    session$userData$points_table_name <- table_name

    session$onSessionEnded(function() {
      with_pool_connection(pool, function(conn) {
        DBI::dbRemoveTable(conn, table_id, fail_if_missing = FALSE)
      })
    })

    table_name
  }

  get_table_name <- function() ensure()
  get_table_id   <- function() DBI::Id(schema = schema, table = ensure())

  # ---- versioning: invalidate reactive readers after DB writes ----
  bump_version <- function() {
    # keep your plain integer counter (optional, but fine)
    session$userData$points_version <- as.integer(session$userData$points_version) + 1L

    # THIS is the reactive invalidation trigger
    v <- session$userData$points_version_rv()
    session$userData$points_version_rv(v + 1L)

    invisible(session$userData$points_version)
  }

  # reactive dependency (use inside reactive({ ... }))
  version <- function() {
    session$userData$points_version_rv()
  }

  table_name_reactive <- shiny::reactive({ ensure() })

  read_points <- function() {
    ensure()
    with_pool_connection(pool, function(conn) {
      read_points_db(conn, get_table_id())
    })
  }

  list(
    ensure         = ensure,
    get_table_name = get_table_name,
    table_name     = table_name_reactive,
    table_id       = get_table_id,
    bump_version   = bump_version,
    version        = version,
    read_points    = read_points
  )
}
