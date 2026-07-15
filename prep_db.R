# prep_db.R - build optimized local database for db-viz-hex
#
# usage:
#   Rscript prep_db.R              # uses latest release
#   Rscript prep_db.R v2026.04.06  # uses specific version
#
# re-run when a new calcofi4r release is available. idempotent:
# skips already-downloaded parquets, rebuilds materialized tables.

# remotes::install_github("calcofi/calcofi4r")
devtools::load_all("../calcofi4r")
librarian::shelf(
  calcofi / calcofi4r,
  DBI,
  duckdb,
  glue,
  here,
  sf,
  purrr,
  stringr,
  quiet = TRUE
)

# resolve version from command line or default to latest
args <- commandArgs(trailingOnly = TRUE)
db_version <- if (length(args) > 0) args[1] else "latest"
db_dir <- here("data")
hex_geo <- here("data/hex.geojson")

# minimal set of source tables needed to build the app database:
#   obs                — consolidated observations (realm bio|env); carries
#                        hex_id (H3 res-10) and is partitioned by dataset_key
#   sample_measurement — std_haul_factor + prop_sorted for bio std_tally
#   species/taxon/taxa_rank — species picker + taxonomic-children queries
keep_tables <- c("obs", "sample_measurement", "species", "taxon", "taxa_rank")

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
all_tables <- info$tables$name
missing    <- setdiff(keep_tables, all_tables)
if (length(missing) > 0)
  stop("release ", db_version, " is missing required tables: ",
       paste(missing, collapse = ", "))
cat("tables to load:", paste(keep_tables, collapse = ", "), "\n")

# step A: download parquets + create local DuckDB tables ----
# delete stale DuckDB to avoid schema conflicts from prior materialized views
db_version_resolved <- if (db_version == "latest") {
  trimws(readLines(
    "https://storage.googleapis.com/calcofi-db/ducklake/releases/latest.txt",
    warn = FALSE)[1])
} else db_version
db_file <- file.path(db_dir, paste0("calcofi_", db_version_resolved, ".duckdb"))
if (file.exists(db_file)) {
  cat("removing stale db:", db_file, "\n")
  file.remove(db_file)
}

con <- cc_get_db(
  version = db_version,
  local_data = TRUE,
  cache_dir = db_dir,
  tables = keep_tables,
  refresh = TRUE
)

# load extensions
dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# step B: bio_obs materialized table ----
# ichthyo (larvae/eggs + folded inverts) observations from the consolidated
# `obs` table: realm='bio', dataset_key='swfsc_ichthyo', measurement_type=
# 'abundance'. taxon_id (VARCHAR) maps 1:1 to species.species_id. std_tally
# reconstructs the standardized haul tally = abundance * std_haul_factor /
# prop_sorted, both pulled from sample_measurement (one row per net sample_key).
# hex_id (H3 res-10) is carried through; coarser resolutions are derived at
# query time via h3_cell_to_parent(). sorted by scientific_name, time_start.
cat("building bio_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE bio_obs AS
  SELECT
    o.life_stage        AS source,
    sp.scientific_name,
    sp.common_name,
    sp.species_id,
    sp.worms_id,
    o.measurement_value AS tally,
    o.measurement_value * shf.measurement_value
      / NULLIF(ps.measurement_value, 0)         AS std_tally,
    o.datetime          AS time_start,
    o.longitude,
    o.latitude,
    EXTRACT(QUARTER FROM o.datetime)::INTEGER   AS quarter,
    o.hex_id
  FROM obs o
  JOIN species sp
    ON TRY_CAST(o.taxon_id AS INTEGER) = sp.species_id
  LEFT JOIN sample_measurement shf
    ON o.sample_key = shf.sample_key AND shf.measurement_type = 'std_haul_factor'
  LEFT JOIN sample_measurement ps
    ON o.sample_key = ps.sample_key  AND ps.measurement_type = 'prop_sorted'
  WHERE o.realm            = 'bio'
    AND o.dataset_key      = 'swfsc_ichthyo'
    AND o.measurement_type = 'abundance'
    AND o.measurement_value IS NOT NULL
  ORDER BY sp.scientific_name, o.datetime"
)

bio_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM bio_obs")$n
cat("  bio_obs:", format(bio_n, big.mark = ","), "rows\n")

# step C: env_obs materialized table ----
# bottle observations from the consolidated `obs` table: realm='env',
# dataset_key='calcofi_bottle', restricted to the measurement types exposed in
# the app UI. hex_id (H3 res-10) is carried through; coarser resolutions derived
# at query time via h3_cell_to_parent(). column names mirror the prior schema
# (cast_id, lat_dec, lon_dec, datetime_utc, qty) so downstream code is unchanged.
cat("building env_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE env_obs AS
  SELECT
    o.sample_key        AS cast_id,
    o.datetime          AS datetime_utc,
    EXTRACT(QUARTER FROM o.datetime)::INTEGER AS quarter,
    o.latitude          AS lat_dec,
    o.longitude         AS lon_dec,
    o.depth_min_m       AS depth_m,
    o.measurement_type,
    o.measurement_value AS qty,
    o.hex_id
  FROM obs o
  WHERE o.realm       = 'env'
    AND o.dataset_key = 'calcofi_bottle'
    AND o.measurement_type IN (
      'temperature', 'salinity', 'oxygen_umol_kg', 'phosphate', 'silicate',
      'nitrite', 'nitrate', 'chlorophyll_a', 'phaeopigment', 'dynamic_height',
      'sigma_theta', 'pressure', 'par', 'ph', 'ammonia')
    AND o.measurement_value IS NOT NULL
  ORDER BY o.measurement_type, o.datetime"
)

env_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM env_obs")$n
cat("  env_obs:", format(env_n, big.mark = ","), "rows\n")

# step D: generate hex.geojson ----
# geometries for every H3 cell referenced by bio_obs / env_obs, at each
# resolution 1-10. finest (res-10) cells come straight from obs' hex_id; coarser
# cells are derived with h3_cell_to_parent() and rendered via h3_cell_to_boundary_wkt().
cat("generating hex.geojson...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE hex_base AS
  SELECT DISTINCT hex_id FROM bio_obs WHERE hex_id IS NOT NULL
  UNION
  SELECT DISTINCT hex_id FROM env_obs WHERE hex_id IS NOT NULL"
)
hex_list <- map(1:10, function(res) {
  dbGetQuery(
    con,
    glue(
      "
    WITH parents AS (
      SELECT h3_cell_to_parent(hex_id, {res}) AS parent FROM hex_base)
    SELECT
      HEX(parent)                          AS hex_id,
      {res}                                AS hex_res,
      COUNT(*)                             AS n_sites,
      h3_cell_to_boundary_wkt(HEX(parent)) AS hex_wkt
    FROM parents
    GROUP BY parent"
    )
  ) |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    st_set_geometry("geometry")
})
sf_hex <- bind_rows(hex_list)
st_write(sf_hex, hex_geo, delete_dsn = TRUE, quiet = TRUE)
cat("  hex.geojson:", nrow(sf_hex), "hexagons across 10 resolutions\n")

# step E: drop build-only objects (keep species/taxon/taxa_rank + bio_obs/env_obs) ----
cat("dropping build-only tables...\n")
drop_obj <- function(con, name) {
  # obs is a remote (partitioned) VIEW; sample_measurement is a local TABLE
  ok <- tryCatch(
    { dbExecute(con, glue("DROP TABLE IF EXISTS \"{name}\"")); TRUE },
    error = function(e) FALSE)
  if (!ok)
    tryCatch(dbExecute(con, glue("DROP VIEW IF EXISTS \"{name}\"")),
             error = function(e) NULL)
}
drop_obj(con, "obs")
drop_obj(con, "sample_measurement")
dbExecute(con, "DROP TABLE IF EXISTS hex_base")

# summary ----
final_tables <- dbListTables(con) |> sort()
cat("\nfinal tables:", paste(final_tables, collapse = ", "), "\n")
cat(
  "done. app database ready at:",
  file.path(db_dir, list.files(db_dir, "calcofi_.*\\.duckdb")),
  "\n"
)

dbDisconnect(con, shutdown = TRUE)
