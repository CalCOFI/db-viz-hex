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

# tables to exclude (CTD is too large, not yet used in app)
exclude_tables <- c(
  "ctd_cast",
  "ctd_measurement",
  "ctd_summary",
  "ctd_wide",
  "dic_sample",
  "dic_measurement",
  "dic_summary",
  "cruise"
)

cat("fetching catalog for version:", db_version, "\n")
info <- cc_db_info(version = db_version)
all_tables <- info$tables$name
keep_tables <- setdiff(all_tables, exclude_tables)
cat("tables to load:", length(keep_tables), "of", length(all_tables), "\n")

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

# helper: generate H3 column expressions for resolutions 1-10
h3_cols <- function(lat_col, lon_col) {
  paste(
    sapply(1:10, function(r) {
      glue(
        "h3_latlng_to_cell({lat_col}, {lon_col}, {r})::BIGINT AS hex_h3res{r}"
      )
    }),
    collapse = ",\n    "
  )
}

# step B: H3-augmented tables (presorted by finest H3) ----
cat("building site_h3...\n")
dbExecute(
  con,
  glue(
    "
  CREATE OR REPLACE TABLE site_h3 AS
  SELECT *,
    {h3_cols('latitude', 'longitude')}
  FROM site
  ORDER BY hex_h3res10"
  )
)

cat("building casts_h3...\n")
dbExecute(
  con,
  glue(
    "
  CREATE OR REPLACE TABLE casts_h3 AS
  SELECT *,
    {h3_cols('lat_dec', 'lon_dec')}
  FROM casts
  ORDER BY hex_h3res10"
  )
)

# step C: bio_obs materialized table ----
# pre-joins ichthyo -> species -> net -> tow -> site_h3 (single query).
# ichthyo table now includes egg, larva, AND invert rows (consolidated).
# pre-computes std_tally, quarter, H3 indices.
# sorted by scientific_name, time_start for species filtering + temporal queries.
cat("building bio_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE bio_obs AS
  SELECT
    i.life_stage        AS source,
    sp.scientific_name,
    sp.common_name,
    sp.species_id,
    sp.worms_id,
    i.tally,
    n.std_haul_factor * i.tally / NULLIF(n.prop_sorted, 0) AS std_tally,
    t.time_start,
    sh.longitude,
    sh.latitude,
    EXTRACT(QUARTER FROM t.time_start)::INTEGER AS quarter,
    sh.hex_h3res1, sh.hex_h3res2, sh.hex_h3res3, sh.hex_h3res4, sh.hex_h3res5,
    sh.hex_h3res6, sh.hex_h3res7, sh.hex_h3res8, sh.hex_h3res9, sh.hex_h3res10
  FROM ichthyo i
  JOIN species sp ON i.species_id = sp.species_id
  JOIN net n      ON i.net_uuid   = n.net_uuid
  JOIN tow t      ON n.tow_uuid   = t.tow_uuid
  JOIN site_h3 sh ON t.site_uuid  = sh.site_uuid
  WHERE i.tally IS NOT NULL
    AND i.measurement_type IS NULL
  ORDER BY sp.scientific_name, t.time_start"
)

bio_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM bio_obs")$n
cat("  bio_obs:", format(bio_n, big.mark = ","), "rows\n")

# step D: env_obs materialized table ----
# pre-joins casts_h3 -> bottle -> bottle_measurement for 15 common types.
# pre-computes quarter. sorted by measurement_type, datetime_utc.
cat("building env_obs...\n")
dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE env_obs AS
  SELECT
    c.cast_id,
    c.datetime_utc,
    EXTRACT(QUARTER FROM c.datetime_utc)::INTEGER AS quarter,
    c.lat_dec,
    c.lon_dec,
    b.bottle_id,
    b.depth_m,
    bm.measurement_type,
    bm.measurement_value AS qty,
    c.hex_h3res1, c.hex_h3res2, c.hex_h3res3, c.hex_h3res4, c.hex_h3res5,
    c.hex_h3res6, c.hex_h3res7, c.hex_h3res8, c.hex_h3res9, c.hex_h3res10
  FROM casts_h3 c
  JOIN bottle b              ON c.cast_id   = b.cast_id
  JOIN bottle_measurement bm ON b.bottle_id = bm.bottle_id
  WHERE bm.measurement_type IN (
    'temperature', 'salinity', 'oxygen_umol_kg', 'phosphate', 'silicate',
    'nitrite', 'nitrate', 'chlorophyll_a', 'phaeopigment', 'dynamic_height',
    'sigma_theta', 'pressure', 'par', 'ph', 'ammonia')
    AND bm.measurement_value IS NOT NULL
  ORDER BY bm.measurement_type, c.datetime_utc"
)

env_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM env_obs")$n
cat("  env_obs:", format(env_n, big.mark = ","), "rows\n")

# step E: drop intermediate source tables ----
cat("dropping intermediate tables...\n")
drop_tables <- c(
  "ichthyo",
  "net",
  "tow",
  "site",
  "casts",
  "bottle",
  "bottle_measurement",
  "cast_condition",
  "segment",
  "grid",
  "cruise",
  "ship",
  "lookup",
  "dataset",
  "_spatial",
  "_spatial_attr"
)
for (tbl in drop_tables) {
  dbExecute(con, glue("DROP TABLE IF EXISTS \"{tbl}\""))
}

# step F: generate hex.geojson ----
cat("generating hex.geojson...\n")
hex_pfx <- "hex_h3res"
hex_list <- map(1:10, function(res) {
  hex_fld <- glue("{hex_pfx}{res}")
  dbGetQuery(
    con,
    glue(
      "
    SELECT
      HEX({hex_fld})                                AS hex_id,
      {res}                                          AS hex_res,
      COUNT(*)                                       AS n_sites,
      h3_cell_to_boundary_wkt(HEX({hex_fld}))       AS hex_wkt
    FROM (SELECT DISTINCT {hex_fld} FROM site_h3 WHERE {hex_fld} IS NOT NULL)
    GROUP BY {hex_fld}"
    )
  ) |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    st_set_geometry("geometry")
})
sf_hex <- bind_rows(hex_list)
st_write(sf_hex, hex_geo, delete_dsn = TRUE, quiet = TRUE)
cat("  hex.geojson:", nrow(sf_hex), "hexagons across 10 resolutions\n")

# summary ----
final_tables <- dbListTables(con) |> sort()
cat("\nfinal tables:", paste(final_tables, collapse = ", "), "\n")
cat(
  "done. app database ready at:",
  file.path(db_dir, list.files(db_dir, "calcofi_.*\\.duckdb")),
  "\n"
)

dbDisconnect(con, shutdown = TRUE)
