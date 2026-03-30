# TODO:
# - [ ] use default species across all time and space: sardine (since motivated creation of CalCOFI; see https://en.wikipedia.org/wiki/CalCOFI)

# Install librarian if needed
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# Load libraries
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r, conductor, data.tree, DBI, dplyr, duckdb,
  geosphere, ggplot2, glue, here, highcharter, htmltools, htmlwidgets, httr2,
  jsonlite, leaflet, litedown, lubridate, mapgl, plotly, purrr, readr, sf, shiny,
  shinyWidgets, stringr, thematic, tibble, tidyr, zip,
  quiet = TRUE)

# variables ----
calcofi_db   <- "https://file.calcofi.io/data/calcofi.duckdb"
local_db     <- here("data/calcofi.duckdb")
local_db_srv <- "/share/public/data/calcofi.duckdb"
tmp_db       <- here("data/tmp.duckdb")
hex_geo      <- here("data/hex.geojson")
cache_dir    <- here("app/app_cache")
is_server    <- Sys.info()[["sysname"]] == "Linux"
use_local_db <- TRUE           # set to FALSE to use remote database, eg for ShinyApps.io
debug        <- interactive()  # set to TRUE for diagnostic console messages
is_tour_on   <- !debug         # turn off while debugging

is_remote_newer <- function(remote_url, local_path) {
  # remote modification time
  resp <- request(remote_url) |> req_method("HEAD") |> req_perform()
  remote_time <- resp_header(resp, "last-modified") |>
    as.POSIXct(format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")

  # local modification time
  if (!file.exists(local_path)) return(TRUE)
  local_time <- file.info(local_path)$mtime

  # compare
  return(remote_time > local_time)
}

if (use_local_db){
  if (!is_server && (!file.exists(local_db) | is_remote_newer(calcofi_db, local_db))){
    message("Downloading latest CalCOFI database...")
    download.file(calcofi_db, local_db)
  }
  if (is_server){
    con <- dbConnect(duckdb(read_only = T, dbdir = local_db_srv))
  } else{
    con <- dbConnect(duckdb(read_only = T, dbdir = local_db))
  }
} else {
  con <- dbConnect(duckdb(), dbdir = tmp_db)
  q <- dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  q <- dbExecute(con, glue("ATTACH IF NOT EXISTS '{calcofi_db}' AS calcofi; USE calcofi"))
}
q <- dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
q <- dbExecute(con, "INSTALL spatial; LOAD spatial;")
# dbListTables(con) |> sort()

# load hexagons
if (FALSE){ # !file.exists(hex_geo)
  hex_list <- list()

  hex_pfx <- "hex_h3res"
  hex_resolutions <- dbListFields(con, "site") |>
    str_subset(hex_pfx) |>
    str_remove(hex_pfx) |>
    as.integer()

  for (hex_res in hex_resolutions){ # hex_res = 1
    hex_fld <- glue("{hex_pfx}{hex_res}")

    hex_list[[hex_res]] <- tbl(con, "site") |>
      rename(
        hex_int = all_of(hex_fld)) |>
      group_by(hex_int) |>
      summarize(
        n_sites = n(),
        .groups = "drop") |>
      mutate(
        hex_id  = sql("HEX(hex_int)"),
        hex_res = !!hex_res) |>
      mutate(
        hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)")) |>
      select(hex_id, hex_res, n_sites, hex_wkt) |>
      # select(all_of(c("hex_id", "hex_res", "n_sites", "hex_wkt"))) |>
      collect() |>
      st_as_sf(
        wkt = "hex_wkt",
        crs = 4326) |>
      st_set_geometry("geometry")
  }
  # save hex_geo
  sf_hex <- bind_rows(hex_list)
  st_write(sf_hex, hex_geo, delete_dsn = T, quiet = T)
}
sf_hex <- st_read(hex_geo, quiet = TRUE)

# spatial layers registry ----
d_spatial_layers <- read_csv(
  here("../workflows/metadata/spatial_layers.csv"),
  show_col_types = FALSE)
# pmtiles_base_url <- "https://file.calcofi.io/_spatial"
pmtiles_base_url <- "https://storage.googleapis.com/calcofi-files-public/_spatial"

# gcloud storage buckets describe gs://calcofi-files-public --format="/Users/bbest/Github/CalCOFI/int-app/data/_cors_file.json"
# cd ~/Github/CalCOFI/int-app/data
# gcloud storage buckets update gs://calcofi-files-public --cors-file=_cors_file.json

# load functions ----
source(here("app/functions.R"))

# extract species names and date range ----
sp_names <- tbl(con, "species") |>
  left_join(
    tbl(con, "taxonomy") |>
      filter(authority == "worms"),
    by = join_by(worms_id == taxonID)) |>
  mutate(
    name = paste0(common_name, " (", tolower(taxonRank), ": ", scientific_name, ")")) |>
  pull(name)

larva_date_rng <- tbl(con, "tow") |>
  summarize(
    date_min = min(time_start, na.rm = T) |> as.Date(),
    date_max = max(time_start, na.rm = T) |> as.Date()) |>
  collect() |>
  as.vector()

bottle_date_rng <- tbl(con, "cast") |>
  summarize(
    date_min = min(date, na.rm = T),
    date_max = max(date, na.rm = T)) |>
  collect() |>
  as.vector()

min_max_date <- c(
  min(larva_date_rng[[1]], bottle_date_rng[[1]]),
  max(larva_date_rng[[2]], bottle_date_rng[[2]]) )

# global constants ----
default_sp_name         <- "Pacific sardine (pilchard) (species: Sardinops sagax)"
default_max_hours_diff  <- 6
default_max_meters_diff <- 2000

ts_res_choices <- list(
  "Year"          = "year",
  "Quarter"       = "quarter",
  "Year, Quarter" = "year_quarter")

env_var_choices <- list(
  "Temperature (ºC)" = "t_deg_c",
  "Salinity"         = "salnty",
  "Oxygen (µmol/kg)" = "oxy_umol_kg",
  "Phosphate (µmol/L)"        = "po4u_m",
  "Silicate (µmol/L)"         = "si_o3u_m",
  "Nitrite (µmol/L)"          = "no2u_m",
  "Nitrate (µmol/L)"          = "no3u_m")

env_stat_choices <- list(
  "Avg."      = "mean",
  "Max"       = "max",
  "Min"       = "min",
  "Std. Dev." = "sd")

# mapping variables ----
min_res     <- 1
max_res     <- 10
res_range   <- min_res:max_res
zoom_breaks <- seq(1, 13, length.out = length(res_range) + 1)
zoom_breaks[1] <- 0
zoom_breaks[length(zoom_breaks)] <- 22

# tour ----
tour <- Conductor$
  new(
    exitOnEsc          = T,
    keyboardNavigation = T)$
  step(
    title = "Welcome",
    text = "The app is initializing with a map comparing Pacific sardine larvae
    and temperature collected on CalCOFI cruises since 1949.<br><br>
    (To exit tour, use keyboard esc button)",
    buttons = list(
      list(
        action = "next",
        text   = "Next" )))$
  step(
    el    = "#sel_data",
    title = "Select Filters",
    text  = "You can change the species and environmental selection here, along with
    temporal and spatial constraints.",
    buttons = list(
      list(
        action = "back",
        text   = "Back"),
      list(
        action = "next",
        text   = "Done")))

# cleanup on exit ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
  duckdb_shutdown(duckdb())
  # rm(con)
})
