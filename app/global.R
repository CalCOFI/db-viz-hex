# TODO:
# - [ ] use default species across all time and space: sardine (since motivated creation of CalCOFI; see https://en.wikipedia.org/wiki/CalCOFI)

# Install librarian if needed
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}

# Load libraries
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r, conductor, DBI, dplyr, duckdb, geosphere,
  ggplot2, glue, here,highcharter, htmltools, htmlwidgets, leaflet, litedown,
  lubridate, mapgl, plotly, purrr, readr, sf, shiny, shinyWidgets, stringr,
  thematic, tibble, tidyr,
  quiet = TRUE)

# variables ----
debug        <- interactive() # set to TRUE for diagnostic console messages
is_tour_on   <- !debug  # turn off while debugging
calcofi_db   <- "https://file.calcofi.io/data/calcofi.duckdb"
use_local_db <- TRUE # set to FALSE to use remote database, eg for ShinyApps.io
hex_geo      <- here("data/hex.geojson")

# field name issues? check these name remapping files:
# bottle, cast: https://github.com/CalCOFI/calcofi4db/blob/main/inst/ingest/calcofi.org/bottle-database/flds_rename.csv
# ...: https://github.com/CalCOFI/calcofi4db/blob/main/inst/ingest/swfsc.noaa.gov/calcofi-db/flds_redefine.csv

if (use_local_db){
  local_db <- here("data/calcofi.duckdb")
  if (!file.exists(local_db))
    download.file(calcofi_db, local_db)
  con <- dbConnect(duckdb(read_only = T, dbdir = local_db))
} else {
  tmp_dk <- here("data/tmp.duckdb")
  con <- dbConnect(duckdb(), dbdir = tmp_dk)

  # load DuckDB extensions with error handling
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  dbExecute(con, glue("ATTACH IF NOT EXISTS '{calcofi_db}' AS calcofi; USE calcofi"))
}
dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")

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

# load functions ----
source(here("app/functions.R"))

# extract species names and date range ----
sp_names <- tbl(con, "species") |>
  mutate(name = paste0(common_name, " (", scientific_name, ")")) |>
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
default_sp_name <- "Pacific sardine (pilchard) (Sardinops sagax)"

ts_res_choices <- list(
  "Year"          = "year",
  "Quarter"       = "quarter",
  "Year, Quarter" = "year_quarter")

env_var_choices <- list(
  "Temperature (ºC)" = "t_deg_c",
  "Salinity"         = "salnty",
  "Oxygen (µmol/kg)" = "oxy_umol_kg")

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
