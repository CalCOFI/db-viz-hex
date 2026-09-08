# h3t tile-source companions to the sf-based map builders in functions.R.
# these are only used when USE_H3T is on (set via the H3T_USE env var or via
# the app UI later). the sf-based path in functions.R remains the fallback.

# ---------------------------------------------------------------------- paths

# default base URL for the h3t API. override with env var for deploys.
h3t_base_url <- function() {
  Sys.getenv("H3T_BASE_URL", "http://127.0.0.1:8889/h3t")
}

# ------------------------------------------------------------------- sql build

# Both builders emit `h3_cell_to_parent(hex_id, {{res}})` with the literal
# placeholder `{{res}}`. The h3t API substitutes `{{res}}` with the tile's
# effective H3 resolution (derived from zoom) before parsing, so one cached SQL
# string serves every zoom level. The parent cell (UBIGINT, < 2^63) casts to
# BIGINT — the API's cell_id contract is unchanged. Requires `LOAD h3`.
HEX_COL <- DBI::SQL("h3_cell_to_parent(hex_id, {{res}})")

# Point-in-polygon clause for a spatial filter (drawn polygon or grid zones),
# in the same shape the dbplyr path uses in server.R. The h3t service runs
# DuckDB with `spatial` loaded, so ST_Within is available there too — verified
# against /h3t/stats. lon/lat columns differ between the two tables.
poly_clause <- function(poly_wkt, lon_col, lat_col) {
  if (is.null(poly_wkt) || !nzchar(poly_wkt)) return(NULL)
  glue::glue_sql(
    "ST_Within(ST_Point({DBI::SQL(lon_col)}, {DBI::SQL(lat_col)}),
                ST_GeomFromText({poly_wkt}))",
    .con = DBI::ANSI())
}

# Sample-spatial membership clause: the h3t-path counterpart to the
# sample_spatial inner_join functions.R::prep_sp_poly()/prep_env_poly() use to
# aggregate and functions.R::sample_spatial_keys()/server.R's classic-path
# submit handler use to filter. For spatial_layers.csv layers with no polygon
# geometry loaded (global.R::sample_spatial_layers), so poly_clause()'s
# ST_Within is not an option -- queries the SAME sample_spatial table this
# h3t service's DuckDB already has loaded, keyed by sample_key (bio_obs) or
# cast_id (env_obs; see sample_key_col).
sample_spatial_clause <- function(spatial_layer, spatial_names, sample_key_col) {
  if (is.null(spatial_layer) || is.null(spatial_names) || length(spatial_names) == 0)
    return(NULL)
  glue::glue_sql(
    "{DBI::SQL(sample_key_col)} IN (
       SELECT sample_key FROM sample_spatial
        WHERE layer = {spatial_layer} AND spatial_name IN ({spatial_names*}))",
    .con = DBI::ANSI())
}

#' Species tile SELECT: projects (cell_id, value, n) from bio_obs.
#'
#' Takes the ALREADY-RESOLVED `taxon_key`s from \code{resolve_sp_ids()} rather
#' than resolving taxa itself. It used to take one display name and walk the
#' hierarchy in a recursive CTE, which meant three ways to disagree with the
#' rest of the app, all of them silent:
#'   * only the FIRST selected taxon reached the tiles (glue_sql interpolates a
#'     length-n vector into `scientific_name = …`, not an IN list);
#'   * ITIS-only taxa (seabirds, marine mammals) have no WoRMS id, so the CTE
#'     matched nothing for them while `get_sp()` matched them by name;
#'   * the dataset checkboxes and the spatial filter were not applied at all.
#' Filtering on exactly what `get_sp()` filtered on makes map and tables agree
#' by construction. Since both now filter `taxon_key`, there is one key space
#' rather than a worms_id set plus a scientific_name set to keep in agreement.
#'
#' \code{cpue_unit}, added 2026-09-08, is the "Standardized as" pick
#' (\code{rx$cpue_chosen}). Before this parameter existed, this SQL had no
#' idea the control existed at all -- it always averaged \code{std_tally}
#' across every \code{cpue_unit} present for the taxon, so switching
#' "Standardized as" relabeled the legend (a separate, correctly-reactive
#' piece of UI) while the tiles themselves, and the color scale built from
#' them, never changed at all (bug report 2026-09-08: "why does nothing
#' change visually when you change standardized as"). The classic/non-h3t
#' path never had this bug -- it filters via \code{filter_cpue_unit(df_sp,
#' cpue_chosen)} in server.R before ever reaching a map builder -- only the
#' h3t path re-queries independently of \code{df_sp} and so needs its own
#' filter here.
build_sp_sql <- function(taxon_keys, qtr, date_range,
                         datasets = NULL, poly_wkt = NULL,
                         spatial_layer = NULL, spatial_names = NULL,
                         cpue_unit = NULL) {
  taxa <- if (length(taxon_keys) > 0) {
    glue::glue_sql("taxon_key IN ({taxon_keys*})", .con = DBI::ANSI())
  } else {
    # nothing resolves; an empty IN () is a SQL error, so say so explicitly
    DBI::SQL("FALSE")
  }

  where <- c(
    "std_tally IS NOT NULL",
    as.character(taxa),
    as.character(glue::glue_sql("quarter IN ({qtr*})", .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "time_start BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}",
      .con = DBI::ANSI())),
    if (!is.null(datasets) && length(datasets) > 0)
      as.character(glue::glue_sql("dataset_key IN ({datasets*})", .con = DBI::ANSI())),
    if (!is.null(cpue_unit) && !is.na(cpue_unit))
      as.character(glue::glue_sql("cpue_unit = {cpue_unit}", .con = DBI::ANSI())),
    as.character(poly_clause(poly_wkt, "longitude", "latitude")),
    as.character(sample_spatial_clause(spatial_layer, spatial_names, "sample_key")))

  as.character(glue::glue_sql(
    "SELECT {hex_col} AS cell_id, AVG(std_tally) AS value, COUNT(*) AS n
       FROM bio_obs
      WHERE {DBI::SQL(paste(where, collapse = ' AND '))}
      GROUP BY 1",
    .con = DBI::ANSI(), hex_col = HEX_COL))
}

# env SELECT: projects (cell_id, value, n) from env_obs.
build_env_sql <- function(measurement_type, qtr, date_range, depth_range,
                          stat = c("mean", "median", "min", "max", "sd"),
                          poly_wkt = NULL,
                          spatial_layer = NULL, spatial_names = NULL) {
  stat <- match.arg(stat)
  agg <- switch(stat,
    mean   = "AVG(qty)",
    median = "MEDIAN(qty)",
    min    = "MIN(qty)",
    max    = "MAX(qty)",
    sd     = "STDDEV_SAMP(qty)"
  )

  where <- c(
    "qty IS NOT NULL AND NOT isnan(qty) AND isfinite(qty)",
    as.character(glue::glue_sql("measurement_type = {measurement_type}", .con = DBI::ANSI())),
    as.character(glue::glue_sql("quarter IN ({qtr*})", .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "datetime_utc BETWEEN {as.character(date_range[1])} AND {as.character(date_range[2])}",
      .con = DBI::ANSI())),
    as.character(glue::glue_sql(
      "depth_m BETWEEN {depth_range[1]} AND {depth_range[2]}", .con = DBI::ANSI())),
    as.character(poly_clause(poly_wkt, "lon_dec", "lat_dec")),
    # env_obs carries the sample grain as `cast_id`, not `sample_key` (see
    # functions.R::prep_env_poly)
    as.character(sample_spatial_clause(spatial_layer, spatial_names, "cast_id")))

  as.character(glue::glue_sql(
    "SELECT {hex_col} AS cell_id, {DBI::SQL(agg)} AS value, COUNT(*) AS n
       FROM env_obs
      WHERE {DBI::SQL(paste(where, collapse = ' AND '))}
      GROUP BY 1",
    .con = DBI::ANSI(), hex_col = HEX_COL))
}

# -------------------------------------------------------- URL / stats helpers

h3t_b64 <- function(sql) {
  # URL-safe base64 (RFC 4648 §5): swap + → -, / → _, strip padding '='
  raw <- charToRaw(sql)
  b64 <- base64enc::base64encode(raw)
  b64 <- chartr("+/", "-_", b64)
  gsub("=+$", "", b64)
}

h3t_tile_url <- function(sql, release = "", base = h3t_base_url()) {
  q <- h3t_b64(sql)
  qs <- paste0("q=", q)
  if (nzchar(release)) qs <- paste0(qs, "&release=", utils::URLencode(release, reserved = TRUE))
  # replace http(s) prefix with h3tiles:// so MapLibre dispatches to the custom protocol
  host_path <- sub("^https?://", "", base)
  sprintf("h3tiles://%s/{z}/{x}/{y}.h3t?%s", host_path, qs)
}

# pull min/max (and p02/p98) across the whole user SQL via /h3t/stats
fetch_h3t_stats <- function(sql, release = "", base = h3t_base_url(),
                            timeout_s = 5) {
  q <- h3t_b64(sql)
  url <- paste0(
    sub("/+$", "", base), "/stats?q=", q,
    if (nzchar(release)) paste0("&release=", utils::URLencode(release, reserved = TRUE)) else ""
  )
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) >= 400) return(NULL)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# build a single mapgl color-scale (matching the existing interpolate_palette
# shape) from stats $min/$max and a palette function.
build_h3t_scale <- function(stats, palette = \(n) hcl.colors(n, "Viridis"),
                            n_stops = 5L, column = "value") {
  if (is.null(stats) || is.null(stats$min) || is.null(stats$max) ||
      !is.finite(stats$min) || !is.finite(stats$max) || stats$min == stats$max) {
    # degenerate: one flat color, no interpolation
    cols <- palette(2)
    return(list(
      breaks     = c(stats$min %||% 0, stats$max %||% 1),
      colors     = cols,
      expression = cols[1]
    ))
  }
  # clamp to p02..p98 when available to avoid outlier domination
  lo <- if (!is.null(stats$p02) && is.finite(stats$p02)) stats$p02 else stats$min
  hi <- if (!is.null(stats$p98) && is.finite(stats$p98)) stats$p98 else stats$max
  if (lo >= hi) { lo <- stats$min; hi <- stats$max }
  breaks <- seq(lo, hi, length.out = n_stops)
  colors <- palette(n_stops)
  list(
    breaks     = breaks,
    colors     = colors,
    expression = mapgl::interpolate(column = column, values = breaks, stops = colors)
  )
}

# ------------------------------------------------------------- map builders

# Default startup extent (2026-09-08 root-cause, after 3 wrong guesses via
# fit_bounds(bbox=...)): fit_bounds() has to ask "what zoom makes this
# lon/lat box exactly fill the map panel", and the map panel's actual pixel
# size depends on the browser window, whether the sidebar is open, the
# monitor -- so the SAME bbox can fit to a DIFFERENT zoom on a different
# screen ("does it depend on screensize" -- yes, this is exactly why: three
# different bboxes were tried here and none reliably landed on "100 km").
# The scale bar itself, though, only depends on ZOOM and LATITUDE, not on
# screen/container size at all (it measures the ground distance spanned by
# a fixed 100px reference at the map's current center) -- so pin the zoom
# directly instead of reverse-engineering a bbox that happens to fit to the
# right zoom on one particular screen. mapgl's scale control snaps to a
# "nice number" ladder (50/100/200/...) and reads "100" for any raw value
# in [100, 200) km.
#
# Two things went into this number, not one -- the first pass (zoom 6.53)
# used ONLY the first and still came back "50", one bucket too low:
#   1. The standard Web Mercator meters-per-pixel formula (256px tiles):
#      156543.03392 * cos(lat) / 2^zoom. Solved for a target of 140 km --
#      the geometric center of the "100" bucket, for margin against either
#      edge -- this alone gives zoom 6.53.
#   2. Mapbox GL / MapLibre GL JS use a 512px internal tile size, and their
#      public `zoom` is consequently ONE LESS than the "standard" 256px
#      zoom for the same visual scale (confirmed against a real MapLibre
#      session, 2026-09-08 -- zoom 6.53 read "50", not "100"; also a long-
#      documented mapbox-gl-js quirk, e.g. github.com/mapbox/mapbox-gl-js
#      issues #685 and #4837: "zoom levels reported in GL are 1 less than
#      the same data source rendered as raster tiles"). So the zoom actually
#      passed to maplibre() needs to be 1 LOWER than the standard-formula
#      answer: 6.53 - 1 = 5.53.
# This is a hardcoded ZOOM, so unlike a bbox it reads the same "100 km" on
# every screen size, every time -- it does not depend on the browser
# window, sidebar state, or monitor.
DEFAULT_MAP_CENTER <- c(-120, 34)  # CalCOFI core grid: Pt. Conception to San Diego
DEFAULT_MAP_ZOOM   <- 5.53

#' @param view optional list(center = c(lon, lat), zoom = n) -- the LAST
#'   KNOWN view from the client (server.R caches every map_before_view
#'   moveend into rx$last_map_view). When present, the widget is constructed
#'   AT that exact view, so a full widget rebuild (env stat, dark mode,
#'   dataset/species change, ...) no longer snaps the map back to the
#'   default extent out from under a user who has already panned or zoomed
#'   (bug report 2026-09-07: "when togling env summary statistics the map
#'   view goes to 50km. i want the default to be 100km until someone pans
#'   in or out" -- every rebuild WAS unconditionally re-fitting to the
#'   default). DEFAULT_MAP_CENTER/DEFAULT_MAP_ZOOM are the first-load-only
#'   fallback, for when no view has been recorded yet.
#'
#'   Either way, a real \code{jump_to()} call (not just the constructor's
#'   center=/zoom=, which is silent) is what fires the client's first
#'   \code{moveend} -- see DEFAULT_MAP_ZOOM's own comment for why this uses
#'   jump_to() to a fixed zoom rather than fit_bounds() to a bbox.
map_sp_h3t <- function(tile_url, scale, view = NULL, is_dark = TRUE) {
  has_view <- !is.null(view) && !is.null(view$center) && !is.null(view$zoom) &&
    length(view$center) == 2 && all(is.finite(unlist(view$center))) &&
    is.finite(view$zoom)

  ctr  <- if (has_view) view$center else DEFAULT_MAP_CENTER
  zm   <- if (has_view) view$zoom   else DEFAULT_MAP_ZOOM

  m <- mapgl::maplibre(
      style  = mapgl::carto_style(ifelse(is_dark, "dark-matter", "voyager")),
      center = ctr, zoom = zm) |>
    # jump_to() to the SAME center/zoom we just constructed with: a no-op on
    # the displayed view, but it's what actually fires the moveend that
    # populates rx$last_map_view (see the param doc above) -- construction
    # alone does not.
    mapgl::jump_to(center = ctr, zoom = zm)
  m <- m |>
    mapgl::add_scale_control(position = "top-left", unit = "metric") |>
    mapgl::add_navigation_control()

  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |> add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  m <- m |>
    mapgl::add_h3t_source(
      id          = "sp",
      tiles       = tile_url,
      sourcelayer = "sp"
    ) |>
    mapgl::add_fill_layer(
      id                 = "sp",
      source             = "sp",
      source_layer       = "sp",
      fill_color         = scale$expression,
      fill_outline_color = "white",
      fill_opacity       = 0.7,
      tooltip            = "value"
    )

  # this map's own layer id only — a control listing "env" here throws when
  # toggled back on, since that layer lives on the other map (see map_sp())
  ctrl <- build_layers_control(vis_ids, d_spatial_layers, "sp")
  m |> mapgl::add_layers_control(
    position = "top-right", layers = ctrl, collapsible = TRUE, margin_right = 45
  )
}

# view: see map_sp_h3t()'s doc -- identical "don't refit over a user's pan/
# zoom on rebuild" fix, applied here too since output$map rebuilds THIS
# widget fresh on every env-stat change (that path is exactly what the "goes
# to 50km" bug report was about), and the same jump_to()-to-a-fixed-zoom
# fallback (DEFAULT_MAP_CENTER/DEFAULT_MAP_ZOOM) for a screen-size-proof
# "100 km" default on first load.
map_env_h3t <- function(tile_url, scale, env_stat_label, env_var_label,
                        view = NULL, is_dark = TRUE) {
  has_view <- !is.null(view) && !is.null(view$center) && !is.null(view$zoom) &&
    length(view$center) == 2 && all(is.finite(unlist(view$center))) &&
    is.finite(view$zoom)

  ctr <- if (has_view) view$center else DEFAULT_MAP_CENTER
  zm  <- if (has_view) view$zoom   else DEFAULT_MAP_ZOOM

  m <- mapgl::maplibre(
      style  = mapgl::carto_style(ifelse(is_dark, "dark-matter", "voyager")),
      center = ctr, zoom = zm) |>
    mapgl::jump_to(center = ctr, zoom = zm)
  m <- m |>
    mapgl::add_scale_control(position = "top-left", unit = "metric") |>
    mapgl::add_navigation_control()

  vis_ids <- d_spatial_layers |> filter(default_visible) |> pull(dataset_id)
  m <- m |> add_spatial_layers(d_spatial_layers, visible_ids = vis_ids, is_dark = is_dark)

  m <- m |>
    mapgl::add_h3t_source(
      id          = "env",
      tiles       = tile_url,
      sourcelayer = "env"
    ) |>
    mapgl::add_fill_layer(
      id                 = "env",
      source             = "env",
      source_layer       = "env",
      fill_color         = scale$expression,
      fill_outline_color = "white",
      fill_opacity       = 0.7,
      tooltip            = "value"
    )

  ctrl <- build_layers_control(vis_ids, d_spatial_layers, "env")
  m |> mapgl::add_layers_control(
    position = "top-right", layers = ctrl, collapsible = TRUE, margin_right = 45
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
