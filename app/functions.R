# data retrieval functions ----

#' Retrieve Species Larval Abundance Data from Database
#'
#' Queries species, larva, net, tow, and site tables with temporal filters,
#' computing standardized tally values. Returns a dbplyr lazy table for
#' efficient downstream processing.
#'
#' @param sp_name Character vector of species names (format: "Common Name (Scientific Name)")
#' @param qtr Numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{name} - species name (common + scientific)
#'     \item \code{tally} - raw larval count
#'     \item \code{std_tally} - standardized tally (adjusted for haul factor and sorting proportion)
#'     \item \code{time_start} - tow start datetime
#'     \item \code{longitude}, \code{latitude} - spatial coordinates
#'     \item \code{quarter} - quarter (1-4)
#'     \item \code{hex_h3res*} - H3 hexagon indices at multiple resolutions
#'   }
#'
#' @details
#' The standardized tally accounts for differences in haul efficiency and
#' subsampling: \code{std_tally = std_haul_factor * tally / prop_sorted}.
#' Only records with non-NA tally values are returned.
#'
#' @examples
#' \dontrun{
#' # retrieve anchovy data for all quarters 2010-2020
#' df_sp <- get_sp(
#'   sp_name    = "Anchovy (Engraulis mordax)",
#'   qtr        = 1:4,
#'   date_range = as.Date(c("2010-01-01", "2020-12-31"))
#' )
#' df_sp |> collect()
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_sp}} for temporal aggregation
#'
#' @importFrom dplyr tbl mutate filter left_join between
#' @importFrom lubridate quarter
#'
#' @export
get_sp <- function(sp_name, qtr, date_range) {
  if (debug) message("get_sp: sp_name = ", sp_name, ", qtr = ", paste(qtr, collapse = ","),
                     ", date_range = ", paste(date_range, collapse = " to "))

  df_sp <- tbl(con, "species") |>
    mutate(
      name = paste0(common_name, " (", scientific_name, ")")) |>
    filter(name %in% sp_name) |>
    left_join(
      tbl(con, "larva"),
      by = "species_id") |>
    left_join(
      tbl(con, "net"),
      by = "net_uuid") |>
    left_join(
      tbl(con, "tow"),
      by = "tow_uuid") |>
    left_join(
      tbl(con, "site"),
      by = "site_uuid") |>
    mutate(
      quarter = quarter(time_start)) |>
    filter(
      !is.na(tally),
      between(time_start, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      std_tally = std_haul_factor * tally / prop_sorted)

  if (debug) {
    # n_rows <- df_sp |> summarize(n = n()) |> pull(n)
    # cat("get_sp: returning lazy table with", n_rows, "rows\n")
    message("get_sp: returning lazy table")
  }

  return(df_sp)
}


#' Retrieve Environmental Data from Database
#'
#' Queries environmental bottle cast data with temporal, depth, and variable filters.
#' Returns a dbplyr lazy table for efficient downstream processing.
#'
#' @param env_var Character string of database column name for environmental variable (e.g., "t_deg_c", "salnty")
#' @param qtr Character or numeric vector of quarters to include (1-4)
#' @param date_range Date vector of length 2 (start date, end date)
#' @param min_depth Numeric minimum depth in meters
#' @param max_depth Numeric maximum depth in meters
#'
#' @return dbplyr lazy table with columns:
#'   \itemize{
#'     \item \code{date} - date of cast
#'     \item \code{time} - time of cast (seconds since midnight)
#'     \item \code{dtime} - datetime (computed via SQL CAST and INTERVAL)
#'     \item \code{depthm} - depth in meters
#'     \item \code{lat_dec} - latitude (decimal degrees)
#'     \item \code{lon_dec} - longitude (decimal degrees)
#'     \item \code{qty} - renamed environmental variable value
#'     \item \code{hex_h3res*} - H3 hexagon indices at multiple resolutions
#'   }
#'
#' @details
#' The function joins \code{cast} and \code{bottle} tables, then joins with
#' \code{site} to obtain H3 spatial indices. Only records with non-NA values
#' for the selected variable are returned. Datetime is constructed from separate
#' date and time fields using DuckDB SQL.
#'
#' @examples
#' \dontrun{
#' # retrieve temperature data for Q1-Q2 2010-2020
#' df_env <- get_env(
#'   env_var    = "t_deg_c",
#'   qtr        = c(1, 2),
#'   date_range = as.Date(c("2010-01-01", "2020-12-31")),
#'   min_depth  = 0,
#'   max_depth  = 100
#' )
#' df_env |> collect()
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for spatial aggregation
#' @seealso \code{\link{prep_ts_env}} for temporal aggregation
#'
#' @importFrom dplyr tbl left_join rename filter mutate select starts_with between
#' @importFrom dbplyr sql join_by
#'
#' @export
get_env <- function(env_var, qtr, date_range, min_depth, max_depth) {
  if (debug) message("get_env: env_var = ", env_var, ", qtr = ", paste(qtr, collapse = ","),
                     ", date_range = ", paste(date_range, collapse = " to "),
                     ", depth = ", min_depth, "-", max_depth)

  df_env <- tbl(con, "cast") |>
    left_join(
      tbl(con, "bottle"),
      by = "cst_cnt") |>
    left_join(
      tbl(con, "site") |>
        distinct(longitude, latitude, .keep_all = TRUE),
      by = join_by(
        lon_dec == longitude,
        lat_dec == latitude)) |>
    rename(
      qty = all_of(env_var)) |>
    filter(
      !is.na(qty),
      between(depthm, min_depth, max_depth),
      between(date, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      # datetime (time is in seconds since midnight)
      dtime = sql("CAST(date AS TIMESTAMP) + INTERVAL (CAST(time AS VARCHAR) || ' second')")) |>
    select(
      date,
      time,
      dtime,
      cst_cnt,
      depthm,
      lat_dec,
      lon_dec,
      qty,
      starts_with("hex_h3res"))

  if (debug) {
    n_rows <- df_env |> summarize(n = n()) |> pull(n)
    message("get_env: returning lazy table with ", n_rows, " rows")
  }

  return(df_env)
}


# data preparation functions ----

#' Aggregate Species Data into H3 Hexagons
#'
#' Converts species occurrence/abundance data into multi-resolution H3 hexagonal
#' bins with aggregated statistics and geometries for mapping.
#'
#' @param df_sp dbplyr lazy table with columns: \code{hex_h3res*}, \code{std_tally}
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{sp.value} - mean standardized tally
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' }
#'
#' @seealso \code{\link{map_sp}} for visualization
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
prep_sp_hex <- function(df_sp, res_range) {
  if (debug) message("prep_sp_hex: aggregating species data for resolutions ", paste(res_range, collapse = ","))

  # precompute and store joins in a temporary table
  df_sp_temp <- df_sp |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{
    hex_fld <- glue("hex_h3res{.x}")

    df_sp_temp |>
      select(hex_int = all_of(hex_fld), std_tally, time_start) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_sp_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value   =  mean(std_tally, na.rm = TRUE),
      n          =  sum(!is.na(std_tally)),
      min_dtime  =  min(time_start, na.rm = TRUE),
      max_dtime  =  max(time_start, na.rm = TRUE),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(sp.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      tooltip = paste0("Avg. Abundance: ", round(sp.value, 2),
                 "</br>Num. Samples: ", n,
                 "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, sp.value, n, min_dtime, max_dtime, tooltip) |>
    collect()

  if (debug) message("prep_sp_hex: collected ", nrow(hex_sp_collected), " hex records before join")

  hex_sp <- hex_sp_collected |>
    left_join(
      sf_hex |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_sp_hex: created ", length(hex_sp), " hex layers")
    for (i in seq_along(hex_sp)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_sp[[i]]), " hexagons")
    }
  }

  return(hex_sp)
}


#' Aggregate Environmental Data into H3 Hexagons
#'
#' Converts environmental point data into multi-resolution H3 hexagonal bins
#' with aggregated statistics and geometries for mapping. Uses dbplyr lazy
#' evaluation to defer collection until after aggregation.
#'
#' @param df_env dbplyr lazy table with H3 index columns (\code{hex_h3res*}) and \code{qty} column
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#' @param env_stat Character string specifying aggregation function: "mean", "median", "min", "max", "sd"
#'
#' @return List of sf objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hexid} - H3 hexagon identifier
#'     \item \code{env.value} - aggregated environmental value
#'     \item \code{tooltip} - rounded value for display
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'   }
#'
#' @details
#' This function uses dbplyr lazy evaluation to efficiently aggregate data
#' across multiple H3 resolutions via \code{union_all}. Geometries are joined
#' from a pre-computed sf object (\code{sf_hex}).
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{map_env}} for visualization
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute sql
#'
#' @export
prep_env_hex <- function(df_env, res_range, env_stat) {
  if (debug) message("prep_env_hex: aggregating env data for resolutions ", paste(res_range, collapse = ","),
                     ", stat = ", env_stat)

  # precompute and store joins in a temporary table
  df_env_temp <- df_env |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{
    hex_fld <- glue("hex_h3res{.x}")

    df_env_temp |>
      select(hex_int = all_of(hex_fld), qty, dtime) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_env_collected <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      env.value = case_when(
        env_stat == "mean"   ~ mean(qty, na.rm = TRUE),
        env_stat == "median" ~ median(qty, na.rm = TRUE),
        env_stat == "min"    ~ min(qty, na.rm = TRUE),
        env_stat == "max"    ~ max(qty, na.rm = TRUE),
        env_stat == "sd"     ~ sd(qty, na.rm = TRUE),
        TRUE ~ mean(qty, na.rm = TRUE)
      ),
      n          =  sum(!is.na(qty)),
      min_dtime  =  min(dtime, na.rm = TRUE),
      max_dtime  =  max(dtime, na.rm = TRUE),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(env.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      tooltip = paste0("Value: ", round(env.value, 2),
                       "</br>Num. Samples: ", n,
                       "</br>Date Range: ", min_dtime, " to ", max_dtime)) |>
    select(resolution, hexid = hex_id, env.value, tooltip) |>
    collect()

  if (debug) message("prep_env_hex: collected ", nrow(hex_env_collected), " hex records before join")

  hex_env <- hex_env_collected |>
    left_join(
      sf_hex |>
        select(hexid = hex_id, hex_res, geometry),
      join_by(
        hexid,
        resolution == hex_res)) |>
    group_split(resolution)

  if (debug) {
    message("prep_env_hex: created ", length(hex_env), " hex layers")
    for (i in seq_along(hex_env)) {
      message("  Resolution ", res_range[i], ": ", nrow(hex_env[[i]]), " hexagons")
    }
  }

  return(hex_env)
}


#' Build Species Time Series Data
#'
#' Aggregates species abundance data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_sp dbplyr lazy table or data.frame with columns: \code{time_start}, \code{name}, \code{std_tally}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{name} - species name
#'     \item \code{avg} - mean standardized tally
#'     \item \code{std} - standard error (sd/n)
#'     \item \code{n} - number of observations
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database before aggregation.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_sp}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows
#'
#' @export
prep_ts_sp <- function(df_sp, ts_res) {

  sp_ts_data <- df_sp |>
    mutate(
      time = !!expr_time_sp(ts_res)
    ) |>
    group_by(time, name) |>
    summarize(
      avg = mean(std_tally, na.rm = TRUE),
      std = sd(std_tally, na.rm = TRUE),
      n = n(),
      .groups = "drop") |>
    mutate(
      upr = avg + std/n,
      lwr = avg - std/n,
      std = std/n) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    sp_ts_data <- sp_ts_data |>
      bind_rows(
        sp_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  return(sp_ts_data)
}


#' Build Environmental Time Series Data
#'
#' Aggregates environmental data by temporal resolution, computing mean and
#' standard error for visualization in time series plots. Uses dbplyr lazy
#' evaluation for efficient database queries.
#'
#' @param df_env dbplyr lazy table with columns: \code{dtime}, \code{qty}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.frame with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{avg} - mean of \code{qty}
#'     \item \code{std} - standard error of \code{qty} (sd/n)
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary. Data is collected
#' from database only at the end of aggregation.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_ts <- prep_ts_env(df_env, ts_res = "year")
#' }
#'
#' @seealso \code{\link{expr_time_env}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows n
#'
#' @export
prep_ts_env <- function(df_env, ts_res) {

  env_ts_data <- df_env |>
    mutate(
      time = !!expr_time_env(ts_res) ) |>
    group_by(time) |>
    summarize(
      avg = mean(qty, na.rm = T),
      std = sd(qty, na.rm = T) / n(),
      .groups = "drop"
    ) |>
    mutate(
      upr = avg + std,
      lwr = avg - std) |>
    collect()

  # add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    env_ts_data <- env_ts_data |>
      bind_rows(
        env_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  return(env_ts_data)
}


#' Prepare Data for Species-Environment Scatterplot
#'
#' Joins species and environmental data by matching observations that are close
#' in time and space, enabling correlation analysis between abundance and
#' environmental variables.
#'
#' @param df_sp dbplyr lazy table or data.frame with species data
#' @param df_env dbplyr lazy table or data.frame with environmental data
#' @param env_stat Character string specifying aggregation function (e.g., "mean", "median")
#' @param max_hours_diff Numeric maximum time difference (in hours) for matching observations (default: 72)
#' @param max_meters_diff Numeric maximum spatial distance (in meters) for matching observations (default: 1000)
#'
#' @return data.frame with matched species-environment observations
#'
#' @details
#' This function performs a fuzzy join based on temporal proximity using
#' \code{fuzzyjoin::difference_inner_join()}. For each species observation,
#' the closest environmental measurement (within \code{max_hours_diff}) is
#' selected. Data is collected from database before joining.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' df_splot <- prep_splot(df_sp, df_env, env_stat = "mean")
#' }
#'
#' @seealso \code{\link{get_sp}} for species data retrieval
#' @seealso \code{\link{get_env}} for environmental data retrieval
#'
#' @importFrom dplyr select collect mutate group_by slice_min ungroup
#' @importFrom fuzzyjoin difference_inner_join
#'
#' @export
prep_splot <- function(df_sp, df_env, env_stat, method = "nearest_time",
                       depth_target = 0, max_hours_diff = 72,
                       max_meters_diff = 1000) {

  # prepare species data
  d_sp <- df_sp |>
    select(
      sp_name  = name,
      sp_dtime = time_start,
      sp_tally = std_tally,
      sp_lon   = longitude,
      sp_lat   = latitude) # |>
  # compute()

  # prepare environmental data
  d_env <- df_env |>
    select(
      env_dtime = dtime,
      env_qty   = qty,
      env_cst   = cst_cnt,
      env_depth = depthm,
      env_lon   = lon_dec,
      env_lat   = lat_dec,
      env_depth = depthm) |>
    mutate(
      env_dtime_lwr = sql(glue("env_dtime - INTERVAL {max_hours_diff} HOUR")),
      env_dtime_upr = sql(glue("env_dtime + INTERVAL {max_hours_diff} HOUR")))

  # join by time difference
  d_sp_env_raw <- d_sp |>
      left_join(
        d_env,
        # join species to env observations within desired time interval
        by = join_by(between(sp_dtime, env_dtime_lwr, env_dtime_upr))) |>
      # compute distance between species and ocean observations
      mutate(
        dist_m = sql("ST_Distance_Sphere(ST_Point(sp_lon, sp_lat), ST_Point(env_lon, env_lat))")) |>
      # get pairs within desired distance
      filter(
        dist_m <= max_meters_diff)

  order_by <- if (method == "nearest_time") {
    expr(tibble(time_diff,dist_m,env_cst))
  } else if (method == "nearest_dist" ) {
    expr(tibble(dist_m,time_diff,env_cst))
  }

  d_sp_env <- if (method == "nearest_time" | method == "nearest_dist") {
    d_sp_env_raw |>
      mutate(
        time_diff = if_else(sp_dtime - env_dtime > lubridate::seconds(0),
                            sp_dtime - env_dtime,
                            env_dtime - sp_dtime)) |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      slice_min(
        !!order_by,
        with_ties = TRUE) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop")
  } else {
    d_sp_env_raw |>
      group_by(
        sp_name, sp_tally, sp_dtime, sp_lon, sp_lat) |>
      summarize(
        env_qty = mean(env_qty, na.rm = TRUE),
        .groups = "drop") |>
      select(
        sp_name, sp_dtime, sp_lon, sp_lat, sp_tally,
        env_qty) }

  d_sp_env
}


#' Build Filter Summary for Display
#'
#' Creates a formatted list of filter criteria for display in the UI.
#' Summarizes species, environmental variables, temporal filters, depth ranges,
#' and spatial constraints into human-readable markdown strings.
#'
#' @param sel_name Character vector of selected species names (format: "Common Name (Scientific Name)")
#' @param sel_env_var Character string of selected environmental variable (e.g., "t_deg_c")
#' @param sel_qtr Numeric vector of selected quarters (1-4)
#' @param sel_date_range Date vector of length 2 (start date, end date)
#' @param sel_depth_range Numeric vector of length 2 (min depth, max depth) in meters
#' @param drawn_polygon sf object or data.frame representing user-drawn polygon (or NULL)
#'
#' @return Character vector of markdown-formatted filter descriptions
#'
#' @examples
#' prep_filter_summary(
#'   sel_name        = c("Anchovy (Engraulis mordax)", "Sardine (Sardinops sagax)"),
#'   sel_env_var     = "t_deg_c",
#'   sel_qtr         = c(1, 2),
#'   sel_date_range  = as.Date(c("2000-01-01", "2020-12-31")),
#'   sel_depth_range = c(0, 100),
#'   drawn_polygon   = NULL
#' )
#'
#' @seealso \code{\link{modal_data}} for the modal dialog that captures these filters
#'
#' @export
prep_filter_summary <- function(sel_name, sel_env_var, sel_qtr, sel_date_range,
                                sel_depth_range, drawn_polygon, selected_grid_zones) {
  filter_list <- list()

  # species
  if (!is.null(sel_name) && length(sel_name) > 0) {
    filter_list <- c(filter_list,
                     if (length(sel_name) <= 3) {
                       paste0("**Species:** ", paste(sel_name, collapse = ", "))
                     } else {
                       paste0("**Species:** ", length(sel_name), " selected")
                     }
    )
  }

  # variable
  filter_list <- c(filter_list, paste0("**Variable:** ", names(which(env_var_choices == sel_env_var))))

  # quarters
  quarter_names <- c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4")
  filter_list <- c(filter_list, paste0("**Quarters:** ", paste(quarter_names[as.character(sel_qtr)], collapse = ", ")))

  # date range
  filter_list <- c(filter_list, paste0("**Date Range:** ",
                                       format(sel_date_range[1], "%Y-%m-%d"), " to ",
                                       format(sel_date_range[2], "%Y-%m-%d")))

  # depth range
  filter_list <- c(filter_list, paste0("**Depth Range:** ", sel_depth_range[1], " - ", sel_depth_range[2], " m"))

  # spatial
  if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
    filter_list <- c(filter_list, "**Spatial:** Custom polygon defined")
  } else if (!is.null(selected_grid_zones) && length(selected_grid_zones) > 0) {
    zone_text <- if (length(selected_grid_zones) <= 5) {
      paste(selected_grid_zones, collapse = ", ")
    } else {
      paste(length(selected_grid_zones), "zones selected")
    }
    filter_list <- c(filter_list, paste0("**Spatial:** Grid zones - ", zone_text))
  } else {
    filter_list <- c(filter_list, "**Spatial:** All locations")
  }

  return(filter_list)
}

prep_summary_stats <- function(df_sp, df_env) {
  stat_list <- list()

  stat_list <- c(stat_list, "**Species Data**")

  stat_list <- c(stat_list, paste0(nrow(df_sp |> collect()), " observations"))

  stat_list <- c(stat_list, "\n**Environmental Data**")

  stat_list <- c(stat_list, paste0(nrow(df_env |> collect()), " observations"))

  return(stat_list)
}


# visualization functions ----

#' Create Interactive Species Distribution Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying species abundance
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param sp_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated species abundance
#' @param sp_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#'
#' @return maplibre object with multi-resolution hexagonal layers, legend, and scale control
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance. Abundance values are standardized as count per 10m² surface area.
#'
#' @examples
#' \dontrun{
#' df_sp <- get_sp(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- prep_sp_hex(df_sp, res_range = 3:5)
#' sp_scale <- lapply(sp_hex, function(x) scales::col_numeric("YlOrRd", domain = range(x$sp.value)))
#' map_sp(sp_hex, sp_scale)
#' }
#'
#' @seealso \code{\link{prep_sp_hex}} for data aggregation
#' @seealso \code{\link{get_sp}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend add_scale_control
#'
#' @export
map_sp <- function(sp_hex_list, sp_scale_list, is_dark = T) {
  if (debug) {
    message("map_sp: creating species map with ", length(sp_hex_list), " resolution layers")
    message("map_sp: first layer has ", nrow(sp_hex_list[[1]]), " hexagons")
  }

  # base map
  carto_style

  sp_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(sp_hex_list[[1]]))

  # add each resolution layer individually
  for (i in 1:length(res_range)) { # i = 1
    sp_map <- sp_map |>
      add_fill_layer(
        id               = paste0("sp", res_range[i]),
        source           = st_as_sf(sp_hex_list[[i]]),
        fill_color       = sp_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity     = 0.6,
        min_zoom         = zoom_breaks[i],
        max_zoom         = zoom_breaks[i+1],
        tooltip          = "tooltip")
  }

  sp_map <- sp_map |>
    add_legend(
      "Avg. Abundance (count/10m^2)",
      values   = round(sp_scale_list[[1]]$breaks),
      colors   = sp_scale_list[[1]]$colors,
      type     = "continuous",
      position = "bottom-left") |>
    add_scale_control(position = "top-left", unit = "metric") |>
    add_navigation_control()

  return(sp_map)
}


#' Create Interactive Environmental Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying environmental data
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param env_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated environmental values
#' @param env_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#' @param env_stat_label Character string describing the statistic (e.g., "Mean", "Median")
#' @param env_var_label Character string describing the variable (e.g., "Temperature (°C)")
#'
#' @return maplibre object with multi-resolution hexagonal layers and legend
#'
#' @details
#' The map uses zoom-dependent layer visibility controlled by \code{zoom_breaks}.
#' Each resolution level displays at appropriate zoom ranges to balance detail
#' and performance.
#'
#' @examples
#' \dontrun{
#' df_env <- get_env("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' env_hex <- prep_env_hex(df_env, res_range = 3:5, env_stat = "mean")
#' env_scale <- lapply(env_hex, function(x) scales::col_numeric("viridis", domain = range(x$env.value)))
#' map_env(env_hex, env_scale, "Mean", "Temperature (°C)")
#' }
#'
#' @seealso \code{\link{prep_env_hex}} for data aggregation
#' @seealso \code{\link{get_env}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend
#'
#' @export
map_env <- function(env_hex_list, env_scale_list, env_stat_label, env_var_label, is_dark = T) {
  if (debug) {
    message("map_env: creating environmental map with ", length(env_hex_list), " resolution layers")
    message("map_env: first layer has ", nrow(env_hex_list[[1]]), " hexagons")
  }

  # create base map
  env_map <- maplibre(
    style = carto_style(ifelse(is_dark, "dark-matter", "voyager"))) |>
    fit_bounds(bbox = st_as_sf(env_hex_list[[1]]))

  # add each resolution layer individually
  for (i in 1:length(res_range)) { # i = 1
    env_map <- env_map |>
      add_fill_layer(
        id               = paste0("env", res_range[i]),
        source           = st_as_sf(env_hex_list[[i]]),
        fill_color       = env_scale_list[[i]]$expression,
        fill_outline_color = "white",
        fill_opacity     = 0.6,
        min_zoom         = zoom_breaks[i],
        max_zoom         = zoom_breaks[i+1],
        tooltip          = "tooltip")
  }

  # add legend
  env_map <- env_map |>
    add_legend(
      paste(env_stat_label, env_var_label),
      values   = signif(env_scale_list[[1]]$breaks, 2),
      colors   = env_scale_list[[1]]$colors,
      type     = "continuous",
      position = "bottom-right") |>
    add_navigation_control()

  return(env_map)
}


#' Create Dual-Panel Time Series Plot
#'
#' Generates an interactive Highcharts plot with species abundance and
#' environmental data in separate panels, with synchronized x-axis zoom and
#' resolution-dependent date formatting.
#'
#' @param sp_ts data.frame from \code{\link{prep_ts_sp}} with columns: \code{time}, \code{name}, \code{avg}, \code{std}, \code{lwr}, \code{upr}
#' @param env_ts data.frame from \code{\link{prep_ts_env}} with columns: \code{time}, \code{avg}, \code{std}, \code{lwr}, \code{upr}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter", "month", "day", etc.
#' @param sel_env_var Character string of environmental variable column name (e.g., "t_deg_c")
#'
#' @return highchart object with dual y-axes, line + ribbon series, and zoom capabilities
#'
#' @details
#' The function creates a two-panel plot with species on top and environmental
#' data on bottom. Date formatting adapts to \code{ts_res}. Standard error ribbons
#' are displayed as \code{arearange} series. Tooltips show values ± standard error.
#'
#' @examples
#' \dontrun{
#' sp_ts <- prep_ts_sp(df_sp, ts_res = "year")
#' env_ts <- prep_ts_env(df_env, ts_res = "year")
#' plot_ts(sp_ts, env_ts, ts_res = "year", sel_env_var = "t_deg_c")
#' }
#'
#' @seealso \code{\link{prep_ts_sp}} for species time series data
#' @seealso \code{\link{prep_ts_env}} for environmental time series data
#'
#' @importFrom highcharter highchart hc_chart hc_exporting hc_xAxis hc_yAxis_multiples hc_tooltip hc_rangeSelector hc_plotOptions hc_legend hc_add_series datetime_to_timestamp
#' @importFrom dplyr mutate bind_rows arrange distinct filter
#'
#' @export
plot_ts <- function(sp_ts, env_ts, ts_res, sel_env_var) {
  # add a 'panel' and consistent 'name' column to each dataset
  sp_ts_mod <- sp_ts |>
    mutate(panel_id = 0) # assign to the first (top) panel

  env_ts_mod <- env_ts |>
    mutate(
      name     = names(which(env_var_choices == sel_env_var)),
      panel_id = 1) # assign to the second (bottom) panel

  # combine into a single data frame
  combined_data <- bind_rows(sp_ts_mod, env_ts_mod) |>
    arrange(time) |>
    mutate(time_ts = datetime_to_timestamp(time))

  # get a list of the unique series to create
  series_list <- combined_data |>
    distinct(name, panel_id)

  # define formatters based on temporal resolution
  if (ts_res == "year") {
    tooltip_date_format <- "function(timestamp) { return Highcharts.dateFormat('%Y', timestamp); }"
    xaxis_label_format <- "{value:%Y}"
  } else if (ts_res == "quarter") {
    tooltip_date_format <- "function(timestamp) {
      var quarter = Math.ceil((new Date(timestamp).getUTCMonth() + 1) / 3);
      return 'Q' + quarter;
    }"
    xaxis_label_format <- NULL  # use custom formatter
  } else if (ts_res == "year_quarter") {
    tooltip_date_format <- "function(timestamp) {
      var quarter = Math.ceil((new Date(timestamp).getUTCMonth() + 1) / 3);
      return Highcharts.dateFormat('%Y', timestamp) + ' Q' + quarter;
    }"
    xaxis_label_format <- NULL  # use custom formatter
  } else {
    # default for other resolutions
    tooltip_date_format <- "function(timestamp) { return Highcharts.dateFormat('%b %e, %Y', timestamp); }"
    xaxis_label_format <- "{value:%b %e, %Y}"
  }

  # initialize the chart with its layout
  hc <- highchart() |>
    hc_chart(zoomType = "x") |>
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          enabled = FALSE # disables the default hamburger menu
        )
      )
    ) |>
    hc_xAxis(type = "datetime", crosshair = TRUE) |>
    hc_yAxis_multiples(
      list(title = list(text = "Average Species Abundance"), height = "47%", top = "0%", offset = 0),
      list(title = list(text = paste0("Average ", names(which(env_var_choices == sel_env_var)))), height = "47%", top = "53%", offset = 0)
    )

  # configure xAxis based on resolution
  if (ts_res == "quarter") {
    hc <- hc |>
      hc_xAxis(
        type = "datetime",
        crosshair = TRUE,
        labels = list(
          formatter = JS("function() {
            var quarter = Math.ceil((new Date(this.value).getUTCMonth() + 1) / 3);
            return 'Q' + quarter;
          }")
        )
      )
  } else if (ts_res == "year_quarter") {
    hc <- hc |>
      hc_xAxis(
        type = "datetime",
        crosshair = TRUE,
        labels = list(
          formatter = JS("function() {
            var quarter = Math.ceil((new Date(this.value).getUTCMonth() + 1) / 3);
            return Highcharts.dateFormat('%Y', this.value) + ' Q' + quarter;
          }")
        )
      )
  } else {
    hc <- hc |>
      hc_xAxis(
        type = "datetime",
        crosshair = TRUE,
        labels = list(format = xaxis_label_format)
      )
  }

  # configure tooltip
  hc <- hc |>
    hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      formatter = JS("
      function() {
        var formatDate = ", tooltip_date_format, ";
        var header = '<b>' + formatDate(this.x) + '</b><br/>';
        var pointLines = this.points.map(function(point) {
          var stdText = '';
          if (point.point && point.point.std !== undefined && point.point.std !== null) {
            stdText = ' (±' + Highcharts.numberFormat(point.point.std, 2) + ')';
          }
          return '<span style=\"color:' + point.color + '\">●</span> ' +
                 point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 2) + stdText + '</b>';
        }).join('<br/>');
        return header + pointLines;
      }
    ")
    ) |>
    hc_rangeSelector(enabled = FALSE) |>
    hc_plotOptions(
      series = list(
        marker = list(
          enabled = TRUE,
          radius = 0,
          states = list(hover = list(enabled = TRUE, radius = 5))
        )
      ),
      arearange = list(
        lineWidth = 0,
        fillOpacity = 0.3,
        enableMouseTracking = FALSE,
        showInLegend = FALSE
      )
    ) |>
    hc_legend(enabled = TRUE)

  # loop through each series to add its line and ribbon
  for (i in 1:nrow(series_list)) {
    series_name <- series_list$name[i]
    panel_index <- series_list$panel_id[i]
    series_data <- combined_data |> filter(name == series_name)

    hc <- hc |>
      hc_add_series(
        data = series_data,
        type = "line",
        hcaes(x = time_ts, y = avg, std = std),
        name = series_name,
        id = series_name,
        yAxis = panel_index
      ) |>
      hc_add_series(
        data = series_data,
        type = "arearange",
        hcaes(x = time_ts, low = lwr, high = upr),
        name = series_name,
        linkedTo = series_name,
        yAxis = panel_index
      )
  }

  # display the final chart
  return(hc)
}


# UI component functions ----

#' Data Selection Modal Dialog
#'
#' Creates a multi-tabbed modal dialog for selecting species, environmental
#' variables, temporal filters, depth ranges, and spatial regions.
#'
#' @return Shiny modal dialog object with four tabs:
#'   \itemize{
#'     \item Species - selectizeInput for multiple species selection
#'     \item Environmental - variable and depth range selection
#'     \item Temporal - quarter and date range selection
#'     \item Spatial - interactive map for polygon drawing
#'   }
#'
#' @details
#' The modal dialog uses \code{bslib::navset_tab()} for tab organization and
#' \code{shiny::input_task_button()} for submission handling. Spatial filtering
#' is implemented via \code{maplibre} with drawing capabilities.
#'
#' @examples
#' \dontrun{
#' # in server.R
#' observeEvent(input$show_filters, {
#'   showModal(modal_data())
#' })
#' }
#'
#' @seealso \code{\link{prep_filter_summary}} for filter summary generation
#'
#' @importFrom shiny modalDialog selectizeInput selectInput numericRangeInput dateRangeInput modalButton tagList
#' @importFrom bslib navset_tab nav_panel
#' @importFrom maplibre maplibreOutput
#'
#' @export
modal_data <- function() {
  modalDialog(
    title = "Data Selection",
    navset_tab(
      nav_panel(
        "Species",
        br(),

        selectizeInput(
          "sel_name",
          "Species",
          choices = NULL,
          multiple = TRUE
        ),
      ),
      nav_panel(
        "Environmental",
        br(),
        selectInput(
          "sel_env_var",
          "Variable",
          env_var_choices,
          selected = "Temperature"
        ),

        numericRangeInput(
          "sel_depth_range",
          "Depth Range (m)",
          c(0, 212), # TODO: pull from data
          width = NULL,
          separator = " to ",
          min = 0,
          max = 512
        ),
      ),
      nav_panel(
        "Temporal",
        br(),
        selectInput(
          "sel_qtr",
          "Quarter",
          c(Q1 = 1,
            Q2 = 2,
            Q3 = 3,
            Q4 = 4),
          selected = 1:4,
          multiple = TRUE),

        dateRangeInput(
          "sel_date_range",
          "Date Range",
          startview = "year",
          start = min_max_date[1],
          end = min_max_date[2],
          min = min_max_date[1],
          max = min_max_date[2]),
      ),
      nav_panel(
        "Spatial",
        br(),
        "Select pre-defined zones by clicking on the grid, or draw a custom polygon. Click selected zones again to deselect them.",
        maplibreOutput("spatial_filter_map", height = "400px")
      ),
    ),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit", "Submit")
    ),

    size = "l",
    fade = FALSE
  )
}


#' Depth Profile Modal Dialog
#'
#' Creates a modal dialog for defining a transect line segment and buffer
#' distance to generate environmental depth profiles.
#'
#' @param map_sp maplibre object (currently unused in implementation, retained for future enhancement)
#'
#' @return Shiny modal dialog object with transect drawing interface and buffer distance input
#'
#' @details
#' Users draw a line segment on the map to define a transect. The buffer distance
#' controls the width of the corridor around the transect for data aggregation.
#' Default buffer is 5 km.
#'
#' @examples
#' \dontrun{
#' # in server.R
#' observeEvent(input$create_profile, {
#'   showModal(modal_depth_profile(map_sp = NULL))
#' })
#' }
#'
#' @seealso \code{\link{buffer_transect}} for transect buffer generation
#'
#' @importFrom shiny modalDialog numericInput modalButton tagList
#' @importFrom maplibre maplibreOutput
#'
#' @export
modal_depth_profile <- function(map_sp) {
  modalDialog(
    title = "Create Depth Profile",

    p("Draw a line segment on the map to define your transect."),

    numericInput(
      "modal_buffer_dist",
      "Buffer Distance (km)",
      value = 5
    ),

    maplibreOutput("transect_map", height = "500px"),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit_transect", "Generate Profile")
    ),

    size = "l",
    fade = FALSE
  )
}


#' Create Placeholder Message UI
#'
#' Generates a centered placeholder message for empty or loading states in the
#' Shiny UI. Useful for displaying instructions or status messages when no data
#' is available.
#'
#' @param title Character string for heading text
#' @param message Character string for body text
#'
#' @return shiny.tag div element with centered, styled placeholder content
#'
#' @examples
#' \dontrun{
#' output$map_placeholder <- renderUI({
#'   ui_placeholder("No Data Selected", "Please select species from the filter menu.")
#' })
#' }
#'
#' @importFrom shiny div h4 p
#'
#' @export
ui_placeholder <- function(title, message) {
  div(
    class = "d-flex align-items-center justify-content-center",
    style = "height: 80vh;",
    div(
      class = "text-center text-muted",
      h4(title),
      p(message)
    )
  )
}


# utility functions ----

#' Generate Time Aggregation Expression for Species Data
#'
#' Creates a SQL-based expression for temporal aggregation of species time series
#' using DuckDB date functions. Used internally by \code{\link{prep_ts_sp}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_sp |> mutate(time = !!expr_time_sp("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_sp}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_sp <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', time_start)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', time_start)), day(date_trunc('quarter', time_start)))")),
         "month"        = expr(sql("extract('month' FROM time_start)")),
         "day"          = expr(sql("extract('doy' FROM time_start)")),
         "year_quarter" = expr(sql("date_trunc('quarter', time_start)")),
         "year_month"   = expr(sql("date_trunc('month', time_start)")),
         "year_day"     = expr(sql("date_trunc('day', time_start)"))
  )
}


#' Generate Time Aggregation Expression for Environmental Data
#'
#' Creates a SQL-based expression for temporal aggregation of environmental
#' time series using DuckDB date functions. Used internally by \code{\link{prep_ts_env}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{date_trunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' df_env |> mutate(time = !!expr_time_env("year"))
#' }
#'
#' @seealso \code{\link{prep_ts_env}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
expr_time_env <- function(ts_res) {
  switch(ts_res,
         "year"         = expr(sql("date_trunc('year', dtime)")),
         "quarter"      = expr(sql("make_date(2000, month(date_trunc('quarter', dtime)), day(date_trunc('quarter', dtime)))")),
         "month"        = expr(sql("extract('month' FROM dtime)")),
         "day"          = expr(sql("extract('doy' FROM dtime)")),
         "year_quarter" = expr(sql("date_trunc('quarter', dtime)")),
         "year_month"   = expr(sql("date_trunc('month', dtime)")),
         "year_day"     = expr(sql("date_trunc('day', dtime)"))
  )
}


#' Create Buffer Around Line Segment with Dateline Handling
#'
#' Creates a buffered polygon around a line segment (transect), handling
#' dateline crossings and projecting to appropriate UTM zone for accurate
#' distance calculations.
#'
#' @param coords Matrix or data.frame of coordinates (longitude, latitude) defining the line segment
#' @param buffer_dist Numeric buffer distance in meters (default: 5000)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{utm_crs} - EPSG code for the UTM projection used
#'     \item \code{segment} - sf linestring object in WGS84 (EPSG:4326)
#'     \item \code{segment_utm} - sf linestring object in UTM projection
#'     \item \code{buffer} - sf polygon buffer in WGS84 (EPSG:4326)
#'     \item \code{buffer_utm} - sf polygon buffer in UTM projection
#'   }
#'
#' @details
#' The function automatically detects the appropriate UTM zone based on the
#' centroid of the input segment. Dateline crossings are handled by normalizing
#' coordinates to 0-360 range when necessary.
#'
#' @examples
#' # create transect across California Current
#' coords <- matrix(c(-120, 34, -118, 36), ncol = 2, byrow = TRUE)
#' result <- buffer_transect(coords, buffer_dist = 10000)
#' plot(result$buffer)
#'
#' @seealso \code{\link{fix_dateline_crossing}} for dateline crossing detection
#' @seealso \code{\link{modal_depth_profile}} for UI implementation
#'
#' @importFrom sf st_sf st_sfc st_linestring st_centroid st_coordinates st_transform st_buffer st_wrap_dateline
#' @importFrom units set_units
#'
#' @export
buffer_transect <- function(coords, buffer_dist = 5000) {
  # create initial segment
  segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # handle dateline crossing
  segment <- fix_dateline_crossing(segment)

  # get centroid to determine UTM zone
  centroid <- st_centroid(segment)
  cent_coords <- st_coordinates(centroid)
  lon <- cent_coords[1, "X"]
  lat <- cent_coords[1, "Y"]

  # adjust longitude for UTM if it was shifted
  lon <- ifelse(lon > 180, lon - 360, lon)

  # calculate UTM zone
  zone <- floor((lon + 180) / 6) + 1
  hemisphere <- if (lat >= 0) 32600 else 32700
  utm_crs <- hemisphere + zone

  # transform to UTM, buffer, and transform back
  segment_utm <- st_transform(segment, utm_crs)
  buffer_utm <- st_buffer(segment_utm, dist = buffer_dist, endCapStyle = "FLAT")
  buffer <- st_transform(buffer_utm, 4326)

  # ensure buffer is valid and handles dateline
  buffer <- st_wrap_dateline(buffer, options = c("WRAPDATELINE=YES"))

  return(list(
    utm_crs     = utm_crs,
    segment     = segment,
    segment_utm = segment_utm,
    buffer      = buffer,
    buffer_utm  = buffer_utm))
}


#' Detect and Handle Dateline Crossing in Line Segments
#'
#' Normalizes longitude coordinates when a line segment crosses the ±180°
#' dateline, preventing discontinuities in buffering and visualization.
#'
#' @param segment sf linestring object representing a transect or track
#'
#' @return sf linestring object with normalized coordinates (0-360° range if dateline is crossed)
#'
#' @details
#' Dateline crossings are detected by checking for longitude jumps > 180°.
#' When detected, negative longitudes are shifted to 0-360° range. The segment
#' is then segmentized to 1000m intervals for smooth buffering.
#'
#' @examples
#' \dontrun{
#' # transect crossing the dateline
#' coords <- matrix(c(175, 30, -175, 35), ncol = 2, byrow = TRUE)
#' segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))
#' normalized <- fix_dateline_crossing(segment)
#' }
#'
#' @seealso \code{\link{buffer_transect}} for usage in buffering workflow
#'
#' @importFrom sf st_coordinates st_sf st_sfc st_linestring st_segmentize
#' @importFrom units set_units
#'
#' @keywords internal
fix_dateline_crossing <- function(segment) {
  coords <- st_coordinates(segment)[, c("X", "Y")]
  lons <- coords[, "X"]

  # check for dateline crossing (large longitude jump)
  lon_diff <- diff(lons)
  crosses_dateline <- any(abs(lon_diff) > 180)

  if (!crosses_dateline) return(segment)

  # normalize longitudes to avoid discontinuity
  # shift coords to a 0-360 range if crossing +180/-180
  if (any(lons < 0)) {
    coords[, "X"] <- ifelse(lons < 0, lons + 360, lons)
  }

  # create new linestring
  new_segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # optional: split into multiple segments if needed
  # use st_segmentize to add points across dateline for smoother buffer
  new_segment <- st_segmentize(new_segment, set_units(1000, "m"))

  return(new_segment)
}
