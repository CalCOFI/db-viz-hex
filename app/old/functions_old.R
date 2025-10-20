# build_filter_summary ----

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
#' build_filter_summary(
#'   sel_name        = c("Anchovy (Engraulis mordax)", "Sardine (Sardinops sagax)"),
#'   sel_env_var     = "t_deg_c",
#'   sel_qtr         = c(1, 2),
#'   sel_date_range  = as.Date(c("2000-01-01", "2020-12-31")),
#'   sel_depth_range = c(0, 100),
#'   drawn_polygon   = NULL
#' )
#'
#' @seealso \code{\link{dataModal}} for the modal dialog that captures these filters
#'
#' @export
build_filter_summary <- function(sel_name, sel_env_var, sel_qtr, sel_date_range,
                                 sel_depth_range, drawn_polygon) {
  filter_list <- list()

  # Species
  if (!is.null(sel_name) && length(sel_name) > 0) {
    filter_list <- c(filter_list,
                     if (length(sel_name) <= 3) {
                       paste0("**Species:** ", paste(sel_name, collapse = ", "))
                     } else {
                       paste0("**Species:** ", length(sel_name), " selected")
                     }
    )
  }

  # Variable
  filter_list <- c(filter_list, paste0("**Variable:** ", names(which(env_var_choices == sel_env_var))))

  # Quarters
  quarter_names <- c("1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4")
  filter_list <- c(filter_list, paste0("**Quarters:** ", paste(quarter_names[as.character(sel_qtr)], collapse = ", ")))

  # Date range
  filter_list <- c(filter_list, paste0("**Date Range:** ",
                                       format(sel_date_range[1], "%Y-%m-%d"), " to ",
                                       format(sel_date_range[2], "%Y-%m-%d")))

  # Depth range
  filter_list <- c(filter_list, paste0("**Depth Range:** ", sel_depth_range[1], " - ", sel_depth_range[2], " m"))

  # Spatial
  if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
    filter_list <- c(filter_list, "**Spatial:** Custom region defined")
  } else {
    filter_list <- c(filter_list, "**Spatial:** All locations")
  }

  return(filter_list)
}


# create_buffer ----

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
#' result <- create_buffer(coords, buffer_dist = 10000)
#' plot(result$buffer)
#'
#' @seealso \code{\link{split_at_dateline}} for dateline crossing detection
#' @seealso \code{\link{depthProfileModal}} for UI implementation
#'
#' @importFrom sf st_sf st_sfc st_linestring st_centroid st_coordinates st_transform st_buffer st_wrap_dateline
#' @importFrom units set_units
#'
#' @export
create_buffer <- function(coords, buffer_dist = 5000) {
  # Create initial segment
  segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # Handle dateline crossing
  segment <- split_at_dateline(segment)

  # Get centroid to determine UTM zone
  centroid <- st_centroid(segment)
  cent_coords <- st_coordinates(centroid)
  lon <- cent_coords[1, "X"]
  lat <- cent_coords[1, "Y"]

  # Adjust longitude for UTM if it was shifted
  lon <- ifelse(lon > 180, lon - 360, lon)

  # Calculate UTM zone
  zone <- floor((lon + 180) / 6) + 1
  hemisphere <- if (lat >= 0) 32600 else 32700
  utm_crs <- hemisphere + zone

  # Transform to UTM, buffer, and transform back
  segment_utm <- st_transform(segment, utm_crs)
  buffer_utm <- st_buffer(segment_utm, dist = buffer_dist, endCapStyle = "FLAT")
  buffer <- st_transform(buffer_utm, 4326)

  # Ensure buffer is valid and handles dateline
  buffer <- st_wrap_dateline(buffer, options = c("WRAPDATELINE=YES"))

  return(list(utm_crs = utm_crs, segment = segment, segment_utm = segment_utm, buffer = buffer, buffer_utm = buffer_utm))
}


# create_ocean_map ----

#' Create Interactive Oceanographic Map with Hexagonal Binning
#'
#' Generates a multi-resolution maplibre map displaying oceanographic data
#' aggregated into H3 hexagons with color-coded values and interactive tooltips.
#'
#' @param ocean_hex_list List of sf objects, one per H3 resolution level, containing hexagonal geometries and aggregated oceanographic values
#' @param ocean_scale_list List of color scale specifications, one per resolution level (from \code{scales::col_numeric()})
#' @param ocean_stat_label Character string describing the statistic (e.g., "Mean", "Median")
#' @param ocean_var_label Character string describing the variable (e.g., "Temperature (°C)")
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
#' ocean_data <- ocean_retrieve("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' ocean_hex <- map_ocean_hex(ocean_data, res_range = 3:5, ocean_stat = "mean")
#' ocean_scale <- lapply(ocean_hex, function(x) scales::col_numeric("viridis", domain = range(x$ocean.value)))
#' create_ocean_map(ocean_hex, ocean_scale, "Mean", "Temperature (°C)")
#' }
#'
#' @seealso \code{\link{map_ocean_hex}} for data aggregation
#' @seealso \code{\link{ocean_retrieve}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend
#'
#' @export
create_ocean_map <- function(ocean_hex_list, ocean_scale_list, ocean_stat_label, ocean_var_label) {
  # create base map
  ocean_map <- maplibre(bounds = ocean_hex_list[[1]])

  # add each resolution layer individually
  for (i in 1:length(res_range)) {
    ocean_map <- ocean_map |>
      add_fill_layer(id = paste0("ocean", res_range[i]),
                     source = ocean_hex_list[[i]],
                     fill_color = ocean_scale_list[[i]]$expression,
                     fill_outline_color = "white",
                     fill_opacity = 0.6,
                     min_zoom = zoom_breaks[i],
                     max_zoom = zoom_breaks[i+1],
                     tooltip = "ocean.value")
  }

  # add legend
  ocean_map <- ocean_map |>
    add_legend(paste(ocean_stat_label, ocean_var_label),
               values = signif(ocean_scale_list[[1]]$breaks, 2),
               colors = ocean_scale_list[[1]]$colors,
               type = "continuous",
               position = "bottom-right")

  return(ocean_map)
}


# create_sp_map ----

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
#' sp_data <- sp_retrieve(sp_name = "Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- map_sp_hex(sp_data, res_range = 3:5)
#' sp_scale <- lapply(sp_hex, function(x) scales::col_numeric("YlOrRd", domain = range(x$sp.value)))
#' create_sp_map(sp_hex, sp_scale)
#' }
#'
#' @seealso \code{\link{map_sp_hex}} for data aggregation
#' @seealso \code{\link{sp_retrieve}} for data retrieval
#'
#' @importFrom maplibre maplibre add_fill_layer add_legend add_scale_control
#'
#' @export
create_sp_map <- function(sp_hex_list, sp_scale_list) {
  # base map
  sp_map <- maplibre(bounds = sp_hex_list[[1]])

  # add each resolution layer individually
  for (i in 1:length(res_range)) {
    sp_map <- sp_map |>
      add_fill_layer(id = paste0("sp", res_range[i]),
                     source = sp_hex_list[[i]],
                     fill_color = sp_scale_list[[i]]$expression,
                     fill_outline_color = "white",
                     fill_opacity = 0.6,
                     min_zoom = zoom_breaks[i],
                     max_zoom = zoom_breaks[i+1],
                     tooltip = "tooltip")
  }

  sp_map <- sp_map |>
    add_legend("Avg. Abundance (count/10m^2)",
               values = round(sp_scale_list[[1]]$breaks),
               colors = sp_scale_list[[1]]$colors,
               type = "continuous",
               position = "bottom-left") |>
    add_scale_control(position = "top-left", unit = "metric")

  return(sp_map)
}


# dataModal ----

#' Data Selection Modal Dialog
#'
#' Creates a multi-tabbed modal dialog for selecting species, oceanographic
#' variables, temporal filters, depth ranges, and spatial regions.
#'
#' @return Shiny modal dialog object with four tabs:
#'   \itemize{
#'     \item Species - selectizeInput for multiple species selection
#'     \item Oceanographic - variable and depth range selection
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
#'   showModal(dataModal())
#' })
#' }
#'
#' @seealso \code{\link{build_filter_summary}} for filter summary generation
#'
#' @importFrom shiny modalDialog selectizeInput selectInput numericRangeInput dateRangeInput modalButton tagList
#' @importFrom bslib navset_tab nav_panel
#' @importFrom maplibre maplibreOutput
#'
#' @export
dataModal <- function() {
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
        "Oceanographic",
        br(),
        selectInput(
          "sel_ocean_var",
          "Variable",
          ocean_var_choices,
          selected = "Temperature"
        ),

        numericRangeInput(
          "sel_depth_range",
          "Depth Range (m)",
          c(0,212), # TODO: pull from data
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
            Q4   = 4),
          selected = 1:4,
          multiple = T),

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
        "Draw a polygon to filter data by region, or leave blank to use all data.",
        maplibreOutput("spatial_filter_map", height = "400px")
      ),
    ),

    footer = tagList(
      modalButton("Cancel"),
      input_task_button("submit", "Submit")
    ),

    size = "m",
    fade = FALSE
  )
}


# depthProfileModal ----

#' Depth Profile Modal Dialog
#'
#' Creates a modal dialog for defining a transect line segment and buffer
#' distance to generate oceanographic depth profiles.
#'
#' @param sp_map maplibre object (currently unused in implementation, retained for future enhancement)
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
#'   showModal(depthProfileModal(sp_map = NULL))
#' })
#' }
#'
#' @seealso \code{\link{create_buffer}} for transect buffer generation
#'
#' @importFrom shiny modalDialog numericInput modalButton tagList
#' @importFrom maplibre maplibreOutput
#'
#' @export
depthProfileModal <- function(sp_map) {
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


# make_ocean_ts ----

#' Create Oceanographic Time Series Data
#'
#' Aggregates oceanographic data by temporal resolution, computing mean and
#' standard error for visualization in time series plots.
#'
#' @param ocean_data data.table or data.frame with columns: \code{datetime}, \code{Qty}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return data.table with columns:
#'   \itemize{
#'     \item \code{time} - aggregated time value
#'     \item \code{avg} - mean of \code{Qty}
#'     \item \code{std} - standard error of \code{Qty} (sd/n)
#'     \item \code{upr} - upper confidence bound (avg + std)
#'     \item \code{lwr} - lower confidence bound (avg - std)
#'   }
#'
#' @details
#' For seasonal plots (\code{ts_res = "quarter"}), the function adds a wrapping
#' row to ensure visual continuity across the year boundary.
#'
#' @examples
#' \dontrun{
#' ocean_data <- data.table(datetime = seq.Date(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "month"),
#'                          Qty = rnorm(252, mean = 15, sd = 2))
#' ocean_ts <- make_ocean_ts(ocean_data, ts_res = "year")
#' }
#'
#' @seealso \code{\link{ocean_time_mutate_expr}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom lubridate floor_date month yday year<-
#' @importFrom dplyr filter mutate bind_rows
#'
#' @export
make_ocean_ts <- function(ocean_data, ts_res) {
  ocean_ts_data <- ocean_data[, time := eval(ocean_time_mutate_expr(ts_res))
                              # calculate average and standard deviation
                              ][, .(avg = mean(Qty, na.rm = TRUE),
                                    std = sd(Qty, na.rm = TRUE)/.N), by = .(time)
                                # create upper and lower bounds
                                ][, `:=`(upr = avg + std, lwr = avg - std)
                                  ][, .(time, avg, lwr, upr, std)]

  # Add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    ocean_ts_data <- ocean_ts_data |>
      bind_rows(
        ocean_ts_data |>
          filter(
            time == as.POSIXct("2000-01-01", tz="UTC")
          ) |>
          mutate(
            time = as.POSIXct("2001-01-01", tz="UTC")
          )
      )
  }

  return(ocean_ts_data)
}


# make_sp_ts ----

#' Create Species Time Series Data
#'
#' Aggregates species abundance data by temporal resolution, computing mean and
#' standard error for visualization in time series plots.
#'
#' @param sp_data dbplyr lazy table or data.frame with columns: \code{time_start}, \code{name}, \code{std_tally}
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
#' sp_data <- sp_retrieve("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_ts <- make_sp_ts(sp_data, ts_res = "year")
#' }
#'
#' @seealso \code{\link{sp_time_mutate_expr}} for temporal transformation logic
#' @seealso \code{\link{plot_ts}} for visualization
#'
#' @importFrom dplyr mutate group_by summarize collect filter bind_rows
#'
#' @export
make_sp_ts <- function(sp_data, ts_res) {

  sp_ts_data <- sp_data |>
    mutate(
      time = !!sp_time_mutate_expr(ts_res)
    ) |>
    group_by(time, name) |>
    summarize(
      avg = mean(std_tally, na.rm = TRUE),
      std = sd(std_tally, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(
      upr = avg + std/n,
      lwr = avg - std/n,
      std = std/n
    ) |>
    collect()

  # Add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    sp_ts_data <- sp_ts_data |>
      bind_rows(
        sp_ts_data |>
          filter(
            time == as.Date("2000-01-01")
          ) |>
          mutate(
            time = time + 366
          )
      )
  }

  return(sp_ts_data)
}


# map_ocean_hex ----

#' Aggregate Oceanographic Data into H3 Hexagons
#'
#' Converts oceanographic point data into multi-resolution H3 hexagonal bins
#' with aggregated statistics and geometries for mapping.
#'
#' @param ocean_data data.table or data.frame with H3 index columns (\code{hex_h3res*}) and \code{Qty} column
#' @param res_range Integer vector of H3 resolution levels to generate (e.g., 3:5)
#' @param ocean_stat Character string specifying aggregation function: "mean", "median", "min", "max", etc.
#'
#' @return List of data.table objects, one per resolution level, each with columns:
#'   \itemize{
#'     \item \code{resolution} - H3 resolution level
#'     \item \code{hex_id} - H3 hexagon identifier
#'     \item \code{ocean.value} - aggregated oceanographic value
#'     \item \code{geometry} - sf geometry (hexagon polygon)
#'     \item \code{tooltip} - rounded value for display
#'   }
#'
#' @details
#' This function uses data.table's melt/reshape operations for efficient
#' multi-resolution aggregation. Geometries are joined from a pre-computed
#' lookup table (\code{hex_geo_dt}).
#'
#' @examples
#' \dontrun{
#' ocean_data <- ocean_retrieve("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' ocean_hex <- map_ocean_hex(ocean_data, res_range = 3:5, ocean_stat = "mean")
#' }
#'
#' @seealso \code{\link{create_ocean_map}} for visualization
#' @seealso \code{\link{ocean_retrieve}} for data retrieval
#'
#' @importFrom data.table melt setnames
#' @importFrom dplyr group_split
#' @importFrom sf st_sf
#'
#' @export
map_ocean_hex <- function(ocean_data, res_range, ocean_stat) {
  # define the columns to melt
  # h3_cols <- paste0("hex_h3_res", res_range)
  h3_cols <- paste0("hex_h3res", res_range)

  # melt the data.table from wide to long format
  # ocean_long_dt <- melt(ocean_data,
  #                       id.vars = "Qty",
  #                       measure.vars = h3_cols,
  #                       variable.name = "resolution",
  #                       value.name = "hex_id")

  browser()

  # tidy up the resolution column
  ocean_hex <- ocean_long_dt[, resolution := as.integer(gsub("hex_h3_res", "", resolution))
                # perform aggregation
                ][!is.na(hex_id),
                  .(ocean.value = get(ocean_stat)(Qty, na.rm = TRUE)),
                  by = .(resolution, hex_id)
                  # add geometries
                  ][hex_geo_dt, on = .(hex_id), geometry := geometry
                    # add tooltip text
                    ][, tooltip := round(ocean.value, 2)]

  ocean_hex <- st_sf(ocean_hex)

  # split into list
  ocean_hex_list <- group_split(ocean_hex, resolution)

  return(ocean_hex_list)
}


# map_sp_hex ----

#' Aggregate Species Data into H3 Hexagons
#'
#' Converts species occurrence/abundance data into multi-resolution H3 hexagonal
#' bins with aggregated statistics and geometries for mapping.
#'
#' @param sp_data dbplyr lazy table with columns: \code{hex_h3res*}, \code{std_tally}
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
#' sp_data <- sp_retrieve("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' sp_hex <- map_sp_hex(sp_data, res_range = 3:5)
#' }
#'
#' @seealso \code{\link{create_sp_map}} for visualization
#' @seealso \code{\link{sp_retrieve}} for data retrieval
#'
#' @importFrom dplyr select mutate group_by summarize filter collect left_join group_split
#' @importFrom purrr map reduce
#' @importFrom glue glue
#' @importFrom dbplyr compute
#'
#' @export
map_sp_hex <- function(sp_data, res_range) {
  # precompute and store joins in a temporary table

  sp_data_temp <- sp_data |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{ # hex_int std_tally resolution
    hex_fld <- glue("hex_h3res{.x}")

    sp_data_temp |>
      select(hex_int = all_of(hex_fld), std_tally) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  hex_sp <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value = mean(std_tally, na.rm = TRUE),
      .groups = "drop") |>
    filter(
      !is.na(hex_int),
      !is.na(sp.value)) |>
    mutate(
      hex_id  = sql("HEX(hex_int)"),
      # hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)"),
      tooltip = round(sp.value, 2) ) |>
    # select(resolution, hexid = hex_id, sp.value, hex_wkt, tooltip) |>
    select(resolution, hexid = hex_id, sp.value, tooltip) |>
    collect() |>
    # st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    left_join(
      sf_hex |>
        select(hexid = hex_id, hex_res, geometry), # hex_id hex_res
      join_by(
        hexid,
        resolution == hex_res ) ) |>
    group_split(resolution)

  return(hex_sp)
}


# ocean_retrieve ----

#' Retrieve Oceanographic Data from Database
#'
#' Queries oceanographic bottle cast data with temporal, depth, and variable filters.
#' Returns a dbplyr lazy table for efficient downstream processing.
#'
#' @param ocean_var Character string of database column name for oceanographic variable (e.g., "t_deg_c", "salinity")
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
#'     \item \code{qty} - renamed oceanographic variable value
#'     \item \code{hex_h3res*} - H3 hexagon indices at multiple resolutions
#'   }
#'
#' @details
#' The function joins \code{cast} and \code{bottle} tables, then joins with
#' \code{site} to obtain H3 spatial indices. Only records with non-NA values
#' for the selected variable are returned. Datetime is constructed from separate
#' date and time fields.
#'
#' @examples
#' \dontrun{
#' # retrieve temperature data for Q1-Q2 2010-2020
#' ocean_data <- ocean_retrieve(
#'   ocean_var  = "t_deg_c",
#'   qtr        = c(1, 2),
#'   date_range = as.Date(c("2010-01-01", "2020-12-31")),
#'   min_depth  = 0,
#'   max_depth  = 100
#' )
#' ocean_data |> collect()
#' }
#'
#' @seealso \code{\link{map_ocean_hex}} for spatial aggregation
#' @seealso \code{\link{make_ocean_ts}} for temporal aggregation
#'
#' @importFrom dplyr tbl left_join rename filter mutate select starts_with
#' @importFrom dbplyr sql join_by
#'
#' @export
ocean_retrieve <- function(ocean_var, qtr, date_range, min_depth, max_depth) {
  # TODO: rename to env_query()

  # ocean_var = "t_deg_c"; qtr = c(1:4) |> as.character();  date_range = c("1949-02-28", "2023-01-25") |> as.Date(); min_depth = 0; max_depth = 212

  # if (!is.data.table(ocean_subset)) {
  #   ocean_subset <- as.data.table(ocean_subset)
  # }
  #
  # filter for selected quarter and dates
  #ocean_data <- ocean_subset[Quarter %in% qtr & datetime <= date_range[2] & datetime >= date_range[1] & Depthm >= min_depth & Depthm <= max_depth]
  # rename selected variable
  #setnames(ocean_data, ocean_var, "qty")
  # drop non-selected variables
  #ocean_data <- ocean_data[!is.na(Qty),-unname(unlist(setdiff(ocean_var_choices, ocean_var))), with = FALSE]

  # dbListFields(con, "cast") |> sort()
  # dbListFields(con, "bottle") |> sort()

  # browser()
  ocean_data <- tbl(con, "cast") |> # 582,823 × 5 -> 5,569,925 × 15
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
      qty = all_of(ocean_var)) |>
    filter(
      !is.na(qty),
      between(depthm, min_depth, max_depth),
      between(date, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      # datetime (time is in seconds since midnight)
      dtime = sql("CAST(date AS TIMESTAMP) + INTERVAL (time || ' second')")) |>
    # TODO: set dtime at ingest of db with timezone UTC
    select(
      date,
      time,
      dtime,
      depthm,
      lat_dec,
      lon_dec,
      qty,
      starts_with("hex_h3res") ) # |>
    # slice_min(lat_dec, n = 10) # DEBUG
    # collect()
  # TODO: consider summarizing by hex_h3res and filter by h3res
  # TODO: skip h3 geom by using mapgl::add_h3j_source() with maplibre https://walker-data.com/mapgl/reference/add_h3j_source.html

  return(ocean_data)
}


# ocean_time_mutate_expr ----

#' Generate Time Aggregation Expression for Oceanographic Data
#'
#' Creates a \code{lubridate}-based expression for temporal aggregation of
#' oceanographic time series. Used internally by \code{\link{make_ocean_ts}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} or data.table
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting.
#'
#' @examples
#' \dontrun{
#' ocean_data[, time := eval(ocean_time_mutate_expr("year"))]
#' }
#'
#' @seealso \code{\link{make_ocean_ts}} for usage context
#'
#' @importFrom lubridate floor_date month yday year<-
#' @importFrom rlang expr
#'
#' @keywords internal
ocean_time_mutate_expr <- function(ts_res) {
  switch(ts_res,
         "year"    = expr(floor_date(datetime, "year")),
         "quarter" = expr(`year<-`(floor_date(datetime, "quarter"), 2000)),
         "month"   = expr(month(datetime)),
         "day"     = expr(yday(datetime)),
         "year_quarter" = expr(floor_date(datetime, "quarter")),
         "year_month" = expr(floor_date(datetime, "month")),
         "year_day" = expr(floor_date(datetime, "day"))
  )
}


# placeholder_message ----

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
#'   placeholder_message("No Data Selected", "Please select species from the filter menu.")
#' })
#' }
#'
#' @importFrom shiny div h4 p
#'
#' @export
placeholder_message <- function(title, message) {
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

# plot_ts ----

#' Create Dual-Panel Time Series Plot
#'
#' Generates an interactive Highcharts plot with species abundance and
#' oceanographic data in separate panels, with synchronized x-axis zoom and
#' resolution-dependent date formatting.
#'
#' @param sp_ts data.frame from \code{\link{make_sp_ts}} with columns: \code{time}, \code{name}, \code{avg}, \code{std}, \code{lwr}, \code{upr}
#' @param ocean_ts data.frame from \code{\link{make_ocean_ts}} with columns: \code{time}, \code{avg}, \code{std}, \code{lwr}, \code{upr}
#' @param ts_res Character string specifying temporal resolution: "year", "quarter", "month", "day", etc.
#' @param sel_ocean_var Character string of oceanographic variable column name (e.g., "t_deg_c")
#'
#' @return highchart object with dual y-axes, line + ribbon series, and zoom capabilities
#'
#' @details
#' The function creates a two-panel plot with species on top and oceanographic
#' data on bottom. Date formatting adapts to \code{ts_res}. Standard error ribbons
#' are displayed as \code{arearange} series. Tooltips show values ± standard error.
#'
#' @examples
#' \dontrun{
#' sp_ts <- make_sp_ts(sp_data, ts_res = "year")
#' ocean_ts <- make_ocean_ts(ocean_data, ts_res = "year")
#' plot_ts(sp_ts, ocean_ts, ts_res = "year", sel_ocean_var = "t_deg_c")
#' }
#'
#' @seealso \code{\link{make_sp_ts}} for species time series data
#' @seealso \code{\link{make_ocean_ts}} for oceanographic time series data
#'
#' @importFrom highcharter highchart hc_chart hc_exporting hc_xAxis hc_yAxis_multiples hc_tooltip hc_rangeSelector hc_plotOptions hc_legend hc_add_series datetime_to_timestamp
#' @importFrom dplyr mutate bind_rows arrange distinct filter
#'
#' @export
plot_ts <- function(sp_ts, ocean_ts, ts_res, sel_ocean_var) {
  # Add a 'panel' and consistent 'name' column to each dataset
  sp_ts_mod <- sp_ts |>
    mutate(panel_id = 0) # Assign to the first (top) panel

  ocean_ts_mod <- ocean_ts |>
    mutate(name = names(which(ocean_var_choices == sel_ocean_var)),
           panel_id = 1) # Assign to the second (bottom) panel

  # Combine into a single data frame
  combined_data <- bind_rows(sp_ts_mod, ocean_ts_mod) |>
    arrange(time) |>
    mutate(time_ts = datetime_to_timestamp(time))

  # Get a list of the unique series to create
  series_list <- combined_data |>
    distinct(name, panel_id)

  # Define formatters based on temporal resolution
  if (ts_res == "year") {
    tooltip_date_format <- "function(timestamp) { return Highcharts.dateFormat('%Y', timestamp); }"
    xaxis_label_format <- "{value:%Y}"
  } else if (ts_res == "quarter") {
    tooltip_date_format <- "function(timestamp) {
      var quarter = Math.ceil((new Date(timestamp).getUTCMonth() + 1) / 3);
      return 'Q' + quarter;
    }"
    xaxis_label_format <- NULL  # Use custom formatter
  } else if (ts_res == "year_quarter") {
    tooltip_date_format <- "function(timestamp) {
      var quarter = Math.ceil((new Date(timestamp).getUTCMonth() + 1) / 3);
      return Highcharts.dateFormat('%Y', timestamp) + ' Q' + quarter;
    }"
    xaxis_label_format <- NULL  # Use custom formatter
  } else {
    # Default for other resolutions
    tooltip_date_format <- "function(timestamp) { return Highcharts.dateFormat('%b %e, %Y', timestamp); }"
    xaxis_label_format <- "{value:%b %e, %Y}"
  }

  # Initialize the chart with its layout
  hc <- highchart() |>
    hc_chart(zoomType = "x") |>
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          enabled = FALSE # Disables the default hamburger menu
        )
      )
    ) |>
    hc_xAxis(type = "datetime", crosshair = TRUE) |>
    hc_yAxis_multiples(
      list(title = list(text = "Average Species Abundance"), height = "47%", top = "0%", offset = 0),
      list(title = list(text = paste0("Average ", names(which(ocean_var_choices == sel_ocean_var)))), height = "47%", top = "53%", offset = 0)
    )

  # Configure xAxis based on resolution
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

  # Configure tooltip
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

  # Loop through each series to add its line and ribbon
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

  # Display the final chart
  return(hc)
}

# splot_prep ----

#' Prepare Data for Species-Environment Scatterplot
#'
#' Joins species and oceanographic data by matching observations that are close
#' in time and space, enabling correlation analysis between abundance and
#' environmental variables.
#'
#' @param sp_data dbplyr lazy table or data.frame with species data
#' @param ocean_data dbplyr lazy table or data.frame with oceanographic data
#' @param ocean_stat Character string specifying aggregation function (e.g., "mean", "median")
#' @param max_hours_diff Numeric maximum time difference (in hours) for matching observations (default: 72)
#' @param max_meters_diff Numeric maximum spatial distance (in meters) for matching observations (default: 1000)
#'
#' @return data.frame with matched species-environment observations
#'
#' @details
#' This function performs a fuzzy join based on temporal proximity using
#' \code{fuzzyjoin::difference_inner_join()}. For each species observation,
#' the closest environmental measurement (within \code{max_hours_diff}) is
#' selected.
#'
#' @examples
#' \dontrun{
#' sp_data <- sp_retrieve("Anchovy (Engraulis mordax)", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"))
#' ocean_data <- ocean_retrieve("t_deg_c", qtr = 1:4, date_range = c("2000-01-01", "2020-12-31"), min_depth = 0, max_depth = 100)
#' splot_data <- splot_prep(sp_data, ocean_data, ocean_stat = "mean")
#' }
#'
#' @seealso \code{\link{sp_retrieve}} for species data retrieval
#' @seealso \code{\link{ocean_retrieve}} for oceanographic data retrieval
#'
#' @importFrom dplyr select collect mutate group_by slice_min ungroup
#' @importFrom fuzzyjoin difference_inner_join
#'
#' @export
splot_prep <- function(sp_data, ocean_data, ocean_stat, max_hours_diff = 72, max_meters_diff = 1000) {
  # convert species data to a data.table
  # sp_dt <- sp_data |>
  #   as.data.table()

  # subset columns and convert time to Date format
  # sp_dt <- sp_dt[, c("name", "std_tally", "time_start", "longitude", "latitude")][
  #   , time_start := as.Date(time_start)
  # ]
  d_sp <- sp_data |>
    # mutate(
    #   date = as.Date(time_start)) |>
    select(
      sp_name  = name,
      sp_dtime = time_start,
      sp_tally = std_tally,
      sp_lon   = longitude,
      sp_lat   = latitude) |>
    collect() |>
    mutate(
      sp_dtime_num = as.numeric(sp_dtime))

  # ocean_dt <- ocean_data[, c("datetime", "Qty", "Depthm", "Lat_Dec", "Lon_Dec")][
  #   , datetime := as.Date(datetime)
  # ]
  d_env <- ocean_data |>
    select(
      env_dtime  = dtime,
      env_qty    = qty,
      env_depth  = depthm,
      env_lat    = lat_dec,
      env_lon    = lon_dec) |>
    collect() |>
    mutate(
      env_dtime_num = as.numeric(env_dtime))

  # join data by date
  # joined_by_time <- sp_dt[ocean_dt,
  #                         on = .(time_start = datetime),
  #                         allow.cartesian = TRUE, # Necessary for one-to-many matches
  #                         nomatch = 0 # Ensures we only keep rows that have a match
  # ]
  # NEW: dplyr
  browser()
  # join closest in time
  d_sp_env <- d_sp |>  #   7,420 × 7
    fuzzyjoin::difference_inner_join(
      d_env,           # 582,823 × 7
      by = c("sp_dtime_num" = "env_dtime_num"),
      max_dist     = 60 * 60 * max_hours_diff,     # 1 day in seconds
      distance_col = "sec_diff") |>
    group_by(sp_name, sp_dtime) |>
    slice_min(
      order_by  = sec_diff,
      n         = 1,
      with_ties = FALSE) |>
    ungroup()          #   5,663 × 13

  d_sp_env_by_date <- d_sp |>
    inner_join(d_env, by = "date")

  # rename columns for clarity after the join
  setnames(joined_by_time, c("longitude", "latitude"),
           c("sp_lon", "sp_lat"))
  setnames(joined_by_time, c("lon_dec", "lat_dec"),
           c("ocean_lon", "ocean_lat"))

  # compute distances
  joined_by_time[, distance_m := distHaversine(
    p1 = cbind(sp_lon, sp_lat),
    p2 = cbind(ocean_lon, ocean_lat)
  )]

  # filter based on distance and aggregate
  splot_data <- joined_by_time[
    distance_m <= dist_within, .(Qty = get(ocean_stat)(Qty, na.rm = TRUE)),
    by = .(name, std_tally, time_start, sp_lon, sp_lat)
  ]

  return(splot_data)
}

# split_at_dateline ----

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
#' normalized <- split_at_dateline(segment)
#' }
#'
#' @seealso \code{\link{create_buffer}} for usage in buffering workflow
#'
#' @importFrom sf st_coordinates st_sf st_sfc st_linestring st_segmentize
#' @importFrom units set_units
#'
#' @keywords internal
split_at_dateline <- function(segment) {
  coords <- st_coordinates(segment)[, c("X", "Y")]
  lons <- coords[, "X"]

  # Check for dateline crossing (large longitude jump)
  lon_diff <- diff(lons)
  crosses_dateline <- any(abs(lon_diff) > 180)

  if (!crosses_dateline) return(segment)

  # Normalize longitudes to avoid discontinuity
  # Shift coords to a 0-360 range if crossing +180/-180
  if (any(lons < 0)) {
    coords[, "X"] <- ifelse(lons < 0, lons + 360, lons)
  }

  # Create new linestring
  new_segment <- st_sf(st_sfc(st_linestring(coords), crs = 4326))

  # Optional: Split into multiple segments if needed
  # Use st_segmentize to add points across dateline for smoother buffer
  new_segment <- st_segmentize(new_segment, set_units(1000, "m"))

  return(new_segment)
}

# sp_retrieve ----

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
#' sp_data <- sp_retrieve(
#'   sp_name    = "Anchovy (Engraulis mordax)",
#'   qtr        = 1:4,
#'   date_range = as.Date(c("2010-01-01", "2020-12-31"))
#' )
#' sp_data |> collect()
#' }
#'
#' @seealso \code{\link{map_sp_hex}} for spatial aggregation
#' @seealso \code{\link{make_sp_ts}} for temporal aggregation
#'
#' @importFrom dplyr tbl mutate filter left_join
#' @importFrom lubridate quarter
#'
#' @export
sp_retrieve <- function(sp_name, qtr, date_range) {

  sp_data <- tbl(con, "species") |>
    mutate(
      name = paste0(common_name, " (", scientific_name, ")") ) |>
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
      quarter = quarter(time_start) ) |>
    filter(
      !is.na(tally),
      between(time_start, !!date_range[1], !!date_range[2]),
      quarter %in% qtr) |>
    mutate(
      std_tally = std_haul_factor * tally / prop_sorted )

  return(sp_data)
}


# sp_time_mutate_expr ----

#' Generate Time Aggregation Expression for Species Data
#'
#' Creates a SQL-based expression for temporal aggregation of species time series
#' using DuckDB date functions. Used internally by \code{\link{make_sp_ts}}.
#'
#' @param ts_res Character string specifying temporal resolution: "year", "quarter",
#'   "month", "day", "year_quarter", "year_month", or "year_day"
#'
#' @return Expression object suitable for use in \code{dplyr::mutate()} with dbplyr
#'
#' @details
#' For seasonal aggregation (\code{ts_res = "quarter"}), all quarters are
#' normalized to year 2000 to enable cyclic plotting. Uses DuckDB's
#' \code{datetrunc()} and \code{extract()} functions for database-side computation.
#'
#' @examples
#' \dontrun{
#' sp_data |> mutate(time = !!sp_time_mutate_expr("year"))
#' }
#'
#' @seealso \code{\link{make_sp_ts}} for usage context
#'
#' @importFrom rlang expr
#' @importFrom dbplyr sql
#'
#' @keywords internal
sp_time_mutate_expr <- function(ts_res) {
  switch(ts_res,
         "year"    = expr(sql("datetrunc('year', time_start)")),
         "quarter" = expr(sql("make_date(2000, month(datetrunc('quarter',time_start)), day(datetrunc('quarter',time_start)))")),
         "month"   = expr(sql("extract('month' FROM time_start)")),
         "day"     = expr(sql("extract('doy' FROM time_start)")),
         "year_quarter" = expr(sql("datetrunc('quarter',time_start)")),
         "year_month" = expr(sql("datetrunc('month',time_start)")),
         "year_day" = expr(sql("datetrunc('day',time_start)"))
  )
}
