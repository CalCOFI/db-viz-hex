# Helper function for placeholder messages ----
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

# Modal dialog for data selection
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
          c(0,212),
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

# Depth profile modal dialog
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

# species data retrieval function
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

# Helper: Build filter summary
build_filter_summary <- function(sel_name, sel_ocean_var, sel_qtr, sel_date_range,
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
  filter_list <- c(filter_list, paste0("**Variable:** ", names(which(ocean_var_choices == sel_ocean_var))))

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

# oceanographic data retrieval
ocean_retrieve <- function(ocean_var, qtr, date_range, min_depth, max_depth) {

  # browser()
  # ocean_var = "t_deg_c"

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
    select(
      date,
      time,
      depthm,
      lat_dec,
      lon_dec,
      qty,
      starts_with("hex_h3res") )

  # TODO: consider summarizing by hex_h3res and filter by h3res

  return(ocean_data)
}

# hexagon mapping functions
map_sp_hex <- function(sp_data, res_range) {
  # precompute and store joins in a temporary table
  sp_data_temp <- sp_data |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{
    hex_fld <- glue("hex_h3res{.x}")

    sp_data_temp |>
      select(hex_int = all_of(hex_fld), std_tally) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  sp_hex_list <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      sp.value = mean(std_tally, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(!is.na(hex_int)) |>
    mutate(hex_id = sql("HEX(hex_int)")) |>
    mutate(
      hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)"),
      tooltip = round(sp.value, 2)
    ) |>
    select(resolution, hexid = hex_id, sp.value, hex_wkt, tooltip) |>
    collect() |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    group_split(resolution)

  return(sp_hex_list)
}

map_ocean_hex <- function(ocean_data, res_range, ocean_stat) {

  # precompute and store joins in a temporary table
  ocean_data_temp <- ocean_data |>
    compute()

  # create and combine tables for each resolution
  combined_res_tbl <- map(res_range, ~{
    hex_fld <- glue("hex_h3res{.x}")

    ocean_data_temp |>
      select(hex_int = all_of(hex_fld), qty) |>
      mutate(resolution = .x)
  }) |>
    reduce(union_all)

  # aggregate and convert to hex geometries
  ocean_hex_list <- combined_res_tbl |>
    group_by(resolution, hex_int) |>
    summarize(
      qty = (!!sym(ocean_stat))(qty, na.rm = TRUE),
      .groups = "drop") |>
    rename(
      ocean.value = "qty") |>
    filter(!is.na(hex_int)) |>
    mutate(hex_id = sql("HEX(hex_int)")) |>
    mutate(
      hex_wkt = sql("h3_cell_to_boundary_wkt(hex_id)"),
      tooltip = round(ocean.value, 2)
    ) |>
    select(resolution, hexid = hex_id, ocean.value, hex_wkt, tooltip) |>
    collect() |>
    st_as_sf(wkt = "hex_wkt", crs = 4326) |>
    group_split(resolution)

  return(ocean_hex_list)
}

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

# helper function for species conversion
time_mutate_expr <- function(ts_res, time_col) {
  switch(ts_res,
         "year"    = expr(sql(!!glue("datetrunc('year', {time_col})"))),
         "quarter" = expr(sql(!!glue("make_date(2000, month(datetrunc('quarter',{time_col})), day(datetrunc('quarter',{time_col})))"))),
         "month"   = expr(sql(!!glue("extract('month' FROM {time_col})"))),
         "day"     = expr(sql(!!glue("extract('doy' FROM {time_col})"))),
         "year_quarter" = expr(sql(!!glue("datetrunc('quarter',{time_col})"))),
         "year_month" = expr(sql(!!glue("datetrunc('month',{time_col})"))),
         "year_day" = expr(sql(!!glue("datetrunc('day',{time_col})")))
  )
}

# species time-series conversion function
make_sp_ts <- function(sp_data, ts_res) {

  sp_ts_data <- sp_data |>
    mutate(
      time = !!time_mutate_expr(ts_res, "time_start")) |>
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

  # Add rows to wrap dates for seasonal plot
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

# oceanographic time-series conversion function
make_ocean_ts <- function(ocean_data, ts_res) {

  ocean_ts_data <- ocean_data |>
    mutate(
      time = !!time_mutate_expr(ts_res, "date")) |>
    group_by(time) |>
    summarize(
      avg = mean(qty, na.rm = TRUE),
      std = sd(qty, na.rm = TRUE)/n()) |>
    mutate(
      upr = avg + std,
      lwr = avg - std) |>
    select(time, avg, std, lwr, upr) |>
    collect()

  # Add rows to wrap dates for seasonal plot
  if (ts_res == "quarter") {
    ocean_ts_data <- ocean_ts_data |>
      bind_rows(
        ocean_ts_data |>
          filter(
            time == as.Date("2000-01-01")) |>
          mutate(
            time = time + 366))
  }

  return(ocean_ts_data)
}

# time-series plotting function
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

# data prep function for scatterplot
splot_prep <- function(sp_data, ocean_data, ocean_stat,
                       time_within = 1, dist_within = 1000) {

  # time_within: hours
  # dist_within: meters

  sp_data_temp <- sp_data |>
    select(
      name, std_tally, time_start, longitude, latitude)

  ocean_data_temp <- ocean_data |>
    select(
      date, time, qty, depthm, lat_dec, lon_dec) |>
    mutate(
      datetime = as.POSIXct((paste(date, time)))) |>
    mutate(
      datetime_upr = sql(paste("datetime + INTERVAL", time_within, "HOUR")),
      datetime_lwr = sql(paste("datetime - INTERVAL", time_within, "HOUR")))

  splot_data <- left_join(
    sp_data_temp, ocean_data_temp,
    # join species to ocean observations within desired time interval
    by = join_by(between(time_start, datetime_lwr, datetime_upr))) |>
  rename(
    sp_lon   = "longitude",
    sp_lat    = "latitude",
    ocean_lon = "lon_dec",
    ocean_lat = "lat_dec") |>
  # compute distance between species and ocean observations
  mutate(
    distance_m = sql("ST_Distance_Sphere(ST_Point(sp_lon,sp_lat),ST_Point(ocean_lon,ocean_lat))")) |>
  # get pairs within desired distance
  filter(
    distance_m <= dist_within) |>
  group_by(
    name, std_tally, time_start, sp_lon, sp_lat) |>
  # summarize ocean observations for each individual species observation
  summarize(
    qty = (!!sym(ocean_stat))(qty, na.rm = TRUE))

  return(splot_data)
}

# Function to detect and split line at the dateline
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

# Main buffering function
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
