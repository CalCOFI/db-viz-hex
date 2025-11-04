server <- function(input, output, session) {

  # thematic for bslib theming ----
  thematic::thematic_shiny()

  # tour ----
  if (is_tour_on)
    tour$init()$start()

  # rx ----
  rx <- reactiveValues(
    df_sp          = NULL,
    df_env         = NULL,
    env_var        = NULL,  # stores the env_var code (e.g., "t_deg_c")
    lbl_env_var    = NULL,  # stores the label (e.g., "Temperature (ºC)")
    sel_zones      = NULL,
    map_sp         = NULL,
    df_splot       = NULL,
    df_dprof       = NULL,
    filter_summary = NULL,
    summary_stats  = NULL,
    plot_depth     = NULL)

  # session.once -> ... ----
  observeEvent(session$clientData, once = TRUE, {
    tryCatch({
      if (debug) message("\n=== LOADING DEFAULT DATA ===")

      # default selections
      sel_name        <- default_sp_name
      sel_env_var     <- "t_deg_c"
      sel_qtr         <- 1:4
      sel_date_range  <- min_max_date
      sel_depth_range <- c(0, 212)

      if (debug) message("Loading default species: ", sel_name)

      # retrieve data (lazy tables from database)
      df_sp  <- get_sp(sel_name, sel_qtr, sel_date_range)
      df_env <- get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2])

      # validate data
      n_sp <- df_sp |> summarize(n = n()) |> pull(n)
      if (debug) message("Default species data: found", n_sp, "rows\n")

      if (n_sp == 0) {
        if (debug) message("WARNING: No data found for default species\n")
        return(NULL)
      }

      # store shared data
      rx$df_sp       <- df_sp
      rx$df_env      <- df_env
      rx$env_var     <- sel_env_var
      rx$lbl_env_var <- names(which(env_var_choices == sel_env_var))

      # build filter summary
      rx$filter_summary <- prep_filter_summary(
        sel_name,
        sel_env_var,
        sel_qtr,
        sel_date_range,
        sel_depth_range,
        drawn_polygon = NULL,
        rx$sel_zones)

      # summary statistics
      rx$summary_stats <- prep_summary_stats(
        rx$df_sp,
        rx$df_env
      )

      # generate species map
      if (debug) message("Generating default species map...\n")
      sp_hex_list   <- prep_sp_hex(df_sp, res_range)
      sp_scale_list <- lapply(
        sp_hex_list,
        interpolate_palette,
        column  = "sp.value",
        palette = \(n) hcl.colors(n, palette = "Viridis"))
      rx$map_sp <- map_sp(sp_hex_list, sp_scale_list)
      if (debug) message("Default species map generated and stored in rx$map_sp\n")

      # reset depth profile
      rx$plot_depth <- NULL

      if (debug) message("=== DEFAULT DATA LOADED ===\n\n")
    }, error = function(e) {
      message("ERROR in default data initialization: ", conditionMessage(e))
      traceback()
    })
  })

  # map_content ----
  output$map_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      div(
        style = "width: 100%; height: 100%; position: relative;",
        maplibreCompareOutput("map", width = "100%", height = "100%")
      )
    }
  })

  # ts_content ----
  output$ts_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      highchartOutput("ts_plot", height = "100%")
    }
  })

  # splot_content ----
  output$splot_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else {
      plotlyOutput("splot", height = "100%")
    }
  })

  # dprof_content ----
  output$dprof_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Click 'Data Selection' in the sidebar to begin exploring CalCOFI data."
      )
    } else if (is.null(rx$plot_depth)) {
      ui_placeholder(
        "No Depth Profile Generated",
        "Click 'Draw Transect' in the sidebar to create a depth profile."
      )
    } else {
      plotlyOutput("dprof_plot", height = "100%")
    }
  })

  # map ----
  output$map <- renderMaplibreCompare({
    req(rx$df_env, rx$map_sp)

    if (debug) message("renderMaplibreCompare: generating environmental map...\n")

    env_stat <- input$sel_env_stat %||% "mean"
    env_stat_label <- names(which(env_stat_choices == env_stat))
    env_hex_list   <- prep_env_hex(rx$df_env, res_range, env_stat)
    env_scale_list <- lapply(
      env_hex_list,
      interpolate_palette,
      column  = "env.value",
      palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
    map_env_obj <- map_env(
      env_hex_list,
      env_scale_list,
      env_stat_label,
      rx$lbl_env_var)

    if (debug) {
      message("renderMaplibreCompare: creating comparison map")
      message("rx$map_sp class: ", paste(class(rx$map_sp), collapse = ", "))
      message("map_env_obj class: ", paste(class(map_env_obj), collapse = ", "))
    }

    compare(rx$map_sp, map_env_obj, elementId = "map")
  })

  # dark_toggle -> map.style ----
  observeEvent(input$dark_toggle, {
    style  <- ifelse(
      input$dark_toggle == "dark",
      "dark-matter",
      "voyager")

    if (debug)
      message("maplibre_compare_proxy -> set_style: ", style)

    maplibre_compare_proxy("map", map_side = "before") |>
      set_style(carto_style(style))

    maplibre_compare_proxy("map", map_side = "after") |>
      set_style(carto_style(style))
  })

  # ts_plot ----
  output$ts_plot <- renderHighchart({
    req(rx$df_sp, rx$df_env, rx$env_var)

    if (debug) message("renderHighchart: generating time series plot\n")

    ts_res <- input$sel_ts_res %||% "year"
    sp_ts  <- prep_ts_sp(rx$df_sp, ts_res) |> arrange(time)
    env_ts <- prep_ts_env(rx$df_env, ts_res)

    plot_ts(sp_ts, env_ts, ts_res, rx$env_var)
  })

  # splot ----
  output$splot <- renderPlotly({
    df_splot <- prep_splot(rx$df_sp, rx$df_env, "mean",
                           method = input$splot_method,
                           max_hours_diff = input$splot_max_hours_diff,
                           max_meters_diff = input$splot_max_meters_diff)
    rx$df_splot <- df_splot

    req(rx$df_splot)

    if (debug) message("renderPlotly: generating scatterplot with ggplotly\n")

    # prepare data with customdata and hover text for plotly
    df_plot <- rx$df_splot |>
      collect() |>
      mutate(
        customdata = 1:n(),
        hover_text = paste0(
          "<b>Date:</b> ", sp_dtime,
          "<br><b>Species:</b> ", sp_name,
          "<br><b>", rx$lbl_env_var, ":</b> ", round(env_qty, 2),
          "<br><b>Abundance:</b> ", round(sp_tally, 2) ))

    # create ggplot (thematic will apply bslib theme automatically)
    p <- ggplot(
      df_plot,
      aes(
        x          = env_qty,
        y          = sp_tally,
        color      = sp_name,
        text       = hover_text,
        customdata = customdata)) +
      geom_point(size = 3, alpha = 0.6) +
      labs(
        x     = rx$lbl_env_var,
        y     = "Species Abundance",
        color = "Species") +
      theme(legend.position="none") # +
    # theme_minimal()

    # convert to plotly with bslib theme support
    ggplotly(p, tooltip = "text", source = "scatterPlotSource") |>
      layout(dragmode = "select") |>
      config(
        displaylogo            = FALSE,
        scrollZoom             = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian") ) |>
      toWebGL() # for performance
  })

  # sel_data -> spatial_filter_map ----
  observeEvent(input$sel_data, {
    showModal(modal_data())
    updateSelectizeInput(session, 'sel_name', choices = sp_names, server = TRUE)

    output$spatial_filter_map <- renderMaplibre({
      maplibre(
        style = carto_style('positron'),
        bounds = cc_grid_zones) |>
        add_fill_layer(
          id = 'base-zones',
          cc_grid_zones,
          fill_color = match_expr(
            "zone_key",
            values = unique(cc_grid_zones$zone_key),
            stops = hcl.colors(length(unique(cc_grid_zones$zone_key)))),
          fill_opacity = 0.5,
          fill_outline_color = "black") |>
        add_fill_layer(
          id = 'sel-zones',
          cc_grid_zones,
          fill_color = match_expr(
            "zone_key",
            values = unique(cc_grid_zones$zone_key),
            stops = hcl.colors(length(unique(cc_grid_zones$zone_key)))),
          fill_opacity = 0.0,
          fill_outline_color = NULL) |>
        add_line_layer(
          id = 'sel-zones-outline',
          cc_grid_zones,
          line_color = 'black',
          line_width = 3,
          line_opacity = 0.0) |>
        add_categorical_legend(
          legend_title = "Zone Key",
          values = unique(cc_grid_zones$zone_key),
          colors = hcl.colors(length(unique(cc_grid_zones$zone_key)))) |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(polygon = TRUE, trash = TRUE))})
  })

  # Observe clicks on the grid layer of spatial filter map
  observeEvent(input$spatial_filter_map_feature_click, {

    click <- input$spatial_filter_map_feature_click

    # Only process clicks on the grid layer
    if (!is.null(click$properties$zone_key) ) {
      clicked_zone <- click$properties$zone_key
      current_zones <- rx$sel_zones

      # Toggle zone selection
      if (clicked_zone %in% current_zones) {
        # Remove if already selected
        new_zones <- setdiff(current_zones, clicked_zone)
      } else {
        # Add to selection
        new_zones <- c(current_zones, clicked_zone)
      }

      rx$sel_zones <- new_zones

      # Update map styling to highlight selected zones
      if (length(new_zones) > 0) {
        maplibre_proxy("spatial_filter_map") |>
          set_filter("sel-zones",
                     list("in", list("get", "zone_key"), list("literal", new_zones))) |>
          set_paint_property("sel-zones", "fill-opacity", 0.8) |>
          set_paint_property("sel-zones", "fill-outline-color", "black") |>
          set_filter("sel-zones-outline",
                     list("in", list("get", "zone_key"), list("literal", new_zones))) |>
          set_paint_property("sel-zones-outline", "line-opacity", 1.0)
      } else {
        # Reset filter if no zones selected
        maplibre_proxy("spatial_filter_map") |>
          set_paint_property("sel-zones", "fill-opacity", 0.0) |>
          set_paint_property("sel-zones", "fill-outline-color", NULL) |>
          set_paint_property("sel-zones-outline", "line-opacity", 0.0)
      }
    }
  })

  # submit -> ... ----
  observeEvent(input$submit, {
    if (debug) message("\n=== DATA SELECTION SUBMITTED ===\n")

    # collect input selections
    sel_name        <- input$sel_name
    sel_env_var     <- input$sel_env_var
    sel_qtr         <- input$sel_qtr
    sel_date_range  <- input$sel_date_range
    sel_depth_range <- input$sel_depth_range

    if (debug) message("Selections: sp_name =", sel_name, ", env_var =", sel_env_var)

    # get spatial filter
    drawn_polygon <- get_drawn_features(maplibre_proxy("spatial_filter_map"))
    if (debug) message("Spatial filter:", if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "custom polygon" else "none")

    # retrieve data (lazy tables from database)
    df_sp <- get_sp(sel_name, sel_qtr, sel_date_range)
    df_env <- get_env(
      sel_env_var,
      sel_qtr,
      sel_date_range,
      sel_depth_range[1],
      sel_depth_range[2])

    # Apply spatial filter based on priority: drawn polygon > selected zones > all data
    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      # Use drawn polygon (existing code)
      polygon_wkt <- st_as_text(drawn_polygon$geometry[[1]])

      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", polygon_wkt, "'))"
        )))

      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", polygon_wkt, "'))"
        )))

    } else if (!is.null(rx$sel_zones) && length(rx$sel_zones) > 0) {
      # Filter by selected grid zones

      zones_wkt <- cc_grid_zones |>
        filter(zone_key %in% rx$sel_zones) |>
        pull(geom) |>
        st_union() |>
        st_as_text()

      # Filter species data by zones
      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", zones_wkt, "'))"
        )))

      # Filter ocean data by zones
      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", zones_wkt, "'))"
        )))
    }

    # validate data (only collect count, not full data)
    n_sp <- df_sp |> summarize(n = n()) |> pull(n)
    if (debug) message("Species data: found", n_sp, "rows\n")

    if (n_sp == 0) {
      showNotification("No observations found for selected species.", type = "warning")
      showModal(modal_data())
      return(NULL)
    }

    # store shared data (still lazy tables)
    rx$df_sp       <- df_sp
    rx$df_env      <- df_env
    rx$env_var     <- sel_env_var
    rx$lbl_env_var <- names(which(env_var_choices == sel_env_var))
    if (debug) message("Stored reactive data: df_sp, df_env, lbl_env_var =", rx$lbl_env_var)

    # build filter summary
    rx$filter_summary <- prep_filter_summary(
      sel_name,
      sel_env_var,
      sel_qtr,
      sel_date_range,
      sel_depth_range,
      drawn_polygon,
      rx$sel_zones)

    # generate map
    if (debug) message("Generating species map...\n")
    is_dark       <- input$dark_toggle == "dark"
    sp_hex_list   <- prep_sp_hex(df_sp, res_range)
    sp_scale_list <- lapply(
      sp_hex_list,
      interpolate_palette,
      column  = "sp.value",
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    rx$map_sp     <- map_sp(sp_hex_list, sp_scale_list, is_dark = is_dark)
    if (debug) message("Species map generated and stored in rx$map_sp\n")

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

    removeModal()
  })

  # plotly_click -> ... ----
  observeEvent(event_data("plotly_click", source = "scatterPlotSource"), {
    click_data <- event_data("plotly_click", source = "scatterPlotSource")
    req(click_data, rx$df_splot)

    clicked_point <- collect(rx$df_splot)[click_data$customdata, ]

    showModal(modalDialog(
      title = "Location of Selected Point",
      leafletOutput("modalMap"),
      footer = modalButton("Close"),
      size = "l"
    ))

    output$modalMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        setView(lng = clicked_point$sp_lon, lat = clicked_point$sp_lat, zoom = 14) |>
        addMarkers(
          lng = clicked_point$sp_lon,
          lat = clicked_point$sp_lat,
          popup = paste0(
            "<b>Date:</b> ", clicked_point$sp_dtime,
            "<br><b>Species:</b> ", clicked_point$sp_name,
            "<br><b>", rx$lbl_env_var, ":</b> ", round(clicked_point$env_qty, 2),
            "<b>Abundance:</b> ", round(clicked_point$sp_tally, 2)
          )
        )
    })
  })

  observeEvent(event_data("plotly_selected", source = "scatterPlotSource"), {
    selected_data <- event_data("plotly_selected", source = "scatterPlotSource")
    req(selected_data, rx$df_splot)

    selected_points <- collect(rx$df_splot)[selected_data$customdata, ]

    if (nrow(selected_points) == 0) {
      showNotification("No points located within selection.", type = "warning")
      return(NULL)
    }

    showModal(modalDialog(
      title = "Locations of Selected Points",
      leafletOutput("modalMap"),
      footer = modalButton("Close"),
      size = "l"
    ))

    output$modalMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        setView(lng = mean(selected_points$sp_lon), lat = mean(selected_points$sp_lat), zoom = 14) |>
        addMarkers(
          lng = selected_points$sp_lon,
          lat = selected_points$sp_lat,
          popup = paste0(
            "<b>Date:</b> ", selected_points$sp_dtime,
            "<br><b>Species:</b> ", selected_points$sp_name,
            "<br><b>", rx$lbl_env_var, ":</b> ", round(selected_points$env_qty, 2),
            "<br><b>Abundance:</b> ", round(selected_points$sp_tally, 2)
          )
        )
    })
  })

  # open_transect_modal -> ... ----
  observeEvent(input$open_transect_modal, {
    req(rx$map_sp)

    showModal(modal_depth_profile())

    output$transect_map <- renderMaplibre({
      rx$map_sp |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(line_string = TRUE, trash = TRUE)
        )
    })
  })

  # submit_transect -> ... ----
  observeEvent(input$submit_transect, {
    req(rx$df_sp, rx$df_env)

    features <- get_drawn_features(maplibre_proxy("transect_map"))

    if (is.null(features) || nrow(features) == 0) {
      showNotification("No line drawn. Please draw a line on the map.", type = "warning")
      return(NULL)
    }

    if (nrow(features) > 1) {
      showNotification("Multiple lines detected; using the last one.", type = "message")
      features <- features[nrow(features), ]
    }

    coords <- st_coordinates(features)
    if (nrow(coords) > 2) {
      coords <- coords[(nrow(coords)-1):nrow(coords), c("X", "Y")]
    }

    buffer_res <- buffer_transect(coords, buffer_dist = input$modal_buffer_dist * 1000)

    # collect data for depth profile (need full data for spatial operations)
    df_sp_collected <- rx$df_sp |> collect()
    df_env_collected <- rx$df_env |> collect()

    sp_sf <- st_as_sf(df_sp_collected, coords = c("longitude", "latitude"), crs = 4326)
    env_sf <- st_as_sf(df_env_collected, coords = c("lon_dec", "lat_dec"), crs = 4326)

    filt_sp_sf <- sp_sf[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)), ]
    filt_sp_data <- df_sp_collected[as.vector(st_intersects(sp_sf, buffer_res$buffer, sparse = FALSE)), ]
    filt_env_data <- df_env_collected[as.vector(st_intersects(env_sf, buffer_res$buffer, sparse = FALSE)), ]

    segment_sfc <- st_geometry(buffer_res$segment_utm)
    filt_sp_data$distance <- st_line_project(
      segment_sfc,
      st_transform(filt_sp_sf, buffer_res$utm_crs) |> st_geometry()) / 1000
    filt_env_data$distance <- st_line_project(
      segment_sfc,
      st_transform(
        st_as_sf(filt_env_data, coords = c("lon_dec", "lat_dec"), crs = 4326),
        buffer_res$utm_crs) |> st_geometry()) / 1000

    segment_length <- st_length(buffer_res$segment_utm) / 1000

    dist_bin_size <- 5
    depth_bin_size <- 20

    sp_plot <- filt_sp_data |>
      mutate(
        tooltip = paste0(
          "Species: ", name, "<br>",
          "Tally: ", std_tally, "<br>",
          "Distance: ", round(distance, 2), " km<br>",
          "Date: ", time_start)
      ) |>
      ggplot(
        aes(
          x = distance,
          y = std_tally,
          color = name,
          text = tooltip
        )
      ) +
      geom_point(alpha = 0.6) +
      labs(
        y = "Species Abundance",
        x = "Distance (km)",
        color = "Species"
      )

    proc_env_data <- filt_env_data |>
      mutate(
        dist_bins = filt_env_data$distance %>%
          cut(seq(0, by = dist_bin_size, length.out = ceiling(max(.))/dist_bin_size+1), include.lowest = TRUE),
        depth_bins = filt_env_data$depthm %>%
          cut(seq(min(.), by = depth_bin_size, length.out = ceiling(max(.)/depth_bin_size)+1), include.lowest = TRUE) ) |>
      group_by(
        dist_bins, depth_bins) |>
      summarize(
        n          =  sum(!is.na(qty)),
        qty        =  mean(qty, na.rm = TRUE),
        min_dtime  =  min(dtime, na.rm = TRUE),
        max_dtime  =  max(dtime, na.rm = TRUE),
        .groups    =  "drop") |>
      mutate(
        min_dist   =  as.numeric(sub("[\\[\\(]([0-9]+),.+", "\\1", dist_bins)),
        max_dist   =  as.numeric(sub(".+,([0-9]+)]",        "\\1", dist_bins)),
        min_depth  =  as.numeric(sub("[\\[\\(]([0-9]+),.+", "\\1", depth_bins)),
        max_depth  =  as.numeric(sub(".+,([0-9]+)]",        "\\1", depth_bins))) |>
      mutate(
        tooltip = paste0(
          "Distance: ", min_dist, "-", max_dist, " km<br>",
          "Depth: ", min_depth, "-", max_depth, " m<br>",
          rx$lbl_env_var, ": ", round(qty, 2), "<br>",
          "Num. Obs: ", n, "<br>",
          "Date Range: ", min_dtime, " to ", max_dtime)
      )

    env_plot <- proc_env_data |>
      ggplot(
        aes(
          xmin = min_dist,
          xmax = max_dist,
          ymin = min_depth,
          ymax = max_depth,
          fill = qty,
          text = tooltip)) +
      geom_rect() +
      scale_y_reverse() +
      scale_fill_continuous(palette = rev(hcl.colors(10, palette = "Spectral"))) +
      labs(
        x = "Distance (km)",
        y = "Depth (m)",
        fill = paste0("Average ", rx$lbl_env_var)
      )

    rx$df_dprof <- list(filt_sp_data, proc_env_data)

    profile_plot <- subplot(
      ggplotly(sp_plot, tooltip = "text"),
      ggplotly(env_plot, tooltip = "text"),
      nrows = 2,
      shareX = TRUE,
      heights = c(0.33, 0.67)
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(title = list(text = "Species")),
        yaxis = list(title = "Species Abundance"),
        yaxis2 = list(title = "Depth (m)"),
        xaxis = list(title = "Distance (km)", range = c(0, segment_length))
      ) |>
      config(
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian")
      )

    rx$plot_depth <- profile_plot
    removeModal()
    showNotification("Depth profile generated!", type = "message")
  })

  output$dprof_plot <- renderPlotly({
    req(rx$plot_depth)
    rx$plot_depth
  })

  # map_env_stat ----
  output$map_env_stat <- renderUI({
    selectInput(
      "sel_env_stat",
      "Environmental Summary Statistic",
      choices  = env_stat_choices,
      selected = input$sel_env_stat %||% "mean")
  })

  output$ts_res <- renderUI({
    selectInput(
      "sel_ts_res",
      "Temporal Resolution",
      choices  = ts_res_choices,
      selected = input$sel_ts_res %||% "year")
  })

  output$filter_summary <- renderUI({
    req(rx$filter_summary)
    div(class = "small", markdown(paste(rx$filter_summary, collapse = "  \n")))
  })

  output$summary_statistics <- renderUI({
    req(rx$summary_stats)
    div(class = "small", markdown(paste(rx$summary_stats, collapse = "  \n")))
  })

  # download_data ----
  output$download_data <- downloadHandler(
    filename = function() paste0("calcofi_data_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {

      raw_sel  <- input$sel_raw_data_download %||% character(0)
      proc_sel <- input$sel_proc_data_download %||% character(0)
      all_sel  <- c(raw_sel, proc_sel)

      if (length(all_sel) == 0) {
        showNotification("Select at least one dataset.", type = "warning")
        return(NULL)
      }

      zip_root <- tempfile(pattern = "calcofi_download_", tmpdir = tempdir())
      dir.create(zip_root, showWarnings = FALSE, recursive = TRUE)
      dir.create(zip_root, showWarnings = FALSE, recursive = TRUE)
      paths   <- character()

      write_data <- function(df, rel_path) {
        full_path <- file.path(zip_root, rel_path)
        dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
        write.csv(df, full_path, row.names = FALSE, quote = TRUE)
        paths <<- c(paths, rel_path)       # <<- adds to the outer variable
      }

      for (i in all_sel) {

        if (i == "raw_sp") {
          req(rx$df_sp)
          write_data(rx$df_sp |> collect(), "raw_sp.csv")

        } else if (i == "raw_env") {
          req(rx$df_env)
          write_data(rx$df_env |> collect(), "raw_env.csv")

        } else if (i == "int") {
          req(rx$df_sp, rx$df_env)

          max_hours_diff <- input$time_window %||% 72
          max_meters_diff <- input$dist_window %||% 1000

          d_sp <- rx$df_sp |>
            select(
              sp_name  = name,
              sp_dtime = time_start,
              sp_tally = std_tally,
              sp_lon   = longitude,
              sp_lat   = latitude)

          d_env <- rx$df_env |>
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
          int_data <- d_sp |>
            left_join(
              d_env,
              # join species to env observations within desired time interval
              by = join_by(between(sp_dtime, env_dtime_lwr, env_dtime_upr))) |>
            # compute distance between species and ocean observations
            mutate(
              dist_m = sql("ST_Distance_Sphere(ST_Point(sp_lon, sp_lat), ST_Point(env_lon, env_lat))")) |>
            # get pairs within desired distance
            filter(
              dist_m <= max_meters_diff) |> collect()

          write_data(int_data, "integrated_data.csv")

        } else if (i == "map") {
          req(rx$df_sp, rx$df_env)
          sp_hex  <- prep_sp_hex(rx$df_sp, res_range) |> bind_rows() |> select(-tooltip)
          env_hex <- prep_env_hex(rx$df_env, res_range,
                                  input$map_env_stat %||% "mean") |>
            bind_rows() |> select(-tooltip)

          write_data(sp_hex , "map/species_map.csv")
          write_data(env_hex, "map/env_map.csv")

        } else if (i == "ts") {
          req(rx$df_sp, rx$df_env)
          sp_ts  <- prep_ts_sp(rx$df_sp, input$sel_ts_res %||% "year")
          env_ts <- prep_ts_env(rx$df_env, input$sel_ts_res %||% "year")

          write_data(sp_ts , "time_series/species_ts.csv")
          write_data(env_ts, "time_series/ocean_ts.csv")

        } else if (i == "splot") {
          data <- rx$df_splot %||%
            prep_splot(rx$df_sp, rx$df_env, "mean",
                       method = input$splot_method %||% "nearest_time",
                       max_hours_diff = input$splot_max_hours_diff %||% 72,
                       max_meters_diff = input$splot_max_meters_diff %||% 1000)

          write_data(data, "scatterplot.csv")

        } else if (i == "dprof") {
          sp_data <- rx$df_dprof[[1]]
          env_data <- rx$df_dprof[[2]]
          write_data(sp_data, "depth_profile/species_dprof.csv")
          write_data(env_data, "depth_profile/env_dprof.csv")
        }
      }

      zip::zip(zipfile = file, files = paths, root = zip_root, include_directories = TRUE)
    },
    contentType = "application/zip"
  )
}
