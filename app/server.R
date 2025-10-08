server <- function(input, output, session) {

  # tour ----
  tour$init()$start()

  # reactive values ----
  rx <- reactiveValues(
    df_sp          = NULL,
    df_env         = NULL,
    env_var        = NULL,  # stores the env_var code (e.g., "t_deg_c")
    lbl_env_var    = NULL,  # stores the label (e.g., "Temperature (ºC)")
    map_sp         = NULL,
    df_splot       = NULL,
    filter_summary = NULL,
    plot_depth     = NULL)

  # default data initialization ----
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
      drawn_polygon = NULL)

    # generate species map
    if (debug) message("Generating default species map...\n")
    sp_hex_list   <- prep_sp_hex(df_sp, res_range)
    sp_scale_list <- lapply(
      sp_hex_list,
      interpolate_palette,
      column  = "sp.value",
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    map_sp_obj <- map_sp(sp_hex_list, sp_scale_list)
    rx$map_sp <- map_sp_obj
    if (debug) message("Default species map generated and stored in rx$map_sp\n")

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

      if (debug) message("=== DEFAULT DATA LOADED ===\n\n")
    }, error = function(e) {
      message("ERROR in default data initialization: ", conditionMessage(e))
      traceback()
    })
  })

  # map content ----
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

  # time series content ----
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

  # scatterplot content ----
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

  # depth profile content ----
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

  # map rendering ----
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

  # time series rendering ----
  output$ts_plot <- renderHighchart({
    req(rx$df_sp, rx$df_env, rx$env_var)

    if (debug) message("renderHighchart: generating time series plot\n")

    ts_res <- input$sel_ts_res %||% "year"
    sp_ts  <- prep_ts_sp(rx$df_sp, ts_res) |> arrange(time)
    env_ts <- prep_ts_env(rx$df_env, ts_res)

    plot_ts(sp_ts, env_ts, ts_res, rx$env_var)
  })

  # scatterplot rendering ----
  output$splot <- renderPlotly({
    req(rx$df_splot)

    if (debug) message("renderPlotly: generating scatterplot\n")

    plot_ly(
      data       = rx$df_splot,
      x          = ~env_qty,
      y          = ~sp_tally,
      color      = ~sp_name,
      type       = "scattergl",
      mode       = "markers",
      marker     = list(size = 10, opacity = 0.8),
      customdata = ~1:nrow(rx$df_splot),
      source     = "scatterPlotSource",
      hoverinfo  = "text",
      text       = ~paste0(
        "<b>Date:</b> ", sp_dtime,
        "<br><b>Species:</b> ", sp_name,
        "<br><b>", rx$lbl_env_var, ":</b> ", round(env_qty, 2),
        "<br><b>Abundance:</b> ", round(sp_tally, 2)
      )
    ) |>
      layout(
        xaxis    = list(title = rx$lbl_env_var),
        yaxis    = list(title = "Species Abundance"),
        legend   = list(title = "Species"),
        dragmode = "select"
      ) |>
      config(
        displaylogo            = FALSE,
        scrollZoom             = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian")
      )
  })

  # data selection modal ----
  observeEvent(input$sel_data, {
    showModal(modal_data())
    updateSelectizeInput(session, 'sel_name', choices = sp_names, server = TRUE)

    output$spatial_filter_map <- renderMaplibre({
      maplibre(
        style  = carto_style("positron"),
        center = c(-120, 35),
        zoom   = 5
      ) |>
        add_draw_control(
          position = "top-right",
          displayControlsDefault = FALSE,
          controls = list(polygon = TRUE, trash = TRUE)
        )
    })
  })

  # submit data selection ----
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

    # apply spatial filter
    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      polygon_wkt <- st_as_text(drawn_polygon$geometry[[1]])

      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", polygon_wkt, "'))")))

      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", polygon_wkt, "'))")))
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
      drawn_polygon)

    # generate map
    if (debug) message("Generating species map...\n")
    sp_hex_list   <- prep_sp_hex(df_sp, res_range)
    sp_scale_list <- lapply(
      sp_hex_list,
      interpolate_palette,
      column  = "sp.value",
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    map_sp_obj <- map_sp(sp_hex_list, sp_scale_list)
    rx$map_sp <- map_sp_obj
    if (debug) message("Species map generated and stored in rx$map_sp\n")

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

    removeModal()
  })

  # scatterplot interactions ----
  observeEvent(event_data("plotly_click", source = "scatterPlotSource"), {
    click_data <- event_data("plotly_click", source = "scatterPlotSource")
    req(click_data, rx$df_splot)

    clicked_point <- rx$df_splot[click_data$customdata, ]

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

    selected_points <- rx$df_splot[selected_data$customdata, ]

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

  # depth profile modal ----
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

  # generate depth profile ----
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

    profile_plot <- subplot(
      plot_ly(
        filt_sp_data,
        x          = ~distance,
        y          = ~std_tally,
        type       = "scattergl",
        mode       = "markers",
        showlegend = FALSE) |>
        layout(yaxis = list(title = "Species Abundance")),
      plot_ly(
        filt_env_data,
        x          = ~distance,
        y          = ~depthm,
        type       = "scattergl",
        mode       = "markers",
        marker     = list(color = ~qty, colorbar = list(title = rx$lbl_env_var)),
        showlegend = FALSE) |>
        layout(
          xaxis = list(title = "Distance (km)", range = c(0, segment_length)),
          yaxis = list(title = "Depth (m)", autorange = "reversed")),
      nrows = 2, shareX = TRUE, heights = c(0.33, 0.67)
    ) |>
      config(
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian"))

    rx$plot_depth <- profile_plot
    removeModal()
    showNotification("Depth profile generated!", type = "message")
  })

  output$dprof_plot <- renderPlotly({
    req(rx$plot_depth)
    rx$plot_depth
  })

  # dynamic UI outputs ----
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

    div(
      class = "mt-3",
      accordion(
        accordion_panel(
          "Current Filters",
          div(class = "small", markdown(paste(rx$filter_summary, collapse = "  \n"))) ),
        open = T) )
  })

  # download handlers ----
  output$download_sp <- downloadHandler(
    filename = function() paste0("species_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(rx$df_sp)
      write.csv(rx$df_sp |> collect(), file, row.names = FALSE)
    }
  )

  output$download_env <- downloadHandler(
    filename = function() paste0("environmental_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(rx$df_env)
      write.csv(rx$df_env |> collect(), file, row.names = FALSE)
    }
  )
}
