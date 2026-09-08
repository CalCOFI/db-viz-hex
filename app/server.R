server <- function(input, output, session) {

  # thematic for bslib theming ----
  thematic::thematic_shiny()

  # usage tracking ----
  # calcofi4r::cc_track() only pushes a message down the websocket the session
  # already has open — it makes NO http request, so instrumenting a hot control
  # cannot add latency to the query that follows. (The previous log_query() ran
  # a synchronous httr2 POST to Apps Script on every submit and every download,
  # stalling the reactive thread for the whole round-trip.)
  # `ignoreInit = TRUE` on the observers below so app startup doesn't emit a
  # burst of synthetic "selections" the user never made.
  calcofi4r::cc_track_session(session)   # the ip + token JS cannot read itself
  trk <- function(event, ...) calcofi4r::cc_track(session, event, ...)
  trk("session_start", h3t = USE_H3T, release = H3T_RELEASE)

  # rx$params is nested (map_params, ts_params, …) and carries Dates, which do
  # not survive unlist(); flatten it to the readable scalars the Sheet wants, so
  # every download row carries the filters that produced it.
  trk_filters <- function(p) list(
    taxa             = p$taxa,
    n_taxa           = length(p$taxa),
    env_var          = p$env_var,
    quarters         = p$sel_qtr %||% p$quarters,
    date_beg         = as.character(p$date_range[1]),
    date_end         = as.character(p$date_range[2]),
    depth_min        = p$depth_range[1],
    depth_max        = p$depth_range[2],
    include_children = p$ck_children %||% p$include_children,
    zones            = p$zones,
    time_window      = p$time_window,
    dist_window      = p$dist_window)

  # which tab users actually work in (the navset carries id = "outputPanel", so
  # this covers About and Download as well as the four visualization panels)
  observeEvent(input$outputPanel, trk("select_tab", tab = input$outputPanel),
               ignoreInit = TRUE)
  # "+ Add a variable to compare" (env card, OFF state) -> flick the switch on;
  # the switch is then the way back off. cmp_env_toggle stays the source of truth.
  observeEvent(input$add_env_var,
    update_switch("cmp_env_toggle", value = TRUE),
    ignoreInit = TRUE)

  # remember the last visualization tab so the About page's "Close" (X) can
  # return to whatever the user was looking at
  observeEvent(input$outputPanel, {
    if (input$outputPanel %in%
        c("Map", "Time Series", "Scatterplot", "Depth Profile"))
      rx$last_viz_tab <- input$outputPanel
  })
  observeEvent(
    input$close_about,
    nav_select("outputPanel", rx$last_viz_tab %||% "Map", session = session),
    ignoreInit = TRUE)
  observeEvent(
    input$close_datasources,
    nav_select("outputPanel", rx$last_viz_tab %||% "Map", session = session),
    ignoreInit = TRUE)

  # Data Sources touchpoints -- which dataset_key(s) back the currently
  # APPLIED species + environmental variable. Feeds the left-panel box, the
  # map title popover, the Download panel's citation block and the "Cite
  # this data" modal.
  #
  # sp_keys reads rx$sp_ds_keys (see sp_dataset_keys()) -- the dataset_key(s)
  # actually present in the already-filtered rx$df_sp -- rather than
  # dataset_keys_for_species(), which looked up every dataset the taxon has
  # EVER been recorded in from the raw taxonomy table. That ignored both the
  # Datasets filter and the "Standardized as" pick, so picking e.g. "Pacific
  # sardine -- CUFES Fish Eggs" showed SWFSC: Ichthyoplankton citations too,
  # since sardine is also caught by that program's nets.
  # groups, not just a flat key list, so the left-panel box can say WHICH
  # half of the current view a source backs -- flattened by active_attrib_keys()
  # below for callers (the modal, the map title popover) that just want the set.
  active_attrib_groups <- reactive({
    sp_keys <- rx$sp_ds_keys %||% character(0)
    # only pull in the env dataset when a variable is actually being compared
    # -- rx$params$env_var still holds its default ("temperature", a Bottle
    # variable) even with the compare toggle off, which is what made
    # CalCOFI: Bottle show up for a species that has nothing to do with it
    env_key <- if (isTRUE(input$cmp_env_toggle))
      dataset_key_for_env_var(rx$params$env_var %||% character(0))
    else character(0)
    # a dataset backing both (unlikely, but not impossible) is named once,
    # under Species -- no reason to show it twice
    list(sp = sp_keys, env = setdiff(env_key, sp_keys))
  })

  active_attrib_keys <- reactive({
    g <- active_attrib_groups()
    unique(c(g$sp, g$env))
  })

  # merges what used to be two separate boxes -- the plain provider list here,
  # and a "Dataset citations & license" panel under Download that repeated
  # the same datasets. Now shown together per source, so there's one place
  # for "who provided this and how do I cite it" instead of two.
  #
  # Grouped under "Species / Taxa" / "Environment" sub-headings when Compare
  # is on and both actually contribute a source -- previously this took one
  # flat key list with no record of which half of the view each key backed,
  # so e.g. a species citation and CalCOFI: Bottle (the env source) rendered
  # as an undifferentiated list (user question 2026-09-07: "does this
  # differentiate which is for the species/taxa and which for the
  # environment" -- it did not). A single group renders with no sub-heading,
  # same as before, so Compare-off stays exactly as plain as it was.
  data_sources_box_ui <- function(groups) {
    one_group <- function(keys) {
      parts <- attrib_provider_parts(keys)
      if (length(parts) == 0) return(NULL)
      rows <- attrib_rows_for(keys)
      lapply(names(parts), function(k) {
        pp <- parts[[k]]
        div(class = "cc-sources-item",
            div(class = "cc-sources-ds", dataset_short_label(k)),
            # one flowing line ("Programs: X | Providers: Y") rather than two
            # stacked divs -- wraps on its own if it doesn't fit, same as any
            # other text (2026-09-07 feedback: "if it fits ... put ... on one
            # line").
            div(class = "cc-sources-provider",
                tags$strong("Programs: "), pp$program,
                if (nzchar(pp$institution) && pp$institution != pp$program)
                  tagList(" | ", tags$strong("Providers: "), pp$institution)),
            dataset_citation_items(rows[rows$dataset_key == k, ]))
      })
    }
    sp_items  <- one_group(groups$sp)
    env_items <- one_group(groups$env)
    if (is.null(sp_items) && is.null(env_items))
      return(p(class = "cc-muted",
               "Select a species or environmental variable to see its data source."))
    both <- !is.null(sp_items) && !is.null(env_items)
    div(
      class = "cc-sources-box",
      if (both) div(class = "cc-sources-group-label", "Species / Taxa"),
      sp_items,
      if (both) div(class = "cc-sources-group-label", "Environment"),
      env_items,
      actionLink("goto_datasources_from_panel", "View full citation & license →",
                 class = "cc-sources-link"))
  }

  output$data_sources_box <- renderUI(data_sources_box_ui(active_attrib_groups()))

  output$map_title_sp_pop <- renderUI({
    # see active_attrib_keys() above -- same fix, same reasoning
    keys  <- rx$sp_ds_keys %||% character(0)
    parts <- attrib_provider_parts(keys)
    if (length(parts) == 0)
      return(p(class = "cc-muted", "No species selected yet."))
    # "Programs:"/"Providers:" labeled lines instead of DATASET_LABELS'
    # "SWFSC: ..." prefix plus a combined "Program (Institution)" string --
    # that repeated the institution twice (2026-09-07 feedback).
    tagList(lapply(names(parts), function(k) {
      pp <- parts[[k]]
      p(tags$strong(dataset_short_label(k)), tags$br(),
        tags$strong("Programs: "), pp$program,
        if (nzchar(pp$institution) && pp$institution != pp$program)
          tagList(" | ", tags$strong("Providers: "), pp$institution))
    }))
  })

  # Environment pill's own "Data source" popover -- mirrors map_title_sp_pop
  # above exactly, just reading active_attrib_groups()$env (the env
  # dataset_key, only populated while actually comparing -- see that
  # reactive's own comment) instead of rx$sp_ds_keys. This pill's info icon
  # never existed before (ui.R's cc-map-title-env had no popover at all, only
  # the species one did), which is why "Environmental — Water temperature"
  # showed no (i) next to it while "Species — Appendicularia" did (bug report
  # 2026-09-07: "why no data source i icon for the environment").
  output$map_title_env_pop <- renderUI({
    keys  <- active_attrib_groups()$env
    parts <- attrib_provider_parts(keys)
    if (length(parts) == 0)
      return(p(class = "cc-muted", "No environmental variable being compared."))
    tagList(lapply(names(parts), function(k) {
      pp <- parts[[k]]
      p(tags$strong(dataset_short_label(k)), tags$br(),
        tags$strong("Programs: "), pp$program,
        if (nzchar(pp$institution) && pp$institution != pp$program)
          tagList(" | ", tags$strong("Providers: "), pp$institution))
    }))
  })

  observeEvent(input$btn_cite_data, {
    trk("open_cite_data")
    showModal(modal_cite_data(active_attrib_keys()))
  }, ignoreInit = TRUE)

  observeEvent(input$goto_datasources_from_panel,
    nav_select("outputPanel", "Data Sources", session = session),
    ignoreInit = TRUE)

  observeEvent(input$cite_modal_datasources_link, {
    removeModal()
    nav_select("outputPanel", "Data Sources", session = session)
  }, ignoreInit = TRUE)
  # hexagons vs a named boundary layer — which layers people actually summarize
  # within is the signal for whether more are worth adding to the registry
  observeEvent(input$sel_agg_unit,
               trk("select_agg_unit", agg_unit = input$sel_agg_unit),
               ignoreInit = TRUE)
  observeEvent(input$dark_toggle, trk("select_theme", theme = input$dark_toggle),
               ignoreInit = TRUE)

  # the two modals that gate everything else — a high open_filters count with
  # few filter_submit rows means people are bouncing off the filter dialog.
  observeEvent(input$edit_filters,        trk("open_filters"),   ignoreInit = TRUE)
  observeEvent(input$edit_spatial,        trk("open_spatial"),   ignoreInit = TRUE)
  observeEvent(input$open_transect_modal, trk("open_transect"),  ignoreInit = TRUE)

  # feedback: the navbar item opens the modal; sending is client-side (the
  # CCFeedback script in ui.R posts to FEEDBACK_URL), so there is no submit
  # handler here -- just open it and log the open.
  observeEvent(input$open_feedback, {
    trk("open_feedback")
    showModal(modal_feedback(DB_RELEASE, FEEDBACK_URL))
  }, ignoreInit = TRUE)

  # tour ----
  # launch the guided tour on load, unless suppressed with ?tour=off in the URL
  # (also accepts false/0/no; the brand contract's rule, cc_tour_enabled()) —
  # handy for clean screenshots; see the db-viz-hex recipe in
  # CalCOFI.github.io/shots.yml
  if (is_tour_on) {
    observeEvent(TRUE, {
      if (calcofi4r::cc_tour_enabled()) {
        trk("start_tour")
        tour$init()$start()
      }
    }, once = TRUE)
  }

  # rx ----
  rx <- reactiveValues(
    # ?datasets=a,b from the URL, parsed once on connect and used until the user
    # opens the filter modal and chooses for themselves (see the url_datasets
    # observer below)
    url_datasets   = NULL,
    url_env_var    = NULL,  # ?env= resolved to a measurement_type (see below)
    df_sp          = NULL,
    df_env         = NULL,
    env_hex_list   = NULL,  # cached env hex list for first map render
    env_var        = NULL,  # stores the env_var code (e.g., "temperature")
    lbl_env_var    = NULL,  # stores the label (e.g., "Temperature (ºC)")
    # names selected within input$sel_places_cat's active category -- any
    # cc_places category (CalCOFI Zones, BOEM Wind Planning Areas, etc.) OR
    # any table-only spatial_layers.csv layer (global.R::sample_spatial_layers)
    # -- all share this one picker. Written by the spatial_filter_map /
    # tbl_places click handlers below. Was named `sel_zones` and read against
    # cc_grid_zones$zone_key at submit, but never assigned by anything -- that
    # mismatch is what made the Spatial tab's filter silently a no-op; see the
    # submit handler.
    sel_places     = NULL,
    # the category (input$sel_places_cat) those names were picked in. The
    # submit handler matches names against THIS category's polygons, so a
    # category change clears the names (observer below) rather than leaving
    # BOEM names to be looked up among CalCOFI Zones.
    sel_places_cat = NULL,
    map_sp         = NULL,
    sp_scale       = NULL,  # scale list for sp map
    env_scale      = NULL,  # scale list for env map
    # Last known client view {center, zoom} -- kept current by the
    # unguarded map_before_view observer below (unlike the legend-drawing
    # one right after it, this one has no req(rx$sp_scale, rx$env_scale)
    # gate, since it just needs to be fresh whenever a widget rebuild reads
    # it). Passed into map_sp_h3t()/map_env_h3t() as `view=` so a rebuild
    # (env stat, dark mode, ...) re-opens the widget exactly where the user
    # left it instead of re-fitting to the default extent every time (bug
    # report 2026-09-07: "when togling env summary statistics the map view
    # goes to 50km. i want the default to be 100km until someone pans in or
    # out") -- NULL only until the very first moveend of a session, which is
    # exactly when the DEFAULT_MAP_CENTER/DEFAULT_MAP_ZOOM fallback in
    # functions_h3t.R should still apply.
    last_map_view  = NULL,
    df_splot       = NULL,
    df_dprof       = NULL,
    filter_summary = NULL,
    summary_stats  = NULL,
    plot_depth     = NULL,
    # Map-view bookkeeping lives HERE and not in rx$params, deliberately.
    # reactiveValues dependencies are per NAME: output$map reads
    # rx$params$sel_qtr, so it depends on `params` as a whole, and any write to
    # rx$params$map_params$* re-renders the compare widget. That is what made
    # the polygon summary vanish the instant it was drawn — apply_poly() added
    # the layers by proxy, then set rx$params$map_params$agg_unit, which
    # rebuilt the widget in hex mode on the same flush.
    agg_unit       = "hex",
    env_stat       = "mean",
    # WKT of the active spatial filter (drawn polygon or a cc_places zone
    # selection), NULL for none — the h3t tile SQL needs it, and it is part of
    # the env tile cache key
    spatial_wkt    = NULL,
    # sample_spatial-backed filter (table-only categories, no WKT) — layer name
    # + selected spatial_name values. Mutually exclusive with spatial_wkt;
    # also part of the h3t tile cache key, so a table-only spatial filter
    # doesn't leave the map showing unfiltered tiles beside filtered plots.
    spatial_layer  = NULL,
    spatial_names  = NULL,
    env_tile_key   = NULL,  # hash of the filters the env tile URL was built from
    params = list( # filter/analysis params
      taxa             = default_sp_name,
      env_var          = "temperature",
      quarters         = 1:4,
      date_range       = min_max_date,
      depth_range      = c(0, 515),
      include_children = FALSE,  # off by default -- see checkboxInput("ck_children", ...) in functions.R
      zones            = NULL,
      time_window      = NULL,
      dist_window      = NULL,
      map_params       = list(env_stat   = NULL),
      ts_params        = list(ts_res     = NULL),
      splot_params     = list(time_window = NULL,
                              dist_window = NULL,
                              method      = NULL),
      dprof_params     = list(transect   = NULL,
                              buffer     = NULL)
    ))

  # map builders ----
  # ONE definition each, called by BOTH the startup preload and the Submit
  # handler. They were written out twice and the copies drifted: Submit never
  # grew an h3t branch, so the first Submit of a session silently downgraded the
  # species side to the classic 10-resolution sf path — ~4 s of server work
  # shipping an 88 MB widget to the browser — while the environmental side
  # beside it kept drawing h3t tiles. The h3t path costs one /h3t/stats call
  # (~0.5 s) and sends a URL.
  #
  # `spec$scales` is a length-10 list indexed by resolution because that is what
  # the zoom observer's legend lookup expects; h3t colors every zoom from one
  # scale, so it hands over the same scale ten times.

  sp_map_spec <- function(df_sp, sel_name, sel_qtr, sel_date_range,
                          ck_children, datasets = NULL, poly_wkt = NULL,
                          spatial_layer = NULL, spatial_names = NULL,
                          cpue_unit = NULL, is_dark = TRUE) {
    if (USE_H3T) {
      # Resolve taxa HERE and hand the taxon_keys to the SQL builder, so the
      # tiles filter on exactly what get_sp() filtered on — same children walk
      # (ITIS for birds, WoRMS otherwise), same dataset checkboxes, same
      # spatial filter (WKT polygon, or a table-only sample_spatial layer).
      #
      # cpue_unit, likewise, must match do_apply_filters()'s own
      # filter_cpue_unit(df_sp, cpue_chosen) -- this h3t branch never reads
      # df_sp (that's classic-path only, in the else below), so without
      # passing cpue_unit through here explicitly, "Standardized as" changed
      # the legend title (a separate, correctly-reactive output) while these
      # tiles kept averaging std_tally across every unit, unchanged (bug
      # report 2026-09-08: "why does nothing change visually when you change
      # standardized as"). See build_sp_sql()'s own doc for the full story.
      ids   <- resolve_sp_ids(sel_name, ck_children)
      sql   <- build_sp_sql(ids$taxon_keys, sel_qtr, sel_date_range,
                            datasets = datasets, poly_wkt = poly_wkt,
                            spatial_layer = spatial_layer, spatial_names = spatial_names,
                            cpue_unit = cpue_unit)
      stats <- fetch_h3t_stats(sql, H3T_RELEASE)
      if (debug) { message("sp stats:"); print(stats) }
      scale <- build_h3t_scale(stats, palette = \(n) hcl.colors(n, palette = "Viridis"))
      list(
        map       = map_sp_h3t(h3t_tile_url(sql, H3T_RELEASE), scale,
                                view = isolate(rx$last_map_view), is_dark = is_dark),
        layer_ids = "sp",
        scales    = rep(list(scale), length(res_range)))
    } else {
      hex_list <- prep_sp_hex(df_sp, res_range)
      scales   <- lapply(hex_list, interpolate_palette, column = "sp.value",
                         palette = \(n) hcl.colors(n, palette = "Viridis"))
      list(
        map       = map_sp(hex_list, scales, is_dark = is_dark),
        layer_ids = paste0("sp", res_range),
        scales    = scales)
    }
  }

  # h3t only: the environmental side is assembled into a widget inside
  # output$map (it needs the stat/variable labels), so this returns the two
  # pieces that depend on the filters rather than a finished map.
  env_tile_spec <- function(env_var, sel_qtr, sel_date_range, sel_depth_range,
                            env_stat, poly_wkt = NULL,
                            spatial_layer = NULL, spatial_names = NULL) {
    sql   <- build_env_sql(env_var, sel_qtr, sel_date_range, sel_depth_range,
                           stat = env_stat, poly_wkt = poly_wkt,
                           spatial_layer = spatial_layer, spatial_names = spatial_names)
    stats <- fetch_h3t_stats(sql, H3T_RELEASE)
    if (debug) { message("env stats:"); print(stats) }
    scale <- build_h3t_scale(stats, palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
    list(tile_url = h3t_tile_url(sql, H3T_RELEASE), scale = scale)
  }

  # Everything the env tile URL depends on. output$map rebuilds the URL when
  # this changes and reuses it when it does not. Previously the URL was cached
  # under `env_stat == "mean"` alone and rx$env_tile_url was only ever written
  # at startup — so submitting a DIFFERENT environmental variable relabeled the
  # legend while the tiles kept showing the old one, with nothing to see it by.
  env_tile_key <- function(env_stat) rlang::hash(list(
    rx$env_var, rx$params$sel_qtr, rx$params$date_range,
    rx$params$depth_range, env_stat, rx$spatial_wkt,
    rx$spatial_layer, rx$spatial_names))

  # ── ?datasets= : open on one dataset (or several) ------------------------
  # A calcofi.io dataset page links here, and the link could only open the app
  # at its own start (UI plan D-6, Decision 10). `?datasets=calcofi_bottle` — a
  # comma-separated list of release dataset_keys, the same values the Taxa
  # datasets checkbox group carries — now selects them for the opening view and
  # for the filter modal when it is first opened.
  #
  # The URL is the source of truth at load, parsed ONCE; after that the user's
  # own choice is, and the observer below writes it back. Unknown keys are
  # dropped and an all-unknown list is ignored, because in this app an empty
  # dataset selection already means "all of them" — a stale link must not look
  # like a deliberate empty filter.
  bio_ds_selected <- reactive(input$sel_bio_ds %||% rx$url_datasets)

  observeEvent(session$clientData$url_search, once = TRUE, {
    q    <- getQueryString(session)
    keep <- parse_datasets_param(q$datasets, d_bio_datasets$dataset_key)
    if (is.null(keep)) {
      if (!is.null(q$datasets) && nzchar(q$datasets))
        message("?datasets=", q$datasets, " names no dataset in this release — ignored")
      return()
    }
    rx$url_datasets        <- keep
    rx$params$bio_datasets <- keep
  }, ignoreNULL = FALSE)

  # ?env=<dataset_key> : an environmental dataset's page links here (the taxa
  # link above cannot select bottle/CTD/DIC/METS/picoplankton). Resolved to the
  # dataset's leading variable by functions.R::parse_env_param(); the default
  # loader below reads rx$url_env_var for its first env query, and the two
  # updates put the top bar in the same state (Compare on, that variable
  # selected). The cmp_env_toggle observer then re-applies once with the same
  # inputs -- one redundant query, in exchange for no special case in
  # do_apply_filters(). Runs before the default loader (same clientData
  # trigger, created first), like the ?datasets= observer.
  observeEvent(session$clientData$url_search, once = TRUE, {
    q  <- getQueryString(session)
    mt <- parse_env_param(q$env, d_env_vars, ENV_HEADLINE_TYPES)
    if (is.null(mt)) {
      if (!is.null(q$env) && nzchar(q$env))
        message("?env=", q$env, " names no environmental dataset in this release — ignored")
      return()
    }
    rx$url_env_var <- mt
    updateSelectizeInput(session, "sel_env_var", selected = mt)
    update_switch("cmp_env_toggle", value = TRUE)
  }, ignoreNULL = FALSE)

  # keep the URL in sync with the dataset selection, the way db-viz-cruise does.
  # Every OTHER parameter is carried through untouched — ?theme= and ?tour= are
  # the brand contract (brand/v2/README items 5 and 9) and a screenshot URL that
  # loses its theme is a broken screenshot.
  observe({
    sel <- input$sel_bio_ds
    # until the user has chosen in the modal, the URL IS the selection: rewriting
    # it from a NULL input at startup turned ?datasets=cce-lter_zooscan into "?"
    # the moment the app opened (measured live, 2026-09-06)
    req(!is.null(sel))
    q   <- isolate(getQueryString(session))
    q$datasets <- if (length(sel)) paste(sel, collapse = ",") else NULL
    q <- q[!vapply(q, function(v) is.null(v) || !nzchar(v), logical(1))]
    updateQueryString(
      if (length(q)) paste0("?", paste0(names(q), "=", unname(q), collapse = "&")) else "?",
      mode = "replace")
  })

  # session.once -> ... ----
  observeEvent(session$clientData, once = TRUE, {
    tryCatch({
      if (debug) message("\n=== LOADING DEFAULT DATA ===")

      # default selections
      sel_name        <- default_sp_name
      sel_env_var     <- isolate(rx$url_env_var) %||% "temperature"
      sel_qtr         <- 1:4
      sel_date_range  <- min_max_date
      sel_depth_range <- c(0, 515)
      ck_children     <- FALSE  # off by default -- see checkboxInput("ck_children", ...) in functions.R
      env_stat        <- "mean"

      if (debug) message("Loading default species: ", sel_name)

      # retrieve data (lazy tables from database) -- always needed for ts, splot, etc.
      # isolate(): read once for this default load — the observer above has
      # already run (url_search fires before clientData completes), and the
      # opening view should not rebuild when the user later changes the filter
      sel_bio_ds     <- isolate(rx$url_datasets)
      df_sp  <- get_sp(sel_name, sel_qtr, sel_date_range, ck_children, datasets = sel_bio_ds)
      df_env <- get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2])

      # "Standardized as" -- see filter_cpue_unit(): no prior UI selection
      # exists yet at startup, so this always falls back to the majority unit
      cpue_units_all <- sp_unit_summary(df_sp)
      cpue_chosen    <- if (nrow(cpue_units_all)) cpue_units_all$cpue_unit[1] else NA_character_
      rx$cpue_units  <- cpue_units_all
      rx$cpue_chosen <- cpue_chosen
      df_sp <- filter_cpue_unit(df_sp, cpue_chosen)

      if (USE_H3T) {
        # h3t path: skip the 10-resolution sf preload entirely. hex data is
        # served on-demand per viewport; we only need a single color scale per
        # side (from /h3t/stats) for the legend.
        if (debug) message("USE_H3T: fetching stats instead of preloading hex lists")

        # the theme the page opened in (?theme= / cookie); isolated so a later toggle
        # restyles via the dark_toggle observer instead of rebuilding the map
        spec <- sp_map_spec(df_sp, sel_name, sel_qtr, sel_date_range, ck_children,
                            datasets = sel_bio_ds, cpue_unit = cpue_chosen,
                            is_dark = isolate(calcofi4r::cc_is_dark(input)))
        rx$map_sp       <- spec$map
        rx$sp_layer_ids <- spec$layer_ids
        rx$sp_scale     <- spec$scales

        env <- env_tile_spec(sel_env_var, sel_qtr, sel_date_range,
                             sel_depth_range, env_stat)
        rx$env_tile_url     <- env$tile_url
        rx$env_scale_single <- env$scale
        rx$env_scale        <- rep(list(env$scale), length(res_range))

        rx$summary_stats <- prep_summary_stats(df_sp, df_env, env_var_label(sel_env_var))

      } else {
        # classic path: 10-resolution sf preload (with RDS cache)
        cached <- load_cache(cache_dir, db_path)

        if (!is.null(cached)) {
          if (debug) message("using cached default data")
          sp_hex_list   <- cached$sp_hex_list
          env_hex_list  <- cached$env_hex_list
          summary_stats <- cached$summary_stats
        } else {
          if (debug) message("cache miss -- computing default data")
          n_sp <- df_sp |> summarize(n = n()) |> pull(n)
          if (debug) message("Default species data: found ", n_sp, " rows")
          if (n_sp == 0) {
            if (debug) message("WARNING: No data found for default species")
            return(NULL)
          }
          sp_hex_list   <- prep_sp_hex(df_sp, res_range)
          env_hex_list  <- prep_env_hex(df_env, res_range, env_stat)
          summary_stats <- prep_summary_stats(df_sp, df_env, env_var_label(sel_env_var))
          save_cache(cache_dir, db_path, sp_hex_list, env_hex_list, summary_stats)
        }

        rx$summary_stats <- summary_stats

        if (debug) message("Generating default species map...")
        sp_scale_list <- lapply(
          sp_hex_list,
          interpolate_palette,
          column  = "sp.value",
          palette = \(n) hcl.colors(n, palette = "Viridis"))
        rx$map_sp       <- map_sp(sp_hex_list, sp_scale_list)
        rx$sp_layer_ids <- paste0("sp", res_range)
        rx$sp_scale     <- sp_scale_list
        rx$env_hex_list <- env_hex_list
      }

      # store shared data (both paths)
      rx$df_sp       <- df_sp
      # unit breakdown alongside the data it describes, so the legend and the
      # sidebar note can never disagree with what is mapped
      rx$sp_units    <- sp_unit_summary(df_sp)
      # dataset_key(s) actually contributing to df_sp -- see sp_dataset_keys():
      # what Sources & Citations should name, not every dataset the taxon has
      # ever appeared in
      rx$sp_ds_keys  <- sp_dataset_keys(df_sp)
      rx$df_env      <- df_env
      rx$env_var     <- sel_env_var
      rx$lbl_env_var <- env_var_label(sel_env_var)
      rx$params$taxa        <- sel_name
      rx$params$env_var     <- sel_env_var
      rx$params$sel_qtr     <- sel_qtr
      rx$params$date_range  <- sel_date_range
      rx$params$depth_range <- sel_depth_range
      rx$params$ck_children <- ck_children

      # Make the default taxon visibly selected in the search box. The server
      # loads its data regardless, but a viewer who lands on the app and sees
      # only "Search species / taxa..." has no idea what the hexagons are --
      # and they are gone in 30-90 s. The observeEvent(input$sel_name) above
      # no-ops on this because rx$params$taxa already equals it.
      updateSelectizeInput(session, "sel_name", selected = sel_name)

      # stamp the key AFTER rx$params is populated (it hashes those fields), so
      # output$map reuses the tile URL just fetched instead of fetching it again
      if (USE_H3T) rx$env_tile_key <- env_tile_key(env_stat)

      # the summary must say which datasets the opening view was cut from — the
      # URL's selection (sel_bio_ds) — or a ?datasets= link reads as "all 10"
      # while the map underneath it is already one dataset (measured live,
      # 2026-09-06, after the UI-E deploy)
      rx$filter_summary <- prep_filter_summary(
        sel_name, sel_env_var, sel_qtr, sel_date_range,
        sel_depth_range, drawn_polygon = NULL, rx$sel_places, ck_children,
        bio_datasets = sel_bio_ds)

      # top-bar filter chip (functions.R::chip_summary_text()) -- same fields
      # as rx$filter_summary above, condensed to the one line shown next to
      # "Edit filters"
      rx$chip_summary <- chip_summary_text(
        sel_qtr, sel_date_range, sel_depth_range, sel_places = rx$sel_places,
        date_bounds = min_max_date)

      rx$plot_depth <- NULL

      if (debug) message("=== DEFAULT DATA LOADED ===\n")
    }, error = function(e) {
      message("ERROR in default data initialization: ", conditionMessage(e))
      traceback()
    })

    # Release the map render, once, now that everything it reads exists. This
    # sits OUTSIDE the tryCatch on purpose: if the preload fails, the map should
    # still be renderable once a Submit provides data, rather than stay disabled
    # for the life of the session.
    map_ready(TRUE)
  })

  # ts_content ----
  output$ts_content <- renderUI({
    if (is.null(rx$df_sp)) {
      ui_placeholder(
        "No Data Selected",
        "Choose a species in Species / Taxa above to begin exploring CalCOFI data."
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
        "Choose a species in Species / Taxa above to begin exploring CalCOFI data."
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
        "Choose a species in Species / Taxa above to begin exploring CalCOFI data."
      )
    } else if (is.null(rx$plot_depth)) {
      ui_placeholder(
        "No Depth Profile Generated",
        "Click 'Draw Transect' above to create a depth profile."
      )
    } else {
      plotlyOutput("dprof_plot", height = "100%")
    }
  })

  # plotly registers a source's events only when the plot is RENDERED, and the
  # Scatterplot tab is hidden at startup (Shiny suspends hidden outputs), so
  # calling event_data() before then warns once per event per flush:
  #   "The 'plotly_click' event tied a source ID of 'scatterPlotSource' is not
  #    registered."
  # This gates the two listeners on the plot existing. event_register() on the
  # plot covers the other half — without it plotly never wires the JS handlers
  # up at all, so a click would silently deliver nothing.
  splot_ready <- reactiveVal(FALSE)

  # map ----
  # This render has EXACTLY TWO reactive dependencies, by design: map_ready()
  # and map_rebuild(). Everything else it touches is isolated.
  #
  # It used to depend on rx$df_env, rx$map_sp, rx$env_tile_url, rx$env_var,
  # rx$lbl_env_var and rx$env_scale_single. The preload observer sets all six in
  # sequence and `session$onFlushed` flipped map_ready independently, so the
  # widget was invalidated repeatedly while an earlier render was still in
  # flight — rebuilding an expensive compare widget several times per startup
  # and producing three client errors on every page load:
  #   "sent a progress message for map, but the output is in an unexpected
  #    state of: running" / "'map' is recalculating, but ... 'idle'" /
  #   "'map' has been recalculated, but ... 'idle'"
  # Those were Shiny's output state machine reporting overlapping recalculation
  # cycles for one output, which is exactly what was happening.
  #
  # map_ready is now flipped by the preload itself, once, AFTER everything the
  # render reads exists — so the render runs once at startup and once per
  # explicit rebuild, and there is no window where it can run half-fed.
  map_ready <- reactiveVal(FALSE)

  # Explicit trigger for a FULL widget rebuild. `input$sel_env_stat` used to be a
  # direct dependency of this render, which meant a polygon summary could be
  # wiped out by an env-stat change re-rendering the widget underneath it. Now
  # the env-stat observer decides: rebuild in hex mode (unchanged behavior), or
  # go through the proxy in polygon mode.
  map_rebuild <- reactiveVal(0)

  output$map <- renderMaplibreCompare({
    map_rebuild()
    # 3rd (and final) non-isolated dependency: the compare layout. Unlike the
    # env-stat trap the "EXACTLY TWO dependencies" note guards against, cmp_mode
    # only ever changes on an explicit user toggle of Compare / Split-Swipe --
    # never mid polygon-summary -- so re-rendering on it is safe and is exactly
    # what a layout switch needs.
    cmp_mode <- rx$cmp_mode %||% "swipe"
    req(map_ready())

    isolate({

    req(rx$df_env, rx$map_sp)

    if (debug) message("renderMaplibreCompare: generating environmental map...\n")

    env_stat       <- isolate(input$sel_env_stat) %||% "mean"
    env_stat_label <- names(which(env_stat_choices == env_stat))

    # cmp_mode was read (non-isolated) at the top of this render. "sync" = two
    # synced maps side by side, "swipe" = the draggable divider. compare()
    # keeps the same elementId and before/after sides in both modes, so every
    # maplibre_compare_proxy() call is unaffected.

    # The aggregation unit is read WITHOUT taking a reactive dependency: the
    # polygon summary is applied to the live maps by `apply_poly()` below,
    # through the compare proxy, never by re-rendering this widget. See the
    # comment on apply_poly() for why re-rendering is not an option.
    rx$agg_unit <- isolate(input$sel_agg_unit) %||% "hex"

    # Name the legend from what the data actually holds. "(density)" was false
    # for anything not gear-standardized; a bare "CPUE" was still wrong for a raw
    # occurrence count, since nothing was divided by effort. The breakdown goes in
    # the sidebar note — a legend title cannot carry it and should not try.
    #
    # Reads rx$sp_units, set beside rx$df_sp in BOTH observers that load a
    # selection. It is not computed here: `df_sp` is a local of the startup
    # observer above, out of scope in this render block, and computing it here
    # silently produced the empty-summary label on every render.
    rx$lbl_sp_value <- sp_value_label(rx$sp_units)

    if (USE_H3T) {
      # h3t path: reuse the tile_url + scale from the preload while the filters
      # behind them are unchanged, refetch when any of them moves.
      #
      # The condition used to be `is.null(rx$env_tile_url) || env_stat != "mean"`
      # and the rebuild branch never wrote rx$env_tile_url back — so after
      # startup set it once, the URL was frozen. A Submit that picked a
      # different environmental variable, date range or depth range updated
      # rx$lbl_env_var (the legend) and left the tiles showing temperature.
      key <- env_tile_key(env_stat)
      if (is.null(rx$env_tile_url) || !identical(key, rx$env_tile_key)) {
        env <- env_tile_spec(
          rx$env_var,
          isolate(rx$params$sel_qtr), isolate(rx$params$date_range),
          isolate(rx$params$depth_range), env_stat, rx$spatial_wkt,
          spatial_layer = rx$spatial_layer, spatial_names = rx$spatial_names)
        rx$env_tile_url     <- env$tile_url
        rx$env_scale_single <- env$scale
        rx$env_scale        <- rep(list(env$scale), length(res_range))
        rx$env_tile_key     <- key
      }
      map_env_obj <- map_env_h3t(rx$env_tile_url, rx$env_scale_single,
                                 env_stat_label, rx$lbl_env_var,
                                 view    = rx$last_map_view,
                                 is_dark = isolate(calcofi4r::cc_is_dark(input)))
      rx$params$map_params$env_stat <- env_stat
      rx$env_stat <- env_stat
      # the hex layer IDs actually on the map, so the polygon switch can hide
      # exactly those and no others — set_layout_property is NOT guarded against
      # a missing layer on the client, it throws
      rx$env_layer_ids <- "env"
      return(compare(rx$map_sp, map_env_obj, elementId = "map", mode = cmp_mode))
    }


    # classic path
    if (!is.null(rx$env_hex_list) && env_stat == "mean") {
      env_hex_list    <- rx$env_hex_list
      rx$env_hex_list <- NULL
    } else {
      env_hex_list <- prep_env_hex(rx$df_env, res_range, env_stat)
    }
    env_scale_list <- lapply(
      env_hex_list,
      interpolate_palette,
      column  = "env.value",
      palette = \(n) rev(hcl.colors(n, palette = "Spectral")))
    map_env_obj <- map_env(
      env_hex_list,
      env_scale_list,
      env_stat_label,
      rx$lbl_env_var,
      is_dark = isolate(calcofi4r::cc_is_dark(input)))

    rx$env_scale <- env_scale_list
    rx$params$map_params$env_stat <- env_stat
    rx$env_stat <- env_stat
    rx$env_layer_ids <- paste0("env", res_range)

    if (debug) {
      message("renderMaplibreCompare: creating comparison map")
      message("rx$map_sp class: ", paste(class(rx$map_sp), collapse = ", "))
      message("map_env_obj class: ", paste(class(map_env_obj), collapse = ", "))
    }

    compare(rx$map_sp, map_env_obj, elementId = "map", mode = cmp_mode)

    })  # isolate
  })

  # Compare-widget mode:
  #   * Compare OFF                 -> "swipe" (single map: the divider is
  #     hidden and the left map un-clipped by CSS -- no sync machinery, so
  #     pan/zoom is a plain single map)
  #   * Compare ON, Split (default) -> "sync": two synced side-by-side panes
  #   * Compare ON, Swipe           -> "swipe": the draggable divider
  # output$map takes a direct (non-isolated) dependency on rx$cmp_mode, so
  # setting it re-renders the widget -- no extra map_rebuild() bump (which
  # previously raced two compare() widgets into #map, "Source ... already
  # exists").
  rx$cmp_mode <- "swipe"
  observeEvent(list(input$cmp_env_toggle, input$cmp_layout), {
    rx$cmp_mode <- if (isTRUE(input$cmp_env_toggle) &&
                       !identical(input$cmp_layout %||% "split", "swipe"))
      "sync" else "swipe"
  }, ignoreInit = TRUE)

  # one-shot "align the two synced maps" latch -- see input$map_before_view.
  # Cleared on every widget rebuild so a fresh compare() re-aligns once.
  cmp_synced <- reactiveVal(FALSE)
  observeEvent(map_rebuild(), { cmp_synced(FALSE) }, ignoreInit = TRUE)

  # per-pane header labels for the side-by-side layout (ui.R's #map_title_sp /
  # #map_title_env, shown only when cmp_layout == "split"). The species name is
  # trimmed to its common name -- "Pacific sardine (pilchard) (species:
  # Sardinops sagax)" -> "Pacific sardine (pilchard)" -- dropping only the
  # parenthetical rank/scientific-name tail prep_db bakes in.
  output$map_title_sp <- renderText({
    nm <- rx$params$taxa %||% ""
    # taxa with no common name arrive as a bare "(rank: Scientific name)" --
    # stripping just the "(rank: ...)" tail then leaves nothing before the
    # dash, so fall back to the scientific name itself in that case. Mirrors
    # ui.R's fmtName() JS, which handles the same two shapes for the dropdown.
    ranks <- "species|subspecies|genus|family|order|class|phylum|kingdom|infraspecies|variety|forma|section|tribe|superfamily|suborder|infraorder"
    bare_re <- paste0("^\\((?:", ranks, "): (.+)\\)$")
    display <- if (grepl(bare_re, nm, perl = TRUE)) {
      sub(bare_re, "\\1", nm, perl = TRUE)
    } else {
      sub(paste0("\\s*\\((?:", ranks, "):.*$"), "", nm, perl = TRUE)
    }
    sprintf("Species — %s", display)
  })
  output$map_title_env <- renderText({
    sprintf("Environmental — %s", rx$lbl_env_var %||% "")
  })

  # summarize within polygons ----
  # The polygon summary is applied to the LIVE maps through the compare proxy
  # rather than by re-rendering `output$map`.
  #
  # Re-rendering does work — but it rebuilds both maps from scratch, which
  # throws away the viewport and every layer toggle the user has set, for what
  # is a change of one overlay. The proxy touches only the layers that change.
  #
  # Two traps cost a lot of time here and are both load-bearing:
  #   * over the proxy, a layer's source must be registered with add_source()
  #     FIRST and referenced by id — see add_poly_side();
  #   * nothing in this block may write to rx$params — see the note on the
  #     rx$agg_unit / rx$env_stat fields.
  # Each produced the same symptom, an empty map with no error anywhere.
  POLY_IDS <- list(
    before = c("sp_poly",  "sp_poly_nodata",  "sp_poly_nodata_hit"),
    after  = c("env_poly", "env_poly_nodata", "env_poly_nodata_hit"))

  hex_ids_for <- function(side)
    if (side == "before") rx$sp_layer_ids else rx$env_layer_ids

  # Layer removal, not visibility: mapgl's own layers control decides what is
  # listed, so a layer that is not on the map simply stops being offered.
  # `clear_layer()` on a compare proxy was a no-op until the fix in
  # bbest/mapgl (the R side sends `layer`, both compare handlers read
  # `message.layer_id`), which is why the hexagons stayed put underneath the
  # first polygon summary and a second switch would have thrown "Layer with id
  # sp_poly already exists". Needs mapgl >= that commit.
  # mapgl 0.5.0's clear_layer() sends `{layer: id}` for a compare proxy, but the
  # maplibregl_compare.js "remove_layer" handler reads `message.layer_id` -- so
  # clear_layer() is a silent no-op and the hexes stayed under the polygon
  # summary. Send the message ourselves with BOTH keys.
  remove_layers <- function(side, ids) {
    p <- maplibre_compare_proxy("map", map_side = side)
    for (id in ids) {
      session$sendCustomMessage("maplibre-compare-proxy", list(
        id = p$id,
        message = list(type = "remove_layer",
                       layer = id, layer_id = id, map = side)))
    }
    invisible(p)
  }

  clear_poly_layers <- function() {
    for (side in c("before", "after")) remove_layers(side, POLY_IDS[[side]])
  }

  # Remove the hex layers rather than setting visibility "none": on the client
  # `clear_layer` is wrapped in `if (map.getLayer(id))` but `set_layout_property`
  # is NOT, so hiding a layer that failed to add throws and takes the rest of the
  # proxy message batch with it. That is not hypothetical — the h3t tile layers
  # are absent whenever the tile service or its custom protocol is unavailable,
  # which is exactly when we would be reaching for them blind.
  clear_hex_layers <- function() {
    for (side in c("before", "after")) {
      ids <- hex_ids_for(side)
      if (!is.null(ids)) remove_layers(side, ids)
    }
  }

  # Add one side's polygon layers: outline + hoverable wash for the unsampled
  # polygons, fill for the summarized ones.
  #
  # The source is registered with add_source() FIRST and referenced by id, rather
  # than passing the sf straight to add_*_layer(). Over the compare proxy those
  # are not equivalent: add_source() serializes the sf to a GeoJSON string that
  # the client's "add_source" handler understands, while add_layer() forwards
  # `source` to map.addLayer() untouched — so an inline sf arrives as
  # `{geojson: …}`, maplibre accepts it as a geojson source spec, and the layer
  # renders NOTHING with no error (verified: querySourceFeatures() == 0 while the
  # layer itself was present on both maps).
  #
  # Each layer gets its OWN source under the same id because clear_layer()
  # removes the layer and the identically-named source together; two layers
  # sharing one source id would leave the second pointing at nothing.
  add_poly_side <- function(side, sf_poly, d_val, scale, is_dark) {
    ids    <- POLY_IDS[[side]]
    sf_all <- sf_poly |> left_join(d_val |> select(-spatial_name), by = "spatial_key")
    # only what the tooltip and the colour expression read: POSIXct columns have
    # no GeoJSON representation and the dates are already in the tooltip string
    keep   <- \(x) x |> select(any_of(c("spatial_key", "spatial_name", "value",
                                        "n", "tooltip")))
    sf_dat <- sf_all |> filter(!is.na(value)) |> keep()
    sf_nul <- sf_all |>
      filter(is.na(value)) |>
      mutate(tooltip = paste0("<strong>", spatial_name, "</strong><br>no data")) |>
      keep()
    p <- \() maplibre_compare_proxy("map", map_side = side)

    if (nrow(sf_nul) > 0) {
      p() |> add_source(id = ids[3], data = sf_nul)
      p() |> add_fill_layer(
        id            = ids[3],
        source        = ids[3],
        fill_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        fill_opacity  = 0.08,
        tooltip       = "tooltip",
        hover_options = list(fill_opacity = 0.25))
      p() |> add_source(id = ids[2], data = sf_nul)
      p() |> add_line_layer(
        id            = ids[2],
        source        = ids[2],
        line_color    = ifelse(is_dark, "#9e9e9e", "#616161"),
        line_width    = 1,
        line_opacity  = 0.6,
        hover_options = list(line_color = "#ffeb3b", line_opacity = 1))
    }
    if (nrow(sf_dat) > 0 && !is.null(scale)) {
      p() |> add_source(id = ids[1], data = sf_dat)
      p() |> add_fill_layer(
        id                 = ids[1],
        source             = ids[1],
        fill_color         = scale$expression,
        fill_outline_color = "white",
        fill_opacity       = 0.65,
        tooltip            = "tooltip",
        hover_options      = list(fill_outline_color = "#ffeb3b",
                                  fill_opacity       = 0.85))
    }
    nrow(sf_dat)
  }

  apply_poly <- function(agg_unit, env_stat) {
    req(rx$df_sp, rx$df_env)
    is_dark <- (input$dark_toggle %||% "dark") == "dark"

    sf_poly <- get_layer_sf(agg_unit)
    if (is.null(sf_poly)) {
      showNotification(paste0("No geometry available for '", agg_unit, "'."),
                       type = "error")
      updateSelectInput(session, "sel_agg_unit", selected = "hex")
      return(invisible(NULL))
    }

    sp_poly  <- calcofi4r::cc_track_query(session, "map_query_sp_poly",
      list(layer = agg_unit, taxa = rx$params$taxa),
      prep_sp_poly(rx$df_sp, agg_unit))
    env_poly <- calcofi4r::cc_track_query(session, "map_query_env_poly",
      list(layer = agg_unit, env_var = rx$env_var, env_stat = env_stat),
      prep_env_poly(rx$df_env, agg_unit, env_stat))

    rx$df_sp_poly  <- sp_poly
    rx$df_env_poly <- env_poly
    rx$n_poly      <- nrow(sf_poly)

    sp_scale  <- poly_scale(sp_poly$data,
      palette = \(n) hcl.colors(n, palette = "Viridis"))
    env_scale <- poly_scale(env_poly,
      palette = \(n) rev(hcl.colors(n, palette = "Spectral")))

    if (is.null(sp_scale) && is.null(env_scale))
      showNotification(paste0(
        "No observations fall within any polygon of '", agg_unit,
        "' for the current filters."), type = "warning")

    clear_poly_layers()
    clear_hex_layers()
    add_poly_side("before", sf_poly, sp_poly$data, sp_scale,  is_dark)
    add_poly_side("after",  sf_poly, env_poly,     env_scale, is_dark)

    # Rebuild the floating layers control so it lists what is actually on the
    # map: the boundary layer being summarized (named after itself, not
    # "Polygon Summary"), and no "Hexagon Data" entry, because the hexagons are
    # gone. Each side gets only its own ids — a control cannot reach the other
    # map, and the ui.R mirror script handles the cross-map half.
    for (side in c("before", "after")) {
      ctrl <- build_layers_control(
        rx$spatial_visible, d_spatial_layers, POLY_IDS[[side]],
        label = agg_unit)
      maplibre_compare_proxy("map", map_side = side) |>
        clear_controls(controls = "layers") |>
        add_layers_control(
          position     = "top-right",
          layers       = ctrl,
          collapsible  = TRUE,
          margin_right = 45)
    }

    # a polygon summary has no zoom-dependent scale, so hand the legend observer
    # the same one at every level (the h3t path does the same)
    rx$sp_scale     <- rep(list(sp_scale),  length(res_range))
    rx$env_scale    <- rep(list(env_scale), length(res_range))
    rx$lbl_sp_value <- if (!is.na(sp_poly$unit))
      paste0("Avg. CPUE (", fmt_cpue_unit(sp_poly$unit), ")") else "Avg. CPUE"
    rx$agg_unit <- agg_unit
    rx$env_stat <- env_stat

    # fit to the polygons that HAVE data, so switching unit lands where the
    # observations are rather than at the full extent of a statewide layer. This
    # also fires the moveend the legend observer listens for.
    sf_dat <- sf_poly |>
      inner_join(sp_poly$data |> select(spatial_key), by = "spatial_key")
    if (nrow(sf_dat) == 0) sf_dat <- sf_poly
    for (side in c("before", "after"))
      maplibre_compare_proxy("map", map_side = side) |> fit_bounds(bbox = sf_dat)

    invisible(NULL)
  }

  # Returning to hexagons drops the polygon layers and rebuilds the widget, which
  # is the only path that knows how to reconstruct BOTH hex paths (h3t tile
  # source, or ten sf resolution layers) — apply_poly() removed them outright
  # rather than hiding them, for the reason given on clear_hex_layers().
  restore_hex <- function() {
    clear_poly_layers()
    # reuse the breakdown computed for the current selection rather than a fixed
    # string — the polygon path may have narrowed the label to its one chosen unit
    rx$lbl_sp_value <- sp_value_label(rx$sp_units %||%
                                        sp_unit_summary(req(rx$df_sp)))
    rx$agg_unit <- "hex"
    map_rebuild(map_rebuild() + 1)
  }

  observeEvent(input$sel_agg_unit, {
    agg_unit <- input$sel_agg_unit %||% "hex"
    if (debug) message("sel_agg_unit -> ", agg_unit)
    if (agg_unit == "hex") restore_hex()
    else apply_poly(agg_unit, input$sel_env_stat %||% "mean")
  }, ignoreInit = TRUE)

  # env_stat: in polygon mode recompute through the proxy; in hex mode rebuild
  # the widget, which is exactly what this control did before.
  observeEvent(input$sel_env_stat, {
    agg_unit <- input$sel_agg_unit %||% "hex"
    if (agg_unit == "hex") map_rebuild(map_rebuild() + 1)
    else apply_poly(agg_unit, input$sel_env_stat %||% "mean")
  }, ignoreInit = TRUE)

  # dark_toggle -> rebuild the map in the new theme ----
  # set_style() on the live compare proxy re-themed the basemap but wiped every
  # added source/layer (hexes, boundaries) without re-adding them, and only the
  # env pane re-rendered afterwards -- so light mode left the species pane on a
  # dark basemap. The species widget bakes its basemap style in at build time,
  # so the fix is to rebuild it and re-render both panes.
  observeEvent(input$dark_toggle, {
    is_dark <- input$dark_toggle == "dark"
    agg     <- isolate(input$sel_agg_unit) %||% "hex"

    # polygon-summary mode: apply_poly() rebuilds both sides with the theme
    if (!identical(agg, "hex")) {
      apply_poly(agg, isolate(input$sel_env_stat) %||% "mean")
      return(invisible(NULL))
    }

    p <- rx$params
    if (!is.null(rx$df_sp) && !is.null(p$taxa)) {
      spec <- sp_map_spec(
        rx$df_sp, p$taxa,
        p$sel_qtr %||% 1:4, p$date_range %||% min_max_date, p$ck_children %||% FALSE,
        datasets      = isolate(input$sel_bio_ds),
        poly_wkt      = rx$spatial_wkt,
        spatial_layer = rx$spatial_layer, spatial_names = rx$spatial_names,
        cpue_unit     = isolate(rx$cpue_chosen),
        is_dark       = is_dark)
      rx$map_sp       <- spec$map
      rx$sp_layer_ids <- spec$layer_ids
      rx$sp_scale     <- spec$scales
    }
    map_rebuild(map_rebuild() + 1)
  }, ignoreInit = TRUE)

  # map layers modal ----
  # "Map Layers" is a chip-row button (input$open_map_layers) opening a rich
  # thumbnail modal (functions.R::modal_map_layers()). The grouped checkboxes
  # keep their ids (lyr_<make.names(group)>); Apply (btn_layers_apply) commits
  # the selection to rx$spatial_visible and the map.
  rx$spatial_visible <- d_spatial_layers |>
    filter(default_visible) |>
    pull(dataset_id)

  layer_grp_ids <- paste0("lyr_", make.names(unique(d_spatial_layers$group)))

  observeEvent(input$open_map_layers, {
    trk("open_layers")
    showModal(modal_map_layers())
  }, ignoreInit = TRUE)

  # modal footer counter -- live over the checkbox inputs themselves
  output$n_layers_selected <- renderText({
    n_sel <- length(unlist(lapply(layer_grp_ids, function(id) input[[id]])))
    sprintf("%d of %d layers selected", n_sel, nrow(d_spatial_layers))
  })

  observeEvent(input$btn_layers_apply, {
    # collect selected layer IDs from all checkbox groups
    selected <- unlist(lapply(layer_grp_ids, function(id) input[[id]]))
    if (is.null(selected)) selected <- character(0)

    rx$spatial_visible <- selected

    # which reference layers people actually turn on — the full id list goes to
    # the Sheet leg (GA4 would bucket a many-valued dimension into "(other)")
    trk("select_layers", layers = selected, n_layers = length(selected))

    # toggle visibility on both sides of compare map
    polygon_layers <- d_spatial_layers |>
      filter(geom_type == "polygon") |>
      pull(dataset_id)

    for (lyr_id in d_spatial_layers$dataset_id) {
      vis <- ifelse(lyr_id %in% selected, "visible", "none")
      for (side in c("before", "after")) {
        maplibre_compare_proxy("map", map_side = side) |>
          set_layout_property(lyr_id, "visibility", vis)
        # also toggle outline layer for polygons
        if (lyr_id %in% polygon_layers) {
          maplibre_compare_proxy("map", map_side = side) |>
            set_layout_property(
              paste0(lyr_id, "_outline"), "visibility", vis)
        }
      }
    }

    # Rebuild the layers control on both sides, each with only ITS OWN data
    # layer ids. Handing every side both sides' ids is why the data toggle only
    # worked one way: the control lists e.g. "env" on the species map, where no
    # such layer exists, and the client's set_layout_property is NOT guarded by
    # `if (map.getLayer(id))` — so switching the group back ON throws and the
    # toggle dies half-applied. The ids come from what was actually added to
    # each map (h3t: "sp"/"env"; classic: sp1..sp10 / env1..env10), not from a
    # hardcoded classic-path guess.
    #
    # Only the "after" side when Compare is ON: with it off the env pane has no
    # data layer, so rebuilding its control against rx$env_layer_ids ("env")
    # throws "non-existing layer env" on the client. The pane is hidden then
    # anyway and rebuilt fresh when Compare is toggled on.
    sides <- if (isTRUE(input$cmp_env_toggle)) c("before", "after") else "before"
    for (side in sides) {
      ctrl <- build_layers_control(
        selected, d_spatial_layers,
        if (side == "before") rx$sp_layer_ids else rx$env_layer_ids)
      maplibre_compare_proxy("map", map_side = side) |>
        clear_controls(controls = "layers") |>
        add_layers_control(
          position     = "top-right",
          layers       = ctrl,
          collapsible  = TRUE,
          margin_right = 45)
    }

    # Re-assert the hex/data layers as visible. Rebuilding the layers control
    # (clear + add) can leave the "Hexagon Data" toggle rendering unchecked and
    # the layer hidden -- so a boundary layer toggle read as "hexes vanished".
    # Same `sides` guard as the control rebuild (the after/env layer only
    # exists when Compare is on).
    for (side in sides) {
      ids <- if (side == "before") rx$sp_layer_ids else rx$env_layer_ids
      for (lid in ids)
        maplibre_compare_proxy("map", map_side = side) |>
          set_layout_property(lid, "visibility", "visible")
    }

    removeModal()
  }, ignoreInit = TRUE)

  # Push the two colour-scale ranges + labels to the cross-pane hover readout
  # (ui.R's cc_hexrange handler). Called on map move (with the live zoom's
  # scale interval) AND whenever the env variable / stat / species-value label
  # changes without a map move -- otherwise the readout keeps naming the old
  # variable ("Avg. Temperature") after a switch to pH until you next pan.
  push_hexrange <- function(i = NULL) {
    if (is.null(i)) {
      z <- isolate(input$map_before_view$zoom)
      i <- if (is.null(z) || !is.finite(z)) 1L
           else findInterval(z, zoom_breaks, rightmost.closed = TRUE)
    }
    sp_scales  <- rx$sp_scale
    env_scales <- rx$env_scale
    if (is.null(sp_scales) && is.null(env_scales)) return(invisible())
    at <- function(l) if (is.null(l)) NULL else l[[max(1L, min(i, length(l)))]]
    sp_scale  <- at(sp_scales)
    env_scale <- at(env_scales)
    env_stat     <- isolate(input$sel_env_stat) %||% "mean"
    lbl_env_stat <- names(which(env_stat_choices == env_stat))
    session$sendCustomMessage("cc_hexrange", list(
      sp        = if (!is.null(sp_scale))  as.numeric(range(sp_scale$breaks))  else NULL,
      env       = if (!is.null(env_scale)) as.numeric(range(env_scale$breaks)) else NULL,
      sp_label  = rx$lbl_sp_value %||% "Species",
      env_label = trimws(paste(lbl_env_stat, rx$lbl_env_var))))
  }

  observeEvent(
    list(rx$lbl_env_var, rx$lbl_sp_value, rx$sp_scale, rx$env_scale,
         input$sel_env_stat),
    push_hexrange(), ignoreInit = TRUE)

  # draw_legends ----
  # Both map legends. Factored out of the map-move observer below, which used
  # to be the ONLY place they were drawn -- so they were entirely gated on an
  # actual moveend ever firing. Turning Compare on, or a fresh species/env
  # selection, updates rx$sp_scale/rx$env_scale without moving the map at
  # all, so the environmental legend (and, after a fresh selection, the
  # species one) just never appeared until the user happened to pan or zoom
  # (bug report 2026-09-07: "the environment legend isnt showing up until
  # click into map"). Mirrors push_hexrange() above, which already solved
  # the identical problem for the hover readout -- same i = NULL fallback,
  # same "call it on every relevant rx$ change, not just on moveend".
  draw_legends <- function(i = NULL) {
    if (is.null(rx$sp_scale) || is.null(rx$env_scale)) return(invisible())
    if (is.null(i)) {
      z <- isolate(input$map_before_view$zoom)
      i <- if (is.null(z) || !is.finite(z)) 1L
           else findInterval(z, zoom_breaks, rightmost.closed = TRUE)
    }
    if (i < 1 || i > length(rx$sp_scale)) return(invisible())

    sp_scale  <- rx$sp_scale[[i]]
    env_scale <- rx$env_scale[[i]]

    env_stat     <- input$sel_env_stat %||% "mean"
    lbl_env_stat <- names(which(env_stat_choices == env_stat))

    # legend as a near-opaque themed card -- the old 50%-white wash was
    # unreadable over the hexagons underneath, especially in light mode
    lg_dark  <- isTRUE(tryCatch(calcofi4r::cc_is_dark(input), error = function(e) FALSE))
    # brand v2 tokens (calcofi.io/brand/v2/theme.css): --panel / --fg /
    # --border per theme -- mapgl paints the legend itself, so it cannot read
    # the CSS variables the rest of the page uses
    lg_style <- legend_style(
      background_color   = if (lg_dark) "#182b49" else "#ffffff",
      background_opacity = 0.94,
      text_color         = if (lg_dark) "#e9edf3" else "#182b49",
      title_color        = if (lg_dark) "#e9edf3" else "#182b49",
      border_color       = if (lg_dark) "#34486b" else "#dddddd",
      border_width       = 1,
      border_radius      = 8,
      # a genuinely compact legend -- smaller type + tighter padding, not just
      # a narrower box
      title_size         = 11,
      text_size          = 9,
      padding            = 6)

    # Species legend (left / before)
    # In hex mode the title is unit-free: CPUE is count/10m² for oblique and
    # vertical tows but count/100m³ for manta, and the hexagon value averages
    # across net types (per-tow unit is in the download). The polygon path picks
    # ONE cpue_unit and names it here instead — rx$lbl_sp_value carries whichever
    # applies. A NULL scale means nothing was summarizable, so draw no legend
    # rather than an empty one.
    if (!is.null(sp_scale)) {
      maplibre_compare_proxy("map", map_side = "before") |>
        add_legend(
          legend_title = rx$lbl_sp_value %||% "Avg. abundance",
          values       = round(sp_scale$breaks, 2),
          colors       = sp_scale$colors,
          type         = "continuous",
          position     = "bottom-left",
          width        = "230px",
          target       = "compare",
          style        = lg_style,
          add          = FALSE
        )
    }

    # Environmental legend (right / after)
    if (!is.null(env_scale)) {
      maplibre_compare_proxy("map", map_side = "after") |>
        add_legend(
          legend_title = paste(lbl_env_stat, rx$lbl_env_var),
          values       = signif(env_scale$breaks, 4),
          colors       = env_scale$colors,
          type         = "continuous",
          position     = "bottom-right",
          width        = "230px",
          target       = "compare",
          style        = lg_style,
          add         = TRUE
        )
    }
  }

  observeEvent(
    list(rx$sp_scale, rx$env_scale, rx$lbl_env_var, rx$lbl_sp_value,
         input$sel_env_stat),
    draw_legends(), ignoreInit = TRUE)

  # Re-push both legends after a FULL widget rebuild -- output$map's own two
  # non-isolated deps are map_rebuild() and rx$cmp_mode (see its render
  # above); either one throws away the live compare widget instance that any
  # earlier draw_legends() proxy call was targeting, so whatever legend was
  # drawn before the rebuild is gone with it. Turning Compare on is exactly
  # this case: it flips rx$cmp_mode, which rebuilds the widget, but doesn't
  # necessarily touch rx$sp_scale/rx$env_scale (those may already be set from
  # before Compare was ever toggled on), so the observer just above never
  # refires -- the environment legend then stayed missing until a pan/zoom
  # happened to call draw_legends() again via the map-move observer below
  # (bug report 2026-09-08, recurrence of "the environment legend isnt
  # showing up until click into map", 2026-09-07).
  #
  # This used to route through session$onFlushed (defer until the new
  # widget's initial payload has been flushed to the client). That only
  # guarantees the R-side message reached the browser -- it says nothing
  # about whether the browser has actually finished creating the WebGL
  # context and loading the maplibre GL style for the NEW map pair, which is
  # an async step of its own. A legend proxy call that lands before that
  # finishes is a silent no-op, and that race is exactly what made the
  # legend flicker / intermittently require a pan-zoom to reappear (bug
  # report 2026-09-07: "why does the legend flicker...you need to click into
  # the map and move the view to have it appear sometimes? why doesnt it
  # just always appear when map view is open?").
  #
  # ui.R's wire() (the cross-pane hex-compare script) already has to poll
  # for each fresh widget instance's underlying maplibre GL JS map objects,
  # and now waits for both to genuinely fire 'load' before signaling
  # Shiny.setInputValue('cc_map_ready', ...) -- a real "the new maps are
  # ready for proxy calls" event instead of a guess about Shiny's own flush
  # timing. Listen for that instead.
  observeEvent(input$cc_map_ready, {
    draw_legends()
  }, ignoreInit = TRUE)

  # Cache the client's current view on every moveend, unconditionally --
  # see rx$last_map_view's own comment above. Deliberately separate from the
  # legend-lookup observer right below (which req()s rx$sp_scale/env_scale
  # and bails on plenty of early/invalid views): a full map rebuild can
  # happen before those are ready, and this needs to have already recorded
  # SOME view by the time it does, not share that observer's gate.
  observeEvent(input$map_before_view, {
    view <- input$map_before_view
    if (!is.null(view$zoom) && is.finite(view$zoom) &&
        length(view$center) == 2 && all(is.finite(unlist(view$center))))
      rx$last_map_view <- list(center = view$center, zoom = view$zoom)
  }, ignoreInit = TRUE)

  # map zoom ----
  observeEvent(input$map_before_view, {
    req(rx$sp_scale, rx$env_scale)

    view <- input$map_before_view
    req(view$zoom)

    # Side-by-side ("sync") layout: mapgl only locks the two maps together once
    # one of them MOVES. On first load each fit_bounds'd its own container, which
    # can leave them at slightly different zooms -- align them ONCE, right after
    # the widget loads. Never on later moveends: doing it every time fought the
    # user's pan/zoom (jump_to -> moveend -> jump_to ... a sync loop that froze
    # the map). cmp_synced is reset by the map_rebuild observer below.
    if (identical(rx$cmp_mode, "sync") && !cmp_synced() &&
        length(view$center) == 2 && is.finite(view$zoom)) {
      cmp_synced(TRUE)
      maplibre_compare_proxy("map", map_side = "after") |>
        jump_to(center = view$center, zoom = view$zoom)
    }

    z <- view$zoom
    i <- findInterval(z, zoom_breaks, rightmost.closed = TRUE)

    # Guard against weird zoom values
    if (i < 1 || i > length(rx$sp_scale)) return(NULL)

    draw_legends(i)

    # hand the two value ranges + labels to the cross-pane hover readout
    # (ui.R script): raw abundance and raw degrees C are not comparable, but
    # "72% of the way up its own colour scale" vs "28% up its own" is.
    push_hexrange(i)
  })

  # cpue_unit_selector ----
  # The "Standardized as" radio group -- only appears when the current
  # taxon/filter selection is actually published in more than one cpue_unit
  # (rx$cpue_units/rx$cpue_chosen are set alongside rx$df_sp; see
  # filter_cpue_unit()). Applies to Map, Time Series, Scatterplot and Depth
  # Profile alike, since all four read the already-filtered rx$df_sp.
  output$cpue_unit_selector <- renderUI(cpue_unit_selector_ui(rx$cpue_units, rx$cpue_chosen))

  # poly_note ----
  # What the polygon summary actually covers: how many of the layer's polygons
  # carry data, which cpue_unit the species side settled on, and how many
  # observations that excluded. The unit choice is not cosmetic — std_tally is a
  # gear-standardized density only where a net tow supports it — so it is stated
  # in the sidebar rather than left to be inferred from the legend.
  output$poly_note <- renderUI({
    agg_unit <- input$sel_agg_unit %||% "hex"

    # HEX MODE: the legend can only name the quantity; it cannot say that two
    # rows under it were measured differently. `std_tally` is a gear-standardized
    # density only where a net tow supports it — Pacific sardine from an oblique
    # tow is count/10m2, computed as tally x std_haul_factor / prop_sorted —
    # whereas Dungeness megalopae are an occurrence count in a lab-examined
    # aliquot of an archived catch, with no tow volume to divide by. Both used to
    # render under one legend reading "Avg. CPUE (density)", which was false for
    # the second and unverifiable for the first. So state it here, from the data.
    if (agg_unit == "hex") {
      u <- rx$sp_units
      if (is.null(u) || !nrow(u)) return(NULL)
      n_tot <- sum(u$n)

      # one unit -> a quiet one-liner. more than one -> a real callout, because
      # the hexagon value is then an average across incompatible quantities.
      if (nrow(u) <= 1) {
        return(div(
          class = "cc-unit-note",
          sprintf("Hexagon values: average %s%s.", fmt_cpue_unit(u$cpue_unit[1]),
                  if (isTRUE(u$standardized[1])) " (effort-standardized)"
                  else " (published as-is, not effort-standardized)")))
      }

      div(
        class = "cc-unit-note",
        div(class = "cc-unit-note-head",
            sprintf("Hexagon values average across %d different units:", nrow(u))),
        tags$ul(
          class = "cc-unit-list",
          lapply(seq_len(nrow(u)), function(i) tags$li(
            tags$strong(sprintf("%s obs (%.0f%%)",
                                format(u$n[i], big.mark = ","), 100 * u$n[i] / n_tot)),
            tags$span(class = "cc-unit-unit", fmt_cpue_unit(u$cpue_unit[i])),
            if (isTRUE(u$standardized[i]))
              tags$span(class = "cc-unit-std", "effort-standardized")))),
        if (any(!u$standardized)) div(
          class = "cc-unit-foot",
          "Rows not marked effort-standardized are shown as published."))
    } else {

    req(rx$df_sp_poly)

    sp_poly <- rx$df_sp_poly
    n_with  <- nrow(sp_poly$data)
    n_tot   <- rx$n_poly %||% n_with

    div(
      class = "small text-muted mb-3",
      div(sprintf("%s of %s polygons contain observations; the rest are drawn as outlines marked “no data”.",
                  format(n_with, big.mark = ","), format(n_tot, big.mark = ","))),
      if (!is.na(sp_poly$unit)) div(
        class = "mt-1",
        sprintf("Species values are averaged in %s.", fmt_cpue_unit(sp_poly$unit)),
        if (sp_poly$n_excluded > 0) sprintf(
          " %s observation(s) in %d other unit(s) are excluded — averaging across units is not a quantity.",
          format(sp_poly$n_excluded, big.mark = ","), nrow(sp_poly$units) - 1L))
    )
    }
  })

  # ts_plot ----
  output$ts_plot <- renderHighchart({
    req(rx$df_sp, rx$df_env, rx$env_var)

    if (debug) message("renderHighchart: generating time series plot\n")

    ts_res <- input$sel_ts_res %||% "year"
    sp_ts  <- prep_ts_sp(rx$df_sp, ts_res) |> arrange(time)
    env_ts <- prep_ts_env(rx$df_env, ts_res)

    rx$params$ts_params$ts_res <- ts_res

    # the package's plot_ts (calcofi4r >= 1.10.0), which no longer reaches for
    # the app-global env_var_choices: hand it the label
    calcofi4r::plot_ts(
      sp_ts, env_ts, ts_res, rx$env_var,
      is_dark   = calcofi4r::cc_is_dark(input),
      env_label = env_var_label(rx$env_var))
  })

  # splot ----
  output$splot <- renderPlotly({
    df_splot <- prep_splot(rx$df_sp, rx$df_env, "mean",
                           method = input$splot_method,
                           max_hours_diff = input$splot_max_hours_diff,
                           max_meters_diff = input$splot_max_meters_diff)
    rx$df_splot <- df_splot
    rx$params$splot_params <- list(
      time_window = input$splot_max_hours_diff,
      dist_window = input$splot_max_meters_diff,
      method      = input$splot_method
    )

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
          "<br><b>CPUE:</b> ", round(sp_tally, 2) ))

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
        color = "Species")

    # convert to plotly with bslib theme support
    p_out <- ggplotly(p, tooltip = "text", source = "scatterPlotSource") |>
      layout(dragmode = "select") |>
      config(
        displaylogo            = FALSE,
        scrollZoom             = TRUE,
        modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian") ) |>
      # Declare the events the observers below consume. Two observers call
      # event_data("plotly_click" / "plotly_selected", source =
      # "scatterPlotSource"), and plotly warns once per unregistered event on
      # every render because it only wires up the JS handlers for events the
      # plot has registered — without these, clicking a point could silently
      # deliver nothing.
      event_register("plotly_click") |>
      event_register("plotly_selected") |>
      toWebGL() # for performance

    splot_ready(TRUE)
    p_out
  })

  # edit_filters -> modal_edit_filters() ----
  # Was triggered by input$sel_data (the sidebar's "Select Filters" button,
  # which opened a 4-tab modal covering Taxa/Environmental/Temporal/Spatial).
  # Taxa and Environmental Variable moved to the always-visible top bar
  # (functions.R::top_bar_taxa_ui()/top_bar_env_ui()); Datasets, Depth, and
  # Time now live directly in this compact panel, and Spatial moved out to
  # its own dialog (modal_spatial_filter(), below) reached via the "Layers"
  # row's "Change" link -- so this observer only needs to show the modal.
  observeEvent(input$edit_filters, {
    # carry the currently-applied depth/quarter/date-range/datasets across a
    # reopen -- the modal is rebuilt from scratch each time, so anything not
    # passed here silently resets to its default (see modal_edit_filters()
    # docs). bio_datasets was missing from this list -- do_apply_filters()
    # always wrote the applied selection to rx$params$bio_datasets, but
    # nothing ever read it back out on reopen, so dataset_list_picker_ui()
    # fell back to its own hardcoded "every dataset" default every time
    # (bug report 2026-09-07: filtering to one dataset, applying, then
    # reopening "Edit filters" showed all 10 checked again).
    showModal(modal_edit_filters(
      depth_range  = rx$params$depth_range  %||% c(0, 515),
      qtr          = rx$params$sel_qtr      %||% 1:4,
      date_range   = rx$params$date_range   %||% min_max_date,
      bio_datasets = rx$params$bio_datasets %||% d_bio_datasets$dataset_key))
  })

  # edit_spatial -> modal_spatial_filter(), spatial_filter_map ----
  # The "Area" row's "Change" link inside modal_edit_filters(). Renders the
  # exact same spatial_filter_map/tbl_places outputs the old Spatial TAB
  # rendered on modal open -- only the trigger id changed.
  observeEvent(input$edit_spatial, {
    # reopen on the category the picked names belong to (rx$sel_places_cat,
    # from main) when one is set, falling back to whatever the user last had
    # the dropdown on otherwise -- read BEFORE showModal() rebuilds
    # sel_places_cat's <select> (and so resets its input value): this is
    # still whatever the user last had it set to (bug report 2026-09-08:
    # reopening "Layers" > "Change" always snapped the Category dropdown
    # back to "CalCOFI Zones"). See modal_spatial_filter()'s own doc for
    # the full story.
    showModal(modal_spatial_filter(
      selected_cat = rx$sel_places_cat %||% input$sel_places_cat %||% "CalCOFI Zones"))

    output$spatial_filter_map <- renderMaplibre({
      if (input$sel_places_cat == "Custom") {
        maplibre(
          style = carto_style(ifelse(
            input$dark_toggle == "dark",
            "dark-matter",
            "voyager"))) |>
          add_draw_control(
            position = "top-right",
            displayControlsDefault = FALSE,
            controls = list(polygon = TRUE, trash = TRUE))
      } else if (input$sel_places_cat %in% sample_spatial_layers) {
        # No polygon geometry loaded locally for these layers (see
        # global.R::sample_spatial_layers) -- membership comes from
        # sample_spatial instead of a drawn/rendered polygon. Plain basemap;
        # selection happens in the table beside it (output$tbl_places).
        maplibre(
          style = carto_style(ifelse(
            input$dark_toggle == "dark",
            "dark-matter",
            "voyager")))
      } else {
        places <- cc_places |>
          filter(
            category == input$sel_places_cat
          )

        maplibre(
          style = carto_style(ifelse(
            input$dark_toggle == "dark",
            "dark-matter",
            "voyager")),
          bounds = places) |>
          add_fill_layer(
            id = 'base-zones',
            places,
            fill_color = match_expr(
              "name",
              values = unique(places$name),
              stops = hcl.colors(length(unique(places$name)))),
            fill_opacity = 0.5,
            fill_outline_color = "black") |>
          add_fill_layer(
            id = 'sel-zones',
            places,
            fill_color = match_expr(
              "name",
              values = unique(places$name),
              stops = hcl.colors(length(unique(places$name)))),
            fill_opacity = 0.0,
            fill_outline_color = NULL) |>
          add_line_layer(
            id = 'sel-zones-outline',
            places,
            line_color = ifelse(input$dark_toggle == "dark", "#dee2e6", "#333333"),
            line_width = 3,
            line_opacity = 0.0)
      }
     })

    output$tbl_places <- renderDataTable({
      places_names_for(input$sel_places_cat)
    })
  })

  # dataset -> taxa list ----
  # Narrowing the datasets narrows the taxa offered. The current selection is
  # carried over where it survives, so unchecking an unrelated dataset does not
  # silently clear what the user already picked.
  observeEvent(input$sel_bio_ds, {
    keep <- intersect(input$sel_name, sp_names_for(input$sel_bio_ds))
    updateSelectizeInput(
      session, "sel_name",
      choices  = sp_choices(input$sel_bio_ds),
      selected = keep,
      # server = FALSE on purpose: the custom .cc-combo2 panel is built by JS
      # from the selectize <optgroup>/<option> DOM, and server-side selectize
      # only ships a partial page of options -- which collapsed the picker to
      # the handful of already-loaded taxa (e.g. "Seabirds & Mammals: 1").
      # ~1,375 short options render client-side with no perceptible cost.
      server   = FALSE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # show-all -> environmental variable list ----
  # No dataset filter on this tab: every measurement type belongs to exactly one
  # dataset, so the grouped list already carries that information.
  observeEvent(input$sel_env_all_vars, {
    ch  <- env_var_choices(show_all = isTRUE(input$sel_env_all_vars))
    cur <- input$sel_env_var
    # keep the current variable if its dataset is still checked, else fall back
    # to the first on offer rather than leaving a value the list no longer has
    sel <- if (!is.null(cur) && cur %in% unlist(ch)) cur else unlist(ch)[1]
    updateSelectInput(session, "sel_env_var", choices = ch, selected = sel)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # switching category drops the names picked in the previous one: they are
  # matched against the active category's polygons at submit, and the table /
  # map highlight for the new category cannot show them anyway. Reopening the
  # dialog on the same category (modal_spatial_filter(selected_cat = )) is not
  # a change.
  observeEvent(input$sel_places_cat, {
    if (!is.null(rx$sel_places_cat) &&
        !identical(input$sel_places_cat, rx$sel_places_cat)) {
      rx$sel_places     <- character(0)
      rx$sel_places_cat <- NULL
    }
  })

  # Observe clicks on the grid layer of spatial filter map
  observeEvent(input$spatial_filter_map_feature_click, {

    # Custom (drawn polygon) has no clickable fill layer at all; table-only
    # categories (global.R::sample_spatial_layers) have no polygon loaded, so
    # there is nothing on spatial_filter_map to click either -- selection for
    # those happens via the tbl_places_rows_selected handler below instead.
    custom <- input$sel_places_cat == "Custom" ||
      input$sel_places_cat %in% sample_spatial_layers

    click <- input$spatial_filter_map_feature_click

    # Only process clicks on the grid layer
    if (!is.null(click$properties$name) & !custom) {
      clicked_place <- click$properties$name
      current_places <- rx$sel_places

      # Toggle zone selection
      if (clicked_place %in% current_places) {
        # Remove if already selected
        new_places <- setdiff(current_places, clicked_place)
      } else {
        # Add to selection
        new_places <- c(current_places, clicked_place)
      }

      rx$sel_places     <- new_places
      rx$sel_places_cat <- input$sel_places_cat

      # Update map styling to highlight selected zones
      if (length(new_places) > 0) {
        maplibre_proxy("spatial_filter_map") |>
          set_filter("sel-zones",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones", "fill-opacity", 0.8) |>
          set_paint_property("sel-zones", "fill-outline-color", "black") |>
          set_filter("sel-zones-outline",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones-outline", "line-opacity", 1.0)
      } else {
        # Reset filter if no zones selected
        maplibre_proxy("spatial_filter_map") |>
          set_paint_property("sel-zones", "fill-opacity", 0.0) |>
          set_paint_property("sel-zones", "fill-outline-color", NULL) |>
          set_paint_property("sel-zones-outline", "line-opacity", 0.0)
      }

      # Update table row selection
      places_tbl <- cc_places |>
        filter(category == input$sel_places_cat)
      rows_to_select <- which(places_tbl$name %in% new_places)

      tbl_proxy <- dataTableProxy("tbl_places")
      selectRows(tbl_proxy, rows_to_select)
    }
  })

  # Observe clicks on table rows
  observeEvent(input$tbl_places_rows_selected, {
    req(input$sel_places_cat)

    sel_rows <- input$tbl_places_rows_selected

    # places_names_for() is source-agnostic: cc_places rows for a mapped
    # category, or sample_spatial-distinct spatial_name rows for a
    # table-only layer (global.R::sample_spatial_layers) -- either way the
    # table renders the same `name` column, so row index -> name works the
    # same regardless of source.
    places_tbl <- places_names_for(input$sel_places_cat)

    if (is.null(sel_rows) || length(sel_rows) == 0) {
      new_places <- character(0)
      rx$sel_places <- character(0)
    } else {
      # Map selected rows to keys
      new_places <- places_tbl$name[sel_rows]

      # Update reactive selection
      rx$sel_places     <- new_places
      rx$sel_places_cat <- input$sel_places_cat
    }

    # Map styling only applies to categories with polygon geometry drawn on
    # spatial_filter_map (the cc_places "sel-zones"/"sel-zones-outline"
    # layers) -- table-only layers have no such layer to style.
    if (!input$sel_places_cat %in% sample_spatial_layers) {
      if (length(new_places) > 0) {
        maplibre_proxy("spatial_filter_map") |>
          set_filter("sel-zones",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones", "fill-opacity", 0.8) |>
          set_paint_property("sel-zones", "fill-outline-color", "black") |>
          set_filter("sel-zones-outline",
                     list("in", list("get", "name"), list("literal", new_places))) |>
          set_paint_property("sel-zones-outline", "line-opacity", 1.0)
      } else {
        # Reset filter if no zones selected
        maplibre_proxy("spatial_filter_map") |>
          set_paint_property("sel-zones", "fill-opacity", 0.0) |>
          set_paint_property("sel-zones", "fill-outline-color", NULL) |>
          set_paint_property("sel-zones-outline", "line-opacity", 0.0)
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # submit -> ... ----
  # do_apply_filters() ----
  # The entire "apply the current selections" pipeline, factored out of what
  # used to be a single observeEvent(input$submit, ...) so it can be invoked
  # from TWO places: input$submit (the "Edit Filters" modal's footer button,
  # for Temporal/Depth/Spatial changes) and the top-bar Species/Taxa +
  # Compare Environmental Variable controls (which now live outside any
  # modal and auto-apply on change, per the redesign -- see the observers
  # right below this function). The body itself is UNCHANGED from the
  # original submit handler: only the trigger moved, not the query logic.
  # reset_* params, added 2026-09-08: an OVERRIDE for the value normally read
  # from input$sel_qtr / input$sel_date_range / input$sel_depth_range /
  # input$sel_bio_ds. Exists ONLY for reset_filters (below) to call this
  # function immediately after resetting those widgets ("reset all should
  # auto apply"). Reading input$sel_qtr etc. right after update*Input()
  # would NOT see the reset value yet -- update*Input() only sends a message
  # asking the CLIENT to change the widget; input$sel_qtr only becomes the
  # new value once the browser round-trips it back as a fresh input message,
  # a separate reactive flush this same function call happens well before.
  # Calling do_apply_filters() straight after update*Input() would silently
  # apply the OLD, pre-reset filters while still closing the modal like a
  # successful reset -- worse than the plain "does nothing until Apply" gap
  # it replaces. Passing the just-set values in directly sidesteps that
  # round-trip entirely; every other caller omits these and gets the normal
  # input-driven behavior, unchanged.
  do_apply_filters <- function(reset_qtr = NULL, reset_date_range = NULL,
                               reset_depth_range = NULL, reset_bio_datasets = NULL) {
    if (debug) message("\n=== DATA SELECTION SUBMITTED ===\n")

    # collect input selections
    #
    # sel_qtr / sel_date_range / sel_depth_range live in modal_edit_filters(),
    # which showModal() only builds on first open -- so on a fresh session
    # input$sel_qtr et al. are NULL until the user has opened "Edit filters"
    # once. The top-bar Species/Taxa + Compare controls call this function
    # directly (no modal), so without a fallback the first taxon change of a
    # session ran get_sp() with a NULL date range / quarter set and always
    # came back empty ("No observations found"). Fall back to the last applied
    # values (rx$params), then to the same startup defaults the session-once
    # loader uses.
    sel_name        <- input$sel_name
    sel_env_var     <- input$sel_env_var
    sel_qtr         <- reset_qtr         %||% input$sel_qtr        %||% rx$params$sel_qtr     %||% 1:4
    sel_date_range  <- reset_date_range  %||% input$sel_date_range %||% rx$params$date_range  %||% min_max_date
    sel_depth_range <- reset_depth_range %||% input$sel_depth_range %||% rx$params$depth_range %||% c(0, 515)
    sel_bio_ds      <- reset_bio_datasets %||% input$sel_bio_ds
    ck_children     <- input$ck_children    %||% rx$params$ck_children %||% FALSE

    if (debug) message("Selections: sp_name =", sel_name, ", env_var =", sel_env_var)

    # get spatial filter
    drawn_polygon <- get_drawn_features(maplibre_proxy("spatial_filter_map"))
    if (debug) message("Spatial filter:", if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "custom polygon" else "none")

    # THE headline signal: the whole filter set, in one row. The taxa names are
    # exactly what makes the log readable, and are why this detail belongs in
    # the Sheet leg — GA4 buckets a dimension this wide into "(other)".
    trk("filter_submit",
        taxa             = sel_name,
        n_taxa           = length(sel_name),
        env_var          = sel_env_var,
        quarters         = sel_qtr,
        date_beg         = sel_date_range[1],
        date_end         = sel_date_range[2],
        depth_min        = sel_depth_range[1],
        depth_max        = sel_depth_range[2],
        include_children = ck_children,
        spatial          = if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) "polygon"
                           else if (length(rx$sel_places) > 0) "zones" else "none",
        zones            = rx$sel_places)

    # retrieve data (lazy tables from database) — timed + logged, non-blocking
    df_sp <- calcofi4r::cc_track_query(session, "map_query_sp",
      list(taxa = sel_name, quarters = sel_qtr, date_beg = sel_date_range[1],
           date_end = sel_date_range[2], include_children = ck_children),
      get_sp(sel_name, sel_qtr, sel_date_range, ck_children,
             datasets = sel_bio_ds))
    df_env <- calcofi4r::cc_track_query(session, "map_query_env",
      list(env_var = sel_env_var, quarters = sel_qtr, date_beg = sel_date_range[1],
           date_end = sel_date_range[2], depth_min = sel_depth_range[1],
           depth_max = sel_depth_range[2]),
      get_env(sel_env_var, sel_qtr, sel_date_range, sel_depth_range[1], sel_depth_range[2]))

    # Apply spatial filter based on priority: drawn polygon > selected zones > all data.
    # The WKT is kept in `spatial_wkt` (and on rx) so the h3t tile SQL can apply
    # the SAME constraint — the tiles are a separate query against a separate
    # service, so a filter applied only to the dbplyr tables would leave the map
    # showing observations the plots beside it exclude.
    spatial_wkt   <- NULL
    spatial_layer <- NULL  # set only for table-only (sample_spatial) categories
    spatial_names <- NULL

    if (!is.null(drawn_polygon) && nrow(drawn_polygon) > 0) {
      spatial_wkt <- st_as_text(drawn_polygon$geometry[[1]])

    } else if (!is.null(rx$sel_places) && length(rx$sel_places) > 0) {
      if (input$sel_places_cat %in% sample_spatial_layers) {
        # Table-only categories (global.R::sample_spatial_layers) have no
        # polygon geometry loaded to build WKT from -- filter by
        # sample_spatial membership instead, the same mechanism
        # prep_sp_poly()/prep_env_poly() already use for Summarize Within.
        spatial_layer <- input$sel_places_cat
        spatial_names <- rx$sel_places
      } else {
        # BUG FIX: this branch used to key off rx$sel_zones, matched against
        # cc_grid_zones$zone_key -- but rx$sel_zones was only ever initialized
        # to NULL, never assigned. The Spatial tab's map/table click handlers
        # write rx$sel_places (by NAME, e.g. "Extended Nearshore"), for
        # whichever cc_places category is active (input$sel_places_cat) -- not
        # just CalCOFI Zones; BOEM Wind Planning Areas, Integrated Ecosystem
        # Assessment and National Marine Sanctuaries use the same picker. So
        # this branch never ran for ANY of the 4 categories: picking zones and
        # hitting Submit silently filtered nothing.
        spatial_wkt <- cc_places |>
          filter(category == input$sel_places_cat, name %in% rx$sel_places) |>
          pull(geom) |>
          st_union() |>
          st_as_text()
      }

    } else if (identical(input$sel_agg_unit %||% "hex", "hex") &&
               !is.null(input$sel_map_area) &&
               !input$sel_map_area %in% c("", "__all__")) {
      # "Restrict map to area" (Plot Options) -- clip everything to one whole
      # boundary layer, via the same sample_spatial membership the Spatial-tab
      # filter uses. Hexagons mode only: a polygon summary is already one layer.
      spatial_layer <- input$sel_map_area
      spatial_names <- summary_layer_names[[input$sel_map_area]]
    }
    rx$spatial_wkt   <- spatial_wkt
    rx$spatial_layer <- spatial_layer
    rx$spatial_names <- spatial_names

    if (!is.null(spatial_wkt)) {
      df_sp <- df_sp |>
        filter(sql(paste0(
          "ST_Within(ST_Point(longitude, latitude), ST_GeomFromText('", spatial_wkt, "'))"
        )))

      df_env <- df_env |>
        filter(sql(paste0(
          "ST_Within(ST_Point(lon_dec, lat_dec), ST_GeomFromText('", spatial_wkt, "'))"
        )))

    } else if (!is.null(spatial_layer)) {
      tbl_spatial_sel <- sample_spatial_keys(spatial_layer, spatial_names)

      # bio_obs/df_sp carries the sample grain as `sample_key`; env_obs/df_env
      # carries the same key named `cast_id` (see functions.R::prep_env_poly).
      df_sp <- df_sp |>
        inner_join(tbl_spatial_sel, by = "sample_key")

      df_env <- df_env |>
        inner_join(tbl_spatial_sel, by = c("cast_id" = "sample_key"))
    }

    # validate data (only collect count, not full data)
    n_sp <- df_sp |> summarize(n = n()) |> pull(n)
    if (debug) message("Species data: found", n_sp, "rows\n")

    if (n_sp == 0) {
      # a dead end the user hit — countable, so an empty combination that keeps
      # recurring (a taxon with no observations in the chosen window) shows up
      # instead of being invisible next to the successful submits
      trk("filter_no_results", taxa = sel_name, env_var = sel_env_var,
          quarters = sel_qtr, date_beg = sel_date_range[1],
          date_end = sel_date_range[2], status = "empty")
      showNotification("No observations found for selected species.", type = "warning")
      # Taxa/Environmental Variable are always-visible top-bar controls now
      # (not modal fields to "come back" to), so there's nothing to reopen --
      # the user already sees exactly what they chose and can adjust it
      # directly. Just stop before overwriting rx$df_sp/map_sp with an empty
      # result.
      return(NULL)
    }

    # "Standardized as" -- see filter_cpue_unit(): keep the user's current pick
    # ONLY when re-filtering the SAME taxa selection (e.g. changing quarter or
    # depth range); a genuine species change always resets to the new
    # species' own majority unit. Without the same_taxa check, a unit that
    # merely exists as a tiny minority for the new species (say 4 raw "count"
    # rows out of thousands) would keep winning just because it happened to
    # match the previous species' chosen unit by name -- e.g. switching from
    # a species standardized as count/100m^3 to one that's 98% count/10m^2
    # kept showing count/100m^3 because 2% of the new species' rows happened
    # to carry that same unit string. rx$params$taxa still holds the
    # PREVIOUS selection here (it isn't overwritten until below), so
    # comparing it against sel_name (this call's new selection) is exactly
    # "did the species change" -- (isolate(): this observer must not re-fire
    # just because do_apply_filters() itself is about to update
    # rx$cpue_chosen/rx$params$taxa below)
    cpue_units_all <- sp_unit_summary(df_sp)
    cur_cpue_unit  <- isolate(input$sel_cpue_unit)
    same_taxa      <- identical(sel_name, isolate(rx$params$taxa))
    cpue_chosen    <- if (same_taxa && !is.null(cur_cpue_unit) &&
                           cur_cpue_unit %in% cpue_units_all$cpue_unit)
      cur_cpue_unit
    else if (nrow(cpue_units_all)) cpue_units_all$cpue_unit[1]
    else NA_character_
    rx$cpue_units  <- cpue_units_all
    rx$cpue_chosen <- cpue_chosen
    df_sp <- filter_cpue_unit(df_sp, cpue_chosen)

    # store shared data (still lazy tables)
    rx$df_sp       <- df_sp
    rx$sp_units    <- sp_unit_summary(df_sp)
    # see sp_dataset_keys() -- keeps Sources & Citations in sync with the
    # Datasets filter and the "Standardized as" pick, not just the taxon
    rx$sp_ds_keys  <- sp_dataset_keys(df_sp)
    rx$df_env      <- df_env
    rx$env_var     <- sel_env_var
    rx$lbl_env_var <- env_var_label(sel_env_var)

    rx$params$taxa        <- sel_name
    rx$params$env_var     <- sel_env_var
    rx$params$sel_qtr     <- sel_qtr
    rx$params$date_range  <- sel_date_range
    rx$params$depth_range <- sel_depth_range
    # was rx$zones, which -- like the sel_zones/sel_places bug fixed
    # elsewhere in this file -- was never assigned by anything; rx$sel_places
    # is what the Spatial-tab picker actually writes.
    rx$params$zones       <- rx$sel_places
    rx$params$ck_children <- ck_children
    # so the download README and the usage log record which datasets the
    # numbers came from, not just which taxa
    rx$params$bio_datasets <- sel_bio_ds
    if (debug) message("Stored reactive data: df_sp, df_env, lbl_env_var =", rx$lbl_env_var)

    # build filter summary
    rx$filter_summary <- prep_filter_summary(
      sel_name,
      sel_env_var,
      sel_qtr,
      sel_date_range,
      sel_depth_range,
      drawn_polygon,
      rx$sel_places,
      ck_children,
      bio_datasets = bio_ds_selected())

    # compact one-line chip shown next to "Edit filters" in the top bar
    # (functions.R::chip_summary_text()) -- Temporal/Depth/Spatial only,
    # since Taxa/Environmental Variable are shown directly in their own
    # top-bar controls and don't need repeating here.
    rx$chip_summary <- chip_summary_text(
      sel_qtr, sel_date_range, sel_depth_range,
      sel_places = rx$sel_places,
      is_custom  = !is.null(drawn_polygon) && nrow(drawn_polygon) > 0,
      date_bounds = min_max_date)

    # build summary stats
    rx$summary_stats <- prep_summary_stats(
      rx$df_sp, rx$df_env, rx$lbl_env_var %||% env_var_label(sel_env_var)
    )

    # generate map
    if (debug) message("Generating species map...\n")
    spec <- sp_map_spec(
      df_sp, sel_name, sel_qtr, sel_date_range, ck_children,
      datasets = sel_bio_ds, poly_wkt = spatial_wkt,
      spatial_layer = spatial_layer, spatial_names = spatial_names,
      cpue_unit = cpue_chosen,
      is_dark  = input$dark_toggle == "dark")
    rx$map_sp       <- spec$map
    rx$sp_layer_ids <- spec$layer_ids
    # rx$sp_scale was NOT updated here, in either path — so after a Submit the
    # species legend kept redrawing the breaks of the STARTUP selection every
    # time the zoom observer fired.
    rx$sp_scale     <- spec$scales
    if (debug) message("Species map generated and stored in rx$map_sp\n")

    # output$map no longer depends on rx$map_sp (it isolates everything but its
    # two triggers), so a new selection has to ask for the rebuild explicitly.
    map_rebuild(map_rebuild() + 1)

    # If a polygon summary (Summarize Within) is active, the rebuild above just
    # put the HEX map back while input$sel_agg_unit still says the layer -- so
    # a filter change / Compare toggle silently dropped the choropleth. Re-lay
    # it once the freshly-rendered widget has loaded.
    agg    <- isolate(input$sel_agg_unit)  %||% "hex"
    estat  <- isolate(input$sel_env_stat)  %||% "mean"
    if (!identical(agg, "hex")) {
      later::later(function() {
        tryCatch(
          if (!is.null(rx$df_sp) && !is.null(rx$df_env)) apply_poly(agg, estat),
          error = function(e)
            if (debug) message("re-apply poly failed: ", conditionMessage(e)))
      }, delay = 0.7)
    }

    # prepare scatterplot data
    df_splot <- prep_splot(df_sp, df_env, "mean")
    rx$df_splot <- df_splot

    # reset depth profile
    rx$plot_depth <- NULL

    removeModal()
  }

  # Trigger #1: the "Edit Filters" modal's Submit button (Temporal/Depth/
  # Spatial changes) -- same id (`submit`) the old modal always used.
  observeEvent(input$submit, { do_apply_filters() })

  # "Restrict map to area" (Plot Options) -- applies immediately, like changing
  # taxon or toggling Compare. do_apply_filters() reads input$sel_map_area in
  # its spatial block and rebuilds the map + plot/download tables clipped to it.
  observeEvent(input$sel_map_area, {
    trk("select_map_area", map_area = input$sel_map_area %||% "")
    do_apply_filters()
  }, ignoreInit = TRUE)

  # Triggers #2-4: the top-bar controls auto-apply on change instead of
  # waiting behind a button, per the redesign ("search is primary and
  # visible"). req(input$sel_name) guards the transient moment a selectize
  # multi-select is cleared to zero taxa before a new one is picked -- do_
  # apply_filters()'s own empty-result handling covers a deliberate
  # zero-observation selection, this just skips the flicker of an
  # in-between empty state firing a full requery.
  observeEvent(input$sel_name, {
    req(input$sel_name)
    # skip the requery when the value just caught up to what is already loaded
    # -- e.g. the one-time updateSelectizeInput() in the default-data loader
    # that makes the default taxon visibly selected in the search box
    if (identical(input$sel_name, rx$params$taxa)) return()
    do_apply_filters()
  }, ignoreInit = TRUE)

  observeEvent(input$cmp_env_toggle, {
    do_apply_filters()
  }, ignoreInit = TRUE)

  observeEvent(input$sel_env_var, {
    if (isTRUE(input$cmp_env_toggle)) do_apply_filters()
  }, ignoreInit = TRUE)

  # BUG FIX: "Include taxonomic children" (ck_children) had no observer of its
  # own -- toggling it silently did nothing until some OTHER control (taxon,
  # env var) happened to re-run do_apply_filters() afterwards, which then
  # picked up the new value. Mirrors the sel_name auto-apply above.
  observeEvent(input$ck_children, {
    req(input$sel_name)
    do_apply_filters()
  }, ignoreInit = TRUE)

  # "Standardized as" -- re-run with the newly picked cpue_unit. Guarded so
  # this doesn't also fire from do_apply_filters() re-rendering the control
  # with a DIFFERENT selection than the user made (a fallback to the majority
  # unit, e.g. because the previous pick has no rows for a new taxon).
  observeEvent(input$sel_cpue_unit, {
    req(input$sel_cpue_unit)
    if (identical(input$sel_cpue_unit, rx$cpue_chosen)) return()
    do_apply_filters()
  }, ignoreInit = TRUE)

  # plotly_click -> ... ----
  observeEvent(
    {
      req(splot_ready())
      event_data("plotly_click", source = "scatterPlotSource")
    }, {
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
            "<b>CPUE:</b> ", round(clicked_point$sp_tally, 2)
          )
        )
    })
  })

  observeEvent(
    {
      req(splot_ready())
      event_data("plotly_selected", source = "scatterPlotSource")
    }, {
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
            "<br><b>CPUE:</b> ", round(selected_points$sp_tally, 2)
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
    t_transect <- Sys.time()

    features <- get_drawn_features(maplibre_proxy("transect_map"))

    if (is.null(features) || nrow(features) == 0) {
      trk("depth_profile_transect", status = "no_line")
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

    # the only step that collects BOTH full datasets into R and runs spatial
    # intersects, so its duration is the one worth watching on this tab
    trk("depth_profile_transect",
        buffer_km   = input$modal_buffer_dist,
        transect_km = round(as.numeric(segment_length), 1),
        n_env       = nrow(filt_env_data),
        n_rows      = nrow(filt_sp_data),
        ms          = as.numeric(difftime(Sys.time(), t_transect, units = "secs")) * 1000,
        status      = if (nrow(filt_sp_data) == 0) "empty" else "ok")

    dist_bin_size <- 5
    depth_bin_size <- 20

    sp_plot <- filt_sp_data |>
      mutate(
        tooltip = paste0(
          "Species: ", name, "<br>",
          "CPUE: ", round(std_tally, 2), "<br>",
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
        depth_bins = filt_env_data$depth_m %>%
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
    rx$params$dprof_params <- list(
      buffer   = input$modal_buffer_dist,
      transect = paste0(
        "start = (", round(coords[1, "X"], 4), ", ", round(coords[1, "Y"], 4), ")",
        "; end = (", round(coords[nrow(coords), "X"], 4), ", ", round(coords[nrow(coords), "Y"], 4), ")"
      )
    )

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

  output$filter_summary <- renderUI({
    req(rx$filter_summary)
    div(class = "small", markdown(paste(rx$filter_summary, collapse = "  \n")))
  })

  # top-bar filter chip row (Temporal/Depth/Spatial condensed to one line;
  # Taxa/Environmental Variable aren't repeated here since they're already
  # shown live in their own top-bar controls) -- see functions.R::
  # chip_summary_text() and the "Edit filters" link beside it in ui.R
  output$filter_chip_summary <- renderUI({
    req(rx$chip_summary)
    rx$chip_summary
  })

  # modal_edit_filters()'s "Area" row -- summarizes the spatial FILTER
  # (rx$sel_places_cat + rx$sel_places), matching the mockup's "CalCOFI
  # Zones, 2 selected". This is the same spatial-filter state chip_summary_text()
  # already reads for "N locations selected" in the chip row; this output just
  # additionally names which category, since the Filters panel has the room.
  output$spatial_layers_summary <- renderText({
    n <- length(rx$sel_places)
    if (n > 0) sprintf("%s, %d selected", rx$sel_places_cat, n)
    else "All locations"
  })

  # reset_filters -> defaults ----
  # modal_edit_filters()'s "Reset all" link. Restores every input in the panel
  # to its original startup default; does NOT call do_apply_filters() itself
  # (the modal is still open, same as changing any field by hand) -- the user
  # still clicks Apply, so nothing is queried until they confirm.
  #
  # rx$sel_places was missing here (bug report 2026-09-08: "reset all
  # doesn't work" -- Datasets/Depth/Time all correctly went back to their
  # defaults, but "Layers" kept showing "CalCOFI Zones, 2 selected"). Zones
  # picked in the separate "Layers" > "Change" dialog are staged in
  # rx$sel_places, not a plain Shiny input, so update*Input() never touches
  # it -- it needs clearing directly, the same as any other staged filter
  # this observer resets. output$spatial_layers_summary reads rx$sel_places
  # for its "N selected" count, so this alone fixes what was visibly stuck;
  # a custom drawn polygon is a separate case (read live off the map's own
  # draw layer only at Apply time, functions.R::get_drawn_features()) with
  # no staged value here to clear -- clearing it means removing the drawn
  # shape from that map, in the "Layers" dialog itself.
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "sel_bio_ds", selected = d_bio_datasets$dataset_key)
    updateSliderInput(session, "sel_depth_range", value = c(0, 515))
    updateCheckboxGroupButtons(session, "sel_qtr", selected = c("1", "2", "3", "4"))
    updateDateRangeInput(
      session, "sel_date_range",
      start = min_max_date[1], end = min_max_date[2])
    rx$sel_places     <- character(0)
    # sel_places_cat (main): tracks which category rx$sel_places' names
    # belong to, and is what clears rx$sel_places automatically on a category
    # SWITCH (see the input$sel_places_cat observer above) -- clear it here
    # too, or a stale category left over from before "Reset all" would still
    # be sitting there for that observer to compare against.
    rx$sel_places_cat <- NULL

    # "Reset all" should behave like Apply, not just reset the widgets and
    # wait for a separate click. The update*Input() calls above only ask the
    # BROWSER to change each widget -- input$sel_qtr etc. won't reflect the
    # reset value until the client round-trips it back, which hasn't happened
    # yet at this point in the same server-side execution. Calling
    # do_apply_filters() with no arguments here would silently re-apply the
    # OLD pre-reset filters while still closing the modal. Passing the reset
    # values straight in as overrides sidesteps that round-trip.
    do_apply_filters(
      reset_qtr          = 1:4,
      reset_date_range   = min_max_date,
      reset_depth_range  = c(0, 515),
      reset_bio_datasets = d_bio_datasets$dataset_key)
  })

  # Summary Statistics: a tight Species / Environment matrix -- label column
  # then a value per side, one row per metric (functions.R::prep_summary_stats).
  output$summary_statistics <- renderUI({
    req(rx$summary_stats)
    s <- rx$summary_stats
    if (!is.data.frame(s) || !all(c("label", "sp", "env", "is_head") %in% names(s))) {
      return(div(class = "small", markdown(paste(unlist(s), collapse = "  \n"))))
    }
    val <- function(txt, head) {
      if (isTRUE(head) && grepl("^[A-Z]\\. ", txt %||% ""))
        return(tags$span(class = "cc-ss-v", tags$em(txt)))   # abbreviated binomial
      tags$span(class = "cc-ss-v", txt)
    }
    div(
      class = "cc-ss",
      lapply(seq_len(nrow(s)), function(i) tagList(
        if (i == 2) div(class = "cc-ss-sep"),
        div(class = "cc-ss-row",
            tags$span(class = "cc-ss-k", s$label[i]),
            val(s$sp[i],  s$is_head[i]),
            val(s$env[i], s$is_head[i])))))
  })

  # observations by selected taxa -- the per-taxon counts behind the matrix
  # above (functions.R::taxa_tree_builder; ui.R's .cc-taxa rules style it)
  output$taxa_tree <- renderUI({
    req(rx$df_sp)
    div(
      class = "cc-taxa",
      div(class = "cc-taxa-head", "Observations by selected taxa"),
      taxa_tree_builder(rx$df_sp))
  })

  # the Map panel's sections collapse via a CSS class (display:none), which
  # Shiny would read as "hidden" and leave these outputs unrendered until the
  # user opens the section AND a reactive flush notices -- keep them live so a
  # collapsed section opens already populated. map_title_sp_pop and
  # data_sources_box have the same problem via a different hiding mechanism:
  # the popover's content div starts display:none until clicked open, and the
  # left-panel Sources & Citations section can be collapsed the same way --
  # so both went stale, showing whichever species/dataset was selected the
  # last time they were actually visible (bug report 2026-09-07: "why is
  # abrali data sourced greyed out but you can see citation for CUFES" -- the
  # popover was still showing an earlier selection's citation, not the
  # current one, while the always-live left-panel box already had it right).
  for (.o in c("filter_summary", "summary_statistics", "taxa_tree", "poly_note",
               "map_title_sp_pop", "map_title_env_pop", "data_sources_box"))
    outputOptions(output, .o, suspendWhenHidden = FALSE)

  # download_data ----
  # Bundles original + summarized data with reproducible SQL. The integrated
  # bio<->env match is built and run by calcofi4r::cc_match_bio_env() against
  # public GCS release parquet (see functions.R::build_download_bundle), so the
  # query/ folder lets anyone re-run it in DuckDB and get identical rows.
  output$download_data <- downloadHandler(
    filename = function() paste0("calcofi_data_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {

      raw_sel  <- input$sel_raw_data_download %||% character(0)
      proc_sel <- input$sel_proc_data_download %||% character(0)
      # the integrated dataset is the headline download -- always in the bundle
      # (the UI leads with it, no checkbox); raw_env/raw_sp/chart tables opt in.
      all_sel  <- unique(c("int", raw_sel, proc_sel))

      # download timing + budget. The zip only streams to the browser at the very
      # END (after all the CSVs are built), so a server-side build that runs long
      # almost certainly outlived the client connection — the user gets a
      # truncated response ("Site wasn't available") even though the server
      # "succeeded". We log any build past this budget as a `timeout` error so the
      # log Sheet shows the real user-facing failure instead of a false ok.
      # Env-overridable (CALCOFI_DOWNLOAD_TIMEOUT_SEC).
      dl_t0        <- Sys.time()
      dl_elapsed   <- function() as.numeric(difftime(Sys.time(), dl_t0, units = "secs"))
      dl_budget_s  <- suppressWarnings(as.numeric(
        Sys.getenv("CALCOFI_DOWNLOAD_TIMEOUT_SEC", "120")))
      if (is.na(dl_budget_s) || dl_budget_s <= 0) dl_budget_s <- 120

      zip_root <- tempfile(pattern = "calcofi_download_", tmpdir = tempdir())
      dir.create(zip_root, showWarnings = FALSE, recursive = TRUE)
      paths   <- character()

      write_data <- function(df, rel_path) {
        full_path <- file.path(zip_root, rel_path)
        dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
        write.csv(df, full_path, row.names = FALSE, quote = TRUE)
        paths <<- c(paths, rel_path)       # <<- adds to the outer variable
      }

      # keep time/dist windows in rx$params so the README + bundle agree
      rx$params$time_window <- input$time_window %||% default_max_hours_diff
      rx$params$dist_window <- input$dist_window %||% default_max_meters_diff

      # Wrap the whole build: any failure below (an errored product, a missing
      # reactive) is logged as status="error" and surfaced, instead of aborting
      # the handler before the success log — which made failed downloads (e.g. a
      # broken product path) silently show as "ok" in the log Sheet.
      tryCatch({
      withProgress(message = "Preparing download", value = 0, {
      for (i in all_sel) {
        incProgress(1 / length(all_sel), detail = i)

        if (i == "raw_sp") {
          req(rx$df_sp)
          # CPUE-forward schema (E. Weber request): expose the raw tally, the
          # effort fields it standardizes by, and the reconstructed density +
          # unit (count/10m2 for oblique/vertical tows, count/100m3 for manta).
          sp_out <- rx$df_sp |>
            collect() |>
            rename(cpue = std_tally) |>
            relocate(tow_type, tally, std_haul_factor, prop_sorted,
                     volume_sampled, cpue, cpue_unit, .after = quarter)
          write_data(sp_out, "data/original/species.csv")

        } else if (i == "raw_env") {
          req(rx$df_env)
          write_data(rx$df_env |> collect(), "data/original/environment.csv")

        } else if (i == "int") {
          # reproducible bundle: data/original/{bio,env}.csv +
          # data/integrated/integrated_<method>.csv + query/ (per-file *.sql,
          # manifest.json, REPRODUCE.md) — single source of truth via
          # calcofi4r::cc_match_bio_env() against GCS release parquet
          req(rx$params$taxa)
          .t0 <- Sys.time()
          .ms <- function() as.numeric(difftime(Sys.time(), .t0, units = "secs")) * 1000
          bundle_paths <- tryCatch(
            build_download_bundle(zip_root, isolate(rx$params)),
            error = function(e) {
              do.call(trk, c(
                list("download_integrated_bundle"), trk_filters(isolate(rx$params)),
                list(ms = .ms(), status = "error", error = conditionMessage(e))))
              showNotification(
                paste("Integrated data / SQL bundle failed:", conditionMessage(e)),
                type = "error", duration = NULL)
              character(0)
            })
          if (length(bundle_paths)) {
            .over   <- .ms() > dl_budget_s * 1000
            .status <- if (.over) "timeout" else "ok"
            .errmsg <- if (.over) sprintf(
              "integrated bundle build took %.0fs (> %.0fs budget); client likely disconnected before the zip streamed",
              .ms() / 1000, dl_budget_s) else ""
            do.call(trk, c(
              list("download_integrated_bundle"), trk_filters(isolate(rx$params)),
              list(n_rows = length(bundle_paths), ms = .ms(),
                   status = .status, error = .errmsg)))
            if (.over)
              showNotification(paste(
                "The integrated data bundle took longer than expected to build,",
                "so your download may not have started. Narrow the filters",
                "(fewer taxa, shorter date range) and try again."),
                type = "warning", duration = NULL)
          }
          paths <- c(paths, bundle_paths)

        } else if (i == "map") {
          req(rx$df_sp, rx$df_env)
          if (is.null(rx$params$map_params$env_stat)) {rx$params$map_params$env_stat <- "mean"}
          env_stat <- rx$env_stat %||% rx$params$map_params$env_stat
          agg_unit <- rx$agg_unit %||% "hex"

          if (agg_unit != "hex") {
            # "Map data" must be the data the map is showing. Recomputed rather
            # than lifted from rx so the CSV cannot lag a filter change, and it
            # is cheap (~0.05s) next to everything else in this bundle.
            sp_poly  <- prep_sp_poly(rx$df_sp, agg_unit)
            env_poly <- prep_env_poly(rx$df_env, agg_unit, env_stat)

            write_data(
              sp_poly$data |>
                select(-tooltip) |>
                mutate(layer = agg_unit, cpue_unit = sp_poly$unit),
              "data/summarized/map/species_polygon.csv")
            write_data(
              env_poly |>
                select(-tooltip) |>
                mutate(layer = agg_unit, env_stat = env_stat),
              "data/summarized/map/env_polygon.csv")

            # the units this summary had to leave out, so the CSV is not the
            # only record that a choice was made
            write_data(
              sp_poly$units |> mutate(summarized = cpue_unit == sp_poly$unit),
              "data/summarized/map/species_polygon_units.csv")

          } else {
            # agg_*, not prep_* — the aggregate WITHOUT the hexagon polygons.
            # Joining them cost a 5.6 s / 153 MB read of hex.geojson and then
            # wrote an sfc column that write.csv() renders as an R literal
            # (`list(c(-113.6, ..., 16.5))`), not WKT — so the geometry column
            # was unreadable to every tool a CSV is opened in. `hexid` is an H3
            # index: a reader recovers the polygon from it with h3_cell_to_boundary().
            sp_hex  <- agg_sp_hex(rx$df_sp, res_range) |> select(-tooltip)
            env_hex <- agg_env_hex(rx$df_env, res_range, env_stat) |> select(-tooltip)

            write_data(sp_hex , "data/summarized/map/species_map.csv")
            write_data(env_hex, "data/summarized/map/env_map.csv")
          }

        } else if (i == "ts") {
          req(rx$df_sp, rx$df_env)
          if (is.null(rx$params$ts_params$ts_res)) {rx$params$ts_params$ts_res <- "year"}
          sp_ts  <- prep_ts_sp(rx$df_sp, rx$params$ts_params$ts_res)
          env_ts <- prep_ts_env(rx$df_env, rx$params$ts_params$ts_res)

          write_data(sp_ts , "data/summarized/time_series/species_ts.csv")
          write_data(env_ts, "data/summarized/time_series/ocean_ts.csv")

        } else if (i == "splot") {
          req(rx$df_sp, rx$df_env)

          if (is.null(rx$params$splot_params$method)) rx$params$splot_params$method <- "nearest_time"
          if (is.null(rx$params$splot_params$time_window)) {rx$params$splot_params$time_window <- default_max_hours_diff}
          if (is.null(rx$params$splot_params$dist_window)) {rx$params$splot_params$dist_window <- default_max_meters_diff}

          data <- rx$df_splot %||%
            prep_splot(rx$df_sp, rx$df_env, "mean",
                       method = rx$params$splot_params$method,
                       max_hours_diff  = rx$params$splot_params$time_window,
                       max_meters_diff = rx$params$splot_params$dist_window)

          write_data(data, "data/summarized/scatterplot.csv")

        } else if (i == "dprof") {
          # df_dprof is only built once the Depth Profile tab is opened with a
          # transect selected; skip gracefully (don't crash the whole download).
          if (is.null(rx$df_dprof) || length(rx$df_dprof) < 2) {
            showNotification(paste(
              "Depth Profile data isn't ready — open the Depth Profile tab and pick",
              "a transect, then re-download. Skipping it for now."),
              type = "warning", duration = NULL)
          } else {
            write_data(rx$df_dprof[[1]], "data/summarized/depth_profile/species_dprof.csv")
            write_data(rx$df_dprof[[2]], "data/summarized/depth_profile/env_dprof.csv")
          }
        }
      }
      })  # withProgress

      readme_path <- file.path(zip_root, "README.md")

      params <- isolate(rx$params)

      # Create a YAML-friendly copy
      yaml_params <- params
      # Coerce date_range to ISO strings if they are Dates
      if (inherits(yaml_params$date_range, "Date")) {
        yaml_params$date_range <- as.character(yaml_params$date_range)
      }

      yaml_block <- yaml::as.yaml(yaml_params)

      body_lines <- c(
        "# CalCOFI Download",
        "",
        "This archive contains data filtered with the following criteria:",
        "",
        glue::glue("- Taxa: {paste(params$taxa, collapse = ', ')}"),
        glue::glue("- Environmental variable: {params$env_var}"),
        glue::glue(
          "- Quarters: {paste(params$sel_qtr %||% params$quarters, collapse = ', ')}"),
        glue::glue("- Date range: {params$date_range[1]} to {params$date_range[2]}"),
        glue::glue("- Depth range (m): {params$depth_range[1]}–{params$depth_range[2]}"),
        glue::glue(
          "- Include children: {params$ck_children %||% params$include_children}"),
        glue::glue(
          "- Spatial filter (zones): {if (is.null(params$zones))
       'All locations' else paste(params$zones, collapse = ', ')}"
        ),
        glue::glue("- Integrated join time window (hours): {params$time_window}"),
        glue::glue("- Integrated join distance window (m): {params$dist_window}"),
        glue::glue("- Map env statistic: {params$map_params$env_stat}"),
        glue::glue("- Time series resolution: {params$ts_params$ts_res}"),
        glue::glue(
          "- Scatterplot matching: method = {params$splot_params$method}, ",
          "time_window = {params$splot_params$time_window} hours, ",
          "dist_window = {params$splot_params$dist_window} m"
        ),
        glue::glue("- Depth profile transect: {params$dprof_params$transect %||% 'NA'}"),
        glue::glue("- Depth profile buffer (km): {params$dprof_params$buffer %||% 'NA'}"),
        "",
        "## Bundle layout",
        "",
        "- `data/original/` — raw species + environmental observations",
        paste(
          "  Species rows carry the raw `tally` (count), the tow effort it is",
          "standardized by (`tow_type`, `std_haul_factor`, `prop_sorted`,",
          "`volume_sampled`), and the resulting `cpue` (catch per unit effort /",
          "density) with its `cpue_unit`: **count/10m²** for oblique & vertical",
          "tows (C1, CB, CV, PV; cpue = tally × std_haul_factor / prop_sorted) and",
          "**count/100m³** for manta surface tows (MT; cpue = tally / prop_sorted /",
          "volume_sampled × 100). Where the gear does not support standardization",
          "— no `tow_type` or no `std_haul_factor` — `cpue` is the value the source",
          "published, in its own `cpue_unit`, and is NOT a density: cdfw_dungeness-crab",
          "is occurrence in a lab-examined aliquot of an archived catch, and the",
          "euphausiid / ZooScan series publish their own per-area units. Read",
          "`cpue_unit` before comparing rows."),
        "- `data/summarized/` — aggregated map / time-series / scatterplot / depth-profile data",
        "- `data/integrated/` — species matched to environment in time + space",
        "- `query/` — the **exact, portable SQL** behind each file, plus",
        "  `manifest.json` and `REPRODUCE.md`",
        "",
        paste(
          "If you included the integrated data, see **`query/REPRODUCE.md`** to",
          "re-run the same queries against the public CalCOFI release parquet in",
          "DuckDB (CLI, Python or R) and get identical rows.")
      )

      md <- c(
        "---",
        yaml_block,
        "---",
        "",
        body_lines
      )
      writeLines(md, readme_path)

      litedown::mark(readme_path)
      paths <- c(paths, "README.md", "README.html")

      zip::zip(zipfile = file, files = paths, root = zip_root, include_directories = TRUE)

      # overall download log — one row per Download click. Flagged `timeout` (an
      # error state) when the total server build exceeded the budget, since the
      # user almost certainly never received the zip. See dl_budget_s above.
      .dl_over   <- dl_elapsed() > dl_budget_s
      do.call(trk, c(
        list("download_bundle"), trk_filters(isolate(rx$params)),
        list(products = all_sel, n_files = length(paths),
             n_rows = length(paths), ms = dl_elapsed() * 1000,
             status = if (.dl_over) "timeout" else "ok",
             error  = if (.dl_over) sprintf(
               "total download build %.0fs (> %.0fs budget); client likely disconnected",
               dl_elapsed(), dl_budget_s) else "")))
      }, error = function(e) {
        emsg <- conditionMessage(e)
        if (!nzchar(emsg)) emsg <- "download aborted (a required input was not available)"
        do.call(trk, c(
          list("download_bundle"), trk_filters(isolate(rx$params)),
          list(products = all_sel, n_files = length(paths),
               n_rows = length(paths), ms = dl_elapsed() * 1000,
               status = "error", error = emsg)))
        showNotification(paste("Download failed:", emsg), type = "error",
                         duration = NULL)
        stop(e)  # re-raise so the browser gets a clean error, not a partial zip
      })
    },
    contentType = "application/zip"
  )
}
