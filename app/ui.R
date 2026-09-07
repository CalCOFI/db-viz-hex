# the About panel is the same on every page load — render the markdown once at
# startup rather than per request, now that the UI is built per request
about_html <- HTML(mark(here("app/about.md"), output = NA))

# ui is a FUNCTION of the request, not a static object, for one reason: the
# client IP. shiny-server does not proxy the websocket upgrade — it opens a
# fresh localhost connection to the R worker — so the server session sees
# REMOTE_ADDR 127.0.0.1 and no X-Forwarded-For (Caddy sets it correctly; it is
# lost at the shiny-server hop). This page request is the only one that still
# carries the real address, so it is captured here and baked into the analytics
# snippet.
#
# page_navbar(), not page_sidebar(): the approved redesign has NO sidebar at
# all -- Species/Taxa search + Compare Environmental Variable live in a top
# bar (below), and everything else that used to live in the sidebar (Map
# Layers, Plot Options, Summary Statistics, Draw Transect) moved into small
# per-tab triggers (see each nav_panel below), so the map and every other
# tab's content can run the FULL width of the page. `id`/`selected` here take
# over exactly what navset_card_underline's `id = "outputPanel"` used to do --
# every existing conditionalPanel("input.outputPanel === ...") and server.R
# reader of input$outputPanel needed no change.
ui <- function(req) page_navbar(
  # no window_title: cc_brand_head() below owns the page's one <title>
  window_title = NULL,
  # page_sidebar owns the top bar, so the calcofi.io lockup pair (brand v2; theme.css
  # shows one per theme) sits in its title slot rather than a second bar
  title = tagList(
    span(
      class = "cc-brand",
      a(
        img(src = "https://calcofi.io/brand/v2/logo_calcofi_h.svg",
            height = "32px", alt = "CalCOFI",
            class = "cc-logo-dark",  .noWS = "after"),
        img(src = "https://calcofi.io/brand/v2/logo_calcofi_h_light.svg",
            height = "32px", alt = "CalCOFI",
            class = "cc-logo-light", .noWS = "after"),
        href = "https://calcofi.io", `aria-label` = "CalCOFI.io home"),
      tags$span(class = "cc-brand-name", "Hexagon Explorer"),
      # which frozen database release everything on screen came from — the brand
      # chip (calcofi4r::cc_release_chip), in the TITLE so it survives every
      # tab switch: a figure screenshotted out of here is only reproducible if
      # the release travelled with it
      calcofi4r::cc_release_chip(DB_RELEASE))),

  id       = "outputPanel",
  selected = "Map",
  # Only the four visualization tabs are fillable (so their map/plot output
  # fills the tab height, mirroring navset_card_underline(height = "100%")
  # before); Download/About keep ordinary flowing content. A character vector
  # here matches nav_panel() values -- bslib makes the page a fill carrier and
  # turns just those panels into fillable flex columns. (nav_panel() has no
  # `fillable` argument of its own in bslib 0.12, so it must be set here.)
  fillable = c("Map", "Time Series", "Scatterplot", "Depth Profile"),

  # top bar: Species/Taxa search + Compare Environmental Variable toggle are
  # the PRIMARY controls (search is always visible; changing either applies
  # immediately -- see server.R's do_apply_filters() and the observers right
  # after it), with Datasets/Depth/Layers/Time collapsed behind "Edit filters"
  # in the chip row underneath. functions.R::top_bar_taxa_ui()/
  # top_bar_env_ui() build the two cards; modal_edit_filters() is the
  # "Edit filters" dialog. Lives in page_navbar's `header` slot, so it shows
  # above every tab's content -- except Download/About, where none of this is
  # relevant.
  #
  # NOTE: page_navbar()'s `...` accepts ONLY nav_panel()/nav_menu() items --
  # everything that isn't one (the extra <head> tags, the <script> blocks,
  # useConductor(), useBusyIndicators()) has to travel inside `header`
  # instead, or bslib errors with "Navigation containers expect a collection
  # of nav_panel()s...". They're bundled into one tagList() below for that
  # reason; htmltools still hoists the tags$head()/tags$script() content to
  # the real document <head> no matter how deep they're nested.
  header = tagList(
  conditionalPanel(
    "input.outputPanel !== 'About'",
    div(
      class = "cc-topbar",
      div(
        class = "row g-3",
        div(
          class = "col-md-6",
          div(class = "cc-topbar-card h-100", top_bar_taxa_ui())),
        div(
          class = "col-md-6",
          div(class = "cc-topbar-card h-100", top_bar_env_ui()))),
      # one row: the filter chip on the left, and (on the Map tab only) the
      # Split/Swipe control pushed to the right. The fuller multi-line filter
      # summary, Stats, Options and Map Layers all live in the Map tab's left
      # panel now (functions.R::map_panel_ui()), so this row stays a single
      # thin line above the map.
      div(
        class = "cc-filter-row d-flex align-items-center gap-2 flex-wrap",
        div(
          class = "cc-filter-chip small text-muted d-flex align-items-center gap-2",
          bs_icon("funnel"),
          uiOutput("filter_chip_summary", inline = TRUE),
          actionLink("edit_filters", "Edit filters")),
        # Map tab: "Map Layers" (always) + Split/Swipe (compare only), pushed right
        conditionalPanel(
          "input.outputPanel === 'Map'",
          div(
            class = "cc-map-chiprow ms-auto d-flex align-items-center gap-2",
            actionButton(
              "open_map_layers", "Map Layers",
              class = "btn btn-sm cc-chip-btn", icon = bs_icon("stack")),
            div(class = "cc-cmp-layout-row",
                map_tab_controls())))))),

  tags$head(
    # the calcofi.io brand contract (title, favicon set, theme.css/js) plus
    # usage tracking: GA4 (aggregate) + a batched beacon to the usage-log Sheet
    # (per-query detail). Both legs are sent by the BROWSER, so no reactive ever
    # performs network I/O — server-side facts reach it via calcofi4r::cc_track()
    # over the websocket the session already has open. The Sheet leg is a silent
    # no-op unless CALCOFI_LOG_URL is set (global.R), so local dev writes nothing.
    calcofi4r::cc_brand_head(
      "CalCOFI Hexagon Explorer", ga_app = "db-viz-hex",
      app_version = APP_VERSION, ip = calcofi4r::cc_client_ip(req)),
    tags$style(HTML("
    /* shrink the app's own UI a notch -- everything Bootstrap sizes in rem
       (cards, buttons, the .cc-* controls, modals) scales with this one knob;
       Bootstrap's default is 16px. The brand header is NOT on this knob:
       cc_brand_head() declares the brand's app scale (cc-scale=app) and
       theme.css sizes the lockup, nav type and header height in px, so the
       header stays contract-exact whatever this is set to. */
    html { font-size: 13.5px; }

    .treeview {
      list-style: none;
      padding-left: 0.1rem;
      margin: 0;
      margin-top: 0;
      font-size: 0.9rem;
    }

    .treeview ul {
      list-style: none;
      margin: 0;
      padding-left: 1.1rem;
    }

    .treeview li {
      position: relative;
      margin: 0.15rem 0;
      padding: 0;
    }

    .tree-label {
      font-weight: 400;
      cursor: pointer;
      display: block;
      padding-left: 1.0rem; /* space for triangle icon */
    }

    /* Flex layout inside the label for name vs obs */
    .tree-label-inner {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      gap: 0.5rem;
      width: 100%;
    }

    /* Name and obs styling */
    .tree-name i {
      font-style: italic;
    }

    .tree-obs {
      white-space: nowrap;
    }

    /* Checkbox used as toggle (invisible) */
    .tree-toggle {
      position: absolute;
      left: 0;
      top: 0.15rem;
      width: 1rem;
      height: 1rem;
      opacity: 0;
      cursor: pointer;
    }

    /* Triangles for branches only */
    .tree-branch > .tree-label::before {
      content: '\\25B8'; /* right-pointing triangle */
      position: absolute;
      left: 0;
      top: 0;
      width: 1rem;
      text-align: center;
      font-size: 1.0rem;
    }

    /* Expanded state */
    .tree-branch > .tree-toggle:checked + .tree-label::before {
      content: '\\25BE'; /* down-pointing triangle */
    }

    /* Children visibility */
    .tree-branch > .tree-toggle + .tree-label + ul {
      display: none;
    }
    .tree-branch > .tree-toggle:checked + .tree-label + ul {
      display: block;
    }

    /* Hover state for consistency with other sidebar text */
    .tree-label:hover {
      /* color: #000; */
    }

    /* Observations-by-selected-taxa block (server.R output$taxa_tree) --
       a labelled section matching the stat grid, with tidy tabular counts */
    .cc-taxa { margin-top: 0.9rem; }
    .cc-taxa-head {
      font-size: 0.68rem; font-weight: 600; letter-spacing: 0.04em;
      text-transform: uppercase; color: var(--cc-muted);
      padding-bottom: 0.35rem; margin-bottom: 0.3rem;
      border-bottom: 1px solid var(--cc-border);
    }
    .cc-taxa .treeview { font-size: 0.84rem; margin: 0; }
    .cc-taxa .treeview li { margin: 0.05rem 0; }
    .cc-taxa .tree-label {
      padding-top: 0.22rem; padding-bottom: 0.22rem; padding-right: 0.3rem;
      border-radius: 6px;
    }
    .cc-taxa .tree-leaf > .tree-label { cursor: default; }
    .cc-taxa .tree-label:hover { background: var(--cc-tool); }
    .cc-taxa .tree-name { color: var(--bs-body-color); }
    .cc-taxa .tree-obs {
      font-variant-numeric: tabular-nums;
      font-weight: 650; color: var(--bs-body-color);
    }

    /* --- Filter / Layers modal widths ---------------------------------- */
    /* Shiny gives .shiny-input-container a fixed 300px width, which in a wide
       modal wrapped every dataset checkbox onto a second line and left the
       Taxa box a narrow stub in a mostly empty dialog. Let the controls use
       the width the modal already has. */
    .modal .shiny-input-container {
      width: 100%;
      max-width: 100%;
    }
    /* one dataset per line, with the checkbox aligned to the first line of a
       label that may still wrap on a narrow window */
    .modal .shiny-input-checkboxgroup .checkbox label {
      display: flex;
      align-items: baseline;
      gap: 0.4rem;
    }

    /* --- modal_edit_filters(): compact single-panel Filters dialog ---- */
    /* left-anchored, dropping from under the 'Edit filters' chip rather than
       centered on screen -- per the FilterSummary mockup. modalDialog()'s
       `class=` lands on .modal-body (not .modal-dialog), so the dialog is
       reached with :has(); auto right margin keeps it left, and a narrow
       window falls back to a normal near-full-width sheet. */
    #shiny-modal:has(.modal-body.cc-filters-modal) .modal-dialog {
      max-width: 340px;
      margin: 4.75rem auto 1.75rem 1.75rem;
    }
    @media (max-width: 575.98px) {
      #shiny-modal:has(.modal-body.cc-filters-modal) .modal-dialog {
        margin: 3.5rem auto; max-width: none;
      }
    }
    /* the Filters modal, in the app's card style: white surface, hairline
       border, soft shadow, consistent type */
    #shiny-modal:has(.cc-filters-modal) .modal-content {
      background: var(--cc-surface);
      border: 1px solid var(--cc-border);
      border-radius: 14px;
      box-shadow: var(--cc-shadow);
      overflow: hidden;
    }
    #shiny-modal:has(.cc-filters-modal) .modal-header {
      border-bottom: 1px solid var(--cc-border); padding: 0.9rem 1.2rem;
    }
    #shiny-modal:has(.cc-filters-modal) .modal-title { font-size: 1.05rem; font-weight: 650; }
    #shiny-modal:has(.cc-filters-modal) .modal-footer {
      border-top: 1px solid var(--cc-border); padding: 0.8rem 1.2rem;
    }
    .cc-filters-modal.modal-body { padding: 0.9rem 1.2rem; font-size: 0.875rem; }
    .cc-filters-modal hr { margin: 0.95rem 0; border-color: var(--cc-border); opacity: 1; }
    .cc-filters-modal strong { font-weight: 650; }
    .cc-filters-modal .text-muted { color: var(--cc-muted) !important; }
    /* section headings: blue icon + label, matching the mockup */
    .cc-filters-modal > div > .bi,
    .cc-filters-modal .cc-ds-tree .bi { color: var(--bs-primary); }
    .cc-filters-modal a { color: var(--bs-primary); font-weight: 550; }
    /* date range inputs */
    .cc-filters-modal .input-daterange .form-control,
    .cc-filters-modal input.form-control {
      background: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--cc-border) !important;
      font-size: 0.85rem; text-align: center;
    }
    .cc-filters-modal .input-group-text,
    .cc-filters-modal .input-group-addon,
    .cc-filters-modal .input-daterange .input-group-addon {
      background: transparent !important; border-color: var(--cc-border) !important;
      color: var(--cc-muted) !important;
    }

    /* --- Filters panel > Datasets tree --------------------------------- */
    .cc-ds-cat { border-bottom: 1px solid rgba(128, 128, 128, 0.15); padding: 0.3rem 0; }
    .cc-ds-cat:last-child { border-bottom: none; }
    .cc-ds-cat-head { display: flex; align-items: center; gap: 0.5rem; cursor: pointer; }
    .cc-ds-cat-arrow { display: inline-block; width: 0.9rem; transition: transform 0.15s; opacity: 0.7; }
    .cc-ds-cat-collapsed .cc-ds-cat-arrow { transform: rotate(-90deg); }
    .cc-ds-cat-collapsed .cc-ds-cat-body { display: none; }
    .cc-ds-cat-body { padding-left: 2.05rem; margin-top: 0.2rem; }
    .cc-ds-cat-count { margin-left: auto; color: #57c78a !important; }
    /* rounded icon tile per category (Fish / Crustaceans / ...) */
    .cc-ds-cat-tile {
      width: 22px; height: 22px;
      border-radius: 7px;
      background: rgba(90, 176, 255, 0.12);
      color: #5ab0ff;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      flex: 0 0 auto;
    }

    /* --- Filters panel: quarter pills + depth slider accent ----------- */
    /* checkboxGroupButtons() renders a joined .btn-group; render them as a
       quiet segmented control -- selected = filled accent, unselected = a
       hairline pill on the track. Same #sel_qtr id/values. */
    #sel_qtr .btn-group {
      display: inline-flex; gap: 3px;
      background: var(--cc-tool); border: 1px solid var(--cc-border);
      border-radius: 0.55rem; padding: 3px;
    }
    #sel_qtr .btn-group > .checkbtn.btn {
      flex: 0 0 auto; min-width: 2.5rem;
      border-radius: 0.4rem !important;
      background: transparent !important; border-color: transparent !important;
      color: var(--cc-muted) !important; font-weight: 550 !important; box-shadow: none !important;
      padding: 0.26rem 0.55rem !important; font-size: 0.8rem;
    }
    #sel_qtr .btn-check:checked + .checkbtn.btn,
    #sel_qtr .btn-group > .checkbtn.btn.active {
      background: var(--bs-primary) !important;
      border-color: var(--bs-primary) !important;
      color: #fff !important;
    }
    .cc-filters-modal .irs--shiny .irs-bar {
      background: linear-gradient(90deg, #215a8c, #41b6c4);
      border-top-color: transparent;
      border-bottom-color: transparent;
    }
    .cc-filters-modal .irs--shiny .irs-handle > i:first-child { background-color: var(--bs-primary); }

    /* --- wide popovers (Map tab's Plot Options trigger; formerly also
       the Taxa > Datasets popover, now modal_edit_filters()'s tree) ----- */
    .cc-ds-popover { --bs-popover-max-width: 30rem; }
    .cc-ds-popover .popover-body { max-height: 45vh; overflow-y: auto; }
    #sel_bio_ds { width: 100%; max-width: 100%; margin-bottom: 0; }
    #sel_bio_ds .checkbox label,
    #sel_bio_ds .form-check-label {
      display: flex;
      align-items: baseline;
      gap: 0.4rem;
    }

    /* The dataset heading in the Variable picker is the only thing
       distinguishing `nitrite` (Bottle) from `btl_nitrite` (CTD Cast), so it
       has to be legible — it renders muted gray by default.

       selectInput() is selectize = TRUE by default, so this is a
       div.optgroup-header inside .selectize-dropdown, NOT an <optgroup>
       element. Styling `select optgroup` had no effect on it whatsoever; that
       rule is kept only for any genuinely native select. */
    .selectize-dropdown .optgroup-header,
    select optgroup {
      font-weight: 600 !important;
      font-style: normal;
      opacity: 1;
    }
    [data-bs-theme='dark'] .selectize-dropdown .optgroup-header,
    [data-bs-theme='dark'] select optgroup {
      color: #f8f9fa !important;
      background-color: #343a40 !important;
    }
    [data-bs-theme='light'] .selectize-dropdown .optgroup-header,
    [data-bs-theme='light'] select optgroup {
      color: #212529 !important;
      background-color: #e9ecef !important;
    }
    /* separate one dataset's variables from the next */
    .selectize-dropdown .optgroup + .optgroup .optgroup-header {
      border-top: 1px solid rgba(128, 128, 128, 0.4);
    }

    /* body-parented dropdowns (sel_agg_unit / sel_map_area escape the panel's
       overflow) -- give them room + bigger rows so the options are easy to read */
    body > .selectize-dropdown {
      z-index: 3000;
    }
    body > .selectize-dropdown .selectize-dropdown-content { max-height: 340px; }
    body > .selectize-dropdown .option {
      font-size: 0.9rem; padding: 0.5rem 0.75rem; line-height: 1.3;
    }
    body > .selectize-dropdown .optgroup-header {
      font-size: 0.78rem; padding: 0.4rem 0.75rem;
    }

    /* darken maplibre tooltip and popup text for readability */
    .maplibregl-popup-content,
    .mapboxgl-popup-content {
      color: #1a1a1a !important;
      font-weight: 500;
    }

    /* cross-pane hex-compare readout (the tags$script that wires the two maps) */
    .cc-xh-read {
      position: fixed; z-index: 3000; pointer-events: none;
      background: var(--bs-body-bg); color: var(--bs-body-color);
      border: 1px solid var(--cc-border); border-radius: 9px;
      box-shadow: 0 6px 24px rgba(0, 0, 0, 0.35);
      padding: 0.7rem 0.85rem; min-width: 250px; max-width: 340px;
      font-size: 0.92rem; line-height: 1.35;
    }
    .cc-xh-row {
      display: grid; grid-template-columns: 1fr auto;
      column-gap: 0.75rem; align-items: baseline; margin-bottom: 0.5rem;
    }
    .cc-xh-row:last-of-type { margin-bottom: 0.25rem; }
    .cc-xh-name { font-weight: 600; font-size: 0.9rem; }
    .cc-xh-num  { font-weight: 700; font-size: 1rem; white-space: nowrap; }
    .cc-xh-bar {
      grid-column: 1 / -1; height: 6px; margin-top: 0.3rem;
      background: rgba(128, 128, 128, 0.25); border-radius: 3px; overflow: hidden;
    }
    .cc-xh-bar > i {
      display: block; height: 100%; width: 0;
      background: var(--bs-primary); border-radius: 3px;
    }
    .cc-xh-foot { color: var(--cc-muted); font-size: 0.78rem; margin-top: 0.45rem; }
    /* single-map (Compare off) variant: species row only */
    .cc-xh-read.cc-xh-solo .cc-xh-row + .cc-xh-row { display: none; }
    /* while our readout is up it replaces maplibre's own hex tooltip */
    body.cc-xh-active #map .maplibregl-popup { display: none !important; }

    /* ================================================================
       cc design tokens -- one raised surface shade for every card,
       toolbar button and tool, a step off the page background, plus a
       hairline border. All derived from bslib's theme variables so they
       flip automatically between light and dark (never hardcoded hex).
       ================================================================ */
    :root {
      /* card surfaces: a gentle raise off the page bg */
      --cc-surface:        var(--bs-tertiary-bg);
      /* tool / button surfaces: a firmer grey so a button never disappears
         against the map behind it */
      --cc-tool:           var(--bs-secondary-bg);
      --cc-tool-hover:     var(--bs-tertiary-bg);
      /* a border that actually reads -- bslib's --bs-border-color is a whisper
         (#dee2e6 on #fff in light; near-invisible against a dark map). Mix in
         a good chunk of text colour so every card / map pane has a clear edge
         in both themes. */
      --cc-border:         color-mix(in srgb, var(--bs-border-color) 35%, var(--bs-body-color) 38%);
      /* a stronger one for the map frame specifically (it sits on the page bg
         AND a near-matching dark map -- needs more contrast to register) */
      --cc-border-strong:  color-mix(in srgb, var(--bs-border-color) 20%, var(--bs-body-color) 55%);
      --cc-muted:          var(--bs-secondary-color);
      --cc-shadow:         0 1px 3px rgba(0, 0, 0, 0.12), 0 6px 16px rgba(0, 0, 0, 0.08);
      --cc-radius:         12px;
      --cc-gutter:         12px;   /* empty space between the two split panes */
      --cc-accent-soft:    color-mix(in srgb, var(--bs-primary) 16%, transparent);
    }

    /* --- dark theme: borders were mixing in ~38% of the near-white body
       colour, so every pill / card outline read whitish. Derive them from the
       dark GROUND instead -- a quiet hairline a step darker than the surface.
       --cc-muted was --bs-secondary-color (~#adb5bd) which read too faint for
       body copy -- pull it toward the text colour so labels are legible. */
    [data-bs-theme='dark'] {
      --cc-border:        color-mix(in srgb, var(--bs-border-color) 52%, var(--bs-body-bg) 48%);
      --cc-border-strong: color-mix(in srgb, var(--bs-border-color) 78%, var(--bs-body-color) 10%);
      --cc-muted:         color-mix(in srgb, var(--bs-body-color) 66%, var(--bs-body-bg));
      /* a touch deeper + faintly blue-tinted so cards lift off the dark ground
         the way the white theme's shadow does */
      --cc-shadow:        0 1px 3px rgba(0, 0, 0, 0.6),
                          0 14px 30px rgba(8, 22, 42, 0.55);
    }

    /* --- light theme: a cool, blue-tinted ground with pure-white cards/pills,
       matching the approved mockup. Only the bslib ground/surface tokens are
       nudged here -- every --cc-* token derives from them, so cards, the panel,
       the map frame and the chip all pick it up. Dark theme is untouched. */
    [data-bs-theme='light'] {
      --bs-body-bg:        #eef2f7;   /* page ground */
      --bs-body-color:     #172029;
      --bs-tertiary-bg:    #ffffff;   /* --cc-surface: cards, chip, panel */
      --bs-secondary-bg:   #e4ebf2;   /* --cc-tool: button / segmented-control track */
      --bs-border-color:   #cfd8e2;
      --bs-secondary-color:#4b5560;
      --cc-muted:        #4b5560;
      --cc-border:        color-mix(in srgb, var(--bs-border-color) 62%, var(--bs-body-color) 14%);
      --cc-border-strong: color-mix(in srgb, var(--bs-border-color) 45%, var(--bs-body-color) 30%);
      --cc-shadow:        0 1px 3px rgba(23, 40, 60, 0.14), 0 10px 24px rgba(23, 40, 60, 0.10);
    }
    [data-bs-theme='light'] body { background-color: var(--bs-body-bg); }

    /* ================================================================
       The Map tab's left pane -- a custom flex column (NOT bslib sidebar).
       .cc-mappane is the flex row [ .cc-panel | .cc-map-wrap ]. All colours
       from --cc-* so both themes track. ==================== */
    /* html-fill-container (bslib) would force column direction -- override to a
       row: [ panel | map ] */
    .cc-mappane {
      display: flex !important;
      flex-direction: row !important;
      gap: 0.7rem; align-items: stretch;
      padding: 0.5rem 0 0;
      min-height: 0;
    }
    .cc-map-wrap { flex: 1 1 auto; min-width: 0; min-height: 0; }
    .cc-panel {
      flex: 0 0 274px; width: 274px; display: flex; min-width: 0; overflow: hidden;
      background: var(--cc-surface);
      border: 1px solid var(--cc-border);
      border-radius: 13px;
      box-shadow: var(--cc-shadow);
      transition: flex-basis 0.16s ease, width 0.16s ease;
    }
    .cc-panel-full {
      display: flex; flex-direction: column; width: 100%; min-width: 0;
    }
    /* slim top strip -- a left chevron in a faint tinted zone (same tone as the
       app's tool buttons); the shade, not a rule, marks it off from the stack */
    .cc-panel-bar {
      flex: 0 0 auto; display: flex; align-items: center;
      padding: 0.45rem 0.6rem;
      background: var(--cc-tool);
    }
    .cc-panel-toggle {
      width: 26px; height: 26px; display: grid; place-items: center;
      border: 0; background: transparent; color: var(--cc-muted);
      border-radius: 7px; cursor: pointer;
    }
    .cc-panel-toggle:hover { background: var(--cc-tool); color: var(--bs-body-color); }
    .cc-panel-toggle svg { width: 15px; height: 15px; transition: transform 0.16s ease; }
    .cc-panel.is-collapsed { flex-basis: 40px !important; width: 40px !important; }
    .cc-panel.is-collapsed .cc-panel-scroll { display: none; }
    .cc-panel.is-collapsed .cc-panel-bar { justify-content: center; padding: 0.55rem 0; }
    .cc-panel.is-collapsed .cc-panel-toggle svg { transform: rotate(180deg); }
    .cc-panel-scroll {
      flex: 1 1 auto; min-height: 0; overflow-y: auto;
      padding: 0.6rem 0.75rem 0.8rem;
      scrollbar-width: none;
    }
    .cc-panel-scroll::-webkit-scrollbar { width: 0; height: 0; }
    /* trailing space so scrolling can bring any section to the top */
    .cc-panel-scroll::after { content: ''; display: block; min-height: 40vh; }

    /* --- sections: one consistent body text size throughout; only the section
       title steps away from it. --cc-panel-fs is the single knob. --- */
    .cc-panel { --cc-panel-fs: 0.94rem; }
    .cc-sec { scroll-margin-top: 4px; }
    .cc-sec + .cc-sec {
      border-top: 1px solid var(--cc-border);
      margin-top: 0.55rem;
    }
    .cc-sec-head {
      display: flex; align-items: center; gap: 0.5rem;
      padding: 0.55rem 0.1rem 0.55rem;
      cursor: pointer; user-select: none;
    }
    .cc-sec:first-child .cc-sec-head { padding-top: 0.3rem; }
    .cc-sec-icon { width: 15px; height: 15px; color: var(--cc-muted); flex: 0 0 auto; }
    .cc-sec-title {
      flex: 1; font-size: 1.02rem; font-weight: 650; letter-spacing: 0.005em;
      color: var(--bs-body-color);
    }
    .cc-sec-chev {
      width: 13px; height: 13px; color: var(--cc-muted);
      transition: transform 0.12s;
    }
    .cc-sec.is-shut .cc-sec-chev { transform: rotate(-90deg); }
    .cc-sec.is-shut .cc-sec-body { display: none; }
    .cc-sec-body { font-size: var(--cc-panel-fs); padding-bottom: 0.45rem; }

    .cc-sec-body .form-group { margin-bottom: 0.6rem; }
    .cc-sec-body .control-label,
    .cc-sec-body label.control-label {
      font-size: 0.82rem; font-weight: 600; letter-spacing: 0.03em;
      text-transform: uppercase; color: var(--cc-muted); margin-bottom: 0.32rem;
    }
    .cc-panel .selectize-input,
    .cc-panel .selectize-input .item,
    .cc-panel .selectize-dropdown,
    .cc-panel select.form-select,
    .cc-panel .form-select {
      font-size: var(--cc-panel-fs) !important;
      color: var(--bs-body-color);
      min-height: 0;
    }
    .cc-panel .selectize-input {
      padding: 0.4rem 1.7rem 0.4rem 0.65rem; line-height: 1.35;
    }
    .cc-panel .selectize-input .item { color: var(--bs-body-color) !important; }
    .cc-panel .selectize-input > input { font-size: var(--cc-panel-fs) !important; }

    /* filter-summary: `Label: **value**` -- same 0.875rem as all panel prose;
       label a readable mid-grey, value bold in the full text colour */
    #filter_summary p {
      margin: 0 0 0.4rem; font-size: var(--cc-panel-fs); line-height: 1.5;
      font-weight: 450;
      color: color-mix(in srgb, var(--bs-body-color) 78%, transparent);
    }
    #filter_summary strong { color: var(--bs-body-color); font-weight: 650; }

    /* --- unit-mix note (server.R output$poly_note) -- plain text + a bullet
       list like the taxa-observations block, NOT a boxed callout --- */
    .cc-unit-note {
      font-size: 0.86rem; color: var(--cc-muted);
      padding: 0.5rem 0.1rem 0; line-height: 1.5;
    }
    .cc-unit-note-head { margin-bottom: 0.3rem; }
    .cc-unit-list { list-style: disc; margin: 0; padding-left: 1.05rem; }
    .cc-unit-list li {
      font-size: 0.86rem; line-height: 1.5; padding: 0.1rem 0;
    }
    .cc-unit-list strong {
      font-weight: 700; color: var(--bs-body-color);
      font-variant-numeric: tabular-nums;
    }
    .cc-unit-unit {
      font-family: ui-monospace, Menlo, Consolas, monospace;
      font-size: 0.8rem; color: var(--bs-body-color);
      background: var(--cc-tool); border-radius: 4px; padding: 0.05rem 0.3rem;
      margin: 0 0.25rem;
    }
    .cc-unit-std { color: var(--cc-muted); font-size: 0.82rem; }
    .cc-unit-foot {
      margin-top: 0.45rem; font-size: 0.82rem; font-style: italic;
      color: var(--cc-muted); line-height: 1.4;
    }
    /* breathing room between the unit note and the restrict-area control */
    .cc-restrict-wrap { margin-top: 1rem; }

    /* Summary Statistics -- tight Species / Environment matrix
       (server.R output$summary_statistics). Sizes track --cc-panel-fs. */
    .cc-ss { font-size: var(--cc-panel-fs); }
    .cc-ss-row {
      display: grid;
      grid-template-columns: 3.6rem 1fr 1fr;
      column-gap: 0.5rem; align-items: baseline;
      padding: 0.26rem 0;
    }
    .cc-ss-sep { border-top: 1px solid var(--cc-border); margin: 0.24rem 0; }
    .cc-ss-k { color: var(--cc-muted); font-size: 0.88rem; }
    .cc-ss-v {
      font-weight: 600;
      font-variant-numeric: tabular-nums;
      color: var(--bs-body-color);
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    }
    .cc-ss-v em { font-style: italic; font-weight: 600; }

    /* the Map Layers button on the chip row -- match the Split/Swipe control's
       height + baseline so the two align */
    .cc-map-chiprow { align-items: center; }
    .cc-map-chiprow > * { display: flex; align-items: center; margin: 0 !important; }
    .cc-cmp-layout-row, .cc-cmp-layout-wrap { align-items: center; }
    /* shinyWidgets wraps the radio group in a .form-group with 1rem bottom
       margin -- kill it so the control centres on the chip row */
    .cc-cmp-layout-wrap .form-group,
    .cc-cmp-layout-wrap .shiny-input-container,
    #cmp_layout, #cmp_layout .btn-group { margin: 0 !important; padding-top: 0 !important; padding-bottom: 0 !important; }
    .cc-chip-btn.btn {
      background: var(--cc-surface); border: 1px solid var(--cc-border);
      color: var(--bs-body-color); border-radius: 999px;
      height: 2rem; padding: 0 0.9rem; font-size: 0.8rem; font-weight: 500;
      box-shadow: var(--cc-shadow);
      display: inline-flex; align-items: center; gap: 0.4rem;
      flex: 0 0 auto;
    }
    .cc-chip-btn.btn:hover { background: var(--cc-tool); color: var(--bs-body-color); }
    .cc-chip-btn.btn .bi { opacity: 0.7; }
    #cmp_layout .btn-group { height: 2rem; align-items: center; }

    /* corner dismiss (X) on every modalDialog -- functions.R::modal_x_btn()
       inside the title. Absolutely placed against .modal-content so it sits in
       the true top-right corner regardless of title height. */
    .modal-content:has(.cc-modal-x) { position: relative; }
    .cc-modal-x {
      position: absolute; top: 0.85rem; right: 0.9rem; z-index: 10;
      padding: 0.35rem;
    }
    .modal-title:has(.cc-modal-x) { padding-right: 2rem; }

    /* --- Create Depth Profile modal: fit the viewport with no scroll, centred */
    #shiny-modal:has(.cc-transect-modal) .modal-dialog {
      max-width: 760px; width: calc(100vw - 3rem);
      min-height: calc(100% - 3rem);
      display: flex; align-items: center; margin: 1.5rem auto;
    }
    #shiny-modal:has(.cc-transect-modal) .modal-content { max-height: calc(100vh - 3rem); }
    #shiny-modal:has(.cc-transect-modal) .modal-body {
      display: flex; flex-direction: column; gap: 0.7rem;
      overflow: hidden; min-height: 0;
    }
    .cc-transect-top {
      display: flex; align-items: flex-end; justify-content: space-between;
      gap: 1rem; flex: 0 0 auto;
    }
    .cc-transect-top .form-group, .cc-transect-top .shiny-input-container { margin: 0; }
    #shiny-modal:has(.cc-transect-modal) #transect_map {
      flex: 0 0 auto;
      height: clamp(230px, calc(100vh - 20rem), 440px) !important;
      border-radius: 8px; overflow: hidden;
    }
    #shiny-modal:has(.cc-transect-modal) #transect_map .maplibregl-map,
    #shiny-modal:has(.cc-transect-modal) #transect_map > div { height: 100% !important; }

    /* Download section in the left panel (functions.R::map_panel_ui).
       Leads with the integrated dataset (the hero block); chart tables + raw
       source files are behind native <details>. */
    .cc-dl-body { display: flex; flex-direction: column; gap: 0.5rem; }
    .cc-dl-body .checkbox, .cc-dl-body .form-check { margin-bottom: 0.15rem; }
    .cc-dl-hero {
      border: 1px solid color-mix(in srgb, var(--bs-primary) 45%, transparent);
      background: var(--cc-accent-soft);
      border-radius: 9px; padding: 0.5rem 0.6rem;
    }
    .cc-dl-hero-h {
      display: flex; align-items: center; gap: 0.35rem;
      font-weight: 650; font-size: 0.9rem;
      color: color-mix(in srgb, var(--bs-primary) 82%, var(--bs-body-color));
    }
    .cc-dl-hero-h .bi { width: 0.82rem; height: 0.82rem; }
    .cc-dl-hero-d {
      font-size: 0.8rem; color: var(--cc-muted); line-height: 1.4; margin-top: 0.15rem;
    }
    .cc-dl-more { border-top: 1px solid var(--cc-border); padding-top: 0.35rem; }
    .cc-dl-more > summary {
      list-style: none; cursor: pointer;
      display: flex; align-items: center; gap: 0.4rem;
      font-size: 0.85rem; font-weight: 550; color: var(--cc-muted);
      padding: 0.15rem 0;
    }
    .cc-dl-more > summary::-webkit-details-marker { display: none; }
    .cc-dl-more > summary::before {
      content: ''; flex: 0 0 auto;
      width: 0; height: 0;
      border-left: 5px solid currentColor;
      border-top: 4px solid transparent; border-bottom: 4px solid transparent;
      transition: transform 0.15s ease;
    }
    .cc-dl-more[open] > summary::before { transform: rotate(90deg); }
    .cc-dl-more[open] > summary { color: var(--bs-body-color); }
    .cc-dl-more .shiny-input-container { margin-top: 0.25rem; }
    .cc-dl-body .btn { margin-top: 0.35rem; }

    /* --- Map Layers modal: grouped thumbnail cards. Sized + vertically
       centred to fit without a scrollbar on a normal screen. */
    #shiny-modal:has(.cc-lyr-block) .modal-dialog {
      max-width: 1060px; width: calc(100vw - 3rem); margin: 1.75rem auto;
      min-height: calc(100% - 3.5rem);
      display: flex; align-items: center;
    }
    #shiny-modal:has(.cc-lyr-block) .modal-body {
      max-height: calc(100vh - 11rem); overflow-y: auto;
      scrollbar-width: none;
    }
    #shiny-modal:has(.cc-lyr-block) .modal-body::-webkit-scrollbar { width: 0; height: 0; }
    .cc-lyr-block { margin-bottom: 0.9rem; }
    .cc-lyr-block-head {
      display: flex; align-items: center; gap: 0.45rem; margin-bottom: 0.4rem;
    }
    .cc-lyr-block-icon { width: 14px; height: 14px; color: var(--bs-primary); }
    .cc-lyr-block-name { font-weight: 600; font-size: 0.86rem; }
    .cc-lyr-block-count { color: var(--cc-muted); font-size: 0.74rem; }
    .cc-lyr-grid .shiny-options-group {
      display: grid; grid-template-columns: repeat(auto-fill, minmax(92px, 1fr));
      gap: 0.45rem; margin: 0;
    }
    .cc-lyr-grid .checkbox, .cc-lyr-grid .form-check { margin: 0; position: relative; }
    .cc-lyr-grid .checkbox label, .cc-lyr-grid .form-check-label {
      display: flex !important; flex-direction: column; gap: 0.3rem;
      border: 1.5px solid var(--cc-border); border-radius: 9px;
      padding: 0 0 0.4rem; overflow: hidden; cursor: pointer;
      font-size: 0.75rem; line-height: 1.25; text-align: left; width: 100%;
      transition: border-color 0.12s;
    }
    .cc-lyr-grid .checkbox label > span:last-child,
    .cc-lyr-grid .form-check-label > span:last-child { padding: 0 0.45rem; }
    .cc-lyr-grid .checkbox label::before,
    .cc-lyr-grid .form-check-label::before {
      content: ''; display: block; width: 100%; aspect-ratio: 100 / 56;
      background: var(--cc-lyr-thumb) center / cover no-repeat, #0d1b28;
      border-bottom: 1px solid var(--cc-border);
    }
    .cc-lyr-grid input[type=checkbox] {
      position: absolute; top: 0.35rem; right: 0.35rem; margin: 0; z-index: 2;
      width: 0.95rem; height: 0.95rem; cursor: pointer;
    }
    .cc-lyr-grid .checkbox label:has(input:checked),
    .cc-lyr-grid .form-check-label:has(input:checked) {
      border-color: var(--bs-primary);
    }

    @media (max-width: 720px) {
      .cc-mappane { flex-direction: column !important; }
      .cc-panel { flex-basis: auto; width: auto; max-height: 40vh; }
    }

    /* --- top bar -- reproduces Main.dc.html to the pixel (colors via the
       --cc-* theme tokens instead of the mockup's fixed dark hex):
         cards row : flex, gap 16px, padding 20px 32px 0
         card      : radius 10px, padding 14px 16px
         label     : 11px, tracking .08em, mb 8px
         value     : 15px / 600, dataset suffix 15px / 400 muted
         chip row  : padding 14px 32px 16px
         chip      : inline-flex, radius 999, padding 8px 16px, 13px       */
    .cc-topbar { margin: 0; padding: 0.7rem 1.75rem 0; }
    /* plain flex row (no Bootstrap gutters) so the card's left edge lines up
       exactly with the .cc-topbar padding -- and therefore with the filter
       chip below it */
    .cc-topbar > .row {
      --bs-gutter-x: 0; --bs-gutter-y: 0;
      display: flex; flex-wrap: wrap; gap: 0.9rem; margin: 0; align-items: stretch;
    }
    .cc-topbar > .row > [class^='col'] {
      display: flex; flex: 1 1 340px; max-width: none; padding: 0;
    }
    .cc-topbar-card {
      background-color: var(--cc-surface) !important;
      border: 1px solid var(--cc-border) !important;
      border-radius: 10px !important;
      padding: 0.62rem 0.85rem !important;
      box-shadow: var(--cc-shadow) !important;
      width: 100%;
      display: flex; flex-direction: column; justify-content: center;
    }
    .cc-card-label {
      display: block;
      letter-spacing: 0.07em;
      font-size: 0.72rem;
      font-weight: 600;
      color: var(--cc-muted) !important;
      margin-bottom: 0.25rem !important;
      line-height: 1.2;
      white-space: nowrap;
    }
    /* env card header row: label, then the add button / switch right next to it
       (left-grouped, not spread to the card edge). */
    .cc-env-row {
      display: flex; align-items: center; gap: 0.55rem;
      margin-bottom: 0.15rem; min-width: 0;
    }
    .cc-env-row .cc-card-label { margin: 0 !important; flex: 0 0 auto; }
    .cc-topbar-card .form-switch .form-check-input {
      width: 2.5em; height: 1.35em; margin: 0; cursor: pointer;
    }
    /* OFF-state control: a filled blue button next to the label, swapped for
       the switch on #cmp_env_toggle:checked so the card height is stable. */
    .cc-env-add {
      display: inline-flex; align-items: center; gap: 0.34rem; flex: 0 0 auto;
      font-size: 0.8rem; font-weight: 600; line-height: 1.1; white-space: nowrap;
      color: #fff; text-decoration: none;
      background: var(--bs-primary); border: 0;
      border-radius: 7px; padding: 0.32rem 0.7rem;
    }
    .cc-env-add:hover, .cc-env-add:focus {
      color: #fff; text-decoration: none;
      background: color-mix(in srgb, var(--bs-primary) 88%, #000);
    }
    .cc-env-add .bi { width: 0.72rem; height: 0.72rem; }
    /* ON state: the switch is pushed to the card's far right (margin-left:auto)
       and reads as a quiet grey toggle, not the bright accent one. */
    .cc-env-switch {
      flex: 0 0 auto; display: flex; align-items: center;
      margin: 0 0 0 auto !important; width: auto !important;
    }
    .cc-env-switch .form-group,
    .cc-env-switch .form-check,
    .cc-env-switch .shiny-input-container {
      margin: 0 !important; width: auto !important; min-width: 0 !important; padding: 0 !important;
    }
    .cc-env-switch .form-check-input:checked {
      background-color: var(--cc-border-strong, #aeb9c4) !important;
      border-color: var(--cc-border-strong, #aeb9c4) !important;
    }
    .cc-env-switch .form-check-input:focus {
      box-shadow: 0 0 0 0.2rem rgba(120, 130, 140, 0.25);
    }
    body:has(#cmp_env_toggle:checked) .cc-env-add { display: none; }
    body:has(#cmp_env_toggle:not(:checked)) .cc-env-switch { display: none; }

    .cc-combo2, .cc-combo2 * { box-sizing: border-box; }
    .cc-combo2 .form-group,
    .cc-combo2 .shiny-input-container { margin: 0 !important; padding: 0 !important; }
    /* the real checkboxes (ck_children, sel_env_all_vars) live in the DOM but
       are mirrored at the top of the dropdown panel -- hide the originals */
    .cc-combo2-extra-src {
      position: absolute !important; width: 1px; height: 1px;
      overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap;
    }

    /* env compare-off helper text, 13px, one line where it fits */
    .cc-topbar-card > .shiny-panel-conditional .small.text-muted,
    .cc-topbar-card .small.text-muted {
      font-size: 0.8125rem; line-height: 1.4; margin-top: 0.55rem; display: block;
    }

    /* filter chip row */
    .cc-filter-chip {
      width: fit-content;
      background-color: var(--cc-surface);
      border: 1px solid var(--cc-border);
      border-radius: 999px;
      padding: 0.42rem 0.9rem !important;
      font-size: 0.8rem; font-weight: 500;
      color: var(--cc-muted) !important;
      box-shadow: var(--cc-shadow);
      margin: 0 !important;
      gap: 0.55rem !important;
    }
    .cc-filter-chip .bi { opacity: 0.7; }
    .cc-filter-chip .btn-link { line-height: 1; }
    .cc-filter-chip a { color: var(--bs-primary); text-decoration: none; }
    .cc-filter-chip a:hover { text-decoration: underline; }
    /* filter_chip_summary is a uiOutput: all four facets, each a bold value
       with a blue dot (.cc-chip-facet) */
    .cc-filter-chip > .shiny-html-output {
      display: inline-flex; align-items: center; gap: 0.55rem; flex-wrap: wrap;
    }
    .cc-filter-chip .cc-chip-facet {
      display: inline-flex; align-items: center;
      font-weight: 650; color: var(--bs-body-color);
    }
    .cc-filter-chip .cc-chip-facet::before {
      content: ''; flex: 0 0 auto;
      width: 4px; height: 4px; border-radius: 50%;
      background: color-mix(in srgb, var(--bs-body-color) 40%, transparent);
      margin-right: 0.45rem;
    }

    /* per-pane titles (server.R #map_title_sp/env): a compact pill floated
       INSIDE the map, top-left, like the mockup -- not a strip above it (that
       ate a row of vertical space). Click-through so map drag works under it.
       The species title shows on every Map view; the environmental one only in
       the side-by-side split, positioned over the right pane. */
    .cc-map-titles { position: absolute; inset: 0 0 auto 0; z-index: 6; pointer-events: none; }
    .cc-map-title {
      position: absolute; top: 0.55rem;
      max-width: calc(50% - 3.5rem);
      padding: 0.3rem 0.7rem;
      font-size: 0.84rem; font-weight: 600; letter-spacing: 0.005em;
      color: var(--bs-body-color);
      background: color-mix(in srgb, var(--bs-body-bg) 92%, transparent);
      -webkit-backdrop-filter: blur(3px); backdrop-filter: blur(3px);
      border: 1px solid var(--cc-border);
      border-radius: 999px;
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
      box-shadow: var(--cc-shadow);
    }
    .cc-map-title-sp  { left: 0.5rem; }
    .cc-map-title-env { left: calc(50% + var(--cc-gutter) / 2 + 0.5rem); }
    /* nudge maplibre's own top-left controls (the scale bar) below the pill so
       the km scale and the title pill do not overlap */
    #map .maplibregl-ctrl-top-left { margin-top: 2.7rem; }

    /* legends: sized server-side (server.R lg_style: title 11 / text 9 / pad 6);
       here just shrink the gradient bar and tighten the label row */
    .maplibregl-legend, .mapboxgl-legend {
      box-shadow: 0 2px 9px rgba(0, 0, 0, 0.26) !important;
    }
    .maplibregl-legend .legend-gradient,
    .mapboxgl-legend .legend-gradient,
    .maplibregl-legend div[style*='linear-gradient'],
    .mapboxgl-legend div[style*='linear-gradient'] {
      height: 9px !important;
    }
    .maplibregl-legend .legend-labels,
    .mapboxgl-legend .legend-labels { margin-top: 2px !important; }
    /* maplibre's own scale bar: theme it instead of a fixed white box */
    .maplibregl-ctrl-scale {
      background-color: color-mix(in srgb, var(--bs-body-bg) 90%, transparent) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--cc-border) !important;
      font-size: 0.62rem !important;
    }
    /* attribution: collapsed to the little ⓘ by default, tiny type when open */
    .maplibregl-ctrl-attrib.maplibregl-compact:not(.maplibregl-compact-show) .maplibregl-ctrl-attrib-inner {
      display: none;
    }
    .maplibregl-ctrl-attrib { font-size: 10px !important; }
    .maplibregl-ctrl-attrib-inner a { color: var(--cc-muted) !important; }

    /* --- compare widget layouts:
         * Compare OFF  -> swipe widget, collapsed by CSS to one plain map
           (divider hidden, the species map un-clipped, the env map + its
           legend hidden). No sync machinery -> pan/zoom is a normal map.
         * Compare ON + Split -> mapgl sync (grid of two .maplibregl-map);
           reshape the grid to a real gutter, per the Compare mockup.
       maplibre-gl's ResizeObserver repaints each canvas to its new size. */

    /* Compare OFF: single full map. Target the maps by ID (they are #map-before
       / #map-after in BOTH the swipe and sync DOM) -- a :last-of-type selector
       was silently matching nothing here (all three children are <div>, and the
       last div is .maplibregl-compare, not a .maplibregl-map), which left the
       swipe divider + the env map showing on load. */
    body:has(input#cmp_env_toggle:not(:checked)) #map .maplibregl-compare { display: none !important; }
    body:has(input#cmp_env_toggle:not(:checked)) #map #map-before {
      clip: rect(auto, auto, auto, auto) !important; clip-path: none !important;
      width: 100% !important;
    }
    body:has(input#cmp_env_toggle:not(:checked)) #map #map-after { display: none !important; }
    /* swipe mode (compare ON, .maplibregl-compare present): both maps overlay
       the SAME geographic view, so the after map scale bar + nav control are a
       duplicate stacked on top of the before map ones -- hide them */
    #map:has(.maplibregl-compare) #map-after .maplibregl-control-container {
      display: none !important;
    }

    /* Compare ON + Split: two separate map cards with EMPTY space between them
       (the page background shows through the gutter -- not a divider line, so
       the panes don't read as one connected map). sync DOM: #map > div.grid */
    body:has(input#cmp_env_toggle:checked) #map:not(:has(.maplibregl-compare)) {
      border: 0 !important; border-radius: 0 !important; overflow: visible !important;
    }
    body:has(input#cmp_env_toggle:checked) #map:not(:has(.maplibregl-compare)) > div {
      grid-template-columns: 1fr 1fr !important;
      gap: var(--cc-gutter) !important;
      background: transparent !important;
    }
    body:has(input#cmp_env_toggle:checked) #map:not(:has(.maplibregl-compare)) > div > .maplibregl-map {
      width: auto !important;
      border: 1.5px solid var(--cc-border-strong);
      border-radius: var(--cc-radius);
      overflow: hidden;
    }

    /* --- page framing: the mockup's 32px side gutter, applied once on the
       nav-panel so the top bar and the map line up, plus a small inset +
       rounding on the map (Main.dc.html: margin 16 32 24, radius 12).
       page_navbar wraps content in .container-fluid; drop its default
       padding and let .cc-topbar / the tab-pane own the gutter. */
    .bslib-page-navbar > .container-fluid { --bs-gutter-x: 0; }
    /* kill the stack of small vertical gaps between the filter chip, the
       Split/Swipe + tool row, and the map -- bslib's fillable tab panes ship
       24px padding + a 1rem flex gap; override both */
    .cc-topbar { padding-bottom: 0.35rem; }
    .bslib-page-navbar > .navbar { margin-bottom: 0; }
    /* brand: smaller logo, a wordmark next to the CalCOFI sun */
    .cc-brand { display: inline-flex; align-items: center; gap: 0.55rem; }
    .cc-brand-name {
      font-size: 1.12rem; font-weight: 600; letter-spacing: -0.005em;
      color: var(--bs-body-color);
    }
    .navbar-brand { padding-top: 0.4rem; padding-bottom: 0.4rem; }
    /* nav links + right-hand links: heavier + higher-contrast so the bar is
       easy to scan (they were thin light-grey) */
    .bslib-page-navbar .navbar-nav .nav-link,
    .bslib-page-navbar .navbar .nav-item .nav-link {
      font-weight: 550;
      color: color-mix(in srgb, var(--bs-body-color) 72%, transparent);
    }
    .bslib-page-navbar .navbar-nav .nav-link:hover { color: var(--bs-body-color); }
    /* active tab indicator: a thicker white (body-colour) underline */
    .bslib-page-navbar .navbar-nav.nav-underline,
    .bslib-page-navbar .navbar-nav.nav-underline .nav-link {
      --bs-nav-underline-border-width: 3px;
    }
    .bslib-page-navbar .navbar-nav .nav-link.active {
      font-weight: 650; color: var(--bs-body-color);
      border-bottom: 3px solid var(--bs-body-color) !important;
      box-shadow: none !important;
    }

    /* Feedback link (navbar): a plain nav-link with a leading icon -- the
       .nav-link class already gives it the shared weight/colour/underline */
    .cc-feedback-link {
      display: inline-flex; align-items: center; gap: 0.35rem;
      white-space: nowrap;
    }
    .cc-feedback-link .bi { opacity: 0.85; }
    /* actionLink adds .action-button; keep it looking like the other nav-links */
    .cc-feedback-link.action-button { text-decoration: none; cursor: pointer; }

    /* --- Feedback modal (functions.R::modal_feedback). 2 columns, app colours. */
    #shiny-modal:has(.cc-feedback-modal) .modal-dialog {
      max-width: 1120px; width: calc(100vw - 3rem);
      min-height: calc(100% - 3.5rem); margin: 1.75rem auto;
      display: flex; align-items: center;
    }
    #shiny-modal:has(.cc-feedback-modal) .modal-content { max-height: calc(100vh - 3.5rem); }
    #shiny-modal:has(.cc-feedback-modal) .modal-body { overflow-y: auto; min-height: 0; }
    .cc-fb-grid {
      display: grid; grid-template-columns: minmax(0, 330px) minmax(0, 1fr);
      gap: 1.6rem; align-items: start;
    }
    .cc-fb-col { display: flex; flex-direction: column; gap: 0.7rem; min-width: 0; }
    @media (max-width: 820px) {
      .cc-fb-grid { grid-template-columns: 1fr; gap: 1rem; }
    }
    .cc-fb-shotlabel {
      font-size: 0.95rem; font-weight: 600; color: var(--bs-body-color);
      margin-bottom: 0.3rem; display: block;
    }
    .cc-fb-shot {
      position: relative;
      border: 1px solid var(--cc-border); border-radius: 10px; overflow: hidden;
      background: var(--cc-tool);
    }
    .cc-fb-shot-img { aspect-ratio: 16 / 9; display: block; background: var(--cc-tool); }
    .cc-fb-shot-img img { width: 100%; height: 100%; object-fit: contain; display: block; }
    .cc-fb-shot.off .cc-fb-shot-img img { opacity: 0.3; }
    .cc-fb-shot-wait {
      width: 100%; height: 100%; display: grid; place-items: center;
      font-size: 0.85rem; color: var(--cc-muted); text-align: center; padding: 0.5rem;
    }
    /* before a capture: a Capture button + hint, and the bar is hidden */
    .cc-fb-shot-ph {
      width: 100%; height: 100%; display: flex; flex-direction: column;
      align-items: center; justify-content: center; gap: 0.5rem; padding: 0.75rem;
    }
    .cc-fb-capture {
      display: inline-flex; align-items: center; gap: 0.4rem;
      font-size: 0.88rem; font-weight: 600; color: #fff;
      background: var(--bs-primary); border: 0; border-radius: 8px;
      padding: 0.5rem 0.95rem; cursor: pointer;
    }
    .cc-fb-capture:hover { background: color-mix(in srgb, var(--bs-primary) 88%, #000); }
    .cc-fb-shot-ph p { margin: 0; font-size: 0.8rem; color: var(--cc-muted); }
    .cc-fb-shot:not(.has-shot) .cc-fb-shot-bar { display: none; }
    .cc-fb-shot-bar {
      display: flex; align-items: center; gap: 0.6rem;
      padding: 0.45rem 0.6rem; border-top: 1px solid var(--cc-border);
    }
    .cc-fb-inc { display: flex; align-items: center; gap: 0.35rem; font-size: 0.88rem; margin: 0; margin-right: auto; font-weight: 400; }
    .cc-fb-retake, .cc-fb-edit {
      display: inline-flex; align-items: center; gap: 0.3rem;
      font-size: 0.82rem; padding: 0.25rem 0.6rem; border-radius: 7px;
      border: 1px solid var(--cc-border); background: var(--cc-surface);
      color: var(--bs-body-color); cursor: pointer;
    }
    .cc-fb-retake:hover, .cc-fb-edit:hover { background: var(--cc-tool-hover); }
    .cc-fb-retake .bi, .cc-fb-edit .bi { width: 0.75rem; height: 0.75rem; color: var(--cc-muted); }

    /* the mark-up overlay (cc-feedback.js) -- covers the screenshot cell */
    .cc-fb-anno {
      position: absolute; inset: 0; z-index: 5;
      display: flex; flex-direction: column;
      background: var(--cc-surface);
    }
    .cc-fb-anno-stage {
      position: relative;
      flex: 1 1 auto; min-height: 0; display: grid; place-items: center;
      background: #0c1116; overflow: hidden;
    }
    .cc-fb-anno-stage canvas {
      max-width: 100%; max-height: 100%; cursor: crosshair; touch-action: none;
    }
    /* the inline editor for the Text tool (cc-feedback.js placeText) */
    .cc-fb-anno-text {
      position: absolute; z-index: 6; margin: 0; padding: 0 3px;
      min-width: 3rem; line-height: 1.15;
      background: rgba(255, 255, 255, 0.14);
      border: 1px dashed rgba(255, 255, 255, 0.75); border-radius: 3px;
      outline: none; font-weight: 700; font-family: system-ui, sans-serif;
    }
    .cc-fb-anno-text::placeholder { color: rgba(255, 255, 255, 0.6); font-weight: 400; }
    .cc-fb-anno-bar {
      flex: 0 0 auto; display: flex; align-items: center; gap: 0.35rem;
      padding: 0.4rem 0.5rem; border-top: 1px solid var(--cc-border);
      flex-wrap: wrap;
    }
    .cc-fb-anno-bar button {
      font: inherit; font-size: 0.78rem; padding: 0.2rem 0.5rem;
      border-radius: 6px; border: 1px solid var(--cc-border);
      background: var(--cc-surface); color: var(--bs-body-color); cursor: pointer;
    }
    .cc-fb-anno-bar button.on { background: var(--cc-accent-soft); border-color: var(--bs-primary); color: var(--accent-ink, var(--bs-primary)); }
    .cc-fb-anno-bar button.prim { background: var(--bs-primary); color: #fff; border-color: var(--bs-primary); }
    .cc-fb-anno-sp { flex: 1 1 auto; }
    .cc-fb-anno-colors { display: inline-flex; gap: 0.25rem; margin: 0 0.15rem; }
    .cc-fb-anno-sw {
      width: 1.15rem; height: 1.15rem; padding: 0 !important; border-radius: 50% !important;
      border: 2px solid transparent !important;
    }
    .cc-fb-anno-sw.on { border-color: var(--bs-body-color) !important; }
    .cc-feedback-modal .form-group { margin-bottom: 0; }
    .cc-feedback-modal .control-label,
    .cc-feedback-modal label {
      font-size: 0.95rem; font-weight: 600; margin-bottom: 0.3rem;
      text-transform: none; letter-spacing: 0; color: var(--bs-body-color);
    }
    .cc-fb-sub { font-weight: 400; color: var(--cc-muted); font-size: 0.85rem; }
    .cc-feedback-modal textarea.form-control {
      min-height: 84px; font-size: 0.95rem; line-height: 1.45;
    }
    .cc-feedback-modal .form-control { font-size: 0.95rem; }
    .cc-fb-sent { font-size: 0.86rem; color: var(--cc-muted); margin: 0.15rem 0 0; line-height: 1.5; }
    .cc-fb-peek {
      color: var(--bs-primary); cursor: pointer;
      text-decoration: underline; text-underline-offset: 2px; white-space: nowrap;
    }
    .cc-fb-detail { margin: 0.45rem 0 0; padding-left: 1.1rem; font-size: 0.85rem; color: var(--cc-muted); }
    .cc-fb-detail li { padding: 0.1rem 0; }
    .cc-fb-detail b { color: var(--bs-body-color); font-weight: 600; }
    #shiny-modal:has(.cc-feedback-modal) .modal-footer {
      display: flex; align-items: center; gap: 0.75rem;
      background: var(--cc-tool); border-top: 1px solid var(--cc-border);
    }
    .cc-fb-gh {
      margin-right: auto; font-size: 0.85rem; color: var(--cc-muted);
      text-decoration: underline; text-underline-offset: 2px;
    }
    .cc-fb-gh:hover { color: var(--bs-body-color); }
    .cc-fb-status { font-size: 0.83rem; color: var(--cc-muted); }
    .cc-fb-status.warn { color: var(--bs-danger); }
    .cc-fb-send { display: inline-flex; align-items: center; gap: 0.4rem; }
    .cc-fb-thanks { font-size: 0.98rem; margin: 0; line-height: 1.5; }
    /* after a successful send the grid is replaced by a one-line thank-you --
       shrink the dialog to fit it and drop the now-pointless footer */
    #shiny-modal:has(.cc-fb-done) .modal-dialog { max-width: 440px; }
    #shiny-modal:has(.cc-fb-done) .modal-footer { display: none; }
    .cc-fb-done .cc-fb-grid { display: block; }

    .tab-content { margin-top: 0; }
    .tab-content > .tab-pane,
    .tab-content > .tab-pane.active,
    .tab-content > .tab-pane.html-fill-container {
      padding: 0 2rem 1rem !important;
      gap: 0.35rem !important;
    }
    .cc-tab-toolbar { margin: 0 !important; min-height: 0; }
    #map {
      border: 1.5px solid var(--cc-border-strong);
      border-radius: var(--cc-radius);
      overflow: hidden;
    }

    /* --- top-bar search comboboxes (Species/Taxa, Compare Env Variable) */
    /* Client-side skin over selectizeInput -- see the `render` templates in
       functions.R::top_bar_taxa_ui()/top_bar_env_ui(). Purely visual: the
       underlying <select>, its id, choices and value are all untouched. */
    .cc-search-combo { position: relative; }
    .cc-search-combo-icon {
      position: absolute;
      left: 0.65rem;
      top: 1.2rem;
      opacity: 0.55;
      pointer-events: none;
      z-index: 3;
    }
    .cc-search-combo .selectize-control.single .selectize-input {
      padding-left: 2.1rem;
      min-height: 2.4rem;
      display: flex;
      align-items: center;
    }
    .cc-search-item { display: flex; align-items: baseline; gap: 0.3rem; flex-wrap: wrap; }
    .cc-search-item-name { font-weight: 600; }
    .cc-search-item-ds { color: var(--bs-secondary-color, #6c757d); font-weight: 400; }
    .cc-search-opt { padding: 0.05rem 0; }
    .cc-search-opt-name { font-weight: 500; }
    .cc-search-opt-ds { color: var(--bs-secondary-color, #6c757d); }
    .cc-search-grp {
      font-weight: 700;
      text-transform: uppercase;
      font-size: 0.7rem;
      letter-spacing: 0.03em;
      opacity: 0.6;
      padding: 0.35rem 0.5rem 0.15rem;
    }

    /* --- tab-tool triggers (Stats / Options / Map Layers / Draw Transect).
       On the Map tab these sit on the filter-chip row (.cc-map-toolbar);
       other tabs still use a small toolbar row (.cc-tab-toolbar). --------- */
    .cc-tab-toolbar {
      display: flex; justify-content: flex-end; align-items: center;
      gap: 0.4rem; margin-bottom: 0.6rem;
    }
    /* About page: a right-aligned Close (X) that returns to the last
       visualization tab -- server.R input$close_about -> nav_select. */
    .cc-util-bar {
      display: flex; justify-content: flex-end;
      margin: -0.25rem 0 0.4rem;
    }
    .cc-util-close {
      display: inline-flex; align-items: center; gap: 0.35rem;
      padding: 0.3rem 0.6rem; border-radius: 8px;
      font-size: 0.86rem; font-weight: 550;
      color: var(--cc-muted); text-decoration: none;
      border: 1px solid var(--cc-border); background: var(--cc-tool);
    }
    .cc-util-close:hover {
      color: var(--bs-body-color); background: var(--cc-tool-hover);
      text-decoration: none;
    }
    .cc-util-close .bi { width: 0.8rem; height: 0.8rem; }
    /* extra vertical breathing room: more gap when the chip pills wrap, and a
       real margin below the row so the map + left panel sit lower / less
       cluttered against the top bar */
    .cc-filter-row { row-gap: 0.55rem; align-items: center; margin: 0.85rem 0 0.7rem; }
    /* Split/Swipe only matters while comparing */
    .cc-cmp-layout-row { display: flex; align-items: center; }
    .cc-cmp-layout-wrap { display: flex; align-items: center; }
    body:has(input#cmp_env_toggle:not(:checked)) .cc-cmp-layout-row { display: none !important; }
    /* the trigger buttons read as solid tools on the raised surface, not
       faint hollow outlines lost against the map -- filled, theme-aware */
    .cc-tab-toolbar .btn,
    .cc-tab-toolbar .btn.btn-outline-secondary,
    .cc-map-toolbar .btn,
    .cc-map-toolbar .btn.btn-outline-secondary {
      --bs-btn-bg: var(--cc-tool);
      --bs-btn-border-color: var(--cc-border);
      --bs-btn-color: var(--bs-body-color);
      --bs-btn-hover-bg: var(--cc-tool-hover);
      --bs-btn-hover-border-color: var(--cc-border);
      --bs-btn-hover-color: var(--bs-body-color);
      --bs-btn-active-bg: var(--cc-tool-hover);
      --bs-btn-active-border-color: var(--bs-primary);
      box-shadow: var(--cc-shadow);
      font-weight: 500;
    }
    .cc-tab-toolbar .btn .bi, .cc-map-toolbar .btn .bi { opacity: 0.75; }

    /* Split / Swipe segmented control -- an iOS-style pill: a soft grey track
       with the SELECTED segment lifted onto the card surface. shinyWidgets
       renders #cmp_layout > .btn-group > (input.btn-check + label.radiobtn),
       the checked input being the label's PRECEDING sibling. Every property is
       set explicitly here (with !important where Bootstrap's .btn cascade and
       shinyWidgets' own stylesheet would otherwise fight it) so the two states
       are unmistakably different -- the earlier version left both segments
       looking identical, so a click looked like it did nothing. */
    #cmp_layout .btn-group {
      background: var(--cc-tool) !important;
      border: 1px solid var(--cc-border) !important;
      border-radius: 0.6rem !important;
      padding: 2px !important; gap: 2px;
      box-shadow: none !important;
    }
    #cmp_layout .radiobtn.btn {
      background-color: transparent !important;
      border-color: transparent !important;
      color: color-mix(in srgb, var(--bs-body-color) 66%, transparent) !important;
      box-shadow: none !important;
      border-radius: 0.45rem !important;
      padding: 0.24rem 0.7rem !important;
      font-size: 0.76rem;
      font-weight: 550 !important;
      transition: color 0.12s, background-color 0.12s;
    }
    #cmp_layout .radiobtn.btn:hover {
      color: var(--bs-body-color) !important;
    }
    #cmp_layout .btn-check:checked + .radiobtn.btn {
      background-color: var(--cc-surface) !important;
      color: var(--bs-body-color) !important;
      font-weight: 600 !important;
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.2) !important;
    }
    #cmp_layout .btn-check:focus-visible + .radiobtn.btn {
      outline: 2px solid var(--bs-primary); outline-offset: 1px;
    }

    /* --- .cc-combo2: grouped/drilldown search combo (Species/Taxa,
       Compare Environmental Variable) -- matches the approved mockup. The
       real selectizeInput stays in the DOM (server.R still targets it via
       updateSelectizeInput()/updateSelectInput()) but is visually hidden
       here; see the JS below for how the visible button + panel drive it. */
    .cc-combo2 { position: relative; }
    .cc-combo2 .selectize-control {
      position: absolute !important; width: 1px !important; height: 1px !important;
      padding: 0 !important; margin: -1px !important; overflow: hidden !important;
      clip: rect(0, 0, 0, 0) !important; white-space: nowrap !important; border: 0 !important;
    }
    .cc-combo2 .selectize-dropdown { display: none !important; }

    /* the search trigger reads as a big clean line (screenshot 4): icon +
       17px value + chevron pinned right, no box. A subtle border/ring only
       appears on hover / focus / while open. */
    .cc-combo2-btn {
      display: flex; align-items: center; gap: 0.5rem; width: 100%;
      border: 1px solid transparent; border-radius: 8px;
      padding: 0.28rem 0.3rem; cursor: pointer;
      background: transparent; min-height: 2rem;
      transition: border-color 0.12s, background-color 0.12s;
    }
    .cc-combo2-btn:hover { border-color: var(--cc-border); }
    .cc-combo2-open .cc-combo2-btn, .cc-combo2-btn:focus-visible {
      outline: none; border-color: var(--bs-primary);
      background: var(--bs-body-bg);
      box-shadow: 0 0 0 0.2rem rgba(var(--bs-primary-rgb), 0.18);
    }
    .cc-combo2-icon { opacity: 0.55; flex: 0 0 auto; width: 1.05rem; height: 1.05rem; }
    .cc-combo2-label {
      flex: 1 1 auto; overflow: hidden; text-overflow: ellipsis;
      white-space: nowrap; text-align: left; font-size: 0.95rem; line-height: 1.4;
    }
    .cc-combo2-label-name { font-weight: 600; }
    .cc-combo2-label-ds { color: var(--cc-muted); font-weight: 400; }
    .cc-combo2-chevron {
      flex: 0 0 auto; opacity: 0.75; color: var(--bs-body-color);
      transition: transform 0.15s; width: 0.85rem; height: 0.85rem;
    }
    .cc-combo2-open .cc-combo2-chevron { transform: rotate(180deg); opacity: 1; }

    /* the mirrored qualifier checkbox at the top of the dropdown panel */
    .cc-combo2-extra {
      display: flex; align-items: center; gap: 0.55rem;
      padding: 0.65rem 0.85rem; margin: 0;
      border-bottom: 1px solid var(--cc-border);
      font-size: 0.875rem; cursor: pointer; user-select: none;
    }
    .cc-combo2-extra input { margin: 0; cursor: pointer; flex: 0 0 auto; }
    .cc-combo2-extra-tip { color: var(--cc-muted); cursor: help; }

    /* dropdown panel: a clearly raised sheet -- solid elevated bg, real
       border, deep shadow -- so it reads over a dark map, not a faint ghost */
    .cc-combo2-panel {
      display: none; flex-direction: column; position: absolute;
      top: calc(100% + 0.35rem); left: 0; right: 0; z-index: 1055;
      background: var(--cc-tool);
      border: 1px solid var(--cc-border);
      border-radius: 10px;
      box-shadow: 0 12px 32px rgba(0, 0, 0, 0.45), 0 0 0 1px rgba(0, 0, 0, 0.05);
      max-height: 70vh; overflow: hidden;
    }
    .cc-combo2-open .cc-combo2-panel { display: flex; }

    .cc-combo2-search { padding: 0.6rem; border-bottom: 1px solid var(--cc-border); flex: 0 0 auto; }
    .cc-combo2-search-input, .cc-combo2-drill-search-input {
      width: 100%; border: 1px solid var(--cc-border); border-radius: 6px;
      padding: 0.45rem 0.65rem; background: var(--bs-body-bg); color: inherit;
      font-size: 0.875rem;
    }
    .cc-combo2-search-input:focus, .cc-combo2-drill-search-input:focus {
      outline: none; border-color: var(--bs-primary);
      box-shadow: 0 0 0 0.15rem rgba(var(--bs-primary-rgb), 0.2);
    }

    .cc-combo2-groups { overflow-y: auto; padding: 0.3rem 0; }
    .cc-combo2-group-head { display: flex; align-items: center; gap: 0.5rem; padding: 0.55rem 0.85rem; cursor: pointer; }
    .cc-combo2-group-head:hover { background: rgba(127, 127, 127, 0.12); }
    /* flat combo (env vars): groups are always open, headers are just labels */
    .cc-combo2-flat .cc-combo2-group-head { cursor: default; padding: 0.5rem 0.85rem 0.2rem; }
    .cc-combo2-flat .cc-combo2-group-head:hover { background: none; }
    .cc-combo2-flat .cc-combo2-group-arrow { display: none; }
    .cc-combo2-flat .cc-combo2-group + .cc-combo2-group { border-top: 1px solid var(--cc-border); }
    .cc-combo2-group-arrow { opacity: 0.6; width: 0.9rem; text-align: center; flex: 0 0 auto; }
    .cc-combo2-group-name { font-weight: 600; flex: 1 1 auto; font-size: 0.9rem; }
    .cc-combo2-group-count {
      font-size: 0.72rem; color: var(--cc-muted);
      background: rgba(127, 127, 127, 0.16); padding: 0.1rem 0.5rem; border-radius: 1rem; flex: 0 0 auto;
    }
    .cc-combo2-group-body { padding-bottom: 0.3rem; }

    .cc-combo2-item {
      position: relative; display: flex; align-items: baseline; gap: 0.4rem;
      padding: 0.5rem 0.85rem 0.5rem 2.1rem; cursor: pointer; border-radius: 6px; margin: 0 0.3rem;
      font-size: 0.875rem;
    }
    .cc-combo2-item:hover { background: rgba(127, 127, 127, 0.14); }
    .cc-combo2-item-sel { background: rgba(var(--bs-primary-rgb), 0.2); }
    .cc-combo2-item-check {
      position: absolute; left: 0.8rem; top: 50%; transform: translateY(-50%); color: var(--bs-primary);
    }
    .cc-combo2-item-name { font-weight: 500; }
    .cc-combo2-item-name i, .cc-combo2-sci { font-style: italic; }
    .cc-combo2-sci { color: var(--cc-muted); }
    .cc-combo2-label-name i { font-style: italic; }
    .cc-combo2-label-name .cc-combo2-sci { color: var(--cc-muted); }
    .cc-combo2-item-ds { color: var(--cc-muted); font-size: 0.85em; }

    .cc-combo2-showall { padding: 0.4rem 0.85rem 0.5rem 2.1rem; color: var(--bs-primary); cursor: pointer; font-size: 0.85rem; }
    .cc-combo2-showall:hover { text-decoration: underline; }

    .cc-combo2-drill-head { display: flex; align-items: center; gap: 0.6rem; padding: 0.6rem 0.8rem; border-bottom: 1px solid var(--bs-border-color); flex: 0 0 auto; }
    .cc-combo2-drill-back { cursor: pointer; font-size: 1.4rem; line-height: 1; opacity: 0.7; padding: 0 0.2rem; }
    .cc-combo2-drill-back:hover { opacity: 1; }
    .cc-combo2-drill-name { flex: 1 1 auto; }
    .cc-combo2-drill-count {
      font-size: 0.72rem; color: var(--bs-secondary-color);
      background: rgba(127, 127, 127, 0.14); padding: 0.05rem 0.5rem; border-radius: 1rem;
    }
    .cc-combo2-drill-list { overflow-y: auto; flex: 1 1 auto; padding: 0.3rem 0; }
    .cc-combo2-drill-footer {
      padding: 0.5rem 0.8rem; text-align: center; font-size: 0.78rem;
      color: var(--bs-secondary-color); border-top: 1px solid var(--bs-border-color); flex: 0 0 auto;
    }

    /* --- small-screen / phone fixes ---------------------------------- */
    @media (max-width: 575.98px) {
      #map { min-height: 70vh; }
      #map .maplibregl-map,
      #map .mapboxgl-map { min-height: 70vh; }
    }
    ")) ),

  # Cross-pane hex compare (side-by-side / sync layout). HOVER a hexagon on
  # either map: the cell at that location is outlined on BOTH maps, and a
  # readout follows the cursor showing each value AND a bar for where it sits
  # in its own colour scale -- 298 abundance vs 12 C are not comparable as
  # numbers, but "72% up its scale" vs "28% up its scale" is. Pure client-side;
  # reads the h3t fill layers ("sp"/"env", or spN/envN classic) already on the
  # maps. The compare widget is rebuilt on every filter change, so this polls
  # and re-wires each fresh map pair.
  tags$script(HTML("
    (function () {
      var HL = 'cc_xh';
      var RANGE = { sp: null, env: null, sp_label: 'Species', env_label: 'Environmental' };
      if (window.Shiny) Shiny.addCustomMessageHandler('cc_hexrange', function (m) {
        RANGE = m;
      });

      var LOG = function () {
        if (window.__ccXhDebug) console.log.apply(console, ['[cc-xh]'].concat([].slice.call(arguments)));
      };
      function num(v) {
        if (v === null || v === undefined || v === '') return null;
        var n = Number(v); return isFinite(n) ? n : null;
      }
      function fmt(n) {
        if (n === null) return '—';
        var a = Math.abs(n);
        return a >= 100 ? n.toFixed(0) : (a >= 1 ? n.toFixed(1) : n.toFixed(2));
      }
      function pct(v, r) {
        if (v === null || !r || r.length !== 2 || r[1] === r[0]) return null;
        var p = (v - r[0]) / (r[1] - r[0]);
        return Math.max(0, Math.min(1, p));
      }
      function paneMeasure(titleId, fallback) {
        var el = document.getElementById(titleId);
        var t = el ? el.textContent.trim() : '';
        return t ? t.replace(/^[^—]*—\\s*/, '') : fallback;   // drop 'Species — ' etc
      }
      function pick(map, point, kind) {
        var re = new RegExp('^' + kind + '([0-9_].*)?$');
        var fs;
        try { fs = map.queryRenderedFeatures(point) || []; } catch (err) { return null; }
        for (var i = 0; i < fs.length; i++) {
          var f = fs[i], lid = f.layer && f.layer.id;
          if ((lid && re.test(lid) && lid.indexOf('poly') === -1 && lid.indexOf(HL) !== 0)
              || f.source === kind) return f;
        }
        return null;
      }
      function ensureHl(map) {
        try {
          if (map.getSource(HL)) return;
          map.addSource(HL, { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
          map.addLayer({ id: HL + '_f', type: 'fill', source: HL,
            paint: { 'fill-color': '#ffffff', 'fill-opacity': 0.18 } });
          map.addLayer({ id: HL + '_l', type: 'line', source: HL,
            paint: { 'line-color': '#ffffff', 'line-width': 3 } });
        } catch (err) { LOG('ensureHl', err); }
      }
      function setHl(map, geom) {
        var s = map.getSource(HL);
        if (!s) return;
        s.setData({ type: 'FeatureCollection',
          features: geom ? [{ type: 'Feature', geometry: geom, properties: {} }] : [] });
      }

      // one readout element, reused
      var read = document.createElement('div');
      read.className = 'cc-xh-read';
      read.style.display = 'none';
      read.innerHTML =
        '<div class=cc-xh-row><div class=cc-xh-name data-r=sp></div>' +
        '<div class=cc-xh-num data-r=spv></div>' +
        '<div class=cc-xh-bar><i data-r=spb></i></div></div>' +
        '<div class=cc-xh-row><div class=cc-xh-name data-r=env></div>' +
        '<div class=cc-xh-num data-r=envv></div>' +
        '<div class=cc-xh-bar><i data-r=envb></i></div></div>' +
        '<div class=cc-xh-foot data-r=foot>bar = position within each map colour scale</div>';
      document.body.appendChild(read);
      function q(k) { return read.querySelector('[data-r=' + k + ']'); }

      // solo = single-map mode (Compare off): show only the species row, and
      // suppress maplibre's own bare-number hex tooltip while the box is up.
      function showRead(x, y, spV, envV, solo) {
        read.classList.toggle('cc-xh-solo', !!solo);
        document.body.classList.add('cc-xh-active');
        q('sp').textContent  = paneMeasure('map_title_sp', RANGE.sp_label || 'Species');
        q('env').textContent = paneMeasure('map_title_env', RANGE.env_label || 'Environmental');
        q('spv').textContent  = fmt(spV);
        q('envv').textContent = fmt(envV);
        q('foot').textContent = solo
          ? 'bar = position in the colour scale'
          : 'bar = position within each map colour scale';
        var sp = pct(spV, RANGE.sp), ep = pct(envV, RANGE.env);
        q('spb').style.width  = sp  === null ? '0' : (sp  * 100).toFixed(0) + '%';
        q('envb').style.width = ep  === null ? '0' : (ep  * 100).toFixed(0) + '%';
        q('spb').parentNode.style.opacity  = sp  === null ? 0.3 : 1;
        q('envb').parentNode.style.opacity = ep  === null ? 0.3 : 1;
        read.style.display = 'block';
        var w = read.offsetWidth, h = read.offsetHeight;
        var px = x + 16, py = y + 16;
        if (px + w > window.innerWidth - 8)  px = x - w - 16;
        if (py + h > window.innerHeight - 8) py = y - h - 16;
        read.style.left = px + 'px';
        read.style.top  = py + 'px';
      }
      function hideRead(before, after) {
        read.style.display = 'none';
        document.body.classList.remove('cc-xh-active');
        setHl(before, null); setHl(after, null);
        if (before) before.getCanvas().style.cursor = '';
        if (after)  after.getCanvas().style.cursor = '';
      }

      function wire(inst) {
        var before = inst.getBeforeMap && inst.getBeforeMap();
        var after  = inst.getAfterMap  && inst.getAfterMap();
        if (!before || !after) return;
        if (before.__ccXhWired && after.__ccXhWired) return;
        before.__ccXhWired = true; after.__ccXhWired = true;
        LOG('wired a fresh map pair');
        var raf = null;

        function move(srcMap, otherMap, srcKind, otherKind, e) {
          if (raf) return;
          var tog = document.getElementById('cmp_env_toggle');
          var comparing = tog && tog.checked;

          // SINGLE-MAP MODE (Compare off): only the species pane is shown. Give
          // the hovered hex the same treatment the compare view uses -- an
          // outline plus a readout with the species label, value and where it
          // sits in the colour scale -- in place of maplibre's bare-number
          // tooltip. The hidden 'after' (env) map is ignored.
          if (!comparing) {
            if (srcMap !== before) return;
            raf = requestAnimationFrame(function () {
              raf = null;
              var f = pick(before, e.point, 'sp');
              if (!f) { hideRead(before, after); return; }
              ensureHl(before);
              setHl(before, f.geometry || null);
              before.getCanvas().style.cursor = 'crosshair';
              showRead(
                e.originalEvent.clientX, e.originalEvent.clientY,
                num(f.properties ? f.properties.value : null), null, true);
            });
            return;
          }

          raf = requestAnimationFrame(function () {
            raf = null;
            var sf = pick(srcMap, e.point, srcKind);
            if (!sf) { hideRead(before, after); return; }
            ensureHl(before); ensureHl(after);
            var ll = e.lngLat;
            var of = pick(otherMap, otherMap.project(ll), otherKind);
            setHl(srcMap, sf.geometry || null);
            setHl(otherMap, (of && of.geometry) || sf.geometry || null);
            srcMap.getCanvas().style.cursor = 'crosshair';

            var spF  = srcKind === 'sp'  ? sf : of;
            var envF = srcKind === 'env' ? sf : of;
            showRead(
              e.originalEvent.clientX, e.originalEvent.clientY,
              num(spF  && spF.properties  ? spF.properties.value  : null),
              num(envF && envF.properties ? envF.properties.value : null));
          });
        }

        before.on('mousemove', function (e) { move(before, after, 'sp',  'env', e); });
        after.on('mousemove',  function (e) { move(after,  before, 'env', 'sp',  e); });
        before.on('mouseout', function () { hideRead(before, after); });
        after.on('mouseout',  function () { hideRead(before, after); });
      }

      // force maplibre's attribution control into its collapsed (i) state
      // regardless of map width, so it never sprawls a long credit line
      function collapseAttrib() {
        document.querySelectorAll('#map .maplibregl-ctrl-attrib').forEach(function (a) {
          a.classList.add('maplibregl-compact');
          a.classList.remove('maplibregl-compact-show');
        });
      }

      setInterval(function () {
        var inst = window.HTMLWidgets && HTMLWidgets.find('#map');
        if (inst) wire(inst);
        collapseAttrib();
      }, 800);
    })();
  ")),

  # Make the floating layers control drive BOTH sides of the compare widget.
  #
  # mapgl's control is deliberately per-map, and in a swipe compare both maps are
  # full-size and stacked, so the two controls land on the same screen position
  # and only the top one (the "after"/right map's) is clickable. The left map's
  # control is underneath, unreachable — so a toggle appeared to work on half the
  # map and nowhere else.
  #
  # This wraps _setVisibility rather than the click handler, so it rides on top
  # of whatever the control decides, including its own `map.getLayer()` guard.
  # Layer ids are side-specific ("sp_poly" vs "env_poly", "sp3" vs "env3") and
  # are mapped across; the shared PMTiles boundary ids are identical on both maps
  # and pass through unchanged. If mapgl ever grows a first-class option for
  # this (add_layers_control(sync_compare = TRUE)), drop this in favor of it.
  tags$script(HTML("
    (function () {
      function counterpart(id) {
        if (/^sp($|[0-9_])/.test(id))  return 'env' + id.slice(2);
        if (/^env($|[0-9_])/.test(id)) return 'sp'  + id.slice(3);
        return id;   // shared overlay: same id on both maps
      }
      function patch() {
        if (!window.MapglLayersControl) return false;
        if (window.__ccLayersMirrorPatched) return true;
        window.__ccLayersMirrorPatched = true;
        var proto = window.MapglLayersControl.prototype;
        var orig  = proto._setVisibility;
        proto._setVisibility = function (layerId, visibility) {
          orig.call(this, layerId, visibility);
          try {
            var el = this._map && this._map.getContainer();
            if (!el || !window.HTMLWidgets) return;
            var m = /^(.*)-(before|after)$/.exec(el.id);
            if (!m) return;
            var inst = HTMLWidgets.find('#' + m[1]);
            if (!inst || !inst.getBeforeMap) return;
            var other = m[2] === 'before' ? inst.getAfterMap() : inst.getBeforeMap();
            var oid = counterpart(layerId);
            if (other && other.getLayer(oid))
              other.setLayoutProperty(oid, 'visibility', visibility);
          } catch (e) {
            // mirroring is a convenience; never let it break the real toggle
          }
        };
        return true;
      }
      if (!patch()) {
        var tries = 0;
        var t = setInterval(function () {
          if (patch() || ++tries > 80) clearInterval(t);
        }, 250);
      }
    })();
  ")),

  # Group modal_edit_filters()'s Datasets checkboxes (sel_bio_ds) into the
  # category tree functions.R::dataset_category_tree_ui() lays the data out
  # for (#cc_ds_tree's data-categories/data-cat-order attributes).
  #
  # Client-side on purpose, same reasoning as the layers/compare scripts
  # above: the modal is inserted fresh each time showModal() fires, so this
  # watches for #cc_ds_tree via MutationObserver rather than hooking the
  # button click. It only ever MOVES each checkbox <div> to a deeper spot
  # inside #sel_bio_ds's own subtree (into a wrapper it inserts), never out of
  # it -- Shiny's value collection (`$(el).find('input:checked')`) searches
  # the whole subtree regardless of nesting, so the reactive value this
  # produces is exactly what a flat, ungrouped checkboxGroupInput would have
  # produced; only the on-screen arrangement changed.
  tags$script(HTML("
    (function () {
      function buildTree(container) {
        if (container.dataset.ccTreeBuilt) return;
        var catsAttr  = container.getAttribute('data-categories');
        var orderAttr = container.getAttribute('data-cat-order');
        if (!catsAttr || !orderAttr) return;
        var cats  = JSON.parse(catsAttr);
        var order = orderAttr.split('|');
        var iconsAttr = container.getAttribute('data-cat-icons');
        var catIcons  = iconsAttr ? JSON.parse(iconsAttr) : {};
        var grp = container.querySelector('#sel_bio_ds .shiny-options-group') ||
                  document.getElementById('sel_bio_ds');
        if (!grp) return;
        var items = Array.prototype.slice.call(
          grp.querySelectorAll(':scope > .checkbox, :scope > .form-check'));
        if (items.length === 0) return;
        container.dataset.ccTreeBuilt = '1';

        var byCat = {};
        order.forEach(function (c) { byCat[c] = []; });
        items.forEach(function (item) {
          var input = item.querySelector('input[type=checkbox]');
          if (!input) return;
          var cat = cats[input.value] || 'Other';
          if (!byCat[cat]) byCat[cat] = [];
          byCat[cat].push(item);
        });

        var wrap = document.createElement('div');
        wrap.className = 'cc-ds-tree-body';

        order.forEach(function (cat) {
          var catItems = byCat[cat];
          if (!catItems || catItems.length === 0) return;

          var section = document.createElement('div');
          // collapsed by default -- the panel opens as a short category list
          // ('click a category to expand it'), matching the FilterSummary mockup
          section.className = 'cc-ds-cat cc-ds-cat-collapsed';

          var head = document.createElement('div');
          head.className = 'cc-ds-cat-head';

          var arrow = document.createElement('span');
          arrow.className = 'cc-ds-cat-arrow';
          arrow.textContent = '\u25BE';

          var check = document.createElement('input');
          check.type = 'checkbox';
          check.className = 'cc-ds-cat-check';

          var label = document.createElement('strong');
          label.textContent = cat;

          var count = document.createElement('span');
          count.className = 'small text-muted cc-ds-cat-count';

          head.appendChild(arrow);
          head.appendChild(check);
          if (catIcons[cat]) {
            var tile = document.createElement('span');
            tile.className = 'cc-ds-cat-tile';
            var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            svg.setAttribute('viewBox', '0 0 24 24');
            svg.setAttribute('width', '15');
            svg.setAttribute('height', '15');
            svg.setAttribute('fill', 'none');
            svg.setAttribute('stroke', 'currentColor');
            svg.setAttribute('stroke-width', '1.7');
            svg.innerHTML = catIcons[cat];
            tile.appendChild(svg);
            head.appendChild(tile);
          }
          head.appendChild(label);
          head.appendChild(count);

          var body = document.createElement('div');
          body.className = 'cc-ds-cat-body';
          catItems.forEach(function (item) { body.appendChild(item); });

          head.addEventListener('click', function (e) {
            if (e.target === check) return;
            section.classList.toggle('cc-ds-cat-collapsed');
          });

          check.addEventListener('click', function (e) { e.stopPropagation(); });
          check.addEventListener('change', function () {
            catItems.forEach(function (item) {
              var input = item.querySelector('input[type=checkbox]');
              if (input) input.checked = check.checked;
            });
            grp.dispatchEvent(new Event('change', { bubbles: true }));
          });

          section.appendChild(head);
          section.appendChild(body);
          wrap.appendChild(section);
        });

        grp.appendChild(wrap);

        function refreshCounts() {
          var totalChecked = 0, totalAll = 0;
          wrap.querySelectorAll('.cc-ds-cat').forEach(function (section) {
            var body  = section.querySelector('.cc-ds-cat-body');
            var boxes = body.querySelectorAll('input[type=checkbox]');
            var n = 0;
            boxes.forEach(function (b) { if (b.checked) n++; });
            totalChecked += n;
            totalAll     += boxes.length;
            var countEl = section.querySelector('.cc-ds-cat-count');
            if (countEl) countEl.textContent = n + '/' + boxes.length;
            var catCheck = section.querySelector('.cc-ds-cat-check');
            if (catCheck) {
              catCheck.checked = n === boxes.length;
              catCheck.indeterminate = n > 0 && n < boxes.length;
            }
          });
          var totalEl = document.getElementById('ds_tree_total');
          if (totalEl) totalEl.textContent = totalChecked + ' of ' + totalAll + ' included';
        }

        grp.addEventListener('change', function (e) {
          if (e.target.classList.contains('cc-ds-cat-check')) return;
          refreshCounts();
        });

        refreshCounts();
      }

      var observer = new MutationObserver(function () {
        var el = document.getElementById('cc_ds_tree');
        if (el) buildTree(el);
      });
      observer.observe(document.body, { childList: true, subtree: true });
    })();
  ")),

  # .cc-combo2: the custom grouped/drilldown search panel for the
  # Species/Taxa and Compare Environmental Variable boxes (top_bar_taxa_ui()/
  # top_bar_env_ui() in functions.R), matching the approved mockup. The real
  # selectizeInput stays in the DOM and is the ONLY source of truth for the
  # Shiny input value -- this only builds a custom visible UI around it and
  # drives it through selectize's own JS API (setValue()), which fires the
  # exact same change event Shiny's own selectize binding already listens
  # for. It always reads choices/groups from the LIVE selectize instance
  # (instance.options/.optgroups), not a static snapshot, so it stays
  # correct after server.R's updateSelectizeInput(session, "sel_name", ...)
  # / updateSelectInput(session, "sel_env_var", ...) rebuild its choices at
  # runtime (dataset-filter pruning, "show all variables", etc.).
  #
  # HTML built here deliberately uses single-quoted attributes throughout
  # (class='...' not class="...") so this JS, embedded in an R HTML("...")
  # string below, never contains a literal " that could prematurely close
  # the R string -- see the "Plot Options" comment bug this app hit earlier
  # from exactly that mistake.
  tags$script(HTML("
    (function () {
      function esc(s) {
        var q = String.fromCharCode(39);
        return String(s).split('&').join('&amp;').split('<').join('&lt;')
          .split('>').join('&gt;').split(q).join('&#39;');
      }

      function splitLabel(label, groupLabel) {
        var suffix = groupLabel ? ' — ' + groupLabel : '';
        if (suffix && label.length > suffix.length && label.slice(-suffix.length) === suffix) {
          return label.slice(0, -suffix.length);
        }
        return label;
      }

      // taxa with no common name arrive as '(rank: Scientific name)' from
      // sp_choices(); show just the scientific name in italics. A common name
      // ('Pacific sardine (pilchard) (species: Sardinops sagax)') is left as-is
      // but its trailing '(rank: ...)' is dimmed.
      var RANKS = 'species|subspecies|genus|family|order|class|phylum|kingdom|infraspecies|variety|forma|section|tribe|superfamily|suborder|infraorder';
      function fmtName(s) {
        var bare = new RegExp('^\\\\((?:' + RANKS + '): (.+)\\\\)$').exec(s);
        if (bare) return '<i>' + esc(bare[1]) + '</i>';
        var tail = new RegExp('^(.*?) \\\\((?:' + RANKS + '): (.+)\\\\)$').exec(s);
        if (tail) return esc(tail[1]) +
          ' <span class=\\'cc-combo2-sci\\'>' + esc(tail[2]) + '</span>';
        return esc(s);
      }

      function CCCombo(root) {
        this.root = root;
        this.select = root.querySelector('select');
        this.btn = root.querySelector('.cc-combo2-btn');
        this.label = root.querySelector('.cc-combo2-label');
        this.panel = root.querySelector('.cc-combo2-panel');
        this.mainPlaceholder = root.getAttribute('data-placeholder-main') || 'Search…';
        this.noun = root.getAttribute('data-noun') || 'items';
        // flat = every group open + listed in full (few options, no accordion)
        this.flat = root.getAttribute('data-flat') === 'true';
        this.extraId  = root.getAttribute('data-extra-id') || null;
        this.extraLbl = root.getAttribute('data-extra-label') || '';
        this.extraTip = root.getAttribute('data-extra-tip') || '';
        this.openGroup = null;
        this.drillGroup = null;
        this.drillBatch = 50;
      }

      CCCombo.prototype.instance = function () { return this.select && this.select.selectize; };

      // the real (hidden) Shiny checkbox this combo mirrors at the panel top
      CCCombo.prototype.extraInput = function () {
        return this.extraId ? document.getElementById(this.extraId) : null;
      };
      CCCombo.prototype.extraRowHtml = function () {
        if (!this.extraId) return '';
        var on = this.extraInput() && this.extraInput().checked;
        return '<label class=\\'cc-combo2-extra\\'>' +
          '<input type=\\'checkbox\\' class=\\'cc-combo2-extra-cb\\'' + (on ? ' checked' : '') + '>' +
          '<span>' + esc(this.extraLbl) + '</span>' +
          (this.extraTip ? '<span class=\\'cc-combo2-extra-tip\\' title=\\'' + esc(this.extraTip) + '\\'>&#9432;</span>' : '') +
          '</label>';
      };
      CCCombo.prototype.wireExtra = function () {
        var self = this;
        var cb = this.panel.querySelector('.cc-combo2-extra-cb');
        if (!cb) return;
        cb.addEventListener('click', function (e) { e.stopPropagation(); });
        cb.addEventListener('change', function () {
          var real = self.extraInput();
          if (!real) return;
          real.checked = cb.checked;
          real.dispatchEvent(new Event('change', { bubbles: true }));
        });
      };

      CCCombo.prototype.groups = function () {
        var inst = this.instance();
        if (!inst) return [];
        var byGroup = {};
        var order = [];
        Object.keys(inst.optgroups).forEach(function (gv) {
          byGroup[gv] = { value: gv, label: inst.optgroups[gv].label || gv, items: [] };
          order.push(gv);
        });
        if (!byGroup['']) byGroup[''] = { value: '', label: '', items: [] };

        // every known group label, to strip a baked-in ' — Dataset' suffix no
        // matter which group we end up showing the item under (a taxon in
        // several datasets carries an ARRAY optgroup and one arbitrary label)
        var allLabels = order.map(function (gv) { return byGroup[gv].label; })
          .filter(function (s) { return s; });
        function cleanLabel(label) {
          for (var i = 0; i < allLabels.length; i++) {
            var suf = ' — ' + allLabels[i];
            if (label.length > suf.length && label.slice(-suf.length) === suf)
              return label.slice(0, -suf.length);
          }
          return label;
        }

        Object.keys(inst.options).forEach(function (ov) {
          var opt = inst.options[ov];
          var gvs = Array.isArray(opt.optgroup) ? opt.optgroup
            : (opt.optgroup !== undefined && opt.optgroup !== null ? [opt.optgroup] : ['']);
          var rawLabel = opt.label || opt.text || String(opt.value);
          var item = { value: String(opt.value), label: rawLabel, name: cleanLabel(rawLabel) };
          gvs.forEach(function (gv) {
            var key = byGroup[gv] !== undefined ? gv : '';
            byGroup[key].items.push(item);
          });
        });
        var groups = order.map(function (gv) { return byGroup[gv]; });
        if (byGroup[''].items.length) groups.push(byGroup['']);
        return groups.filter(function (g) { return g.items.length > 0; });
      };

      CCCombo.prototype.currentValue = function () {
        var inst = this.instance();
        if (!inst) return null;
        var items = inst.items || [];
        return items.length ? items[0] : null;
      };

      CCCombo.prototype.refreshLabel = function () {
        var inst = this.instance();
        if (!inst) return;
        var v = this.currentValue();
        if (!v || !inst.options[v]) { this.label.textContent = this.mainPlaceholder; return; }
        // reuse groups() so the name is cleaned and the dataset resolved the
        // same way as the dropdown (handles the array-optgroup / multi-dataset
        // taxon case that a bare splitLabel misses)
        var nm = null, grp = '';
        var gs = this.groups();
        for (var i = 0; i < gs.length && !nm; i++) {
          for (var j = 0; j < gs[i].items.length; j++) {
            if (gs[i].items[j].value === v) { nm = gs[i].items[j].name; grp = gs[i].label; break; }
          }
        }
        if (nm === null) {
          var opt = inst.options[v];
          nm = opt.label || opt.text || String(v);
        }
        this.label.innerHTML = '<span class=\\'cc-combo2-label-name\\'>' + fmtName(nm) + '</span>' +
          (grp ? '<span class=\\'cc-combo2-label-ds\\'> — ' + esc(grp) + '</span>' : '');
      };

      CCCombo.prototype.select_ = function (value) {
        var inst = this.instance();
        if (!inst) return;
        inst.setValue(value);
        this.refreshLabel();
        this.close();
      };

      CCCombo.prototype.open = function () {
        if (this.root.classList.contains('cc-combo2-open')) return;
        this.root.classList.add('cc-combo2-open');
        var cur = this.currentValue();
        var curGroup = null;
        if (cur) {
          var inst = this.instance();
          var opt = inst && inst.options[cur];
          if (opt && opt.optgroup) curGroup = opt.optgroup;
        }
        this.openGroup = curGroup;
        this.renderList('');
        document.addEventListener('mousedown', this.onDocMouseDownBound);
        document.addEventListener('keydown', this.onDocKeyDownBound);
      };

      CCCombo.prototype.close = function () {
        this.root.classList.remove('cc-combo2-open');
        this.panel.innerHTML = '';
        document.removeEventListener('mousedown', this.onDocMouseDownBound);
        document.removeEventListener('keydown', this.onDocKeyDownBound);
      };

      CCCombo.prototype.renderList = function (query) {
        var self = this;
        var groups = this.groups();
        var q = (query || '').trim().toLowerCase();
        var cur = this.currentValue();

        var html = this.extraRowHtml() +
          '<div class=\\'cc-combo2-search\\'><input type=\\'text\\' class=\\'cc-combo2-search-input\\' placeholder=\\'' +
          esc(this.mainPlaceholder) + '\\' value=\\'' + esc(query || '') + '\\'></div>' +
          '<div class=\\'cc-combo2-groups\\'>';

        groups.forEach(function (g) {
          var items = g.items;
          if (q) {
            items = items.filter(function (it) {
              return it.label.toLowerCase().indexOf(q) !== -1 || g.label.toLowerCase().indexOf(q) !== -1;
            });
          }
          if (q && items.length === 0) return;

          var isOpen = q ? true : (self.flat || g.value === self.openGroup);
          html += '<div class=\\'cc-combo2-group' + (isOpen ? ' cc-combo2-group-open' : '') + '\\' data-group=\\'' +
            esc(g.value) + '\\'>' +
            '<div class=\\'cc-combo2-group-head\\' data-group=\\'' + esc(g.value) + '\\'>' +
            '<span class=\\'cc-combo2-group-arrow\\'>' + (self.flat ? '' : (isOpen ? '▾' : '▸')) + '</span>' +
            '<span class=\\'cc-combo2-group-name\\'>' + esc(g.label) + '</span>' +
            '<span class=\\'cc-combo2-group-count\\'>' + g.items.length + ' ' + esc(self.noun) + '</span>' +
            '</div>';

          if (isOpen) {
            // small groups (< 12) list in full -- no drill button; larger
            // groups preview 3 then link to the scrollable full list.
            // flat combos always list in full.
            var full = q || self.flat || items.length < 12;
            var shown = full ? items : items.slice(0, 3);
            html += '<div class=\\'cc-combo2-group-body\\'>';
            shown.forEach(function (it) {
              var sel = it.value === cur;
              var nm = it.name || splitLabel(it.label, g.label);
              html += '<div class=\\'cc-combo2-item' + (sel ? ' cc-combo2-item-sel' : '') + '\\' data-value=\\'' +
                esc(it.value) + '\\'>' +
                (sel ? '<span class=\\'cc-combo2-item-check\\'>✓</span>' : '') +
                '<span class=\\'cc-combo2-item-name\\'>' + fmtName(nm) + '</span>' +
                (q && g.label ? '<span class=\\'cc-combo2-item-ds\\'> — ' + esc(g.label) + '</span>' : '') +
                '</div>';
            });
            if (!full) {
              html += '<div class=\\'cc-combo2-showall\\' data-group=\\'' + esc(g.value) + '\\'>' +
                'Show all ' + items.length + ' ' + esc(self.noun) + ' →</div>';
            }
            html += '</div>';
          }
          html += '</div>';
        });

        html += '</div>';
        this.panel.innerHTML = html;
        this.wireList();
        this.wireExtra();
        var input = this.panel.querySelector('.cc-combo2-search-input');
        if (input && query) {
          input.focus();
          input.setSelectionRange(input.value.length, input.value.length);
        }
      };

      CCCombo.prototype.wireList = function () {
        var self = this;
        var input = this.panel.querySelector('.cc-combo2-search-input');
        if (input) {
          input.addEventListener('input', function () { self.renderList(input.value); });
          input.addEventListener('keydown', function (e) { if (e.key === 'Escape') { self.close(); self.btn.focus(); } });
        }
        if (!this.flat) this.panel.querySelectorAll('.cc-combo2-group-head').forEach(function (head) {
          head.addEventListener('click', function () {
            var gv = head.getAttribute('data-group');
            self.openGroup = (self.openGroup === gv) ? null : gv;
            self.renderList(input ? input.value : '');
          });
        });
        this.panel.querySelectorAll('.cc-combo2-item').forEach(function (el) {
          el.addEventListener('click', function () { self.select_(el.getAttribute('data-value')); });
        });
        this.panel.querySelectorAll('.cc-combo2-showall').forEach(function (el) {
          el.addEventListener('click', function () { self.openDrill(el.getAttribute('data-group')); });
        });
      };

      CCCombo.prototype.openDrill = function (groupValue) {
        this.drillGroup = groupValue;
        this.drillBatch = 50;
        this.renderDrill('');
      };

      CCCombo.prototype.renderDrill = function (query) {
        var self = this;
        var groups = this.groups();
        var g = groups.filter(function (x) { return x.value === self.drillGroup; })[0];
        if (!g) { this.renderList(''); return; }
        var q = (query || '').trim().toLowerCase();
        var cur = this.currentValue();

        var items = g.items.filter(function (it) { return !q || it.label.toLowerCase().indexOf(q) !== -1; });
        items = items.slice().sort(function (a, b) {
          var as = a.value === cur ? -1 : 0, bs = b.value === cur ? -1 : 0;
          return as - bs;
        });
        var shown = items.slice(0, this.drillBatch);

        var html = this.extraRowHtml() +
          '<div class=\\'cc-combo2-drill-head\\'>' +
          '<span class=\\'cc-combo2-drill-back\\' role=\\'button\\'>‹</span>' +
          '<strong class=\\'cc-combo2-drill-name\\'>' + esc(g.label) + '</strong>' +
          '<span class=\\'cc-combo2-drill-count\\'>' + g.items.length + ' ' + esc(self.noun) + '</span>' +
          '</div>' +
          '<div class=\\'cc-combo2-search\\'><input type=\\'text\\' class=\\'cc-combo2-drill-search-input\\' placeholder=\\'Search within ' +
          esc(g.label) + '…\\' value=\\'' + esc(query || '') + '\\'></div>' +
          '<div class=\\'cc-combo2-drill-list\\'>';

        shown.forEach(function (it) {
          var sel = it.value === cur;
          var nm = it.name || splitLabel(it.label, g.label);
          html += '<div class=\\'cc-combo2-item' + (sel ? ' cc-combo2-item-sel' : '') + '\\' data-value=\\'' +
            esc(it.value) + '\\'>' +
            (sel ? '<span class=\\'cc-combo2-item-check\\'>✓</span>' : '') +
            '<span class=\\'cc-combo2-item-name\\'>' + fmtName(nm) + '</span>' +
            '</div>';
        });

        html += '</div><div class=\\'cc-combo2-drill-footer\\'>Showing ' + shown.length + ' of ' + items.length +
          (items.length > shown.length ? ' — scroll or search to find more' : '') + '</div>';

        this.panel.innerHTML = html;
        this.wireDrill();
        this.wireExtra();
        var input = this.panel.querySelector('.cc-combo2-drill-search-input');
        if (input && query) { input.focus(); input.setSelectionRange(input.value.length, input.value.length); }
      };

      CCCombo.prototype.wireDrill = function () {
        var self = this;
        var back = this.panel.querySelector('.cc-combo2-drill-back');
        if (back) back.addEventListener('click', function () { self.renderList(''); });
        var input = this.panel.querySelector('.cc-combo2-drill-search-input');
        if (input) input.addEventListener('input', function () { self.drillBatch = 50; self.renderDrill(input.value); });
        var list = this.panel.querySelector('.cc-combo2-drill-list');
        if (list) {
          list.addEventListener('scroll', function () {
            if (list.scrollTop + list.clientHeight > list.scrollHeight - 60) {
              var before = self.drillBatch;
              self.drillBatch += 50;
              if (self.drillBatch !== before) {
                var q = input ? input.value : '';
                var scrollTop = list.scrollTop;
                self.renderDrill(q);
                var newList = self.panel.querySelector('.cc-combo2-drill-list');
                if (newList) newList.scrollTop = scrollTop;
              }
            }
          });
        }
        this.panel.querySelectorAll('.cc-combo2-item').forEach(function (el) {
          el.addEventListener('click', function () { self.select_(el.getAttribute('data-value')); });
        });
      };

      CCCombo.prototype.init = function () {
        var self = this;
        this.onDocMouseDownBound = function (e) { if (!self.root.contains(e.target)) self.close(); };
        this.onDocKeyDownBound = function (e) { if (e.key === 'Escape') { self.close(); self.btn.focus(); } };
        this.btn.addEventListener('click', function () {
          if (self.root.classList.contains('cc-combo2-open')) self.close(); else self.open();
        });
        this.btn.addEventListener('keydown', function (e) {
          if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); self.btn.click(); }
        });
        var inst = this.instance();
        if (inst) inst.on('change', function () { self.refreshLabel(); });
        this.refreshLabel();
      };

      function tryInit(root) {
        if (root.dataset.ccCombo2Init) return true;
        var select = root.querySelector('select');
        if (!select || !select.selectize) return false;
        root.dataset.ccCombo2Init = '1';
        new CCCombo(root).init();
        return true;
      }

      function scan() {
        document.querySelectorAll('.cc-combo2').forEach(function (root) { tryInit(root); });
      }

      scan();
      var tries = 0;
      var t = setInterval(function () {
        scan();
        if (document.querySelectorAll('.cc-combo2:not([data-cc-combo2-init])').length === 0 || ++tries > 80) {
          clearInterval(t);
        }
      }, 250);
    })();
  ")),

  # --- CCPanel: the Map tab's stacked left pane (functions.R::map_panel_ui()).
  # Section headers collapse their own body (.is-shut); the top chevron
  # collapses the whole panel to a thin strip (.is-collapsed).
  tags$script(HTML("
    (function () {
      function wire(p) {
        if (p.dataset.ccPanelInit) return;
        p.dataset.ccPanelInit = '1';
        p.querySelectorAll('.cc-sec-head').forEach(function (h) {
          function toggle() { h.parentNode.classList.toggle('is-shut'); }
          h.addEventListener('click', toggle);
          h.addEventListener('keydown', function (e) {
            if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggle(); }
          });
        });
        var tog = p.querySelector('.cc-panel-toggle');
        if (tog) tog.addEventListener('click', function () {
          p.classList.toggle('is-collapsed');
          setTimeout(function () {
            window.dispatchEvent(new Event('resize'));
          }, 200);
        });
        // the Summarize Within / Restrict-map selectize dropdowns render at
        // <body> level (dropdownParent), so they do NOT track the input when
        // the panel scrolls -- close any open one on scroll instead of letting
        // it float detached.
        var scroller = p.querySelector('.cc-panel-scroll');
        if (scroller) scroller.addEventListener('scroll', function () {
          p.querySelectorAll('select').forEach(function (s) {
            if (s.selectize && s.selectize.isOpen) s.selectize.close();
          });
        }, { passive: true });
      }
      var t = setInterval(function () {
        var p = document.querySelector('.cc-mappane .cc-panel');
        if (p) wire(p);
      }, 300);
      setTimeout(function () { clearInterval(t); }, 20000);
    })();
  ")),

  # --- CCFeedback: the feedback modal (functions.R::modal_feedback) -- capture
  # the map on open, mark it up (pen/arrow/box), post client-side to the Apps
  # Script endpoint or a prefilled GitHub issue. Vendored .js so the annotator
  # isn't fighting R string escaping.
  tags$script(src = "cc-feedback.js"),

  useConductor(),
  useBusyIndicators(spinners = TRUE, fade = TRUE)
  ),

  # Map ----
  nav_panel(
    "Map",
    # Map tab only: a stacked left pane (functions.R::map_panel_ui()) beside the
    # map, in a plain flex row (NOT bslib's layout_sidebar). Section headers
    # collapse their own body (CCPanel JS below). The other viz tabs keep their
    # small per-tab toolbars untouched.
    div(
      class = "cc-mappane html-fill-item html-fill-container",
      map_panel_ui(),
      # html-fill-item/container so the maplibre widget fills; position:relative
      # anchors the map's absolutely-positioned controls / title pills / legend.
      div(
        class = "cc-map-wrap html-fill-item html-fill-container",
        style = "position: relative;",
        # compact title pills floated inside the map (CSS .cc-map-title). The
        # species pill shows on every Map view; the environmental one only in
        # the side-by-side split, positioned over the right pane.
        # pointer-events:none (on .cc-map-titles) keeps map drag untouched.
        div(
          class = "cc-map-titles",
          div(class = "cc-map-title cc-map-title-sp",
              textOutput("map_title_sp", inline = TRUE)),
          conditionalPanel(
            "input.cmp_layout == 'split' && input.cmp_env_toggle",
            div(class = "cc-map-title cc-map-title-env",
                textOutput("map_title_env", inline = TRUE)))),
        maplibreCompareOutput("map", width = "100%", height = "100%"))) ),

  nav_panel(
    "Time Series",
    div(
      class = "cc-tab-toolbar",
      popover(
        tags$button(
          type = "button", class = "btn btn-outline-secondary btn-sm",
          bs_icon("sliders"), " Options"),
        selectInput(
          "sel_ts_res",
          "Temporal Resolution",
          choices  = ts_res_choices,
          selected = "year"),
        title = "Plot Options", placement = "bottom")),
    uiOutput("ts_content") ),

  nav_panel(
    "Scatterplot",
    div(
      class = "cc-tab-toolbar align-items-center",
      p(class = "small text-muted me-auto mb-0",
        "Click on a point or use the box/lasso tool to select points to see their location."),
      popover(
        tags$button(
          type = "button", class = "btn btn-outline-secondary btn-sm",
          bs_icon("sliders"), " Options"),
        numericInput("splot_max_hours_diff",
                     tagList("Time Window (Hrs.)",
                             popover(bs_icon("question-circle"),
                                     "The default thresholds of 2 km and 6 hours to match CTD casts and net tows for larval fish have been selected to represent approximate spatial and temporal scales within a single tidal cycle and over which water masses are expected to remain broadly similar (although this assumption may vary depending on proximity to coastal fronts or distance from shore). The 2 km threshold also falls within the typical range of potential station drift and/or delay between CTD casts and net tows during sampling operations. Additionally, all stations are spaced farther apart than 2km.")),
                     value = default_max_hours_diff,  min = 0, max = 72),
        numericInput("splot_max_meters_diff",
                     tagList("Distance Window (m)",
                             popover(bs_icon("question-circle"),
                                     "The default thresholds of 2 km and 6 hours to match CTD casts and net tows for larval fish have been selected to represent approximate spatial and temporal scales within a single tidal cycle and over which water masses are expected to remain broadly similar (although this assumption may vary depending on proximity to coastal fronts or distance from shore). The 2 km threshold also falls within the typical range of potential station drift and/or delay between CTD casts and net tows during sampling operations. Additionally, all stations are spaced farther apart than 2km.")),
                     value = default_max_meters_diff, min = 0, max = 5000),
        selectInput("splot_method",
                    tagList("Join Method",
                            popover(bs_icon("question-circle"),
                                    HTML("Specify how <strong>environmental observations</strong> within the time and distance windows should be joined to the species observations.<br>
                                          <strong>Single Nearest Cast:</strong> Averages observations in the nearest cast (in time or distance).<br>
                                          <strong>Average Within Range:</strong> Averages all observations within the chosen window.")) ),
                    c("Single nearest cast (by time)"      =  "nearest_time",
                      "Single nearest cast (by distance)"  =  "nearest_dist",
                      "Average within range"               =  "average"),
                    selected = "nearest_time"),
        title = "Plot Options", placement = "bottom")),
    uiOutput("splot_content") ),

  nav_panel(
    "Depth Profile",
    div(
      class = "cc-tab-toolbar",
      actionButton("open_transect_modal", "Draw Transect",
                   class = "btn-outline-secondary btn-sm", icon = icon("pencil"))),
    uiOutput("dprof_content") ),

  nav_spacer(),

  nav_item(
    # user feedback -> in-app modal (functions.R::modal_feedback, opened by
    # server observeEvent(input$open_feedback)). Same plain nav-link styling as
    # About, leading icon. Was a link out to a Google Form.
    actionLink(
      "open_feedback",
      tagList(bs_icon("chat-square-text"), "Feedback"),
      class = "nav-link cc-feedback-link") ),

  nav_item(
    # starts in the theme the request asks for (?theme= / cc_theme cookie)
    input_dark_mode(id = "dark_toggle", mode = calcofi4r::cc_theme(req)) ),

  nav_panel(
    "About",
    div(
      class = "cc-util-bar",
      actionLink("close_about",
                 tagList(bs_icon("x-lg"), "Close"),
                 class = "cc-util-close")),
    about_html )
)
