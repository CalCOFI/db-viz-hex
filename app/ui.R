ui <- page_sidebar(
  title = "CalCOFI Integrated App",

  useConductor(),
  useBusyIndicators(spinners = TRUE, fade = TRUE),

  # sidebar ----
  sidebar = sidebar(
    width = 300,

    # action buttons
    actionButton("sel_data", "Select Filters", width = "100%", class = "mb-2", icon = icon("filter")),

    # conditional panels for tab-specific controls
    conditionalPanel(
      "input.outputPanel === 'Map'",
      uiOutput("map_env_stat") ),

    conditionalPanel(
      "input.outputPanel === 'Time Series'",
      uiOutput("ts_res") ),

    conditionalPanel(
      "input.outputPanel === 'Scatterplot'",
      p("Click on a point or use the box/lasso tool to select points to see their location.",
        class = "small text-muted mt-2") ),

    conditionalPanel(
      "input.outputPanel === 'Depth Profile'",
      actionButton("open_transect_modal", "Draw Transect",
                   width = "100%", class = "mt-2") ),

    # Filter summary accordion
    uiOutput("filter_summary") ),

  # main content ----
  navset_card_underline(
    id = "outputPanel",
    height = "100%",

    nav_panel(
      "Map",
      uiOutput("map_content") ),

    nav_panel(
      "Time Series",
      uiOutput("ts_content") ),

    nav_panel(
      "Scatterplot",
      uiOutput("splot_content") ),

    nav_panel(
      "Depth Profile",
      uiOutput("dprof_content") ),

    nav_spacer(),

    nav_panel(
      "Download",
      downloadButton("download_sp",  "Download Species Data",       class = "btn-secondary mb-2", style = "width: 100%;"),
      downloadButton("download_env", "Download Environmental Data", class = "btn-secondary mb-2", style = "width: 100%;") ),
    # TODO: Download Species - Env Data

    nav_panel(
      "About",
      HTML(mark(here("app/about.md"), output = NA)) ))
)
