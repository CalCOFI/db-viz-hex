ui <- page_sidebar(
  window_title = "CalCOFI.io Integrated Database Application",
  title = tagList(
    span(
      a(
        img(src = "./logo_calcofi.svg", height="50px", .noWS = "after"),
        href = "https://calcofi.io"),
      "Integrated App")),

  tags$head(includeHTML("google-analytics.html")),

  useConductor(),
  useBusyIndicators(spinners = TRUE, fade = TRUE),

  # sidebar ----
  sidebar = sidebar(
    width = 300,

    # action buttons
    actionButton("sel_data", "Select Filters", width = "100%", class = "mb-2", icon = icon("filter")),

    conditionalPanel(
      "input.outputPanel === 'Scatterplot'",
      p("Click on a point or use the box/lasso tool to select points to see their location.",
        class = "small text-muted mt-2")),

    conditionalPanel(
      "input.outputPanel === 'Depth Profile'",
      actionButton("open_transect_modal", "Draw Transect",
                   width = "100%", class = "mt-2") ),

    accordion(
      accordion_panel(
        "Filter Summary",
        uiOutput("filter_summary") ),

      accordion_panel(
        "Summary Statistics",
        uiOutput("summary_statistics") ),

      accordion_panel(
        "Plot Options",

        conditionalPanel(
          "input.outputPanel === 'Map'",
          uiOutput("map_env_stat") ),

        conditionalPanel(
          "input.outputPanel === 'Time Series'",
          uiOutput("ts_res") ),

        conditionalPanel(
          "input.outputPanel === 'Scatterplot'",
          numericInput("splot_max_hours_diff", "Time Window (Hrs.)",   value = default_max_hours_diff,  min = 0, max = 72),
          numericInput("splot_max_meters_diff", "Distance Window (m)", value = default_max_meters_diff, min = 0, max = 5000),
          selectInput("splot_method",
                      tagList("Join Method",
                              tooltip(bs_icon("question-circle"),
                                      # Wrap the content in HTML() to enable HTML formatting
                                      HTML("Specify how <strong>environmental observations</strong> within the time and distance windows should be joined to the species observations.<br>
                                            <strong>Single Nearest Cast:</strong> Averages observations in the nearest cast (in time or distance).<br>
                                            <strong>Average Within Range:</strong> Averages all observations within the chosen window.")) ),
                      c("Single nearest cast (by time)"      =  "nearest_time",
                        "Single nearest cast (by distance)"  =  "nearest_dist",
                        "Average within range"               =  "average"),
                      selected = "nearest_time")))) ),

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

    nav_item(
      input_dark_mode(id = "dark_toggle", mode = "dark") ),

    nav_panel(
      "Download",
      "Select the datasets you'd like to download, then click \"Download.\"",
      checkboxGroupInput(
        "sel_raw_data_download",
        "Raw datasets",
        c(
          "Raw environmental data"    =  "raw_env",
          "Raw species data"          =  "raw_sp",
          "Integrated data (raw
           environmental and species
           combined)"                 =  "int"),
        width = "100%"),
      conditionalPanel(
        condition = "input.sel_raw_data_download.includes('int')",
        div(
          style = "margin-top: 8px; padding-left: 20px; border-left: 3px solid #337ab7;",
          div(
            div(
              style = "display: inline-block; margin-right: 20px;",
                numericInput(
                  "time_window",
                  "Time window (hours)",
                  value = default_max_hours_diff,
                  min   = 0,
                  width = 200) ),
            div(
              style = "display: inline-block;",
              numericInput(
                "dist_window",
                "Distance window (meters)",
                value = 1000,
                min   = 0,
                width = 200
              ) ) ),
          helpText("These windows define the tolerance for joining environmental and species data."),
        )
      ),
      checkboxGroupInput(
        "sel_proc_data_download",
        "Processed datasets (the aggregated data used for the visualizations)",
        c(
          "Map data"                  =  "map",
          "Time-series data"          =  "ts",
          "Scatterplot data"          =  "splot",
          "Depth Profile data"        =  "dprof"),
        width = "100%"),
        downloadButton("download_data", "Download", class = "btn-secondary mb-2") ),

    nav_panel(
      "About",
      HTML(mark(here("app/about.md"), output = NA)) ))
)
