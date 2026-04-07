# ==============================================================================
# Sativan - Satellite Data Analysis Dashboard
# A modular Shiny application for satellite imagery workflows:
#   discovery, preprocessing, analysis, classification, and visualization
#
# Supports all major satellite data types via Microsoft Planetary Computer:
#   Optical (Landsat, Sentinel-2, MODIS, NAIP, ASTER, HLS)
#   SAR (Sentinel-1 GRD/RTC)
#   DEM/Elevation (Copernicus, ALOS, NASADEM, 3DEP)
#   Land Cover (ESA WorldCover, IO LULC, USDA CDL, JRC GSW)
#   And more (MTBS, gNATSGO, HREA, Chloris Biomass, etc.)
# ==============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(leafpm)
library(terra)
library(sf)
library(DT)
library(rstac)
library(caret)
library(randomForest)
library(raster)
library(jsonlite)
library(shinycssloaders)

# Source modules
source("R/utils.R")
source("R/mod_aoi.R")
source("R/mod_search.R")
source("R/mod_preprocess.R")
source("R/mod_indices.R")
source("R/mod_classify.R")
source("R/mod_visualize.R")
source("R/mod_export.R")
source("R/mod_changedetect.R")
source("R/mod_cartography.R")
source("R/mod_documentation.R")

# ==============================================================================
# Home tab UI (defined before ui object so it's available at call time)
# ==============================================================================
home_ui <- function() {

  home_css <- "
  /* ---- Hero banner ---- */
  .sativan-hero {
    background: linear-gradient(135deg, #0b3d91 0%, #1565c0 50%, #1e88e5 100%);
    color: #ffffff;
    border-radius: 12px;
    padding: 1.1rem 1.8rem 0.8rem;
    margin-bottom: 0.5rem;
    position: relative;
    overflow: hidden;
    box-shadow: 0 4px 24px rgba(11, 61, 145, 0.25);
  }
  .sativan-hero::before {
    content: '';
    position: absolute; top: -40%; right: -15%;
    width: 420px; height: 420px;
    background: radial-gradient(circle, rgba(255,193,7,0.18) 0%, transparent 70%);
    border-radius: 50%;
    pointer-events: none;
  }
  .sativan-hero .hero-title {
    font-size: 1.5rem; font-weight: 700; margin-bottom: 0.2rem;
    text-shadow: 0 2px 8px rgba(0,0,0,0.18);
  }
  .sativan-hero .hero-title .accent { color: #ffd54f; }
  .sativan-hero .hero-subtitle {
    font-size: 0.92rem; opacity: 0.92; line-height: 1.4; max-width: 700px;
    margin-bottom: 0;
  }
  .sativan-hero .hero-badge {
    display: inline-block;
    background: rgba(255,213,79,0.22);
    border: 1px solid rgba(255,213,79,0.45);
    color: #fff8e1; border-radius: 20px;
    padding: 2px 12px; font-size: 0.75rem; font-weight: 600;
    margin-bottom: 0.3rem;
  }

  /* ---- Stat value boxes ---- */
  .sativan-stat-row { margin-bottom: 0.4rem; }
  .sativan-stat {
    background: #ffffff; border-radius: 10px;
    border-left: 4px solid #1565c0;
    padding: 0.5rem 0.8rem;
    box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    text-align: center;
    transition: transform 0.15s, box-shadow 0.15s;
  }
  .sativan-stat:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 18px rgba(0,0,0,0.10);
  }
  .sativan-stat .stat-value {
    font-size: 1.35rem; font-weight: 800; color: #0b3d91;
  }
  .sativan-stat .stat-label {
    font-size: 0.78rem; color: #5a6a7a; font-weight: 600;
    text-transform: uppercase; letter-spacing: 0.5px;
  }
  .sativan-stat.gold  { border-left-color: #f9a825; }
  .sativan-stat.gold .stat-value { color: #e65100; }
  .sativan-stat.teal  { border-left-color: #00897b; }
  .sativan-stat.teal .stat-value { color: #00695c; }

  /* ---- Workflow steps ---- */
  .sativan-workflow { padding: 0; list-style: none; counter-reset: step; }
  .sativan-workflow li {
    display: flex; align-items: flex-start; gap: 10px;
    padding: 6px 0; border-bottom: 1px solid #edf2f7;
    font-size: 0.85rem;
  }
  .sativan-workflow li:last-child { border-bottom: none; }
  .sativan-workflow .step-num {
    flex-shrink: 0; width: 26px; height: 26px;
    background: linear-gradient(135deg, #1565c0, #1e88e5);
    color: #ffffff; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    font-size: 0.7rem; font-weight: 700;
    box-shadow: 0 2px 6px rgba(21,101,192,0.25);
  }
  .sativan-workflow .step-label { font-weight: 700; color: #0b3d91; }
  .sativan-workflow .step-desc { color: #546e7a; }

  /* ---- Quick-start card ---- */
  .sativan-quickstart {
    background: linear-gradient(135deg, #fff8e1 0%, #fffde7 100%);
    border: 1px solid #ffe082; border-radius: 10px;
  }
  .sativan-quickstart .card-header {
    background: #ffd54f !important;
    color: #4e342e !important; font-weight: 700;
    border-bottom: 1px solid #ffca28;
  }
  .btn-sativan-go {
    background: linear-gradient(135deg, #f9a825, #fbc02d);
    color: #3e2723 !important; border: none;
    font-weight: 700; font-size: 1rem;
    padding: 10px 0; border-radius: 8px;
    box-shadow: 0 3px 10px rgba(249,168,37,0.35);
    transition: transform 0.15s, box-shadow 0.15s;
  }
  .btn-sativan-go:hover {
    transform: translateY(-1px);
    box-shadow: 0 6px 18px rgba(249,168,37,0.45);
    background: linear-gradient(135deg, #f57f17, #f9a825);
    color: #ffffff !important;
  }

  /* ---- Data categories ---- */
  .sativan-categories .cat-item {
    display: flex; justify-content: space-between; align-items: center;
    padding: 6px 14px; border-bottom: 1px solid #eceff1;
    font-size: 0.83rem; color: #37474f;
    transition: background 0.12s;
  }
  .sativan-categories .cat-item:hover { background: #e3f2fd; }
  .sativan-categories .cat-item:last-child { border-bottom: none; }
  .sativan-categories .cat-badge {
    background: #1565c0; color: #fff;
    border-radius: 12px; padding: 2px 10px;
    font-size: 0.75rem; font-weight: 700;
    min-width: 28px; text-align: center;
  }
  .sativan-categories .cat-icon {
    color: #1e88e5; margin-right: 8px;
  }

  /* ---- Developer info bar ---- */
  .sativan-dev-bar {
    background: linear-gradient(90deg, #e3f2fd, #f1f8ff);
    border: 1px solid #bbdefb; border-radius: 8px;
    padding: 12px 18px; font-size: 0.82rem;
    color: #37474f;
  }
  .sativan-dev-bar a { color: #1565c0; font-weight: 600; }
  .sativan-dev-bar strong { color: #0b3d91; }
  "

  tagList(
    tags$style(HTML(home_css)),

    # ---- Hero Banner ----
    tags$div(class = "sativan-hero",
      tags$span(class = "hero-badge",
        bsicons::bs_icon("radar", size = "0.85em"), " Satellite Intelligence Platform"
      ),
      tags$div(class = "hero-title",
        bsicons::bs_icon("globe-americas", size = "1em"),
        " Welcome to ", tags$span(class = "accent", "Sativan")
      ),
      tags$p(class = "hero-subtitle",
        "A comprehensive satellite data analysis dashboard powered by ",
        tags$strong("Microsoft Planetary Computer"), ". ",
        "Search, preprocess, analyse, classify, and export satellite
         imagery \u2014 all from your browser."
      )
    ),

    # ---- Stat highlights ----
    layout_column_wrap(
      class = "sativan-stat-row",
      width = 1/4, fill = FALSE,
      tags$div(class = "sativan-stat",
        tags$div(class = "stat-value", "34"),
        tags$div(class = "stat-label", "Collections")
      ),
      tags$div(class = "sativan-stat gold",
        tags$div(class = "stat-value", "24"),
        tags$div(class = "stat-label", "Spectral Indices")
      ),
      tags$div(class = "sativan-stat teal",
        tags$div(class = "stat-value", "4"),
        tags$div(class = "stat-label", "ML Algorithms")
      ),
      tags$div(class = "sativan-stat",
        tags$div(class = "stat-value", "30+"),
        tags$div(class = "stat-label", "Export Formats")
      )
    ),

    # ---- Main content: Workflow | Data Categories | Quick Start ----
    layout_columns(
      col_widths = c(4, 4, 4),

      # Workflow Pipeline
      card(
        card_header(
          class = "bg-white border-bottom",
          tags$h6(class = "mb-0 fw-bold",
            style = "color: #0b3d91;",
            bsicons::bs_icon("signpost-split"), " Workflow Pipeline"
          )
        ),
        card_body(
          tags$ul(class = "sativan-workflow",
            tags$li(
              tags$span(class = "step-num", "1"),
              tags$div(tags$span(class = "step-label", "AOI"),
                       tags$span(class = "step-desc",
                         " \u2014 Define your area of interest"))
            ),
            tags$li(
              tags$span(class = "step-num", "2"),
              tags$div(tags$span(class = "step-label", "Search"),
                       tags$span(class = "step-desc",
                         " \u2014 Find imagery from 30+ collections"))
            ),
            tags$li(
              tags$span(class = "step-num", "3"),
              tags$div(tags$span(class = "step-label", "Preprocess"),
                       tags$span(class = "step-desc",
                         " \u2014 Load bands, mask clouds, clip to AOI"))
            ),
            tags$li(
              tags$span(class = "step-num", "4"),
              tags$div(tags$span(class = "step-label", "Indices"),
                       tags$span(class = "step-desc",
                         " \u2014 Compute 24 spectral indices"))
            ),
            tags$li(
              tags$span(class = "step-num", "5"),
              tags$div(tags$span(class = "step-label", "Classify"),
                       tags$span(class = "step-desc",
                         " \u2014 Supervised ML classification"))
            ),
            tags$li(
              tags$span(class = "step-num", "6"),
              tags$div(tags$span(class = "step-label", "Visualize"),
                       tags$span(class = "step-desc",
                         " \u2014 Interactive map exploration"))
            ),
            tags$li(
              tags$span(class = "step-num", "7"),
              tags$div(tags$span(class = "step-label", "Map"),
                       tags$span(class = "step-desc",
                         " \u2014 Compose publication-ready maps"))
            ),
            tags$li(
              tags$span(class = "step-num", "8"),
              tags$div(tags$span(class = "step-label", "Export"),
                       tags$span(class = "step-desc",
                         " \u2014 Download GeoTIFF, vectors, reports"))
            )
          )
        )
      ),

      # Data Categories
      card(
        card_header(
          class = "bg-white border-bottom",
          tags$h6(class = "mb-0 fw-bold",
            style = "color: #0b3d91;",
            bsicons::bs_icon("database"), " Data Categories"
          )
        ),
        card_body(
          class = "p-0",
          tags$div(class = "sativan-categories",
            tags$div(class = "cat-item",
              tags$span(
                bsicons::bs_icon("brightness-high", class = "cat-icon"),
                "Optical / Multispectral"),
              tags$span(class = "cat-badge", "14")
            ),
            tags$div(class = "cat-item",
              tags$span(
                bsicons::bs_icon("broadcast", class = "cat-icon"),
                "SAR"),
              tags$span(class = "cat-badge", "2")
            ),
            tags$div(class = "cat-item",
              tags$span(
                bsicons::bs_icon("triangle", class = "cat-icon"),
                "DEM / Elevation"),
              tags$span(class = "cat-badge", "7")
            ),
            tags$div(class = "cat-item",
              tags$span(
                bsicons::bs_icon("tree-fill", class = "cat-icon"),
                "Land Cover"),
              tags$span(class = "cat-badge", "7")
            ),
            tags$div(class = "cat-item",
              tags$span(
                bsicons::bs_icon("grid-3x3-gap", class = "cat-icon"),
                "Other Thematic"),
              tags$span(class = "cat-badge", "4")
            )
          )
        )
      ),

      # Quick Start
      card(
        class = "sativan-quickstart",
        card_header(
          bsicons::bs_icon("rocket-takeoff"), " Quick Start"
        ),
        card_body(
          class = "small",
          tags$p(bsicons::bs_icon("1-circle-fill",
            class = "me-1", style = "color:#1565c0;"),
            "Go to ", tags$strong("AOI"),
            " and draw or upload your area of interest."),
          tags$p(bsicons::bs_icon("2-circle-fill",
            class = "me-1", style = "color:#1565c0;"),
            "Go to ", tags$strong("Search"),
            " and find satellite imagery."),
          tags$p(bsicons::bs_icon("3-circle-fill",
            class = "me-1", style = "color:#1565c0;"),
            "Select a scene and move to ",
            tags$strong("Processing > Preprocess"), "."),
          tags$p(class = "mb-3 mt-2",
            style = "font-weight:600; color:#4e342e;",
            bsicons::bs_icon("stars", style = "color:#f9a825;"),
            " Start exploring your data!"
          ),
          actionButton("go_aoi", "Get Started",
                       class = "btn-sativan-go w-100",
                       icon = icon("arrow-right"))
        )
      )
    ),

    # ---- Developer attribution bar ----
    tags$div(class = "sativan-dev-bar mt-2",
      bsicons::bs_icon("person-badge", style = "color:#1565c0;"), " ",
      "Developed by ", tags$strong("Ivan Innocent Sekibenga"),
      " \u2014 Department of Mathematics and Statistics, Kyambogo University",
      tags$span(class = "mx-2", "|"),
      bsicons::bs_icon("envelope", style = "color:#1565c0;"), " ",
      tags$a(href = "mailto:isekibenga@kyu.ac.ug", "isekibenga@kyu.ac.ug"),
      tags$span(class = "mx-2", "|"),
      bsicons::bs_icon("telephone", style = "color:#1565c0;"),
      " +256 752 651 098"
    )
  )
}

# ==============================================================================
# Settings tab UI
# ==============================================================================
settings_ui <- function() {
  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Session Information"),
      card_body(
        verbatimTextOutput("session_info")
      )
    ),

    card(
      card_header("Package Versions"),
      card_body(
        DT::dataTableOutput("pkg_versions")
      )
    ),

    card(
      card_header("About Sativan"),
      card_body(
        tags$p(
          "Sativan is an open-source satellite data analysis dashboard
           built with R, Shiny, and bslib."
        ),
        tags$dl(
          tags$dt("STAC Endpoint"),
          tags$dd(tags$code(PC_STAC_URL)),
          tags$dt("Collections Registry"),
          tags$dd(paste(length(.PC_COLLECTIONS), "curated collections")),
          tags$dt("Spectral Indices"),
          tags$dd(paste(length(get_index_bands()), "indices"))
        )
      )
    ),

    card(
      card_header("Actions"),
      card_body(
        actionButton("load_session", "Load Previous Session",
                     class = "btn-outline-primary w-100 mb-3",
                     icon = icon("folder-open")),
        tags$small(class = "text-muted d-block mb-3",
          "Reload saved training and validation points from GeoJSON files."
        ),
        actionButton("clear_state", "Clear All Data",
                     class = "btn-outline-danger w-100 mb-2",
                     icon = icon("trash")),
        tags$small(class = "text-muted",
          "Resets all loaded rasters, indices, and classification results.")
      )
    )
  )
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
    bsicons::bs_icon("globe-americas", size = "1.2em", class = "logo-icon"),
    "Sativan"
  ),
  id = "main_nav",
  window_title = "Sativan - Satellite Data Analysis",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1a5276",
    success = "#27ae60",
    info = "#2980b9",
    "navbar-bg" = "#1a5276",
    base_font = font_google("Inter"),
    code_font = font_google("Fira Code"),
    font_scale = 0.92
  ),

  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .sativan-status-bar {
        background: #f8f9fa; border-bottom: 1px solid #dee2e6;
        padding: 2px 16px; font-size: 0.78rem; color: #6c757d;
        display: flex; gap: 18px; align-items: center;
      }
      .sativan-status-bar .badge { font-size: 0.72rem; }
    "))
  ),
  footer = tags$div(
    class = "sativan-status-bar",
    bsicons::bs_icon("stack", class = "me-1"),
    textOutput("status_scene", inline = TRUE),
    tags$span(class = "text-muted", "|"),
    bsicons::bs_icon("grid-3x3", class = "me-1"),
    textOutput("status_raster", inline = TRUE),
    tags$span(class = "text-muted", "|"),
    bsicons::bs_icon("calculator", class = "me-1"),
    textOutput("status_indices", inline = TRUE)
  ),

  # --- Home ---
  nav_panel(
    title = "Home",
    icon = bsicons::bs_icon("house"),
    home_ui()
  ),

  # --- AOI (top-level: workflow entry point) ---
  nav_panel(
    title = "AOI",
    icon = bsicons::bs_icon("geo-alt"),
    aoi_ui("aoi")
  ),

  # --- Search (top-level: second workflow step) ---
  nav_panel(
    title = "Search",
    icon = bsicons::bs_icon("search"),
    search_ui("search")
  ),

  # --- Processing ---
  nav_menu(
    title = "Processing",
    icon = bsicons::bs_icon("sliders"),

    nav_panel(
      title = "Preprocess",
      icon = bsicons::bs_icon("sliders"),
      preprocess_ui("preprocess")
    ),

    nav_panel(
      title = "Indices",
      icon = bsicons::bs_icon("calculator"),
      indices_ui("indices")
    )
  ),

  # --- Analysis ---
  nav_menu(
    title = "Analysis",
    icon = bsicons::bs_icon("bar-chart"),

    nav_panel(
      title = "Classify",
      icon = bsicons::bs_icon("diagram-3"),
      classify_ui("classify")
    ),

    nav_panel(
      title = "Change Detection",
      icon = bsicons::bs_icon("arrow-left-right"),
      changedetect_ui("changedetect")
    )
  ),

  # --- Output ---
  nav_menu(
    title = "Output",
    icon = bsicons::bs_icon("easel"),

    nav_panel(
      title = "Visualize",
      icon = bsicons::bs_icon("map"),
      visualize_ui("visualize")
    ),

    nav_panel(
      title = "Map",
      icon = bsicons::bs_icon("easel2"),
      cartography_ui("cartography")
    ),

    nav_panel(
      title = "Export",
      icon = bsicons::bs_icon("download"),
      export_ui("export")
    )
  ),

  # --- Documentation ---
  nav_panel(
    title = "Docs",
    icon = bsicons::bs_icon("book"),
    documentation_ui("docs")
  ),

  nav_spacer(),

  nav_panel(
    title = "Settings",
    icon = bsicons::bs_icon("gear"),
    settings_ui()
  ),

  nav_item(
    tags$a(
      href = "https://github.com",
      target = "_blank",
      class = "nav-link",
      bsicons::bs_icon("github"),
      title = "Source Code"
    )
  ),

  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

# ==============================================================================
# Server
# ==============================================================================
server <- function(input, output, session) {

  # Auto-match ggplot2 plots to app theme (skip silently if thematic not installed)
  tryCatch(thematic::thematic_shiny(), error = function(e) NULL)

  # Shared application state across modules
  app_state <- reactiveValues(
    selected_scene = NULL,
    collection_id = NULL,
    data_category = NULL,
    sensor_type = NULL,
    scene_id = NULL,
    scene_date = NULL,
    processed_raster = NULL,
    band_mapping = NULL,
    computed_indices = NULL,
    classified_raster = NULL,
    classification_model = NULL,
    training_data = NULL,
    rstack = NULL,
    raster_t1 = NULL,
    raster_t2 = NULL,
    classified_t1 = NULL,
    classified_t2 = NULL,
    date_t1 = NULL,
    date_t2 = NULL,
    change_result = NULL,
    transition_matrix = NULL,
    validation_data = NULL,
    validation_accuracy = NULL,
    class_colors = NULL,
    session_load_trigger = 0L
  )

  # --- Module servers ---
  aoi_reactive <- aoi_server("aoi", app_state)

  search_results <- search_server("search", aoi_reactive, app_state)

  processed_raster <- preprocess_server("preprocess", aoi_reactive, app_state)

  computed_indices <- indices_server(
    "indices", aoi_reactive, processed_raster, app_state
  )

  classification <- classify_server(
    "classify", aoi_reactive, processed_raster, app_state
  )

  changedetect_server("changedetect", aoi_reactive, processed_raster, app_state)

  visualize_server("visualize", aoi_reactive, processed_raster, app_state)

  cartography_server("cartography", aoi_reactive, processed_raster, app_state)

  export_server("export", aoi_reactive, processed_raster, app_state)

  documentation_server("docs")

  # --- Home tab "Get Started" button ---
  observeEvent(input$go_aoi, {
    nav_select("main_nav", "AOI")
  })

  # --- Status bar outputs ---
  output$status_scene <- renderText({
    sid <- app_state$scene_id
    if (is.null(sid)) "No scene loaded" else paste0("Scene: ", sid)
  })

  output$status_raster <- renderText({
    r <- app_state$processed_raster
    if (is.null(r)) {
      "No raster"
    } else {
      crs_name <- tryCatch(terra::crs(r, describe = TRUE)$code, error = function(e) "?")
      paste0(nrow(r), "\u00d7", ncol(r), "\u00d7", terra::nlyr(r),
             " | ", paste(round(terra::res(r), 1), collapse = "\u00d7"), "m",
             " | EPSG:", crs_name)
    }
  })

  output$status_indices <- renderText({
    idx <- app_state$computed_indices
    n <- if (!is.null(idx)) length(idx) else 0
    paste0("Indices: ", n)
  })

  # --- Settings tab outputs ---
  output$session_info <- renderPrint({
    cat("R version:", R.version.string, "\n")
    cat("Platform: ", R.version$platform, "\n")
    cat("OS:       ", Sys.info()["sysname"], Sys.info()["release"], "\n")
    cat("Time:     ", format(Sys.time()), "\n")
    cat("Locale:   ", Sys.getlocale("LC_COLLATE"), "\n")
  })

  output$pkg_versions <- DT::renderDataTable({
    pkgs <- c("shiny", "bslib", "leaflet", "terra", "sf", "rstac",
              "caret", "randomForest", "raster", "DT", "jsonlite")
    versions <- vapply(pkgs, \(p) as.character(packageVersion(p)), character(1))
    DT::datatable(
      data.frame(Package = pkgs, Version = versions),
      options = list(dom = "t", paging = FALSE),
      rownames = FALSE, selection = "none"
    )
  })

  observeEvent(input$clear_state, {
    app_state$selected_scene <- NULL
    app_state$collection_id <- NULL
    app_state$data_category <- NULL
    app_state$sensor_type <- NULL
    app_state$scene_id <- NULL
    app_state$scene_date <- NULL
    app_state$processed_raster <- NULL
    app_state$band_mapping <- NULL
    app_state$computed_indices <- NULL
    app_state$classified_raster <- NULL
    app_state$classification_model <- NULL
    app_state$training_data <- NULL
    app_state$rstack <- NULL
    app_state$raster_t1 <- NULL
    app_state$raster_t2 <- NULL
    app_state$classified_t1 <- NULL
    app_state$classified_t2 <- NULL
    app_state$date_t1 <- NULL
    app_state$date_t2 <- NULL
    app_state$change_result <- NULL
    app_state$transition_matrix <- NULL
    app_state$validation_data <- NULL
    app_state$validation_accuracy <- NULL
    app_state$class_colors <- NULL
    showNotification("All data cleared.", type = "message")
  })

  # --- Load Previous Session modal ---
  observeEvent(input$load_session, {
    showModal(modalDialog(
      title = "Load Previous Session",
      size = "m",
      easyClose = TRUE,

      tags$p(class = "small text-muted",
        "Upload previously exported GeoJSON files to restore training",
        "and/or validation points. Both fields are optional."
      ),

      fileInput("load_training_file", "Training Points (GeoJSON)",
                accept = c(".geojson", ".json", ".gpkg")),
      fileInput("load_validation_file", "Validation Points (GeoJSON)",
                accept = c(".geojson", ".json", ".gpkg")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("do_load_session", "Load",
                     class = "btn-primary", icon = icon("upload"))
      )
    ))
  })

  observeEvent(input$do_load_session, {
    loaded <- character(0)

    # --- Training points ---
    tf <- input$load_training_file
    if (!is.null(tf)) {
      tryCatch({
        td <- sf::st_read(tf$datapath, quiet = TRUE)
        td <- sf::st_transform(td, 4326)
        app_state$training_data <- td
        loaded <- c(loaded, paste0("Training: ", nrow(td), " points"))
      }, error = function(e) {
        showNotification(paste("Training file error:", e$message),
                         type = "error")
      })
    }

    # --- Validation points ---
    vf <- input$load_validation_file
    if (!is.null(vf)) {
      tryCatch({
        vd <- sf::st_read(vf$datapath, quiet = TRUE)
        vd <- sf::st_transform(vd, 4326)
        app_state$validation_data <- vd
        loaded <- c(loaded, paste0("Validation: ", nrow(vd), " points"))
      }, error = function(e) {
        showNotification(paste("Validation file error:", e$message),
                         type = "error")
      })
    }

    if (length(loaded) > 0) {
      # Trigger classify module to sync
      app_state$session_load_trigger <- isolate(app_state$session_load_trigger) + 1L
      removeModal()
      showNotification(
        paste("Session loaded:", paste(loaded, collapse = "; ")),
        type = "message", duration = 6
      )
    } else {
      showNotification("No files selected.", type = "warning")
    }
  })

  # --- Global session bookkeeping ---
  session$onSessionEnded(function() {
    terra::tmpFiles(remove = TRUE)
    # Clean up any .tif tempfiles created for leaflet display
    tifs <- list.files(tempdir(), pattern = "\\.tif$", full.names = TRUE)
    unlink(tifs)
  })
}

# ==============================================================================
# Launch
# ==============================================================================
shinyApp(ui = ui, server = server)
