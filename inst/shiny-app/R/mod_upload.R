# --- Upload Module ---

upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$link(rel = "stylesheet", href = "style.css"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::h3("Upload your serology data"),
        shiny::fileInput(ns("csv_file"), "Choose CSV file",
                         accept = ".csv",
                         placeholder = "No file selected"),
        shiny::p("OR"),
        shiny::actionButton(ns("load_demo"), "Load Demo Data",
                            icon = shiny::icon("flask"),
                            class = "btn-info"),
        shiny::hr(),
        shiny::h4("Required columns"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$code("age"), " \u2014 numeric (years)"),
          shiny::tags$li(shiny::tags$code("raw_titer"), " \u2014 numeric (HAI titer: 5, 10, 20, 40, ...)")
        ),
        shiny::h4("Optional columns"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$code("time"), " \u2014 numeric (enables time series analysis)"),
          shiny::tags$li(shiny::tags$code("epi"), " \u2014 epidemic ID (enables baseline comparison)"),
          shiny::tags$li(shiny::tags$code("baseline"), " \u2014 'yes'/'no' (for baseline filtering)")
        )
      ),
      shiny::column(6,
        shiny::uiOutput(ns("demo_banner")),
        shiny::uiOutput(ns("column_status")),
        shiny::uiOutput(ns("data_summary")),
        shiny::uiOutput(ns("column_mapping"))
      )
    ),
    shiny::hr(),
    shiny::h4("Data preview"),
    DT::dataTableOutput(ns("data_preview")),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(12, align = "right",
        shiny::actionButton(ns("next_btn"), "Next: Configure Parameters",
                            icon = shiny::icon("arrow-right"),
                            class = "btn-primary btn-lg")
      )
    )
  )
}

upload_server <- function(id, rv, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load CSV file
    shiny::observeEvent(input$csv_file, {
      req(input$csv_file)
      tryCatch({
        df <- utils::read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
        rv$raw_data  <- df
        rv$demo_mode <- FALSE
      }, error = function(e) {
        shiny::showNotification(paste("Error reading CSV:", e$message),
                                type = "error")
      })
    })

    # Load demo data
    shiny::observeEvent(input$load_demo, {
      demo_path <- system.file("shiny-app", "data", "demo_data.csv",
                               package = "ImmuPop")
      if (demo_path == "") {
        demo_path <- file.path(getwd(), "data", "demo_data.csv")
      }
      if (file.exists(demo_path)) {
        rv$raw_data  <- utils::read.csv(demo_path, stringsAsFactors = FALSE)
        rv$demo_mode <- TRUE
      } else {
        # Fallback: use bundled R data
        e <- new.env()
        data("ImmuPop_raw_data", package = "ImmuPop", envir = e)
        rv$raw_data  <- e$ImmuPop_raw_data
        rv$demo_mode <- TRUE
      }
    })

    # Demo banner
    output$demo_banner <- shiny::renderUI({
      if (isTRUE(rv$demo_mode)) {
        shiny::div(class = "alert alert-warning",
          shiny::icon("info-circle"),
          " Demo mode: using bundled example data. Upload your own CSV to analyze real data."
        )
      }
    })

    # Detect columns
    col_info <- shiny::reactive({
      req(rv$raw_data)
      df <- rv$raw_data
      cols <- tolower(names(df))
      list(
        has_age       = "age" %in% cols,
        has_titer     = "raw_titer" %in% cols,
        has_time      = "time" %in% cols,
        has_epi       = "epi" %in% cols,
        has_baseline  = "baseline" %in% cols,
        col_names     = names(df)
      )
    })

    # Update rv flags
    shiny::observe({
      info <- col_info()
      rv$has_time     <- info$has_time
      rv$has_epi      <- info$has_epi
      rv$has_baseline <- info$has_baseline
    })

    # Column status display
    output$column_status <- shiny::renderUI({
      req(col_info())
      info <- col_info()
      make_badge <- function(found, name, required = FALSE) {
        if (found) {
          shiny::span(class = "badge bg-success",
                      shiny::icon("check"), name)
        } else if (required) {
          shiny::span(class = "badge bg-danger",
                      shiny::icon("times"), paste(name, "(missing!)"))
        } else {
          shiny::span(class = "badge bg-secondary",
                      shiny::icon("minus"), paste(name, "(not found)"))
        }
      }
      shiny::tagList(
        shiny::h4("Column detection"),
        shiny::div(
          make_badge(info$has_age, "age", required = TRUE), " ",
          make_badge(info$has_titer, "raw_titer", required = TRUE), " ",
          make_badge(info$has_time, "time"), " ",
          make_badge(info$has_epi, "epi"), " ",
          make_badge(info$has_baseline, "baseline")
        )
      )
    })

    # Column mapping (when standard names not found)
    output$column_mapping <- shiny::renderUI({
      req(col_info())
      info <- col_info()
      if (info$has_age && info$has_titer) return(NULL)

      choices <- c("(not mapped)" = "", info$col_names)
      shiny::tagList(
        shiny::h4("Column mapping"),
        shiny::p("Some required columns were not found. Please map them:"),
        if (!info$has_age) {
          shiny::selectInput(ns("map_age"), "Which column contains age?",
                             choices = choices)
        },
        if (!info$has_titer) {
          shiny::selectInput(ns("map_titer"), "Which column contains raw titer?",
                             choices = choices)
        },
        shiny::actionButton(ns("apply_mapping"), "Apply Mapping",
                            class = "btn-sm btn-warning")
      )
    })

    # Apply column mapping
    shiny::observeEvent(input$apply_mapping, {
      req(rv$raw_data)
      df <- rv$raw_data
      if (!is.null(input$map_age) && input$map_age != "") {
        names(df)[names(df) == input$map_age] <- "age"
      }
      if (!is.null(input$map_titer) && input$map_titer != "") {
        names(df)[names(df) == input$map_titer] <- "raw_titer"
      }
      rv$raw_data <- df
      shiny::showNotification("Column mapping applied.", type = "message")
    })

    # Data summary
    output$data_summary <- shiny::renderUI({
      req(rv$raw_data)
      df <- rv$raw_data
      n <- nrow(df)
      age_range <- if ("age" %in% names(df)) {
        paste(range(df$age, na.rm = TRUE), collapse = " - ")
      } else "N/A"
      titer_range <- if ("raw_titer" %in% names(df)) {
        paste(range(df$raw_titer, na.rm = TRUE), collapse = " - ")
      } else "N/A"
      n_time <- if ("time" %in% names(df)) length(unique(df$time)) else 0
      n_epi  <- if ("epi" %in% names(df)) length(unique(df$epi)) else 0

      shiny::tagList(
        shiny::h4("Data summary"),
        shiny::tags$table(class = "table table-sm",
          shiny::tags$tr(shiny::tags$td("Rows:"), shiny::tags$td(shiny::strong(n))),
          shiny::tags$tr(shiny::tags$td("Age range:"), shiny::tags$td(age_range)),
          shiny::tags$tr(shiny::tags$td("Titer range:"), shiny::tags$td(titer_range)),
          if (n_time > 0) shiny::tags$tr(shiny::tags$td("Time points:"), shiny::tags$td(n_time)),
          if (n_epi > 0) shiny::tags$tr(shiny::tags$td("Epidemics:"), shiny::tags$td(n_epi))
        )
      )
    })

    # Data preview table
    output$data_preview <- DT::renderDataTable({
      req(rv$raw_data)
      DT::datatable(utils::head(rv$raw_data, 50),
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # Next button
    shiny::observeEvent(input$next_btn, {
      info <- col_info()
      if (!info$has_age || !info$has_titer) {
        shiny::showNotification(
          "Please ensure 'age' and 'raw_titer' columns are present (use column mapping if needed).",
          type = "error")
        return()
      }
      shiny::updateNavbarPage(parent_session, "wizard", selected = "tab_config")
    })
  })
}
