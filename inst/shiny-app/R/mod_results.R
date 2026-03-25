# --- Results Module ---

results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Run analysis"),

    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(4,
          shiny::radioButtons(ns("analysis_type"), "Analysis type",
            choices = c("Single Time Point" = "timepoint",
                        "Baseline Comparison" = "baseline",
                        "Time Series" = "timeseries"),
            selected = "timepoint"
          )
        ),
        shiny::column(4,
          shiny::uiOutput(ns("time_selector"))
        ),
        shiny::column(4,
          shiny::br(),
          shiny::actionButton(ns("run_btn"), "Run Analysis",
                              icon = shiny::icon("play"),
                              class = "btn-success btn-lg")
        )
      ),
      shiny::uiOutput(ns("analysis_notes"))
    ),

    # Results section (hidden until results exist)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_results"), "']"),

      shiny::hr(),
      shiny::h3("Results"),

      shiny::tabsetPanel(
        id = ns("result_tabs"),

        shiny::tabPanel("Summary Table",
          shiny::br(),
          DT::dataTableOutput(ns("summary_table"))
        ),

        shiny::tabPanel("Estimates Plot",
          shiny::br(),
          shiny::plotOutput(ns("plot_estimates"), height = "500px")
        ),

        shiny::tabPanel("Titer Distribution",
          shiny::br(),
          shiny::plotOutput(ns("plot_titer_dist"), height = "400px")
        ),

        shiny::tabPanel("Titer Jitter",
          shiny::br(),
          shiny::plotOutput(ns("plot_titer_jitter"), height = "400px")
        )
      ),

      # Downloads
      shiny::hr(),
      shiny::h4("Download results"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::downloadButton(ns("dl_csv"), "Results CSV",
                                class = "btn-outline-primary")
        ),
        shiny::column(4,
          shiny::downloadButton(ns("dl_table_csv"), "Summary Table CSV",
                                class = "btn-outline-primary")
        ),
        shiny::column(4,
          shiny::downloadButton(ns("dl_pdf"), "All Plots PDF",
                                class = "btn-outline-primary")
        )
      )
    )
  )
}

results_server <- function(id, rv, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update available analysis types based on detected columns
    shiny::observe({
      choices <- c("Single Time Point" = "timepoint")
      if (isTRUE(rv$has_epi)) {
        choices <- c(choices, "Baseline Comparison" = "baseline")
      }
      if (isTRUE(rv$has_time)) {
        choices <- c(choices, "Time Series" = "timeseries")
      }
      shiny::updateRadioButtons(session, "analysis_type",
                                choices = choices, selected = "timepoint")
    })

    # Time point selector (for single timepoint mode)
    output$time_selector <- shiny::renderUI({
      if (!identical(input$analysis_type, "timepoint")) return(NULL)
      req(rv$processed_data)
      df <- rv$processed_data
      if ("time" %in% names(df)) {
        times <- sort(unique(df$time))
        # Default to time==2 (matching README example) if available
        default_time <- if ("2" %in% as.character(times)) "2" else as.character(times[1])
        shiny::selectInput(ns("selected_time"), "Select time point",
                           choices = c("All data" = "all", as.character(times)),
                           selected = default_time)
      }
    })

    # Notes about analysis type
    output$analysis_notes <- shiny::renderUI({
      at <- input$analysis_type
      if (is.null(at)) return(NULL)
      msg <- switch(at,
        timepoint  = "Estimates immunity at a single time point.",
        baseline   = "Compares pre-epidemic baseline immunity across epidemics. Uses rows where baseline = 'yes'.",
        timeseries = "Tracks immunity over all time points in your data."
      )
      shiny::helpText(msg)
    })

    # Has results flag (for conditional panel)
    output$has_results <- shiny::reactive({ !is.null(rv$result) })
    shiny::outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Run analysis
    shiny::observeEvent(input$run_btn, {
      req(rv$processed_data, rv$get_params)

      params <- rv$get_params()
      if (is.null(params)) {
        shiny::showNotification("Parameters not ready. Please configure first.",
                                type = "error")
        return()
      }

      # Validate age_prop sum
      if (abs(sum(params$age_prop) - 1) > 0.05) {
        shiny::showNotification("Population proportions must sum to ~1.0.",
                                type = "error")
        return()
      }

      # Validate matrix dimensions
      if (nrow(params$contact_matrix) != rv$n_age_groups) {
        shiny::showNotification("Contact matrix dimensions don't match age groups.",
                                type = "error")
        return()
      }

      df <- rv$processed_data
      at <- input$analysis_type

      shiny::withProgress(message = "Running analysis...", value = 0.3, {

        tryCatch({
          result <- switch(at,

            timepoint = {
              # Filter to selected time point if applicable
              if (!is.null(input$selected_time) &&
                  input$selected_time != "all" &&
                  "time" %in% names(df)) {
                df <- df[df$time == as.numeric(input$selected_time), ]
              }
              ImmuPop_est_timepoint(
                df             = df,
                protect_c      = params$protect_c,
                protect_a      = params$protect_a,
                age_prop       = params$age_prop,
                contact_matrix = params$contact_matrix,
                sim_num        = params$sim_num,
                seed           = params$seed
              )
            },

            baseline = {
              df_bl <- df
              if ("baseline" %in% names(df)) {
                df_bl <- df[df$baseline == "yes", ]
              }
              ImmuPop_est_baseline(
                df_baseline    = df_bl,
                protect_c      = params$protect_c,
                protect_a      = params$protect_a,
                age_prop       = params$age_prop,
                contact_matrix = params$contact_matrix,
                sim_num        = params$sim_num,
                seed           = params$seed
              )
            },

            timeseries = {
              ImmuPop_est_timeseries(
                df_long        = df,
                protect_c      = params$protect_c,
                protect_a      = params$protect_a,
                age_prop       = params$age_prop,
                contact_matrix = params$contact_matrix,
                sim_num        = params$sim_num,
                seed           = params$seed
              )
            }
          )

          shiny::setProgress(1)
          rv$result <- result
          shiny::showNotification("Analysis complete!", type = "message")

        }, error = function(e) {
          shiny::showNotification(paste("Analysis error:", e$message),
                                  type = "error", duration = 10)
        })
      })
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      req(rv$result)
      summ <- summary(rv$result)
      DT::datatable(summ, rownames = FALSE,
                    options = list(pageLength = 20, dom = "t"))
    })

    # Estimates plot
    output$plot_estimates <- shiny::renderPlot({
      req(rv$result)
      plot_estimates(rv$result)
    })

    # Titer distribution
    output$plot_titer_dist <- shiny::renderPlot({
      req(rv$processed_data)
      df <- rv$processed_data
      # For timepoint mode, filter to selected time
      if (identical(input$analysis_type, "timepoint") &&
          !is.null(input$selected_time) &&
          input$selected_time != "all" &&
          "time" %in% names(df)) {
        df <- df[df$time == as.numeric(input$selected_time), ]
      }
      plot_titer_dist(df)
    })

    # Titer jitter
    output$plot_titer_jitter <- shiny::renderPlot({
      req(rv$processed_data)
      df <- rv$processed_data
      if (identical(input$analysis_type, "timepoint") &&
          !is.null(input$selected_time) &&
          input$selected_time != "all" &&
          "time" %in% names(df)) {
        df <- df[df$time == as.numeric(input$selected_time), ]
      }
      plot_titer_jitter(df, seed = 42)
    })

    # Download: results CSV
    output$dl_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("immupop_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(as.data.frame(rv$result), file, row.names = FALSE)
      }
    )

    # Download: summary table CSV
    output$dl_table_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("immupop_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(summary(rv$result), file, row.names = FALSE)
      }
    )

    # Download: all plots PDF
    output$dl_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("immupop_plots_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        grDevices::pdf(file, width = 10, height = 8)
        on.exit(grDevices::dev.off())

        # Page 1: estimates
        if (!is.null(rv$result)) {
          plot_estimates(rv$result)
        }

        # Page 2: titer distribution
        df <- rv$processed_data
        if (!is.null(df)) {
          plot_titer_dist(df)
        }

        # Page 3: titer jitter
        if (!is.null(df)) {
          plot_titer_jitter(df, seed = 42)
        }
      }
    )
  })
}
