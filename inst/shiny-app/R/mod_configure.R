# --- Configure Module ---

configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Configure analysis parameters"),

    # Section 1: Age groups
    shiny::wellPanel(
      shiny::h4("Age group breakpoints"),
      shiny::helpText("Enter ages that define group boundaries. Example: '0, 18, 50, 100'",
                      "creates three groups: [0,18), [18,50), [50,100)."),
      shiny::textInput(ns("cut_age"), "Age breakpoints (comma-separated)",
                       value = "0, 18, 50, 100"),
      shiny::uiOutput(ns("age_group_preview"))
    ),

    # Section 2: Population proportions
    shiny::wellPanel(
      shiny::h4("Population age proportions"),
      shiny::helpText("Fraction of the total population in each age group.",
                      "Should reflect census demographics, not your sample. Must sum to 1.0."),
      shiny::uiOutput(ns("age_prop_inputs")),
      shiny::uiOutput(ns("age_prop_sum"))
    ),

    # Section 3: Contact matrix
    shiny::wellPanel(
      shiny::h4("Contact matrix"),
      shiny::helpText("Average daily contacts between age groups.",
                      "Row i, column j = contacts a person in group i has with group j.",
                      "Can be obtained from POLYMOD or socialmixr R package."),
      shiny::uiOutput(ns("contact_matrix_inputs"))
    ),

    # Section 4: Protection vectors
    shiny::wellPanel(
      shiny::h4("Protection probabilities by titer level"),
      shiny::helpText("Probability of protection at each antibody titer level,",
                      "from lowest to highest. Values should increase (0 to 1)."),
      shiny::uiOutput(ns("n_levels_display")),
      shiny::uiOutput(ns("protect_inputs"))
    ),

    # Reset + navigation
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("reset_defaults"), "Reset to Defaults",
                            icon = shiny::icon("undo"), class = "btn-secondary")
      ),
      shiny::column(6, align = "right",
        shiny::actionButton(ns("next_btn"), "Next: Run Analysis",
                            icon = shiny::icon("arrow-right"),
                            class = "btn-primary btn-lg")
      )
    )
  )
}

configure_server <- function(id, rv, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Parse age breakpoints
    cut_age <- shiny::reactive({
      txt <- input$cut_age
      vals <- suppressWarnings(
        as.numeric(trimws(unlist(strsplit(txt, ","))))
      )
      vals <- vals[!is.na(vals)]
      sort(unique(vals))
    })

    # Process data when cut_age or raw_data changes
    shiny::observe({
      req(rv$raw_data)
      ca <- cut_age()
      if (length(ca) < 2) return()

      tryCatch({
        rv$processed_data <- generate_data(rv$raw_data, cut_age = ca)
        age_groups <- sort(unique(rv$processed_data$agegp1))
        rv$n_age_groups <- length(age_groups)
        rv$age_labels   <- as.character(age_groups)
      }, error = function(e) {
        shiny::showNotification(paste("Error processing data:", e$message),
                                type = "error")
      })
    })

    # Age group preview with sample counts
    output$age_group_preview <- shiny::renderUI({
      req(rv$processed_data, rv$age_labels)
      df <- rv$processed_data
      counts <- table(df$agegp1)
      labels <- paste0(names(counts), " \u2014 ", as.integer(counts), " subjects")
      shiny::tagList(
        shiny::tags$strong("Resulting age groups:"),
        shiny::tags$ul(
          lapply(labels, function(l) shiny::tags$li(l))
        )
      )
    })

    # Dynamic age_prop inputs
    output$age_prop_inputs <- shiny::renderUI({
      req(rv$n_age_groups, rv$age_labels)
      n <- rv$n_age_groups
      # Defaults matching README example
      defaults <- if (n == 3) c(0.2, 0.4, 0.4) else if (n == 2) c(0.2, 0.8) else rep(round(1/n, 3), n)
      lapply(seq_len(n), function(i) {
        shiny::numericInput(
          ns(paste0("age_prop_", i)),
          paste0("Proportion ", rv$age_labels[i]),
          value = defaults[i], min = 0, max = 1, step = 0.01
        )
      })
    })

    # age_prop sum indicator
    output$age_prop_sum <- shiny::renderUI({
      req(rv$n_age_groups)
      vals <- vapply(seq_len(rv$n_age_groups), function(i) {
        v <- input[[paste0("age_prop_", i)]]
        if (is.null(v) || is.na(v)) 0 else v
      }, numeric(1))
      s <- sum(vals)
      cls <- if (abs(s - 1) < 0.02) "text-success" else "text-danger"
      shiny::tags$p(class = cls,
        shiny::tags$strong(paste0("Sum: ", round(s, 3))),
        if (abs(s - 1) >= 0.02) " (must be ~1.0)"
      )
    })

    # Dynamic contact matrix inputs
    output$contact_matrix_inputs <- shiny::renderUI({
      req(rv$n_age_groups, rv$age_labels)
      n <- rv$n_age_groups
      # Defaults matching README example
      defaults <- if (n == 3) {
        matrix(c(22, 16, 15, 24, 28, 30, 18, 32, 35), nrow = 3, byrow = TRUE)
      } else if (n == 2) {
        matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE)
      } else {
        diag(20, n)
      }

      header_row <- shiny::fluidRow(
        shiny::column(3, ""),
        lapply(seq_len(n), function(j) {
          shiny::column(max(1, floor(9 / n)),
                        shiny::tags$strong(rv$age_labels[j]))
        })
      )

      rows <- lapply(seq_len(n), function(i) {
        shiny::fluidRow(
          shiny::column(3, shiny::tags$strong(rv$age_labels[i])),
          lapply(seq_len(n), function(j) {
            shiny::column(max(1, floor(9 / n)),
              shiny::numericInput(
                ns(paste0("cm_", i, "_", j)),
                label = NULL,
                value = defaults[i, j],
                min = 0, step = 1
              )
            )
          })
        )
      })

      shiny::tagList(header_row, rows)
    })

    # Number of titer levels display
    output$n_levels_display <- shiny::renderUI({
      req(rv$processed_data)
      n_levels <- length(unique(rv$processed_data$titer_level))
      shiny::tags$p(paste0("Detected ", n_levels, " titer levels in your data."))
    })

    # Dynamic protection vector inputs
    output$protect_inputs <- shiny::renderUI({
      req(rv$processed_data)
      levels <- sort(unique(rv$processed_data$titer_level))
      n_levels <- length(levels)
      # Standard HAI labels
      titer_labels <- c("<10", "10", "20", "40", "80", "160", "320", ">=640")
      # Default: linear ramp
      defaults <- seq(0.1, 0.5, length.out = n_levels)
      if (n_levels > length(titer_labels)) {
        titer_labels <- c(titer_labels,
                          paste0("Level ", (length(titer_labels) + 1):n_levels))
      }

      header <- shiny::fluidRow(
        shiny::column(3, ""),
        shiny::column(4, shiny::tags$strong("Children (first age group)")),
        shiny::column(4, shiny::tags$strong("Adults (other age groups)"))
      )

      rows <- lapply(seq_len(n_levels), function(i) {
        lbl <- if (i <= length(titer_labels)) titer_labels[i] else paste("Level", i)
        shiny::fluidRow(
          shiny::column(3, paste0("Titer ", lbl)),
          shiny::column(4,
            shiny::numericInput(ns(paste0("protect_c_", i)), label = NULL,
                                value = round(defaults[i], 2),
                                min = 0, max = 1, step = 0.05)
          ),
          shiny::column(4,
            shiny::numericInput(ns(paste0("protect_a_", i)), label = NULL,
                                value = round(defaults[i], 2),
                                min = 0, max = 1, step = 0.05)
          )
        )
      })

      shiny::tagList(header, rows)
    })

    # Reset to defaults
    shiny::observeEvent(input$reset_defaults, {
      shiny::updateTextInput(session, "cut_age", value = "0, 18, 50, 100")
      shiny::showNotification("Parameters reset to defaults.", type = "message")
    })

    # Next button
    shiny::observeEvent(input$next_btn, {
      shiny::updateNavbarPage(parent_session, "wizard", selected = "tab_results")
    })

    # Store a parameter-collection function in rv for the results module
    shiny::observe({
      rv$get_params <- function() {
        n <- rv$n_age_groups
        if (is.null(n) || n < 1) return(NULL)

        age_prop <- vapply(seq_len(n), function(i) {
          v <- input[[paste0("age_prop_", i)]]
          if (is.null(v) || is.na(v)) 0 else v
        }, numeric(1))

        cm <- matrix(0, nrow = n, ncol = n)
        for (i in seq_len(n)) {
          for (j in seq_len(n)) {
            v <- input[[paste0("cm_", i, "_", j)]]
            cm[i, j] <- if (is.null(v) || is.na(v)) 0 else v
          }
        }

        n_levels <- length(unique(rv$processed_data$titer_level))
        protect_c <- vapply(seq_len(n_levels), function(i) {
          v <- input[[paste0("protect_c_", i)]]
          if (is.null(v) || is.na(v)) 0 else v
        }, numeric(1))
        protect_a <- vapply(seq_len(n_levels), function(i) {
          v <- input[[paste0("protect_a_", i)]]
          if (is.null(v) || is.na(v)) 0 else v
        }, numeric(1))

        list(
          age_prop = age_prop,
          contact_matrix = cm,
          protect_c = protect_c,
          protect_a = protect_a,
          sim_num = 1000,
          seed = 42
        )
      }
    })
  })
}
