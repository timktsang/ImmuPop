library(shiny)
library(DT)
library(ImmuPop)

# Source modules
app_dir <- getwd()
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# Allow up to 10 MB uploads
options(shiny.maxRequestSize = 10 * 1024^2)

ui <- navbarPage(

  title = "ImmuPop",
  id = "wizard",
  theme = NULL,

  tabPanel("1. Upload Data",   value = "tab_upload",    upload_ui("upload")),
  tabPanel("2. Parameters",    value = "tab_config",    configure_ui("config")),
  tabPanel("3. Run & Results", value = "tab_results",   results_ui("results"))
)

server <- function(input, output, session) {
  # Shared state across modules
  rv <- reactiveValues(
    raw_data       = NULL,   # uploaded/demo data frame
    processed_data = NULL,   # after generate_data()
    result         = NULL,   # ImmuPop_result object
    has_time       = FALSE,
    has_epi        = FALSE,
    has_baseline   = FALSE,
    n_age_groups   = 2L,
    age_labels     = character(0),
    demo_mode      = FALSE
  )

  upload_server("upload", rv, parent_session = session)
  configure_server("config", rv, parent_session = session)
  results_server("results", rv, parent_session = session)
}

shinyApp(ui, server)
