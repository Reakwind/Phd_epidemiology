# --- 1. Load required libraries ---
library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)
library(shinyjs)
library(bslib)
library(fresh)

# --- Pre-computation Step (runs only once when the app starts) ---
tryCatch({
  con <- dbConnect(RSQLite::SQLite(), "780_database.db")
  all_patient_data <- dbGetQuery(con, "SELECT * FROM V_all_client_data")
  dbDisconnect(con)

  if (nrow(all_patient_data) == 0) {
    stop("No data found in database view V_all_client_data")
  }

  cat("Successfully loaded", nrow(all_patient_data), "patient records\n")
}, error = function(e) {
  stop(paste("Database error:", e$message))
})

# --- Data Cleaning and Type Conversion Step ---
for(col_name in names(all_patient_data)) {
  if (!col_name %in% c("client_name", "gender")) {
    all_patient_data[[col_name]] <- suppressWarnings(as.numeric(all_patient_data[[col_name]]))
  }
}

# Calculate quantiles and prepare data
calculate_quantiles <- function(data) {
  numeric_cols <- names(data)[sapply(data, is.numeric) & names(data) != "client_name"]
  lapply(data[, numeric_cols], function(col) {
    quantile(col, probs = c(0.10, 0.90), na.rm = TRUE)
  })
}

quantile_thresholds <- calculate_quantiles(all_patient_data)
all_variable_names <- names(all_patient_data)
numeric_variable_names <- names(all_patient_data)[sapply(all_patient_data, is.numeric)]
client_names <- sort(unique(all_patient_data$client_name))

# Data quality metrics
data_quality <- data.frame(
  Variable = names(all_patient_data),
  Type = sapply(all_patient_data, function(x) class(x)[1]),
  Missing = sapply(all_patient_data, function(x) sum(is.na(x))),
  Complete = sapply(all_patient_data, function(x) round(100 * (1 - sum(is.na(x)) / length(x)), 1)),
  Unique = sapply(all_patient_data, function(x) length(unique(x))),
  stringsAsFactors = FALSE
)

# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),

  # Custom CSS and theme
  theme = bs_theme(
    version = 4,
    primary = "#2c3e50",
    secondary = "#34495e",
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),

  tags$head(
    tags$style(HTML("
      :root {
        --primary-color: #2c3e50;
        --secondary-color: #34495e;
        --accent-color: #3498db;
        --success-color: #27ae60;
        --warning-color: #f39c12;
        --background-light: #f8f9fa;
        --text-muted: #6c757d;
      }

      .main-header {
        background: linear-gradient(135deg, var(--primary-color), var(--secondary-color));
        color: white;
        padding: 2rem 1rem;
        margin-bottom: 2rem;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }

      .stat-card {
        background: white;
        border-radius: 8px;
        padding: 1.5rem;
        margin: 0.5rem 0;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-left: 4px solid var(--accent-color);
        transition: transform 0.2s, box-shadow 0.2s;
      }

      .stat-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 20px rgba(0,0,0,0.12);
      }

      .quality-indicator {
        width: 12px;
        height: 12px;
        border-radius: 50%;
        display: inline-block;
        margin-right: 8px;
      }

      .quality-high { background-color: var(--success-color); }
      .quality-medium { background-color: var(--warning-color); }
      .quality-low { background-color: var(--danger-color); }

      .breadcrumb {
        background: var(--background-light);
        border-radius: 6px;
        padding: 0.75rem 1rem;
        margin-bottom: 1rem;
        font-size: 0.9rem;
      }

      .help-icon {
        color: var(--accent-color);
        cursor: pointer;
        font-size: 14px;
        margin-left: 5px;
      }

      .help-icon:hover {
        color: var(--primary-color);
      }

      .custom-tab {
        border-radius: 8px 8px 0 0;
      }

      .loading-message {
        text-align: center;
        color: var(--text-muted);
        font-style: italic;
        padding: 2rem;
      }

      .filter-section {
        background: var(--background-light);
        border-radius: 8px;
        padding: 1rem;
        margin: 1rem 0;
      }

      .export-section {
        background: linear-gradient(135deg, #f8f9fa, #e9ecef);
        border-radius: 8px;
        padding: 1rem;
        margin: 1rem 0;
      }

      .theme-toggle {
        position: fixed;
        top: 20px;
        right: 20px;
        z-index: 1000;
        background: var(--primary-color);
        color: white;
        border: none;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        cursor: pointer;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        transition: transform 0.2s;
      }

      .theme-toggle:hover {
        transform: scale(1.1);
      }

      .dark-theme {
        background-color: #1a1a1a !important;
        color: #ffffff !important;
      }

      .dark-theme .stat-card {
        background-color: #2d2d2d !important;
        color: #ffffff !important;
      }

      .dark-theme .filter-section {
        background-color: #2d2d2d !important;
      }

      .comparison-container {
        border: 2px dashed var(--accent-color);
        border-radius: 8px;
        padding: 1rem;
        margin: 1rem 0;
        text-align: center;
        color: var(--text-muted);
      }

      .success-notification {
        position: fixed;
        top: 80px;
        right: 20px;
        background: var(--success-color);
        color: white;
        padding: 1rem 1.5rem;
        border-radius: 6px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        z-index: 1001;
        transform: translateX(100%);
        transition: transform 0.3s;
      }

      .success-notification.show {
        transform: translateX(0);
      }

      .recent-selections {
        background: var(--background-light);
        border-radius: 6px;
        padding: 0.75rem;
        margin: 0.5rem 0;
        font-size: 0.85rem;
      }

      .progress-indicator {
        background: var(--background-light);
        height: 4px;
        border-radius: 2px;
        overflow: hidden;
        margin: 1rem 0;
      }

      .progress-bar {
        background: linear-gradient(90deg, var(--accent-color), var(--success-color));
        height: 100%;
        border-radius: 2px;
        transition: width 0.3s;
      }

      @media (max-width: 768px) {
        .main-header {
          padding: 1rem;
          text-align: center;
        }

        .stat-card {
          margin: 1rem 0;
        }

        .theme-toggle {
          top: 10px;
          right: 10px;
          width: 40px;
          height: 40px;
        }
      }

      .resizable-sidebar {
        resize: horizontal;
        overflow: auto;
        min-width: 200px;
        max-width: 500px;
      }
    "))
  ),

  # Theme toggle button
  tags$button(
    id = "theme_toggle",
    class = "theme-toggle",
    onclick = "toggleTheme()",
    icon("moon")
  ),

  # Success notification div
  div(id = "success_notification", class = "success-notification"),

  # Header section
  div(class = "main-header",
    fluidRow(
      column(8,
        h1("780 Data Explorer", style = "margin: 0; font-weight: 600;"),
        h5(paste("Dataset:", nrow(all_patient_data), "patients with", ncol(all_patient_data), "variables"),
           style = "margin: 0.5rem 0 0 0; opacity: 0.9;")
      ),
      column(4,
        div(style = "text-align: right; padding-top: 1rem;",
          actionButton("tour_btn", "Take Tour", icon = icon("question-circle"),
                      class = "btn-outline-light", style = "margin-right: 10px;"),
          actionButton("help_btn", "Help", icon = icon("info-circle"),
                      class = "btn-outline-light")
        )
      )
    )
  ),

  # Breadcrumb navigation
  div(class = "breadcrumb",
    uiOutput("breadcrumb_ui")
  ),

  # Progress indicator
  div(class = "progress-indicator",
    div(id = "analysis_progress", class = "progress-bar", style = "width: 0%")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "resizable-sidebar",
      uiOutput("conditional_sidebar")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "pills",

        tabPanel("Patient Lookup",
          icon = icon("user-md"),
          value = "patient_lookup",
          br(),
          uiOutput("patient_lookup_ui")
        ),

        tabPanel("Variable Distribution",
          icon = icon("chart-bar"),
          value = "variable_distribution",
          br(),
          fluidRow(
            column(8,
              div(id = "dist_plot_container",
                conditionalPanel(
                  condition = "input.variable_select != null && input.variable_select != ''",
                  withSpinner(
                    plotlyOutput("variable_plot", height = "500px"),
                    type = 8,
                    color = "#3498db",
                    size = 0.8
                  )
                )
              )
            ),
            column(4,
              div(class = "stat-card",
                h4("Summary Statistics",
                   span(class = "help-icon", `data-toggle` = "tooltip",
                        title = "Statistical measures of central tendency and spread",
                        icon("question-circle"))),
                tableOutput("variable_summary")
              ),
              div(class = "export-section",
                h4("Export Options"),
                div(style = "display: flex; flex-direction: column; gap: 10px;",
                  downloadButton("download_plot", "Download Plot (PNG)",
                               class = "btn-primary", icon = icon("download")),
                  downloadButton("download_data", "Download Data (CSV)",
                               class = "btn-secondary", icon = icon("file-csv")),
                  downloadButton("download_pdf_report", "Full Report (PDF)",
                               class = "btn-info", icon = icon("file-pdf"))
                )
              ),
              div(class = "recent-selections",
                h5("Recent Selections"),
                textOutput("recent_variables")
              )
            )
          )
        ),

        tabPanel("Bivariate Analysis",
          icon = icon("chart-line"),
          value = "bivariate_analysis",
          br(),
          fluidRow(
            column(8,
              withSpinner(
                plotlyOutput("bivariate_plot", height = "500px"),
                type = 8,
                color = "#3498db"
              )
            ),
            column(4,
              div(class = "stat-card",
                h4("Correlation Analysis",
                   span(class = "help-icon", `data-toggle` = "tooltip",
                        title = "Measures the strength and direction of linear relationship between variables",
                        icon("question-circle"))),
                verbatimTextOutput("correlation_info")
              ),
              div(class = "export-section",
                h4("Export Options"),
                div(style = "display: flex; flex-direction: column; gap: 10px;",
                  downloadButton("download_bivariate_plot", "Download Plot (PNG)",
                               class = "btn-primary", icon = icon("download")),
                  downloadButton("download_bivariate_data", "Download Data (CSV)",
                               class = "btn-secondary", icon = icon("file-csv"))
                )
              )
            )
          )
        ),

        tabPanel("Data Overview",
          icon = icon("table"),
          value = "data_overview",
          br(),
          fluidRow(
            column(12,
              h3("Dataset Summary Dashboard"),
              div(class = "stat-card",
                h4("Data Quality Overview"),
                DT::dataTableOutput("data_quality_table")
              )
            )
          )
        ),

        tabPanel("Compare Patients",
          icon = icon("balance-scale"),
          value = "compare_patients",
          br(),
          fluidRow(
            column(6,
              div(class = "stat-card",
                h4("Patient A"),
                selectizeInput("patient_a", "Select Patient A:",
                             choices = c("", client_names),
                             options = list(placeholder = "Type to search...")),
                tableOutput("patient_a_data")
              )
            ),
            column(6,
              div(class = "stat-card",
                h4("Patient B"),
                selectizeInput("patient_b", "Select Patient B:",
                             choices = c("", client_names),
                             options = list(placeholder = "Type to search...")),
                tableOutput("patient_b_data")
              )
            )
          ),
          br(),
          div(class = "stat-card",
            h4("Comparison Summary"),
            tableOutput("patient_comparison")
          )
        ),

        tabPanel("Analytics Dashboard",
          icon = icon("dashboard"),
          value = "analytics",
          br(),
          fluidRow(
            column(3,
              div(class = "stat-card",
                h3(textOutput("total_patients"), style = "color: #3498db; margin: 0;"),
                p("Total Patients", style = "margin: 0;")
              )
            ),
            column(3,
              div(class = "stat-card",
                h3(textOutput("total_variables"), style = "color: #27ae60; margin: 0;"),
                p("Variables", style = "margin: 0;")
              )
            ),
            column(3,
              div(class = "stat-card",
                h3(textOutput("avg_completeness"), style = "color: #f39c12; margin: 0;"),
                p("Avg Completeness", style = "margin: 0;")
              )
            ),
            column(3,
              div(class = "stat-card",
                h3(textOutput("analysis_count"), style = "color: #e74c3c; margin: 0;"),
                p("Analyses Run", style = "margin: 0;")
              )
            )
          ),
          br(),
          fluidRow(
            column(6,
              div(class = "stat-card",
                h4("Data Completeness by Variable"),
                plotlyOutput("completeness_plot", height = "300px")
              )
            ),
            column(6,
              div(class = "stat-card",
                h4("Variable Type Distribution"),
                plotlyOutput("variable_types_plot", height = "300px")
              )
            )
          )
        )
      )
    )
  ),

  # JavaScript for theme toggle and notifications
  tags$script(HTML("
    let isDarkTheme = false;
    let analysisCounter = 0;

    function toggleTheme() {
      const body = document.body;
      const toggle = document.getElementById('theme_toggle');

      if (isDarkTheme) {
        body.classList.remove('dark-theme');
        toggle.innerHTML = '<i class=\"fa fa-moon\"></i>';
        isDarkTheme = false;
      } else {
        body.classList.add('dark-theme');
        toggle.innerHTML = '<i class=\"fa fa-sun\"></i>';
        isDarkTheme = true;
      }
    }

    function showNotification(message) {
      const notification = document.getElementById('success_notification');
      notification.textContent = message;
      notification.classList.add('show');

      setTimeout(() => {
        notification.classList.remove('show');
      }, 3000);
    }

    function updateProgress(percentage) {
      const progressBar = document.getElementById('analysis_progress');
      progressBar.style.width = percentage + '%';
    }

    // Initialize tooltips
    $(document).ready(function(){
      $('[data-toggle=\"tooltip\"]').tooltip();
    });
  "))
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {

  # Reactive values for state management
  values <- reactiveValues(
    recent_variables = c(),
    recent_patients = c(),
    analysis_count = 0,
    current_tab = "patient_lookup"
  )

  # Update current tab
  observe({
    values$current_tab <- input$main_tabs
  })

  # --- Breadcrumb navigation ---
  output$breadcrumb_ui <- renderUI({
    tab_names <- list(
      "patient_lookup" = "Patient Lookup",
      "variable_distribution" = "Variable Distribution",
      "bivariate_analysis" = "Bivariate Analysis",
      "data_overview" = "Data Overview",
      "compare_patients" = "Compare Patients",
      "analytics" = "Analytics Dashboard"
    )

    current_name <- tab_names[[values$current_tab]] %||% "Home"

    tagList(
      span("Home", style = "color: #6c757d;"),
      span(" / ", style = "color: #6c757d;"),
      span(current_name, style = "font-weight: 500;")
    )
  })

  # --- Enhanced Conditional Sidebar UI ---
  output$conditional_sidebar <- renderUI({
    if (input$main_tabs == "patient_lookup") {
      tagList(
        div(class = "filter-section",
          h4("Patient Search", style = "color: #2c3e50; margin-bottom: 1rem;"),
          selectizeInput("client_id_input", "Select Client Name:",
                         choices = c("", client_names),
                         options = list(
                           placeholder = "Start typing client name...",
                           maxOptions = 50,
                           searchField = c('value', 'text'),
                           create = FALSE,
                           selectOnTab = TRUE
                         )),
          radioButtons("view_option", "Display Options:",
                       choices = c("Show all data" = "all",
                                   "Show only missing data" = "missing",
                                   "Show outliers (10%/90%)" = "outliers"),
                       selected = "all"),
          actionButton("search_button", "Search Patient",
                      icon = icon("search"), class = "btn-primary",
                      style = "width: 100%; margin: 10px 0;")
        ),

        div(class = "stat-card",
          h5("Quick Statistics"),
          textOutput("patient_count"),
          br(),
          h6("Recently Viewed:"),
          textOutput("recent_patients_list")
        ),

        div(class = "stat-card",
          h5("Advanced Options"),
          checkboxInput("show_advanced", "Show advanced filters", FALSE),
          conditionalPanel(
            condition = "input.show_advanced",
            checkboxInput("highlight_outliers", "Highlight outliers", TRUE),
            sliderInput("outlier_threshold", "Outlier threshold (percentile):",
                       min = 5, max = 20, value = 10, step = 1)
          )
        )
      )

    } else if (input$main_tabs == "variable_distribution") {
      tagList(
        div(class = "filter-section",
          h4("Variable Analysis", style = "color: #2c3e50;"),
          selectizeInput("variable_select", "Select Variable:",
                        choices = all_variable_names,
                        selected = ifelse(length(values$recent_variables) > 0,
                                        values$recent_variables[1], "age"),
                        options = list(
                          placeholder = "Type to search variables...",
                          maxOptions = 100,
                          searchField = c('value', 'text')
                        )),

          h5("Data Filters", style = "margin-top: 1rem;"),
          if ("gender" %in% names(all_patient_data)) {
            checkboxGroupInput("gender_filter", "Gender:",
                             choices = unique(na.omit(all_patient_data$gender)),
                             selected = unique(na.omit(all_patient_data$gender)),
                             inline = TRUE)
          },

          if ("age" %in% names(all_patient_data)) {
            sliderInput("age_filter", "Age Range:",
                       min = floor(min(all_patient_data$age, na.rm = TRUE)),
                       max = ceiling(max(all_patient_data$age, na.rm = TRUE)),
                       value = c(floor(min(all_patient_data$age, na.rm = TRUE)),
                                ceiling(max(all_patient_data$age, na.rm = TRUE))),
                       step = 1)
          },

          actionButton("reset_filters", "Reset Filters",
                      icon = icon("undo"), class = "btn-outline-secondary",
                      style = "margin-top: 10px; width: 100%;")
        ),

        div(class = "stat-card",
          h5("Plot Settings"),
          conditionalPanel(
            condition = "input.variable_select != null",
            numericInput("bins", "Number of bins (for histograms):",
                        value = 30, min = 5, max = 100, step = 5),
            checkboxInput("show_density", "Show density curve", FALSE),
            checkboxInput("log_scale", "Log scale", FALSE)
          )
        )
      )

    } else if (input$main_tabs == "bivariate_analysis") {
      tagList(
        div(class = "filter-section",
          h4("Two-Variable Analysis", style = "color: #2c3e50;"),
          selectizeInput("var_x", "X-axis Variable:",
                        choices = all_variable_names,
                        selected = "age",
                        options = list(placeholder = "Select X variable...")),
          selectizeInput("var_y", "Y-axis Variable:",
                        choices = all_variable_names,
                        selected = "moca_total_score_v1_v1",
                        options = list(placeholder = "Select Y variable...")),

          h5("Filters"),
          if ("gender" %in% names(all_patient_data)) {
            checkboxGroupInput("gender_filter_biv", "Gender:",
                             choices = unique(na.omit(all_patient_data$gender)),
                             selected = unique(na.omit(all_patient_data$gender)),
                             inline = TRUE)
          }
        ),

        div(class = "stat-card",
          h5("Plot Options"),
          checkboxInput("show_regression", "Show regression line", TRUE),
          checkboxInput("show_confidence", "Show confidence interval", FALSE),
          selectInput("plot_theme", "Plot theme:",
                     choices = c("Minimal" = "minimal", "Classic" = "classic",
                                "Dark" = "dark", "Void" = "void"),
                     selected = "minimal")
        )
      )

    } else if (input$main_tabs == "compare_patients") {
      div(class = "stat-card",
        h4("Patient Comparison Settings"),
        p("Select two patients to compare their data side by side."),
        selectInput("comparison_variables", "Variables to compare:",
                   choices = all_variable_names,
                   selected = head(numeric_variable_names, 10),
                   multiple = TRUE),
        checkboxInput("show_differences_only", "Show differences only", FALSE)
      )

    } else if (input$main_tabs == "data_overview") {
      div(class = "stat-card",
        h4("Dataset Information"),
        p(paste("Total Patients:", nrow(all_patient_data))),
        p(paste("Total Variables:", ncol(all_patient_data))),
        p(paste("Numeric Variables:", length(numeric_variable_names))),
        br(),
        actionButton("refresh_data", "Refresh Data Quality",
                    icon = icon("sync"), class = "btn-info")
      )

    } else if (input$main_tabs == "analytics") {
      div(class = "stat-card",
        h4("Dashboard Settings"),
        selectInput("metric_timeframe", "Analysis timeframe:",
                   choices = c("Current session" = "session",
                              "All time" = "all"),
                   selected = "session"),
        checkboxInput("show_trends", "Show trend analysis", TRUE)
      )
    }
  })

  # --- Patient count output ---
  output$patient_count <- renderText({
    paste("Total patients in database:", nrow(all_patient_data))
  })

  # --- Recent patients list ---
  output$recent_patients_list <- renderText({
    if (length(values$recent_patients) == 0) {
      "None"
    } else {
      paste(tail(values$recent_patients, 3), collapse = ", ")
    }
  })

  # --- Recent variables ---
  output$recent_variables <- renderText({
    if (length(values$recent_variables) == 0) {
      "No variables selected yet"
    } else {
      paste("Last 3:", paste(tail(values$recent_variables, 3), collapse = ", "))
    }
  })

  # --- Filtered data reactive with progress indicator ---
  filtered_data <- reactive({
    data <- all_patient_data

    # Apply gender filter if available
    if (input$main_tabs %in% c("variable_distribution", "bivariate_analysis")) {
      if ("gender" %in% names(data)) {
        gender_input <- if (input$main_tabs == "variable_distribution") input$gender_filter else input$gender_filter_biv
        if (!is.null(gender_input) && length(gender_input) > 0) {
          data <- data[data$gender %in% gender_input | is.na(data$gender), ]
        }
      }

      # Apply age filter if available
      if (input$main_tabs == "variable_distribution" && "age" %in% names(data) && !is.null(input$age_filter)) {
        data <- data[is.na(data$age) | (data$age >= input$age_filter[1] & data$age <= input$age_filter[2]), ]
      }
    }

    # Update progress
    session$sendCustomMessage("updateProgress", 100)

    return(data)
  })

  # --- Enhanced Patient Lookup Logic ---
  search_result <- eventReactive(input$search_button, {
    req(input$client_id_input)
    if (input$client_id_input == "") {
      return(data.frame(Status = "Please select a client name."))
    }

    # Add to recent patients
    values$recent_patients <- unique(c(values$recent_patients, input$client_id_input))

    client_data <- all_patient_data[all_patient_data$client_name == input$client_id_input, ]
    if (nrow(client_data) == 0) return(data.frame(Status = "No data found for this client."))

    transposed_data <- as.data.frame(t(client_data))
    colnames(transposed_data) <- "Value"
    transposed_data$Variable <- rownames(transposed_data)
    transposed_data <- transposed_data[, c("Variable", "Value")]
    transposed_data$Status <- "Normal"
    transposed_data$Quality <- ""

    # Enhanced outlier detection
    outlier_threshold <- input$outlier_threshold %||% 10

    for (i in 1:nrow(transposed_data)) {
      var_name <- transposed_data$Variable[i]
      value <- transposed_data$Value[i]

      if (is.na(value)) {
        transposed_data$Status[i] <- "Missing"
        transposed_data$Quality[i] <- "🔴"
        next
      }

      if (var_name %in% names(quantile_thresholds)) {
        thresholds <- quantile(all_patient_data[[var_name]],
                             probs = c(outlier_threshold/100, 1 - outlier_threshold/100),
                             na.rm = TRUE)
        q1 <- thresholds[1]
        q3 <- thresholds[2]
        numeric_value <- as.numeric(value)

        if (!is.na(numeric_value)) {
          if (numeric_value < q1) {
            transposed_data$Status[i] <- paste0("Low (below ", outlier_threshold, "%)")
            transposed_data$Quality[i] <- "🟡"
          } else if (numeric_value > q3) {
            transposed_data$Status[i] <- paste0("High (above ", 100-outlier_threshold, "%)")
            transposed_data$Quality[i] <- "🟡"
          } else {
            transposed_data$Quality[i] <- "🟢"
          }
        }
      }
    }

    # Filter based on view option
    view_type <- input$view_option
    if (view_type == "missing") {
      return(transposed_data[transposed_data$Status == "Missing", c("Variable", "Value", "Quality")])
    } else if (view_type == "outliers") {
      return(transposed_data[grepl("Low|High", transposed_data$Status), ])
    } else {
      return(transposed_data)
    }
  })

  # Dynamic UI for patient lookup tab
  output$patient_lookup_ui <- renderUI({
    if (input$search_button == 0) {
      div(class = "stat-card", style = "text-align: center; padding: 3rem;",
        icon("user-md", style = "font-size: 4rem; color: #3498db; margin-bottom: 1rem;"),
        h4("Welcome to Patient Lookup"),
        p("Select a client name from the sidebar and click 'Search' to view their comprehensive data profile."),
        p("You can filter the view to show all data, only missing values, or statistical outliers.")
      )
    } else {
      tagList(
        div(class = "stat-card",
          h4(icon("user"), " ", textOutput("client_header", inline = TRUE)),
          withSpinner(
            DT::dataTableOutput("client_data_table"),
            type = 8, color = "#3498db"
          )
        )
      )
    }
  })

  output$client_header <- renderText({
    req(input$search_button > 0)
    paste("Patient Profile:", input$client_id_input)
  })

  output$client_data_table <- DT::renderDataTable({
    search_result()
  }, options = list(
    pageLength = 15,
    scrollY = "400px",
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'print')
  ),
  escape = FALSE,
  rownames = FALSE)

  # Track variable selection for recent list
  observe({
    if (!is.null(input$variable_select) && input$variable_select != "") {
      values$recent_variables <- unique(c(values$recent_variables, input$variable_select))
      values$analysis_count <- values$analysis_count + 1
    }
  })

  # --- Enhanced Variable Distribution Plotting ---
  output$variable_plot <- renderPlotly({
    req(input$variable_select)
    selected_var <- input$variable_select
    plot_data <- data.frame(value = filtered_data()[[selected_var]])
    n_obs <- sum(!is.na(plot_data$value))
    plot_data <- na.omit(plot_data)

    bins <- input$bins %||% 30
    show_density <- input$show_density %||% FALSE
    log_scale <- input$log_scale %||% FALSE

    is_continuous <- is.numeric(plot_data$value) && length(unique(plot_data$value)) > 10

    plot_title <- paste("Distribution of", selected_var,
                       "<br><i>n =", n_obs, "observations</i>")

    if (is_continuous) {
      p <- ggplot(plot_data, aes(x = value)) +
        geom_histogram(bins = bins, aes(text = paste("Count:", ..count..)),
                      fill = "#3498db", color = "white", alpha = 0.7) +
        labs(title = plot_title, x = selected_var, y = "Frequency") +
        theme_minimal(base_size = 14)

      if (show_density) {
        p <- p + geom_density(aes(y = ..density.. * nrow(plot_data) *
                                 (max(plot_data$value, na.rm = TRUE) -
                                  min(plot_data$value, na.rm = TRUE)) / bins),
                             color = "#e74c3c", size = 1)
      }

      if (log_scale && min(plot_data$value, na.rm = TRUE) > 0) {
        p <- p + scale_x_log10()
      }
    } else {
      p <- ggplot(plot_data, aes(x = factor(value))) +
        geom_bar(aes(text = paste("Count:", ..count..)),
                fill = "#3498db", color = "white", alpha = 0.7) +
        labs(title = plot_title, x = selected_var, y = "Count") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE, modeBarButtonsToAdd = c("drawline", "drawopenpath"))
  })

  # --- Enhanced Bivariate Analysis ---
  output$bivariate_plot <- renderPlotly({
    req(input$var_x, input$var_y)
    x_var <- input$var_x
    y_var <- input$var_y

    plot_data <- filtered_data()[, c(x_var, y_var)]
    plot_data <- na.omit(plot_data)
    n_obs <- nrow(plot_data)

    x_is_num <- is.numeric(plot_data[[x_var]])
    y_is_num <- is.numeric(plot_data[[y_var]])

    plot_title <- paste(y_var, "vs.", x_var, "<br><i>n =", n_obs, "complete observations</i>")

    theme_choice <- switch(input$plot_theme %||% "minimal",
                          "minimal" = theme_minimal(),
                          "classic" = theme_classic(),
                          "dark" = theme_dark(),
                          "void" = theme_void(),
                          theme_minimal())

    if (x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(alpha = 0.6, color = "#3498db", size = 2,
                  aes(text = paste("X:", round(.data[[x_var]], 3),
                                  "<br>Y:", round(.data[[y_var]], 3))))

      if (input$show_regression %||% TRUE) {
        p <- p + geom_smooth(method = "lm",
                           se = input$show_confidence %||% FALSE,
                           color = "#e74c3c", formula = 'y ~ x')
      }

      p <- p + labs(title = plot_title, x = x_var, y = y_var) + theme_choice

    } else if (x_is_num && !y_is_num) {
      p <- ggplot(plot_data, aes(x = factor(.data[[y_var]]), y = .data[[x_var]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
        labs(title = plot_title, x = y_var, y = x_var) +
        theme_choice +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else if (!x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
        labs(title = plot_title, x = x_var, y = y_var) +
        theme_choice +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else {
      p <- ggplot(plot_data, aes(x = factor(.data[[x_var]]), y = factor(.data[[y_var]]))) +
        geom_jitter(alpha = 0.5, color = "#9b59b6", width = 0.2, height = 0.2, size = 2) +
        labs(title = plot_title, x = x_var, y = y_var) +
        theme_choice +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE)
  })

  # --- Enhanced Variable Summary Statistics ---
  output$variable_summary <- renderTable({
    req(input$variable_select)
    selected_var <- input$variable_select
    data_col <- filtered_data()[[selected_var]]

    if (is.numeric(data_col)) {
      q25 <- quantile(data_col, 0.25, na.rm = TRUE)
      q75 <- quantile(data_col, 0.75, na.rm = TRUE)
      iqr <- q75 - q25

      stats <- data.frame(
        Statistic = c("Count", "Missing", "Mean", "Median", "Std Dev",
                     "Min", "Max", "Q25", "Q75", "IQR"),
        Value = c(
          sum(!is.na(data_col)),
          sum(is.na(data_col)),
          round(mean(data_col, na.rm = TRUE), 3),
          round(median(data_col, na.rm = TRUE), 3),
          round(sd(data_col, na.rm = TRUE), 3),
          round(min(data_col, na.rm = TRUE), 3),
          round(max(data_col, na.rm = TRUE), 3),
          round(q25, 3),
          round(q75, 3),
          round(iqr, 3)
        )
      )
    } else {
      freq_table <- table(data_col, useNA = "ifany")
      total <- sum(freq_table)
      stats <- data.frame(
        Category = names(freq_table),
        Count = as.numeric(freq_table),
        Percentage = paste0(round(100 * as.numeric(freq_table) / total, 1), "%")
      )
    }
    return(stats)
  }, striped = TRUE, hover = TRUE)

  # --- Enhanced Correlation Information ---
  output$correlation_info <- renderText({
    req(input$var_x, input$var_y)
    x_data <- filtered_data()[[input$var_x]]
    y_data <- filtered_data()[[input$var_y]]

    if (is.numeric(x_data) && is.numeric(y_data)) {
      complete_data <- na.omit(data.frame(x = x_data, y = y_data))
      if (nrow(complete_data) > 3) {
        cor_val <- cor(complete_data$x, complete_data$y)
        cor_test <- cor.test(complete_data$x, complete_data$y)

        # Enhanced interpretation
        strength <- ifelse(abs(cor_val) > 0.8, "Very Strong",
                          ifelse(abs(cor_val) > 0.6, "Strong",
                                ifelse(abs(cor_val) > 0.4, "Moderate",
                                      ifelse(abs(cor_val) > 0.2, "Weak", "Very Weak"))))

        direction <- ifelse(cor_val > 0, "positive", "negative")
        significance <- ifelse(cor_test$p.value < 0.001, "highly significant (p < 0.001)",
                              ifelse(cor_test$p.value < 0.01, "significant (p < 0.01)",
                                    ifelse(cor_test$p.value < 0.05, "significant (p < 0.05)",
                                          "not significant (p >= 0.05)")))

        paste0(
          "Pearson Correlation: ", round(cor_val, 4), "\n",
          "P-value: ", format(cor_test$p.value, scientific = TRUE, digits = 3), "\n",
          "95% CI: [", round(cor_test$conf.int[1], 3), ", ", round(cor_test$conf.int[2], 3), "]\n",
          "Sample size: ", nrow(complete_data), "\n\n",
          "Interpretation:\n",
          "• ", strength, " ", direction, " correlation\n",
          "• Result is ", significance, "\n",
          "• R² = ", round(cor_val^2, 3), " (", round(100 * cor_val^2, 1), "% variance explained)"
        )
      } else {
        "Insufficient data for correlation analysis\n(Need at least 4 complete observations)"
      }
    } else {
      "Correlation analysis requires numeric variables\n\nFor categorical variables, consider:\n• Chi-square test of independence\n• Cramér's V coefficient\n• Contingency table analysis"
    }
  })

  # --- Data Quality Table ---
  output$data_quality_table <- DT::renderDataTable({
    quality_data <- data_quality
    quality_data$Quality <- ifelse(quality_data$Complete >= 90, "🟢 High",
                                  ifelse(quality_data$Complete >= 70, "🟡 Medium", "🔴 Low"))

    quality_data$`Missing %` <- paste0(round(100 - quality_data$Complete, 1), "%")
    quality_data <- quality_data[, c("Variable", "Type", "Missing", "Missing %", "Complete", "Unique", "Quality")]

    return(quality_data)
  }, options = list(
    pageLength = 25,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    columnDefs = list(
      list(targets = 6, orderable = FALSE)
    )
  ), escape = FALSE, rownames = FALSE)

  # --- Patient Comparison Logic ---
  output$patient_a_data <- renderTable({
    req(input$patient_a)
    if (input$patient_a != "") {
      patient_data <- all_patient_data[all_patient_data$client_name == input$patient_a, ]
      if (nrow(patient_data) > 0) {
        vars_to_show <- input$comparison_variables %||% head(all_variable_names, 10)
        result <- data.frame(
          Variable = vars_to_show,
          Value = sapply(vars_to_show, function(x) patient_data[[x]]),
          stringsAsFactors = FALSE
        )
        return(result)
      }
    }
    return(data.frame(Variable = "No patient selected", Value = ""))
  }, striped = TRUE)

  output$patient_b_data <- renderTable({
    req(input$patient_b)
    if (input$patient_b != "") {
      patient_data <- all_patient_data[all_patient_data$client_name == input$patient_b, ]
      if (nrow(patient_data) > 0) {
        vars_to_show <- input$comparison_variables %||% head(all_variable_names, 10)
        result <- data.frame(
          Variable = vars_to_show,
          Value = sapply(vars_to_show, function(x) patient_data[[x]]),
          stringsAsFactors = FALSE
        )
        return(result)
      }
    }
    return(data.frame(Variable = "No patient selected", Value = ""))
  }, striped = TRUE)

  output$patient_comparison <- renderTable({
    req(input$patient_a, input$patient_b)
    if (input$patient_a != "" && input$patient_b != "") {
      patient_a_data <- all_patient_data[all_patient_data$client_name == input$patient_a, ]
      patient_b_data <- all_patient_data[all_patient_data$client_name == input$patient_b, ]

      if (nrow(patient_a_data) > 0 && nrow(patient_b_data) > 0) {
        vars_to_show <- input$comparison_variables %||% head(all_variable_names, 10)

        comparison <- data.frame(
          Variable = vars_to_show,
          `Patient A` = sapply(vars_to_show, function(x) patient_a_data[[x]]),
          `Patient B` = sapply(vars_to_show, function(x) patient_b_data[[x]]),
          Difference = sapply(vars_to_show, function(x) {
            a_val <- patient_a_data[[x]]
            b_val <- patient_b_data[[x]]
            if (is.numeric(a_val) && is.numeric(b_val) && !is.na(a_val) && !is.na(b_val)) {
              round(a_val - b_val, 3)
            } else {
              ifelse(a_val == b_val, "Same", "Different")
            }
          }),
          stringsAsFactors = FALSE
        )

        if (input$show_differences_only %||% FALSE) {
          comparison <- comparison[comparison$Difference != "Same" & comparison$Difference != 0, ]
        }

        return(comparison)
      }
    }
    return(data.frame(Variable = "Select both patients to compare", `Patient A` = "", `Patient B` = "", Difference = ""))
  }, striped = TRUE)

  # --- Analytics Dashboard ---
  output$total_patients <- renderText({
    as.character(nrow(all_patient_data))
  })

  output$total_variables <- renderText({
    as.character(ncol(all_patient_data))
  })

  output$avg_completeness <- renderText({
    avg_complete <- mean(data_quality$Complete)
    paste0(round(avg_complete, 1), "%")
  })

  output$analysis_count <- renderText({
    as.character(values$analysis_count)
  })

  # --- Completeness plot ---
  output$completeness_plot <- renderPlotly({
    plot_data <- data_quality[order(data_quality$Complete), ]
    plot_data$Variable <- factor(plot_data$Variable, levels = plot_data$Variable)

    p <- ggplot(plot_data, aes(x = Variable, y = Complete,
                              fill = ifelse(Complete >= 90, "High",
                                           ifelse(Complete >= 70, "Medium", "Low")))) +
      geom_col() +
      scale_fill_manual(values = c("High" = "#27ae60", "Medium" = "#f39c12", "Low" = "#e74c3c")) +
      labs(title = "Data Completeness by Variable", x = "Variable", y = "Completeness (%)",
           fill = "Quality") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = TRUE)
  })

  # --- Variable types plot ---
  output$variable_types_plot <- renderPlotly({
    type_counts <- table(data_quality$Type)
    plot_data <- data.frame(
      Type = names(type_counts),
      Count = as.numeric(type_counts)
    )

    p <- ggplot(plot_data, aes(x = Type, y = Count, fill = Type)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Variable Types Distribution", x = "Data Type", y = "Count") +
      theme_minimal()

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = TRUE)
  })

  # --- Download Handlers ---
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("variable_plot_", input$variable_select, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste0("analysis_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # This would require additional PDF generation logic
      # For now, create a simple text report
      report_content <- paste(
        "780 Data Explorer - Analysis Report",
        paste("Generated:", Sys.time()),
        paste("Variable analyzed:", input$variable_select),
        paste("Total observations:", nrow(filtered_data())),
        "",
        "Summary Statistics:",
        capture.output(summary(filtered_data()[[input$variable_select]])),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )

  output$download_bivariate_plot <- downloadHandler(
    filename = function() {
      paste0("bivariate_plot_", input$var_x, "_vs_", input$var_y, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_bivariate_data <- downloadHandler(
    filename = function() {
      paste0("bivariate_data_", input$var_x, "_", input$var_y, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      bivariate_data <- filtered_data()[, c(input$var_x, input$var_y)]
      write.csv(bivariate_data, file, row.names = FALSE)
    }
  )

  # --- Event handlers ---

  # Reset filters
  observeEvent(input$reset_filters, {
    if ("gender" %in% names(all_patient_data)) {
      updateCheckboxGroupInput(session, "gender_filter",
                              selected = unique(na.omit(all_patient_data$gender)))
    }
    if ("age" %in% names(all_patient_data)) {
      updateSliderInput(session, "age_filter",
                       value = c(floor(min(all_patient_data$age, na.rm = TRUE)),
                                ceiling(max(all_patient_data$age, na.rm = TRUE))))
    }

    session$sendCustomMessage("showNotification", "Filters have been reset")
  })

  # Tour and help buttons
  observeEvent(input$tour_btn, {
    showModal(modalDialog(
      title = "Welcome to 780 Data Explorer!",
      HTML("
      <h4>Quick Tour:</h4>
      <ul>
        <li><strong>Patient Lookup:</strong> Search and view individual patient data</li>
        <li><strong>Variable Distribution:</strong> Explore data distributions with filters</li>
        <li><strong>Bivariate Analysis:</strong> Analyze relationships between variables</li>
        <li><strong>Compare Patients:</strong> Side-by-side patient comparison</li>
        <li><strong>Analytics Dashboard:</strong> Overview of data quality and usage</li>
      </ul>
      <h4>Features:</h4>
      <ul>
        <li>🌙 Dark/Light theme toggle (top-right)</li>
        <li>📊 Interactive plots with zoom and pan</li>
        <li>📁 Export data and plots</li>
        <li>🔍 Advanced filtering options</li>
        <li>📈 Statistical analysis with interpretations</li>
      </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("Got it!")
    ))
  })

  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Help & Documentation",
      HTML("
      <h4>Need Help?</h4>
      <p>This application provides comprehensive tools for exploring clinical data:</p>

      <h5>Statistical Terms:</h5>
      <ul>
        <li><strong>Correlation:</strong> Measures linear relationship strength (-1 to +1)</li>
        <li><strong>P-value:</strong> Probability that results occurred by chance</li>
        <li><strong>Outliers:</strong> Values outside the normal range (default: 10th/90th percentile)</li>
        <li><strong>IQR:</strong> Interquartile Range (difference between 25th and 75th percentiles)</li>
      </ul>

      <h5>Data Quality Indicators:</h5>
      <ul>
        <li>🟢 High quality: >90% complete data</li>
        <li>🟡 Medium quality: 70-90% complete data</li>
        <li>🔴 Low quality: <70% complete data</li>
      </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)