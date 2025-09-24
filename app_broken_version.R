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
  theme = shinytheme("cosmo"),

  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      .main-header {
        background: linear-gradient(135deg, #2c3e50, #34495e);
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
        border-left: 4px solid #3498db;
        transition: transform 0.2s;
      }

      .stat-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 20px rgba(0,0,0,0.12);
      }

      .filter-section {
        background: #f8f9fa;
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

      .help-tooltip {
        color: #3498db;
        cursor: help;
        margin-left: 5px;
      }

      .breadcrumb-nav {
        background: #f8f9fa;
        border-radius: 6px;
        padding: 0.75rem 1rem;
        margin-bottom: 1rem;
        font-size: 0.9rem;
        color: #6c757d;
      }

      .recent-selections {
        background: #f8f9fa;
        border-radius: 6px;
        padding: 0.75rem;
        margin: 0.5rem 0;
        font-size: 0.85rem;
      }

      .comparison-row {
        background: #fff3cd;
        border-left: 4px solid #ffc107;
      }

      @media (max-width: 768px) {
        .main-header {
          padding: 1rem;
          text-align: center;
        }
        .stat-card {
          margin: 1rem 0;
        }
      }

      .quality-high { color: #28a745; font-weight: bold; }
      .quality-medium { color: #ffc107; font-weight: bold; }
      .quality-low { color: #dc3545; font-weight: bold; }
    "))
  ),

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
          actionButton("help_btn", "Help & Guide", icon = icon("info-circle"),
                      class = "btn btn-outline-light")
        )
      )
    )
  ),

  # Breadcrumb navigation
  div(class = "breadcrumb-nav", textOutput("breadcrumb_text")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
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
              withSpinner(
                plotlyOutput("variable_plot", height = "500px"),
                type = 1,
                color = "#3498db"
              )
            ),
            column(4,
              div(class = "stat-card",
                h4("Summary Statistics",
                   span(class = "help-tooltip", title = "Statistical measures",
                        icon("question-circle"))),
                tableOutput("variable_summary")
              ),
              div(class = "export-section",
                h4("Export Options"),
                downloadButton("download_plot", "Download Plot (PNG)",
                             class = "btn-primary btn-sm", icon = icon("download"),
                             style = "margin: 5px; width: 100%;"),
                br(),
                downloadButton("download_data", "Download Data (CSV)",
                             class = "btn-secondary btn-sm", icon = icon("file-csv"),
                             style = "margin: 5px; width: 100%;")
              ),
              div(class = "recent-selections",
                h5("Recent Variables"),
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
                type = 1,
                color = "#3498db"
              )
            ),
            column(4,
              div(class = "stat-card",
                h4("Correlation Analysis"),
                verbatimTextOutput("correlation_info")
              ),
              div(class = "export-section",
                h4("Export Options"),
                downloadButton("download_bivariate_plot", "Download Plot (PNG)",
                             class = "btn-primary btn-sm", icon = icon("download"),
                             style = "margin: 5px; width: 100%;"),
                br(),
                downloadButton("download_bivariate_data", "Download Data (CSV)",
                             class = "btn-secondary btn-sm", icon = icon("file-csv"),
                             style = "margin: 5px; width: 100%;")
              )
            )
          )
        ),

        tabPanel("Data Overview",
          icon = icon("table"),
          value = "data_overview",
          br(),
          div(class = "stat-card",
            h3("Dataset Quality Dashboard"),
            DT::dataTableOutput("data_quality_table")
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
            h4("Side-by-Side Comparison"),
            DT::dataTableOutput("patient_comparison")
          )
        )
      )
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {

  # Reactive values for state management
  values <- reactiveValues(
    recent_variables = c(),
    recent_patients = c()
  )

  # --- Breadcrumb navigation ---
  output$breadcrumb_text <- renderText({
    tab_names <- list(
      "patient_lookup" = "Patient Lookup",
      "variable_distribution" = "Variable Distribution",
      "bivariate_analysis" = "Bivariate Analysis",
      "data_overview" = "Data Overview",
      "compare_patients" = "Compare Patients"
    )

    current_name <- tab_names[[input$main_tabs]] %||% "Patient Lookup"
    paste("Home /", current_name)
  })

  # --- Enhanced Conditional Sidebar UI ---
  output$conditional_sidebar <- renderUI({
    if (input$main_tabs == "patient_lookup") {
      tagList(
        div(class = "filter-section",
          h4("Patient Search", style = "color: #2c3e50;"),
          selectizeInput("client_id_input", "Select Client Name:",
                         choices = c("", client_names),
                         options = list(
                           placeholder = "Start typing client name...",
                           maxOptions = 50,
                           searchField = c('value', 'text')
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
          p(paste("Total patients:", nrow(all_patient_data))),
          h6("Recently Viewed:"),
          textOutput("recent_patients_list")
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

          h5("Data Filters"),
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
                                ceiling(max(all_patient_data$age, na.rm = TRUE))))
          },

          actionButton("reset_filters", "Reset All Filters",
                      icon = icon("refresh"), class = "btn-outline-secondary",
                      style = "width: 100%; margin-top: 10px;")
        ),

        div(class = "stat-card",
          h5("Plot Settings"),
          conditionalPanel(
            condition = "input.variable_select != null",
            numericInput("bins", "Histogram bins:",
                        value = 30, min = 5, max = 100, step = 5),
            checkboxInput("show_density", "Show density overlay", FALSE)
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
          checkboxInput("show_confidence", "Show confidence interval", FALSE)
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
        p(paste("Average Completeness:", round(mean(data_quality$Complete), 1), "%"))
      )
    }
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

  # --- Filtered data reactive ---
  filtered_data <- reactive({
    data <- all_patient_data

    if (input$main_tabs %in% c("variable_distribution", "bivariate_analysis")) {
      if ("gender" %in% names(data)) {
        gender_input <- if (input$main_tabs == "variable_distribution") input$gender_filter else input$gender_filter_biv
        if (!is.null(gender_input) && length(gender_input) > 0) {
          data <- data[data$gender %in% gender_input | is.na(data$gender), ]
        }
      }

      if (input$main_tabs == "variable_distribution" && "age" %in% names(data) && !is.null(input$age_filter)) {
        data <- data[is.na(data$age) | (data$age >= input$age_filter[1] & data$age <= input$age_filter[2]), ]
      }
    }

    return(data)
  })

  # --- Enhanced Patient Lookup Logic ---
  search_result <- eventReactive(input$search_button, {
    req(input$client_id_input)
    if (input$client_id_input == "") {
      return(data.frame(Variable = "Status", Value = "Please select a client name."))
    }

    values$recent_patients <- unique(c(values$recent_patients, input$client_id_input))

    client_data <- all_patient_data[all_patient_data$client_name == input$client_id_input, ]
    if (nrow(client_data) == 0) return(data.frame(Variable = "Status", Value = "No data found for this client."))

    transposed_data <- as.data.frame(t(client_data))
    colnames(transposed_data) <- "Value"
    transposed_data$Variable <- rownames(transposed_data)
    transposed_data <- transposed_data[, c("Variable", "Value")]
    transposed_data$Status <- "Normal"

    for (i in 1:nrow(transposed_data)) {
      var_name <- transposed_data$Variable[i]
      value <- transposed_data$Value[i]

      if (is.na(value)) {
        transposed_data$Status[i] <- "Missing"
        next
      }

      if (var_name %in% names(quantile_thresholds)) {
        thresholds <- quantile_thresholds[[var_name]]
        q1 <- thresholds[1]
        q3 <- thresholds[2]
        numeric_value <- as.numeric(value)

        if (!is.na(numeric_value)) {
          if (numeric_value < q1) {
            transposed_data$Status[i] <- "Low (below 10%)"
          } else if (numeric_value > q3) {
            transposed_data$Status[i] <- "High (above 90%)"
          }
        }
      }
    }

    view_type <- input$view_option
    if (view_type == "missing") {
      return(transposed_data[transposed_data$Status == "Missing", c("Variable", "Value")])
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
        p("Select a client name from the sidebar and click 'Search' to view their data."),
        p("You can filter to show all data, missing values only, or statistical outliers.")
      )
    } else {
      div(class = "stat-card",
        h4(paste("Patient Profile:", input$client_id_input)),
        DT::dataTableOutput("client_data_table")
      )
    }
  })

  output$client_data_table <- DT::renderDataTable({
    search_result()
  }, options = list(
    pageLength = 15,
    scrollY = "400px",
    scrollX = TRUE
  ), rownames = FALSE)

  # Track variable selection
  observe({
    if (!is.null(input$variable_select) && input$variable_select != "") {
      values$recent_variables <- unique(c(values$recent_variables, input$variable_select))
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

    is_continuous <- is.numeric(plot_data$value) && length(unique(plot_data$value)) > 10

    plot_title <- paste("Distribution of", selected_var, "(n =", n_obs, "observations)")

    if (is_continuous) {
      p <- ggplot(plot_data, aes(x = value)) +
        geom_histogram(bins = bins, fill = "#3498db", color = "white", alpha = 0.7) +
        labs(title = plot_title, x = selected_var, y = "Frequency") +
        theme_minimal(base_size = 14)

      if (show_density) {
        p <- p + geom_density(aes(y = ..density.. * nrow(plot_data) *
                                 (max(plot_data$value, na.rm = TRUE) -
                                  min(plot_data$value, na.rm = TRUE)) / bins),
                             color = "#e74c3c", size = 1)
      }
    } else {
      p <- ggplot(plot_data, aes(x = factor(value))) +
        geom_bar(fill = "#3498db", color = "white", alpha = 0.7) +
        labs(title = plot_title, x = selected_var, y = "Count") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    ggplotly(p) %>% config(displayModeBar = TRUE)
  })

  # --- Bivariate Analysis ---
  output$bivariate_plot <- renderPlotly({
    req(input$var_x, input$var_y)
    x_var <- input$var_x
    y_var <- input$var_y

    plot_data <- filtered_data()[, c(x_var, y_var)]
    plot_data <- na.omit(plot_data)
    n_obs <- nrow(plot_data)

    x_is_num <- is.numeric(plot_data[[x_var]])
    y_is_num <- is.numeric(plot_data[[y_var]])

    plot_title <- paste(y_var, "vs.", x_var, "(n =", n_obs, "observations)")

    if (x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(alpha = 0.6, color = "#3498db", size = 2)

      if (input$show_regression %||% TRUE) {
        p <- p + geom_smooth(method = "lm",
                           se = input$show_confidence %||% FALSE,
                           color = "#e74c3c")
      }
    } else if (x_is_num && !y_is_num) {
      p <- ggplot(plot_data, aes(x = factor(.data[[y_var]]), y = .data[[x_var]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7)
    } else if (!x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7)
    } else {
      p <- ggplot(plot_data, aes(x = factor(.data[[x_var]]), y = factor(.data[[y_var]]))) +
        geom_jitter(alpha = 0.5, color = "#9b59b6", size = 2)
    }

    p <- p + labs(title = plot_title, x = x_var, y = y_var) +
         theme_minimal(base_size = 14)

    ggplotly(p) %>% config(displayModeBar = TRUE)
  })

  # --- Variable Summary Statistics ---
  output$variable_summary <- renderTable({
    req(input$variable_select)
    selected_var <- input$variable_select
    data_col <- filtered_data()[[selected_var]]

    if (is.numeric(data_col)) {
      stats <- data.frame(
        Statistic = c("Count", "Missing", "Mean", "Median", "Std Dev", "Min", "Max"),
        Value = c(
          sum(!is.na(data_col)),
          sum(is.na(data_col)),
          round(mean(data_col, na.rm = TRUE), 3),
          round(median(data_col, na.rm = TRUE), 3),
          round(sd(data_col, na.rm = TRUE), 3),
          round(min(data_col, na.rm = TRUE), 3),
          round(max(data_col, na.rm = TRUE), 3)
        )
      )
    } else {
      freq_table <- table(data_col, useNA = "ifany")
      stats <- data.frame(
        Category = names(freq_table),
        Count = as.numeric(freq_table)
      )
    }
    return(stats)
  }, striped = TRUE, hover = TRUE)

  # --- Correlation Information ---
  output$correlation_info <- renderText({
    req(input$var_x, input$var_y)
    x_data <- filtered_data()[[input$var_x]]
    y_data <- filtered_data()[[input$var_y]]

    if (is.numeric(x_data) && is.numeric(y_data)) {
      complete_data <- na.omit(data.frame(x = x_data, y = y_data))
      if (nrow(complete_data) > 3) {
        cor_val <- cor(complete_data$x, complete_data$y)
        cor_test <- cor.test(complete_data$x, complete_data$y)

        strength <- ifelse(abs(cor_val) > 0.7, "Strong",
                          ifelse(abs(cor_val) > 0.3, "Moderate", "Weak"))

        paste0(
          "Pearson Correlation: ", round(cor_val, 3), "\n",
          "P-value: ", format(cor_test$p.value, scientific = TRUE, digits = 3), "\n",
          "Sample size: ", nrow(complete_data), "\n\n",
          "Interpretation: ", strength, " ",
          ifelse(cor_val > 0, "positive", "negative"), " correlation\n",
          "R-squared: ", round(cor_val^2, 3), " (", round(100 * cor_val^2, 1), "% variance explained)"
        )
      } else {
        "Insufficient data for correlation analysis\n(Need at least 4 complete observations)"
      }
    } else {
      "Correlation analysis requires numeric variables"
    }
  })

  # --- Data Quality Table ---
  output$data_quality_table <- DT::renderDataTable({
    quality_data <- data_quality
    quality_data$Quality <- ifelse(quality_data$Complete >= 90, "High",
                                  ifelse(quality_data$Complete >= 70, "Medium", "Low"))
    quality_data$`Missing %` <- paste0(round(100 - quality_data$Complete, 1), "%")

    return(quality_data[, c("Variable", "Type", "Missing", "Missing %", "Complete", "Unique", "Quality")])
  }, options = list(
    pageLength = 25,
    scrollX = TRUE
  ), rownames = FALSE) %>%
  formatStyle("Quality",
    color = styleEqual(c("High", "Medium", "Low"),
                      c("#28a745", "#ffc107", "#dc3545")))

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

  output$patient_comparison <- DT::renderDataTable({
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
    return(data.frame(Variable = "Select both patients", `Patient A` = "", `Patient B` = "", Difference = ""))
  }, options = list(scrollX = TRUE), rownames = FALSE)

  # --- Download Handlers ---
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot_", input$variable_select, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 12, height = 8, dpi = 300)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  output$download_bivariate_plot <- downloadHandler(
    filename = function() {
      paste0("bivariate_", input$var_x, "_vs_", input$var_y, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 12, height = 8, dpi = 300)
    }
  )

  output$download_bivariate_data <- downloadHandler(
    filename = function() {
      paste0("bivariate_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      bivariate_data <- filtered_data()[, c(input$var_x, input$var_y)]
      write.csv(bivariate_data, file, row.names = FALSE)
    }
  )

  # --- Event handlers ---
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
  })

  # Help button
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "780 Data Explorer - Help Guide",
      HTML("
      <h4>Application Features:</h4>
      <ul>
        <li><strong>Patient Lookup:</strong> Search individual patient records with filtering options</li>
        <li><strong>Variable Distribution:</strong> Explore data distributions with interactive plots</li>
        <li><strong>Bivariate Analysis:</strong> Analyze relationships between two variables</li>
        <li><strong>Data Overview:</strong> View data quality metrics and completeness</li>
        <li><strong>Compare Patients:</strong> Side-by-side patient data comparison</li>
      </ul>

      <h4>Navigation:</h4>
      <ul>
        <li>Use the sidebar to select variables, patients, and adjust filters</li>
        <li>Interactive plots can be zoomed, panned, and exported</li>
        <li>Download buttons export plots as PNG and data as CSV</li>
        <li>Recent selections are tracked for convenience</li>
      </ul>

      <h4>Statistical Terms:</h4>
      <ul>
        <li><strong>Correlation:</strong> Measures relationship strength (-1 to +1)</li>
        <li><strong>P-value:</strong> Statistical significance (< 0.05 = significant)</li>
        <li><strong>Outliers:</strong> Values outside 10th-90th percentile range</li>
      </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("Got it!")
    ))
  })
}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)