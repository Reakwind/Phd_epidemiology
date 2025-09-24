# --- 1. Load required libraries (minimal set) ---
library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)

# --- Pre-computation Step ---
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

# --- Data Cleaning ---
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

# --- 2. UI Definition ---
ui <- fluidPage(
  theme = shinytheme("cosmo"),

  # Simple custom CSS
  tags$head(
    tags$style(HTML("
      .header-section {
        background: linear-gradient(135deg, #2c3e50, #3498db);
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 5px;
      }

      .info-box {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
      }

      .stat-summary {
        background: white;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),

  # Header
  div(class = "header-section",
    h1("780 Data Explorer", style = "margin: 0;"),
    h4(paste("Dataset:", nrow(all_patient_data), "patients,", ncol(all_patient_data), "variables"),
       style = "margin: 5px 0 0 0; opacity: 0.9;")
  ),

  # Navigation breadcrumb
  div(class = "info-box",
    textOutput("current_section")
  ),

  # Main layout
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("sidebar_content")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        tabPanel("Patient Lookup",
          value = "patient_lookup",
          br(),
          uiOutput("patient_ui")
        ),

        tabPanel("Variable Analysis",
          value = "variable_analysis",
          br(),
          fluidRow(
            column(8,
              withSpinner(plotlyOutput("var_plot", height = "450px"))
            ),
            column(4,
              div(class = "stat-summary",
                h4("Statistics"),
                tableOutput("var_stats")
              ),
              div(class = "info-box",
                h4("Export"),
                downloadButton("dl_plot", "Download Plot", class = "btn-primary btn-sm"),
                br(), br(),
                downloadButton("dl_data", "Download Data", class = "btn-secondary btn-sm")
              )
            )
          )
        ),

        tabPanel("Two Variables",
          value = "bivariate",
          br(),
          fluidRow(
            column(8,
              withSpinner(plotlyOutput("biv_plot", height = "450px"))
            ),
            column(4,
              div(class = "stat-summary",
                h4("Correlation"),
                verbatimTextOutput("correlation")
              ),
              div(class = "info-box",
                h4("Export"),
                downloadButton("dl_biv_plot", "Download Plot", class = "btn-primary btn-sm"),
                br(), br(),
                downloadButton("dl_biv_data", "Download Data", class = "btn-secondary btn-sm")
              )
            )
          )
        ),

        tabPanel("Data Quality",
          value = "data_quality",
          br(),
          div(class = "stat-summary",
            h3("Dataset Overview"),
            DT::dataTableOutput("quality_table")
          )
        )
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {

  # Current section display
  output$current_section <- renderText({
    section_names <- list(
      "patient_lookup" = "Patient Lookup",
      "variable_analysis" = "Variable Analysis",
      "bivariate" = "Two Variables Analysis",
      "data_quality" = "Data Quality Overview"
    )

    current <- section_names[[input$main_tabs]] %||% "Patient Lookup"
    paste("Current Section:", current)
  })

  # Dynamic sidebar
  output$sidebar_content <- renderUI({
    if (input$main_tabs == "patient_lookup") {
      tagList(
        h4("Search Patient"),
        selectizeInput("patient_id", "Client Name:",
                       choices = c("", client_names),
                       options = list(placeholder = "Type to search...")),
        radioButtons("patient_view", "Show:",
                     choices = c("All data" = "all",
                                "Missing only" = "missing",
                                "Outliers only" = "outliers"),
                     selected = "all"),
        actionButton("search_btn", "Search", class = "btn-primary", style = "width: 100%;"),
        br(), br(),
        div(class = "info-box",
          strong("Total Patients: "), nrow(all_patient_data)
        )
      )

    } else if (input$main_tabs == "variable_analysis") {
      tagList(
        h4("Select Variable"),
        selectizeInput("variable", "Choose Variable:",
                       choices = all_variable_names,
                       selected = "age",
                       options = list(placeholder = "Type to search...")),

        h5("Filters"),
        if ("gender" %in% names(all_patient_data)) {
          checkboxGroupInput("gender_filt", "Gender:",
                           choices = unique(na.omit(all_patient_data$gender)),
                           selected = unique(na.omit(all_patient_data$gender)),
                           inline = TRUE)
        },

        if ("age" %in% names(all_patient_data)) {
          sliderInput("age_range", "Age Range:",
                     min = floor(min(all_patient_data$age, na.rm = TRUE)),
                     max = ceiling(max(all_patient_data$age, na.rm = TRUE)),
                     value = c(floor(min(all_patient_data$age, na.rm = TRUE)),
                              ceiling(max(all_patient_data$age, na.rm = TRUE))))
        },

        actionButton("reset_btn", "Reset Filters", class = "btn-outline-secondary btn-sm")
      )

    } else if (input$main_tabs == "bivariate") {
      tagList(
        h4("Select Variables"),
        selectizeInput("var_x", "X Variable:",
                       choices = all_variable_names,
                       selected = "age"),
        selectizeInput("var_y", "Y Variable:",
                       choices = all_variable_names,
                       selected = "moca_total_score_v1_v1"),

        h5("Options"),
        checkboxInput("show_line", "Show trend line", TRUE),

        if ("gender" %in% names(all_patient_data)) {
          checkboxGroupInput("gender_biv", "Gender Filter:",
                           choices = unique(na.omit(all_patient_data$gender)),
                           selected = unique(na.omit(all_patient_data$gender)),
                           inline = TRUE)
        }
      )

    } else if (input$main_tabs == "data_quality") {
      div(class = "info-box",
        h4("Dataset Info"),
        p(paste("Patients:", nrow(all_patient_data))),
        p(paste("Variables:", ncol(all_patient_data))),
        p(paste("Numeric:", length(numeric_variable_names)))
      )
    }
  })

  # Patient lookup logic
  patient_result <- eventReactive(input$search_btn, {
    req(input$patient_id)
    if (input$patient_id == "") {
      return(data.frame(Variable = "Error", Value = "Please select a patient"))
    }

    patient_data <- all_patient_data[all_patient_data$client_name == input$patient_id, ]
    if (nrow(patient_data) == 0) {
      return(data.frame(Variable = "Error", Value = "Patient not found"))
    }

    # Transpose data
    result <- as.data.frame(t(patient_data))
    colnames(result) <- "Value"
    result$Variable <- rownames(result)
    result <- result[, c("Variable", "Value")]
    result$Status <- "Normal"

    # Check for outliers and missing
    for (i in 1:nrow(result)) {
      var_name <- result$Variable[i]
      value <- result$Value[i]

      if (is.na(value)) {
        result$Status[i] <- "Missing"
        next
      }

      if (var_name %in% names(quantile_thresholds)) {
        thresholds <- quantile_thresholds[[var_name]]
        numeric_value <- as.numeric(value)

        if (!is.na(numeric_value)) {
          if (numeric_value < thresholds[1]) {
            result$Status[i] <- "Low"
          } else if (numeric_value > thresholds[2]) {
            result$Status[i] <- "High"
          }
        }
      }
    }

    # Filter based on view option
    if (input$patient_view == "missing") {
      return(result[result$Status == "Missing", c("Variable", "Value")])
    } else if (input$patient_view == "outliers") {
      return(result[result$Status %in% c("Low", "High"), ])
    } else {
      return(result)
    }
  })

  # Patient UI
  output$patient_ui <- renderUI({
    if (input$search_btn == 0) {
      div(class = "stat-summary", style = "text-align: center; padding: 40px;",
        icon("user", style = "font-size: 60px; color: #3498db;"),
        h3("Patient Search"),
        p("Select a patient from the sidebar to view their data."),
        p("You can filter to show all data, missing values, or outliers.")
      )
    } else {
      div(class = "stat-summary",
        h3(paste("Patient:", input$patient_id)),
        DT::dataTableOutput("patient_table")
      )
    }
  })

  output$patient_table <- DT::renderDataTable({
    patient_result()
  }, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)

  # Filtered data for analysis
  filtered_data <- reactive({
    data <- all_patient_data

    if (input$main_tabs == "variable_analysis") {
      # Apply gender filter
      if ("gender" %in% names(data) && !is.null(input$gender_filt)) {
        data <- data[data$gender %in% input$gender_filt | is.na(data$gender), ]
      }

      # Apply age filter
      if ("age" %in% names(data) && !is.null(input$age_range)) {
        data <- data[is.na(data$age) | (data$age >= input$age_range[1] & data$age <= input$age_range[2]), ]
      }
    } else if (input$main_tabs == "bivariate") {
      # Apply gender filter for bivariate
      if ("gender" %in% names(data) && !is.null(input$gender_biv)) {
        data <- data[data$gender %in% input$gender_biv | is.na(data$gender), ]
      }
    }

    return(data)
  })

  # Variable plot
  output$var_plot <- renderPlotly({
    req(input$variable)

    plot_data <- data.frame(value = filtered_data()[[input$variable]])
    plot_data <- na.omit(plot_data)
    n_obs <- nrow(plot_data)

    is_numeric <- is.numeric(plot_data$value) && length(unique(plot_data$value)) > 10

    title <- paste("Distribution of", input$variable, "(n =", n_obs, ")")

    if (is_numeric) {
      p <- ggplot(plot_data, aes(x = value)) +
        geom_histogram(bins = 30, fill = "#3498db", alpha = 0.7) +
        labs(title = title, x = input$variable, y = "Count") +
        theme_minimal()
    } else {
      p <- ggplot(plot_data, aes(x = factor(value))) +
        geom_bar(fill = "#3498db", alpha = 0.7) +
        labs(title = title, x = input$variable, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    ggplotly(p)
  })

  # Variable statistics
  output$var_stats <- renderTable({
    req(input$variable)

    data_col <- filtered_data()[[input$variable]]

    if (is.numeric(data_col)) {
      data.frame(
        Measure = c("Count", "Missing", "Mean", "Median", "SD", "Min", "Max"),
        Value = c(
          sum(!is.na(data_col)),
          sum(is.na(data_col)),
          round(mean(data_col, na.rm = TRUE), 2),
          round(median(data_col, na.rm = TRUE), 2),
          round(sd(data_col, na.rm = TRUE), 2),
          round(min(data_col, na.rm = TRUE), 2),
          round(max(data_col, na.rm = TRUE), 2)
        )
      )
    } else {
      freq <- table(data_col, useNA = "ifany")
      data.frame(
        Category = names(freq),
        Count = as.numeric(freq)
      )
    }
  }, striped = TRUE)

  # Bivariate plot
  output$biv_plot <- renderPlotly({
    req(input$var_x, input$var_y)

    plot_data <- filtered_data()[, c(input$var_x, input$var_y)]
    plot_data <- na.omit(plot_data)
    n_obs <- nrow(plot_data)

    x_numeric <- is.numeric(plot_data[[input$var_x]])
    y_numeric <- is.numeric(plot_data[[input$var_y]])

    title <- paste(input$var_y, "vs", input$var_x, "(n =", n_obs, ")")

    if (x_numeric && y_numeric) {
      p <- ggplot(plot_data, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
        geom_point(alpha = 0.6, color = "#3498db")

      if (input$show_line) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "#e74c3c")
      }
    } else if (x_numeric && !y_numeric) {
      p <- ggplot(plot_data, aes(x = factor(.data[[input$var_y]]), y = .data[[input$var_x]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7)
    } else if (!x_numeric && y_numeric) {
      p <- ggplot(plot_data, aes(x = factor(.data[[input$var_x]]), y = .data[[input$var_y]])) +
        geom_boxplot(fill = "#3498db", alpha = 0.7)
    } else {
      p <- ggplot(plot_data, aes(x = factor(.data[[input$var_x]]), y = factor(.data[[input$var_y]]))) +
        geom_jitter(alpha = 0.5, color = "#9b59b6")
    }

    p <- p + labs(title = title, x = input$var_x, y = input$var_y) + theme_minimal()

    ggplotly(p)
  })

  # Correlation info
  output$correlation <- renderText({
    req(input$var_x, input$var_y)

    x_data <- filtered_data()[[input$var_x]]
    y_data <- filtered_data()[[input$var_y]]

    if (is.numeric(x_data) && is.numeric(y_data)) {
      complete_data <- na.omit(data.frame(x = x_data, y = y_data))

      if (nrow(complete_data) > 3) {
        cor_val <- cor(complete_data$x, complete_data$y)
        cor_test <- cor.test(complete_data$x, complete_data$y)

        paste0(
          "Correlation: ", round(cor_val, 3), "\n",
          "P-value: ", format(cor_test$p.value, digits = 3), "\n",
          "Sample size: ", nrow(complete_data), "\n\n",
          "Strength: ",
          ifelse(abs(cor_val) > 0.7, "Strong",
                ifelse(abs(cor_val) > 0.3, "Moderate", "Weak"))
        )
      } else {
        "Need more data points"
      }
    } else {
      "Requires numeric variables"
    }
  })

  # Data quality table
  output$quality_table <- DT::renderDataTable({
    quality_data <- data.frame(
      Variable = names(all_patient_data),
      Type = sapply(all_patient_data, function(x) class(x)[1]),
      Missing = sapply(all_patient_data, function(x) sum(is.na(x))),
      Complete_Pct = sapply(all_patient_data, function(x) round(100 * (1 - sum(is.na(x))/length(x)), 1)),
      Unique = sapply(all_patient_data, function(x) length(unique(na.omit(x)))),
      stringsAsFactors = FALSE
    )

    quality_data$Quality <- ifelse(quality_data$Complete_Pct >= 90, "High",
                                  ifelse(quality_data$Complete_Pct >= 70, "Medium", "Low"))

    return(quality_data)
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)

  # Download handlers
  output$dl_plot <- downloadHandler(
    filename = function() paste0("plot_", input$variable, "_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, last_plot(), width = 10, height = 6)
  )

  output$dl_data <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )

  output$dl_biv_plot <- downloadHandler(
    filename = function() paste0("bivariate_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, last_plot(), width = 10, height = 6)
  )

  output$dl_biv_data <- downloadHandler(
    filename = function() paste0("bivariate_data_", Sys.Date(), ".csv"),
    content = function(file) {
      biv_data <- filtered_data()[, c(input$var_x, input$var_y)]
      write.csv(biv_data, file, row.names = FALSE)
    }
  )

  # Reset filters
  observeEvent(input$reset_btn, {
    if ("gender" %in% names(all_patient_data)) {
      updateCheckboxGroupInput(session, "gender_filt",
                              selected = unique(na.omit(all_patient_data$gender)))
    }
    if ("age" %in% names(all_patient_data)) {
      updateSliderInput(session, "age_range",
                       value = c(floor(min(all_patient_data$age, na.rm = TRUE)),
                                ceiling(max(all_patient_data$age, na.rm = TRUE))))
    }
  })
}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)