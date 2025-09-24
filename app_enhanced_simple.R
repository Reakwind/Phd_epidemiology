# --- 1. Load required libraries ---
library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)          # For creating high-quality plots
library(dplyr)            # For data manipulation
library(shinythemes)      # For professional UI themes
library(shinycssloaders)  # For loading animations
library(plotly)           # For interactive plots
library(DT)               # For enhanced data tables

# --- Pre-computation Step (runs only once when the app starts) ---

# Connect to the DB and load ALL data with error handling
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

# Now, we proceed with the cleaned data
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

# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  theme = shinytheme("cosmo"), # Apply a professional theme

  titlePanel(div(
    h1("780 Data Explorer - Enhanced", style = "color: #2c3e50; font-weight: bold;"),
    h4(paste("Dataset:", nrow(all_patient_data), "patients with", ncol(all_patient_data), "variables"),
       style = "color: #7f8c8d; font-weight: normal;")
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("conditional_sidebar")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs",
                  tabPanel("Patient Lookup",
                           icon = icon("user"),
                           uiOutput("patient_lookup_ui")),

                  tabPanel("Variable Distribution",
                           icon = icon("chart-bar"),
                           fluidRow(
                             column(8, withSpinner(plotlyOutput("variable_plot", height = "500px"))),
                             column(4,
                                    wellPanel(
                                        h4("Summary Statistics"),
                                        tableOutput("variable_summary")
                                    ),
                                    wellPanel(
                                        h4("Export Options"),
                                        downloadButton("download_plot", "Download Plot", class = "btn-primary"),
                                        br(), br(),
                                        downloadButton("download_data", "Download Data", class = "btn-secondary")
                                    )
                             )
                           )),

                  tabPanel("Bivariate Analysis",
                           icon = icon("chart-line"),
                           fluidRow(
                             column(8, withSpinner(plotlyOutput("bivariate_plot", height = "500px"))),
                             column(4,
                                    wellPanel(
                                        h4("Correlation Info"),
                                        verbatimTextOutput("correlation_info")
                                    ),
                                    wellPanel(
                                        h4("Export Options"),
                                        downloadButton("download_bivariate_plot", "Download Plot", class = "btn-primary"),
                                        br(), br(),
                                        downloadButton("download_bivariate_data", "Download Data", class = "btn-secondary")
                                    )
                             )
                           )),

                  tabPanel("Data Overview",
                           icon = icon("table"),
                           fluidRow(
                             column(12,
                                    h3("Dataset Summary"),
                                    wellPanel(
                                        DT::dataTableOutput("data_overview_table")
                                    )
                             )
                           ))
      )
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output) {

  # --- A. Conditional Sidebar UI ---
  output$conditional_sidebar <- renderUI({
    if (input$main_tabs == "Patient Lookup") {
      tagList(
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
        actionButton("search_button", "Search", icon = icon("search"), class = "btn-primary"),
        br(), br(),
        wellPanel(
            h5("Quick Stats"),
            textOutput("patient_count")
        )
      )
    } else if (input$main_tabs == "Variable Distribution") {
      tagList(
        h4("Variable Analysis", style = "color: #2c3e50;"),
        selectInput("variable_select", "Select Variable:",
                    choices = all_variable_names, selected = "age"),
        hr(),
        h5("Filters"),
        if ("gender" %in% names(all_patient_data)) {
          checkboxGroupInput("gender_filter", "Gender:",
                           choices = unique(na.omit(all_patient_data$gender)),
                           selected = unique(na.omit(all_patient_data$gender)))
        },
        if ("age" %in% names(all_patient_data)) {
          sliderInput("age_filter", "Age Range:",
                     min = min(all_patient_data$age, na.rm = TRUE),
                     max = max(all_patient_data$age, na.rm = TRUE),
                     value = c(min(all_patient_data$age, na.rm = TRUE),
                              max(all_patient_data$age, na.rm = TRUE)))
        }
      )
    } else if (input$main_tabs == "Bivariate Analysis") {
      tagList(
        h4("Two-Variable Analysis", style = "color: #2c3e50;"),
        selectInput("var_x", "X-axis Variable:",
                    choices = all_variable_names, selected = "age"),
        selectInput("var_y", "Y-axis Variable:",
                    choices = all_variable_names, selected = "moca_total_score_v1_v1"),
        hr(),
        h5("Filters"),
        if ("gender" %in% names(all_patient_data)) {
          checkboxGroupInput("gender_filter_biv", "Gender:",
                           choices = unique(na.omit(all_patient_data$gender)),
                           selected = unique(na.omit(all_patient_data$gender)))
        }
      )
    } else if (input$main_tabs == "Data Overview") {
      tagList(
        h4("Dataset Information", style = "color: #2c3e50;"),
        wellPanel(
            h5("Dataset Summary"),
            p(paste("Total Patients:", nrow(all_patient_data))),
            p(paste("Total Variables:", ncol(all_patient_data))),
            p(paste("Numeric Variables:", length(numeric_variable_names)))
        )
      )
    }
  })

  # --- Patient count output ---
  output$patient_count <- renderText({
    paste("Total patients in database:", nrow(all_patient_data))
  })

  # --- Filtered data reactive ---
  filtered_data <- reactive({
    data <- all_patient_data

    # Apply gender filter if available
    if (input$main_tabs %in% c("Variable Distribution", "Bivariate Analysis")) {
      if ("gender" %in% names(data)) {
        gender_input <- if (input$main_tabs == "Variable Distribution") input$gender_filter else input$gender_filter_biv
        if (!is.null(gender_input) && length(gender_input) > 0) {
          data <- data[data$gender %in% gender_input | is.na(data$gender), ]
        }
      }

      # Apply age filter if available
      if (input$main_tabs == "Variable Distribution" && "age" %in% names(data) && !is.null(input$age_filter)) {
        data <- data[is.na(data$age) | (data$age >= input$age_filter[1] & data$age <= input$age_filter[2]), ]
      }
    }

    return(data)
  })

  # --- B. Patient Lookup Logic ---
  search_result <- eventReactive(input$search_button, {
    req(input$client_id_input)
    if (input$client_id_input == "") {
      return(data.frame(Status = "Please select a client name."))
    }
    client_data <- all_patient_data[all_patient_data$client_name == input$client_id_input, ]
    if (nrow(client_data) == 0) return(data.frame(Status = "No data found for this client."))
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

  # Dynamic UI for patient lookup tab (shows guidance or results)
  output$patient_lookup_ui <- renderUI({
    if (input$search_button == 0) {
      div(
        h4("Welcome to the Patient Lookup Tool"),
        p("Please enter a client name in the sidebar and click 'Search' to view their data.")
      )
    } else {
      tagList(
        h4(textOutput("client_header")),
        withSpinner(tableOutput("client_data_table"))
      )
    }
  })

  output$client_header <- renderText({
    req(input$search_button > 0)
    paste("Showing data for client:", input$client_id_input, "(n=1)")
  })
  output$client_data_table <- renderTable({
    search_result()
  }, striped = TRUE, hover = TRUE, width = "100%")

  # --- C. Variable Distribution Plotting Logic ---
  output$variable_plot <- renderPlotly({
    req(input$variable_select)
    selected_var <- input$variable_select
    plot_data <- data.frame(value = filtered_data()[[selected_var]])
    n_obs <- sum(!is.na(plot_data$value))
    plot_data <- na.omit(plot_data)
    is_continuous <- is.numeric(plot_data$value) && length(unique(plot_data$value)) > 10

    plot_title <- paste("Distribution of", selected_var, "<br><i>n =", n_obs, "observations</i>")

    if (is_continuous) {
      p <- ggplot(plot_data, aes(x = .data[["value"]])) +
        geom_histogram(bins = 30, aes(text = ..count..), fill = "steelblue", color = "white") +
        labs(title = plot_title, x = selected_var, y = "Frequency") +
        theme_minimal(base_size = 14)
    } else {
      p <- ggplot(plot_data, aes(x = factor(.data[["value"]]))) +
        geom_bar(aes(text = ..count..), fill = "skyblue", color = "black") +
        labs(title = plot_title, x = selected_var, y = "Count") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplotly(p, tooltip = "text")
  })

  # --- D. Bivariate Analysis Plotting Logic ---
  output$bivariate_plot <- renderPlotly({
    req(input$var_x, input$var_y)
    x_var <- input$var_x
    y_var <- input$var_y

    plot_data <- filtered_data()[, c(x_var, y_var)]

    # Explicitly remove rows with NA in *either* selected column before plotting
    plot_data <- na.omit(plot_data)

    n_obs <- nrow(plot_data) # Recalculate n with complete data

    x_is_num <- is.numeric(plot_data[[x_var]])
    y_is_num <- is.numeric(plot_data[[y_var]])

    plot_title <- paste(y_var, "vs.", x_var, "<br><i>n =", n_obs, "complete observations</i>")

    if (x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(method = "lm", se = FALSE, color = "red", formula = 'y ~ x') +
        labs(title = plot_title, x = x_var, y = y_var) +
        theme_minimal(base_size = 14)
    } else if (x_is_num && !y_is_num) {
      p <- ggplot(plot_data, aes(x = .data[[y_var]], y = .data[[x_var]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = plot_title, x = y_var, y = x_var) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (!x_is_num && y_is_num) {
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = plot_title, x = x_var, y = y_var) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_jitter(alpha = 0.5, color = "purple", width = 0.2, height = 0.2) +
        labs(title = plot_title, x = x_var, y = y_var) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplotly(p)
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
        paste0(
          "Correlation: ", round(cor_val, 3), "\n",
          "P-value: ", format(cor_test$p.value, scientific = TRUE, digits = 3), "\n",
          "Sample size: ", nrow(complete_data), "\n",
          "Interpretation: ",
          ifelse(abs(cor_val) > 0.7, "Strong",
                ifelse(abs(cor_val) > 0.3, "Moderate", "Weak")),
          " ", ifelse(cor_val > 0, "positive", "negative"), " correlation"
        )
      } else {
        "Insufficient data for correlation analysis"
      }
    } else {
      "Correlation analysis requires numeric variables"
    }
  })

  # --- Data Overview Table ---
  output$data_overview_table <- DT::renderDataTable({
    result <- data.frame(
      Variable = names(all_patient_data),
      Type = sapply(all_patient_data, function(x) class(x)[1]),
      Missing = sapply(all_patient_data, function(x) sum(is.na(x))),
      `Missing_Percent` = sapply(all_patient_data, function(x) round(100 * sum(is.na(x)) / length(x), 1)),
      Unique = sapply(all_patient_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    names(result)[4] <- "Missing %"

    return(result)
  }, options = list(
    pageLength = 25,
    scrollX = TRUE
  ))

  # --- Download Handlers ---
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("variable_plot_", input$variable_select, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 10, height = 6, dpi = 300)
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

  output$download_bivariate_plot <- downloadHandler(
    filename = function() {
      paste0("bivariate_plot_", input$var_x, "_vs_", input$var_y, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 10, height = 6, dpi = 300)
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

}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)