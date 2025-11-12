library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(hexbin)

ui <- page_sidebar(
  title = "Hex Bin Plot from CSV Data",
  
  sidebar = sidebar(
    # File upload input
    fileInput("file", "Choose CSV File",
              accept = c(".csv", ".CSV")),
    
    # Help text
    helpText("Upload a CSV file with numeric columns for plotting."),
    
    # Conditional panel for column selection
    conditionalPanel(
      condition = "output.file_uploaded",
      hr(),
      h5("Select Columns:"),
      selectInput("x_col", "X Variable:", choices = NULL),
      selectInput("y_col", "Y Variable:", choices = NULL),
      
      hr(),
      h5("Filter Data:"),
      selectInput("filter_col", "Filter by Category:", 
                  choices = NULL,
                  selected = NULL),
      conditionalPanel(
        condition = "input.filter_col != ''",
        selectInput("filter_values", "Select Values:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        actionButton("clear_filter", "Clear All", class = "btn-outline-secondary btn-sm")
      ),
      
      hr(),
      
      selectInput("color_scale", "Color Scale:", 
                  choices = list("viridis" = "viridis", 
                                 "plasma" = "plasma",
                                 "inferno" = "inferno",
                                 "magma" = "magma"),
                  selected = "viridis")
      ),
    
      h5("Plot Options:"),
      sliderInput("bins", "Number of Bins:", value = 30, min = 10, max = 50, step = 5),
  ),
  
  # Main content area
  conditionalPanel(
    condition = "!output.file_uploaded",
    div(class = "text-center p-5",
        h4("Please upload a CSV file to begin"),
        p("Your file should contain numeric columns for x and y variables.")
    )
  ),
  
  conditionalPanel(
    condition = "output.file_uploaded",
    layout_columns(
      col_widths = c(6,6),
      
      card(
        card_header("Hex Bin Plot"),
        plotOutput("hex_plot", height = "400px"),
      ),
      
      card(
        card_header("Scatter Plot"),
        plotOutput("scatter_plot", height = "400px")
      )        
    ),
  ),
  
  # Data preview card
  conditionalPanel(
    condition = "output.file_uploaded",
    card(
      card_header("Data Preview"),
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store uploaded data
  data <- reactive({
    req(input$file)
    
    # Read the uploaded CSV file
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Check if data has at least 2 columns
      if (ncol(df) < 2) {
        showNotification("CSV file must have at least 2 columns", type = "error")
        return(NULL)
      }
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Update column choices when data is loaded
  observe({
    req(data())
    
    df <- data()
    
    # Get numeric columns
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    # Get categorical/character columns (including factors)
    categorical_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
    
    if (length(numeric_cols) < 2) {
      showNotification("CSV file must have at least 2 numeric columns", type = "warning")
      return()
    }
    
    updateSelectInput(session, "x_col", choices = numeric_cols, selected = numeric_cols[1])
    updateSelectInput(session, "y_col", choices = numeric_cols, selected = numeric_cols[2])
    
    # Update filter column choices (include "None" option)
    filter_choices <- c("None" = "", categorical_cols)
    updateSelectInput(session, "filter_col", choices = filter_choices, selected = "")
  })
  
  # Update filter values when filter column changes
  observe({
    req(data(), input$filter_col)
    
    if (input$filter_col != "") {
      df <- data()
      unique_values <- sort(unique(df[[input$filter_col]]))
      
      updateSelectInput(session, "filter_values", 
                        choices = unique_values,
                        selected = NULL)  # Select none by default
    }
  })
  
  # Clear all filter selections
  observeEvent(input$clear_filter, {
    updateSelectInput(session, "filter_values", selected = character(0))
  })
  
  # Filtered data based on selected filter
  filtered_data <- reactive({
    req(data())
    
    df <- data()
    
    # Apply filter if a filter column is selected AND values are selected
    if (!is.null(input$filter_col) && input$filter_col != "" && 
        !is.null(input$filter_values) && length(input$filter_values) > 0) {
      df <- df[df[[input$filter_col]] %in% input$filter_values, ]
    }
    
    return(df)
  })
  
  # Output variable to control conditional panels
  output$file_uploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Create hex bin plot
  output$hex_plot <- renderPlot({
    req(filtered_data(), input$x_col, input$y_col)
    
    df <- filtered_data()
    
    # Check if filtered data is empty
    if (nrow(df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "No data matches the selected filters.\nSelect some values from the filter options.") +
               theme_void())
    }
    
    # Validate that selected columns exist and are numeric
    if (!(input$x_col %in% names(df)) || !(input$y_col %in% names(df))) {
      return(NULL)
    }
    
    if (!is.numeric(df[[input$x_col]]) || !is.numeric(df[[input$y_col]])) {
      showNotification("Selected columns must be numeric", type = "error")
      return(NULL)
    }
    
    # Create plot title with filter info
    filter_info <- ""
    if (!is.null(input$filter_col) && input$filter_col != "" && 
        !is.null(input$filter_values) && length(input$filter_values) > 0) {
      filter_info <- paste0(" (Filtered by ", input$filter_col, ")")
    }
    
    # Create the hex bin plot
    ggplot(df, aes_string(x = input$x_col, y = input$y_col)) +
      stat_binhex(bins = input$bins) +
      scale_fill_viridis_c(option = input$color_scale, name = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10)
      ) +
      labs(
        title = paste0("Hex Bin Plot: ", input$y_col, " vs ", input$x_col, filter_info),
        x = input$x_col,
        y = input$y_col,
        subtitle = if (filter_info != "") paste("n =", nrow(df), "observations") else NULL
      )
  })
  
  # Create scatter plot
  output$scatter_plot <- renderPlot({
    req(filtered_data(), input$x_col, input$y_col)
    
    df <- filtered_data()
    
    # Check if filtered data is empty
    if (nrow(df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "No data matches the selected filters.\nSelect some values from the filter options.") +
               theme_void())
    }
    
    # Validate that selected columns exist and are numeric
    if (!(input$x_col %in% names(df)) || !(input$y_col %in% names(df))) {
      return(NULL)
    }
    
    if (!is.numeric(df[[input$x_col]]) || !is.numeric(df[[input$y_col]])) {
      showNotification("Selected columns must be numeric", type = "error")
      return(NULL)
    }
    
    # Create plot title with filter info
    filter_info <- ""
    if (!is.null(input$filter_col) && input$filter_col != "" && 
        !is.null(input$filter_values) && length(input$filter_values) > 0) {
      filter_info <- paste0(" (Filtered by ", input$filter_col, ")")
    }
    
    # Create the hex bin plot
    ggplot(df, aes_string(x = input$x_col, y = input$y_col)) +
      geom_point() +
      scale_fill_viridis_c(option = input$color_scale, name = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10)
      ) +
      labs(
        title = paste0("Hex Bin Plot: ", input$y_col, " vs ", input$x_col, filter_info),
        x = input$x_col,
        y = input$y_col,
        subtitle = if (filter_info != "") paste("n =", nrow(df), "observations") else NULL
      )
  })
  
  # Display data table (show filtered data)
  output$data_table <- renderDT({
    req(filtered_data())
    
    datatable(filtered_data(), 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
