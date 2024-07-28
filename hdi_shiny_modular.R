# load libraries
library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(magrittr)
library(scales) 
library(shinythemes)

# -----------------------------------
# Define UI
# -----------------------------------
ui <- fluidPage(
  # Set theme 
  theme = shinytheme("sandstone"),
  
  # Application title - update dynamically based on user input
  titlePanel("Shiny App to Explore HDI Data"),
  uiOutput("dynamicTitle"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = c("Nigeria", "Afghanistan", "Singapore", "India", "Japan", "Ireland")),
      numericInput("rowsToShow", "Number of Rows in Table:", 1000),
      selectInput("columnsToShow", "Filter Columns:", choices = NULL, multiple = TRUE),
      fileInput("file", "Upload additional data:", accept = ".csv"),
      actionButton("clearFile", "Clear/Toggle Uploaded Data"),  # Button to clear the uploaded file
      # Additional inputs for plot customization
      selectInput("indicator", "Select Indicator for Indicator Table and Plots:", choices = NULL), # Update choices based on data
      sliderInput("yearRange", "Select Year Range for Plots:", min = 1990, max = 2022, value = c(1990, 2022)),
      checkboxInput("logScale", "Change Plot to Logarithmic Scale", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("table")),
        tabPanel("Indicator Table", DTOutput("indicator_table")),
        tabPanel("Trend Over Time", plotOutput("plot1")),
        tabPanel("Distribution", plotOutput("plot2"))
      )
    )
  )
)


# -----------------------------------
# Define server logic
# -----------------------------------
server <- function(input, output, session) {
  
  
  # Dynamic Title based on the selected country
  output$dynamicTitle <- renderUI({
    # if a file is uploaded, display a different title using the country_name column
    if (!is.null(input$file)) {
      HTML(sprintf("<h1>Human Development Indicators - %s</h1>", data()$country_name[1]))
    } else {
      HTML(sprintf("<h1>Human Development Indicators - %s</h1>", input$country))
    }
  })


  # Initialize a reactive value to track the toggle state of the uploaded file
  toggleState <- reactiveVal(FALSE)
  

  # Reactive expression for reading data
  data <- reactive({
    # Check if a file is uploaded
    if(!is.null(input$file) && !toggleState()) {
      # Read the uploaded file
      inFile <- input$file
      return(read_HDI(inFile$datapath))
    } else {
      # map country names to abbreviations
      country_abbreviations <- c("Nigeria" = "nga", "Afghanistan" = "afg", "Singapore" = "sgp", "India" = "ind", "Japan" = "jpn", "Ireland" = "irl")
      country <- country_abbreviations[input$country]
      # read the user-selected country
      data_file_path <- sprintf("data/hdro_indicators_%s.csv", tolower(country))
      read_HDI(data_file_path)
    }
  })

  # Function to reset input
  reset <- function(inputId) {
    session$sendInputMessage(inputId, list(value = NULL))
  }


  # Observe the clear file button
  observeEvent(input$clearFile, {
    current_state <- toggleState()
    toggleState(!current_state)  # Toggle the state between TRUE and FALSE
    if (toggleState()) {
      reset("file")
    }
  })
  
  
  # Update dropdowns based on data
  observe({
    updateSelectInput(session, "indicator", choices = unique(data()$indicator_id))
    updateSelectInput(session, "columnsToShow", choices = names(data()))
    
  })


  # Render tables and plots
  output$table <- renderDT({
    dataTableOutput(data(), input$columnsToShow, input$rowsToShow)
  })
  
  output$indicator_table <- renderDT({
    dataTableOutput(filterIndicatorData(data(), input$indicator), input$columnsToShow, input$rowsToShow)
  })
  
  output$plot1 <- renderPlot({
    renderLinePlot(data(), input$indicator, input$yearRange, input$logScale)
  })
  
  output$plot2 <- renderPlot({
    renderHistogram(data(), input$indicator, input$yearRange, input$logScale)
  })
}


# -----------------------------------
# Helper functions to modularise data rendering
# -----------------------------------
dataTableOutput <- function(data, columnsToShow, rowsToShow) {
  if (!is.null(data) && !is.null(columnsToShow) && length(columnsToShow) > 0) {
    valid_columns <- names(data) %in% columnsToShow
    if (any(valid_columns)) {
      data <- data %>% dplyr::select(all_of(columnsToShow))
    } else {
      data <- data.frame(Message = "No valid columns selected")
    }
  }
  datatable(head(data, n = rowsToShow), options = list(pageLength = rowsToShow))
}

filterIndicatorData <- function(data, indicator) {
  data %>% dplyr::filter(indicator_id == indicator)
}

renderLinePlot <- function(data, indicator, yearRange, logScale) {
  plot_data <- filterPlotData(data, indicator, yearRange)
  p <- ggplot(data = plot_data, aes(x = year, y = value)) +
    geom_line(size = 1, color = "skyblue") +
    geom_point(size = 2, color = "skyblue") +
    ggtitle(paste("Trend of", indicator)) +
    labs(x = "Year", y = "Value") +
    theme_minimal()
  
  if (logScale) {
    p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
  }
  print(p)
}

renderHistogram <- function(data, indicator, yearRange, logScale) {
  plot_data <- filterPlotData(data, indicator, yearRange)
  p <- ggplot(data = plot_data, aes(x = value)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste("Distribution of", indicator)) +
    theme_minimal()
  
  if (logScale) {
    p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
  }
  print(p)
}

filterPlotData <- function(data, indicator, yearRange) {
  data[data$indicator_id == indicator & data$year >= yearRange[1] & data$year <= yearRange[2], ]
}

read_HDI <- function(file_name){
  # Define column types
  col_types <- c(
    country_code = "factor",  
    country_name = "character",  
    indicator_id = "factor",  
    indicator_name = "character",
    index_id = "factor",      
    index_name = "factor",    
    value = "numeric",
    year = "integer"
  )
  
  # read in data for Nigeria using data.table
  file = file_name
  
  # Skip the first two lines (header and the problematic second line) and specify column names manually
  df <- fread(file, 
              skip = 2, 
              header = FALSE, 
              sep = ",",
              #  col.names = c("country_code", "country_name", "indicator_id", "indicator_name", "index_id", "index_name", "value", "year"),
              col.names = names(col_types)
  )
  # Apply column types
  for (col in names(col_types)) {
    if (col_types[col] == "factor") {
      df[[col]] <- as.factor(df[[col]])
    } else if (col_types[col] == "character") {
      df[[col]] <- as.character(df[[col]])
    } else if (col_types[col] == "numeric") {
      df[[col]] <- as.numeric(df[[col]])
    } else if (col_types[col] == "integer") {
      df[[col]] <- as.integer(df[[col]])
    }
  }
  
  # create and return an object of class 'HDIdata' while inheriting 
  # from original data.table
  class(df) <- c("HDIdata", class(df))
  
  return(df)
}


# -----------------------------------
# Run the application 
# -----------------------------------
shinyApp(ui = ui, server = server)