library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(DT)
library(corrplot)
library(viridis)
library(heatmaply)
library(bslib)
library(shinythemes)

# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("superhero"),  # Applying the 'superhero' theme
  titlePanel("DataVizExplorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:",
                  choices = c("mtcars", "iris", "diamonds", "CO2", "airquality")),
      fileInput("upload", "Upload your dataset (CSV)", accept = ".csv"),
      uiOutput("varselect"),
      hr(),
      uiOutput("filter_ui"),
      hr(),
      checkboxInput("show_summary", "Show Data Summary", value = FALSE),
      hr(),
      uiOutput("plot_options_ui"),
      hr(),
      checkboxGroupInput("plots", "Select Plots to Display:",
                         choices = c("Scatter Plot" = "scatter", "Line Chart" = "line", 
                                     "Bar Plot" = "bar", "Box Plot" = "box", 
                                     "Histogram" = "histogram", "Heatmap" = "heatmap",
                                     "Correlation Matrix" = "correlation", "Interactive Map" = "map"),
                         selected = c("scatter", "line", "bar", "box", "histogram", "heatmap", "correlation", "map")),
      hr(),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(id = "plotsPanel",
                  tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
                  tabPanel("Line Chart", plotlyOutput("lineChart")),
                  tabPanel("Bar Plot", plotlyOutput("barPlot")),
                  tabPanel("Box Plot", plotlyOutput("boxPlot")),
                  tabPanel("Histogram", plotlyOutput("histPlot")),
                  tabPanel("Heatmap", plotlyOutput("heatmapPlot")),
                  tabPanel("Correlation Matrix", plotOutput("corrMatrix")),
                  tabPanel("Interactive Map", leafletOutput("map")),
                  tabPanel("Data Table", DTOutput("dataTable")),
                  tabPanel("Data Summary", verbatimTextOutput("dataSummary")),
                  tabPanel("Help",
                           h3("About the App"),
                           p("DataVizExplorer is an interactive data visualization tool that allows users to explore and visualize datasets. Users can select from preloaded datasets or upload their own CSV files. The app supports various types of plots including scatter plots, line charts, bar plots, box plots, histograms, heatmaps, and correlation matrices. It also includes an interactive map feature."),
                           h3("How to Use"),
                           tags$ul(
                             tags$li("Select a dataset from the dropdown menu or upload your own CSV file."),
                             tags$li("Choose variables for the X and Y axes from the dynamic variable selection dropdowns."),
                             tags$li("Optionally, select a color variable to differentiate data points."),
                             tags$li("Use the filter options to filter the dataset based on numeric variables."),
                             tags$li("Select the plots you want to display using the checkboxes."),
                             tags$li("Customize the plots by selecting a color palette and theme from the dropdown menus."),
                             tags$li("View the selected plots in the main panel tabs."),
                             tags$li("Download the filtered dataset by clicking the 'Download Filtered Data' button.")
                           ),
                           h3("Known Drawbacks"),
                           tags$ul(
                             tags$li("Only numeric variables can be used for filtering."),
                             tags$li("Large datasets may cause the app to become slow or unresponsive."),
                             tags$li("Customizing plots is limited to the options provided in the app."),
                             tags$li("Uploaded CSV files must be properly formatted to be read correctly by the app."),
                             tags$li("The app may not handle datasets with missing values or non-standard formats well.")
                           )
                  )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive dataset based on user selection or upload
  datasetInput <- reactive({
    if (!is.null(input$upload)) {
      read.csv(input$upload$datapath)
    } else {
      get(input$dataset)
    }
  })
  
  # Dynamic UI for variable selection
  output$varselect <- renderUI({
    df <- datasetInput()
    varnames <- names(df)
    cat_vars <- varnames[sapply(df, is.factor)]
    
    tagList(
      selectInput("xvar", "X-axis variable:", choices = varnames),
      selectInput("yvar", "Y-axis variable:", choices = varnames),
      if (length(cat_vars) > 0) {
        selectInput("colorvar", "Color variable:", choices = cat_vars, selected = cat_vars[1])
      }
    )
  })
  
  # Dynamic UI for filtering
  output$filter_ui <- renderUI({
    df <- datasetInput()
    varnames <- names(df)
    numeric_vars <- varnames[sapply(df, is.numeric)]
    
    tagList(
      selectInput("filter_var", "Filter variable:", choices = numeric_vars, selected = numeric_vars[1]),
      uiOutput("filter_value_ui")
    )
  })
  
  output$filter_value_ui <- renderUI({
    req(input$filter_var)
    df <- datasetInput()
    filter_var <- input$filter_var
    unique_vals <- unique(df[[filter_var]])
    
    sliderInput("filter_value", "Filter value:", 
                min = min(unique_vals, na.rm = TRUE), max = max(unique_vals, na.rm = TRUE), 
                value = c(min(unique_vals, na.rm = TRUE), max(unique_vals, na.rm = TRUE)))
  })
  
  # Dynamic UI for plot customization
  output$plot_options_ui <- renderUI({
    tagList(
      selectInput("color_palette", "Choose Color Palette:",
                  choices = c("Default", "Viridis", "Plasma", "Inferno", "Magma", "Cividis")),
      selectInput("theme", "Choose Theme:",
                  choices = c("Minimal", "Classic", "Gray", "Light", "Dark"))
    )
  })
  
  # Reactive data based on filtering
  filtered_data <- reactive({
    df <- datasetInput()
    filter_var <- input$filter_var
    filter_value <- input$filter_value
    
    if (is.null(filter_value) || length(filter_value) == 0) {
      return(df)
    }
    
    df <- df %>% filter(between(.[[filter_var]], filter_value[1], filter_value[2]))
    df
  })
  
  # Apply selected theme and color palette
  apply_plot_options <- function(p) {
    color_palette <- switch(input$color_palette,
                            "Viridis" = scale_color_viridis_d(),
                            "Plasma" = scale_color_viridis_d(option = "A"),
                            "Inferno" = scale_color_viridis_d(option = "B"),
                            "Magma" = scale_color_viridis_d(option = "C"),
                            "Cividis" = scale_color_viridis_d(option = "D"),
                            scale_color_discrete())
    
    theme_choice <- switch(input$theme,
                           "Minimal" = theme_minimal(),
                           "Classic" = theme_classic(),
                           "Gray" = theme_gray(),
                           "Light" = theme_light(),
                           "Dark" = theme_dark())
    
    p + color_palette + theme_choice
  }
  
  # Scatter Plot
  output$scatterPlot <- renderPlotly({
    if ("scatter" %in% input$plots) {
      req(input$xvar, input$yvar)
      df <- filtered_data()
      p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
        geom_point() +
        theme_minimal()
      p <- apply_plot_options(p)
      ggplotly(p)
    }
  })
  
  # Line Chart
  output$lineChart <- renderPlotly({
    if ("line" %in% input$plots) {
      req(input$xvar, input$yvar)
      df <- filtered_data()
      p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
        geom_line() +
        theme_minimal()
      p <- apply_plot_options(p)
      ggplotly(p)
    }
  })
  
  # Bar Plot
  output$barPlot <- renderPlotly({
    if ("bar" %in% input$plots) {
      req(input$xvar)
      df <- filtered_data()
      p <- ggplot(df, aes_string(x = input$xvar, fill = input$colorvar)) +
        geom_bar(stat = "count") +
        theme_minimal()
      p <- apply_plot_options(p)
      ggplotly(p)
    }
  })
  
  # Box Plot
  output$boxPlot <- renderPlotly({
    if ("box" %in% input$plots) {
      req(input$xvar, input$yvar)
      df <- filtered_data()
      p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
        geom_boxplot() +
        theme_minimal()
      p <- apply_plot_options(p)
      ggplotly(p)
    }
  })
  
  # Histogram
  output$histPlot <- renderPlotly({
    if ("histogram" %in% input$plots) {
      req(input$xvar)
      df <- filtered_data()
      p <- ggplot(df, aes_string(x = input$xvar, fill = input$colorvar)) +
        geom_histogram(binwidth = 1) +
        theme_minimal()
      p <- apply_plot_options(p)
      ggplotly(p)
    }
  })
  
  # Heatmap
  output$heatmapPlot <- renderPlotly({
    if ("heatmap" %in% input$plots) {
      df <- filtered_data()
      num_vars <- df %>% select(where(is.numeric))
      if (ncol(num_vars) > 1) {
        heatmaply_cor(cor(num_vars, use = "complete.obs"), colors = viridis(256))
      }
    }
  })
  
  # Correlation Matrix
  output$corrMatrix <- renderPlot({
    if ("correlation" %in% input$plots) {
      df <- filtered_data()
      num_vars <- df %>% select(where(is.numeric))
      corr <- cor(num_vars, use = "complete.obs")
      corrplot(corr, method = "circle")
    }
  })
  
  # Interactive Map
  output$map <- renderLeaflet({
    if ("map" %in% input$plots) {
      df <- filtered_data()
      if (input$xvar %in% names(df) && input$yvar %in% names(df)) {
        leaflet(df) %>%
          addTiles() %>%
          addCircleMarkers(lng = df[[input$xvar]], lat = df[[input$yvar]], 
                           popup = ~paste(input$xvar, ":", df[[input$xvar]], "<br>", 
                                          input$yvar, ":", df[[input$yvar]]),
                           clusterOptions = markerClusterOptions()) %>%
          setView(lng = mean(df[[input$xvar]], na.rm = TRUE), lat = mean(df[[input$yvar]], na.rm = TRUE), zoom = 4)
      }
    }
  })
  
  # Data Summary
  output$dataSummary <- renderPrint({
    if (input$show_summary) {
      summary(filtered_data())
    }
  })
  
  # Interactive Data Table
  output$dataTable <- renderDT({
    df <- filtered_data()
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "_filtered.csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}        

# Run the application 
shinyApp(ui = ui, server = server)
