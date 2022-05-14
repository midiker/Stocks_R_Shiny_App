
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Reactivity"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Widget 1
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = h3("Input a caption:"),
                value = "Data Summary"),
      
      #WIDGET 2
      # Input: Selector for choosing dataset ----
      radioButtons(inputId = "dataset",
                  label = h3("Choose a stock list:"),
                  choices = c("GOOG", "NASDAQ", "Russell2000")
                  ),
      
      #WIDGET 3
      # Input: Numeric entry for number of observations to view ----
      sliderInput('obs', label = h4("Number of Observations:"), min = 0, 
                  max = 100, value = 10),
      
      #WIDGET 4
      # Input: Date format range to plot a scatter plot for the 'Open', 'Close', 
      sliderInput("range", label = h3("Date Range:"), min = as.Date(c("2020-08-26"),"%Y-%m-%d"),
                  max = as.Date(c("2021-01-19"),"%Y-%m-%d"),
                 value=as.Date(c("2020-09-01","2020-10-01"),timeFormat="%Y-%m-%d")
                 ),
      
      #WIDGET 5
      # The Action Button plots the Histogram and the Scatter Plot
      actionButton("openHist", h4("Show Plots"))
      ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view"),

      #Plotting Histogram
      plotOutput("plot"), 
      
      #Plotting Scatter Plot
      plotOutput(outputId = "lines", height = "70vh")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  GOOG = read.csv('GOOG.csv')
  NASDAQ = read.csv('NASDAQ.csv')
  Russell2000 = read.csv('Russell2000.csv')
  datasetInput <- reactive({
    switch(input$dataset,
           "GOOG" = GOOG,
           "NASDAQ" = NASDAQ,
           "Russell2000" = Russell2000)
  })

  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
 
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed by the slider
  # "Number of Observations"
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Show the Histogram for the selected Stock List ----
  # Plots and shows the Histogram for the Stock List Selected
  # by the user.The output$plot depends on both the datasetInput reactive
  # expression and input$dataset, so it will be re-executed whenever
  # the user chooses a different Stock List.
  
  v <- reactiveValues(doPlot = TRUE)
  observeEvent(input$openHist, {
    v$doPlot = FALSE
  })
  
  output$plot <- renderPlot({
    if(v$doPlot)return()
    hist(datasetInput()$Open, main=input$dataset, xlab = 'Stock Price' )
  })
  
  #Show Scatter Plot for the selected Stock List and Date range ----
  # Plots and shows the Scatter Plot for the Stock List selected
  # by the user. The plot contains the plots for the Open, Close, High
  # and Low columns of the user selected Stock List.
  # The ggplot() and all the geom_point() depends on both the datasetInput reactive
  # expression and input$range[1] and input$range[2], so it will be re-executed whenever
  # the user chooses a different Stock List or changes the slider range inputs.
  
  output$lines <- renderPlot({
    if(v$doPlot)return()
    Sys.getlocale()
    datasetInput() %>%
      filter(datasetInput()$Date > as.Date(c(input$range[1]), "%Y-%m-%d")
             & datasetInput()$Date < as.Date(c(input$range[2]), "%Y-%m-%d")) %>%
      ggplot(aes(x = Date, y=Open), group = 1) +
      geom_point(aes(x = Date, y=Open, color = "Open" )) +
      geom_point(aes(x = Date, y=Close, color = "Close" )) +
      geom_point(aes(x = Date, y=High, color = "High" )) +
      geom_point(aes(x = Date, y=Low, color = "Low" )) +
      labs(title = "Stock Trend", x = "Date", y = "Stock Price") +
      guides(x =  guide_axis(angle = 90))
  
  })
 
}

# Create Shiny app ----
shinyApp(ui, server)
