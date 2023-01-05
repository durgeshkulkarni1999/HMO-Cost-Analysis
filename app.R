#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(tidyverse);
dfhco<-data.frame(read_csv('https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv',show_col_types = FALSE))
# Define UI for application that draws a histogram
ui <- fluidPage(

  #Read the data
  fileInput("upload", label="HMO Data Input File", accept = c(".csv")),
  #Read the actual (solution) data
  fileInput("upload_Solution", label="HMO Data Solution file", accept = c(".csv")),
  #get a number (how much of the dataframe to show)
  numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
  #a place to output a table (i.e., a dataframe)
  tableOutput("headForDF"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 1, max = 40, value = 10),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("hist1")
    )
),
  #output the results (for now, just simple text)
  verbatimTextOutput("txt_results", placeholder = TRUE),
  verbatimTextOutput("txt_results1", placeholder = TRUE)

)

server <- function(input, output, session) {
  #require an input file, then read a CSV file
  getTestData <- reactive({
    req(input$upload)
    read_csv(input$upload$name)
  })
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({
    req(input$upload_Solution)
    read_csv(input$upload_Solution$name)
  })
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  output$txt_results1 <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    prediction(dataset, dataset_solution)
  })
  #show a few lines of the dataframe
  output$headForDF <- renderTable({
    df <- getTestData()
    head(df, input$n)
  })
  output$hist1 <- renderPlot({
    df <- dfhco
    bins <- seq(min(df$cost), max(df$cost), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(df$cost, breaks = bins, col = 'darkgray', border = 'white',main="Histogram of cost")
  })
}
#these libraries are needed, will be used with predict
library(caret); library(kernlab); library(e1071)
#load a model, do prediction and compute the confusion matrix
use_model_to_predict <- function(df, df_solution){
  #load the pre-built model, we named it ‘out_model.rda’)
  load(file="our_model.rda")
  #use the model with new data
  rpartmod <- predict(our_model, df, type = "raw")
  #show how the model performed
  df_solution$expensive <- as.factor(df_solution$expensive)
  confusionMatrix(rpartmod, df_solution$expensive)
}
prediction<-function(df, df_solution){
  load(file="our_model.rda")
  #use the model with new data
  load(file="our_model.rda")
  rpartmod <- predict(our_model, df, type = "raw")
  df$expensive<-rpartmod
  df[c('X','expensive')]
  
}
shinyApp(ui = ui, server = server)