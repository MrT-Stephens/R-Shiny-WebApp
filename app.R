#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Including the library needed.
library(shiny)
library(ggplot2)
library(DT)

# Code for reading the .csv file and the data in it.
mobility <- read.csv("movement_data.csv", sep = ';')
mobility$Date <- as.Date(mobility$Date)
mobility$Province <- as.factor(mobility$Province)

# Ui code.
ui <- fluidPage(tabsetPanel(
  tabPanel("Main Page!",
           
           # Application title.
           titlePanel("COVID-19 Data!"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "dv", label = "Category",
                           choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                           selected = "Grocery_Pharmarcy"),
               selectInput(inputId = "provinces", "Province(s)",
                           choices = levels(mobility$Province),
                           multiple = TRUE,
                           selected = c("Ctesiphon", "Petra", "Pompeii")),
               dateRangeInput(inputId = "date", label = "Date Range",
                              start = min(mobility$Date),
                              end = max(mobility$Date)),
               downloadButton(outputId = "download_data", label = "Download")
             ),
             mainPanel(
               plotOutput(outputId = "plot0"),
               em("Positive and negative percentages indicate an increase and decrease the aseline period(median value between January 3 and Febuary 6, 2020) respectively."),
               DT::dataTableOutput(outputId = "table"),
             )
           )
           ),
  tabPanel("Extra Graphs!",
           
           # Application title.
           titlePanel("COVID-19 Data - Extra Graphs!"),
           plotOutput(outputId = "plot1"),
           plotOutput(outputId = "plot2"),
           ),
)
)

# Server code.
server <- function(input, output) {
  filtered_data <- reactive({
    subset(mobility,
           Province %in% input$provinces &
             Date >= input$date[1] & Date <= input$date[2])
  })
  output$plot0 <- renderPlot({
    ggplot(filtered_data(),
           aes_string(x="Date",y=input$dv, color = "Province")) + geom_point(alpha = 0.5) + ylab("%change from baseline")
  })
  output$plot1 <- renderPlot({
    ggplot(filtered_data(),
           aes_string(x="Date",y=input$dv, color = "Province")) + geom_point(alpha = 0.5) + geom_line() + ylab("%change from baseline")
  })
  output$plot2 <- renderPlot({
    ggplot(filtered_data(),
           aes_string(x="Date",y=input$dv, color = "Province")) + geom_line() + ylab("%change from baseline")
  })
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
