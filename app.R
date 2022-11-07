# Including the library needed.
library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(bslib)

# Code for reading the .csv file and the data in it.
mobility <- read.csv("movement_data.csv", sep = ';')
mobility$Date <- as.Date(mobility$Date)
mobility$Province <- as.factor(mobility$Province)

# Ui code.
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "flatly"),
  navbarPage(title = h4(strong("Shiny Web Dashboard")),
  tabPanel(h4(strong("Main Page!")),
           
           # Application title.
           titlePanel(h1(strong("COVID-19 Data!"))),
           h5(strong("By: Tom Stephens (30048598)")),
           sidebarLayout(
             sidebarPanel(
               h6(strong(radioButtons(inputId = "dv", label = h4(strong("Category:")),
                           choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                           selected = "Grocery_Pharmarcy"))),
               h6(strong(checkboxGroupInput(inputId = "provinces", h4(strong("Province(s):")),
                           choices = levels(mobility$Province),
                           selected = c("Ctesiphon", "Petra", "Pompeii")))),
               h6(strong(dateRangeInput(inputId = "date", label = h4(strong("Date Range:")),
                              start = min(mobility$Date),
                              end = max(mobility$Date)))),
               downloadButton(outputId = "download_data", label = h5(strong("Download"))),
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(h6(strong("Plot Scatter Graph!")), plotOutput(outputId = "plot0")),
                 tabPanel(h6(strong("Plot Line Graph!")), plotOutput(outputId = "plot1")),
                 tabPanel(h6(strong("Plot Scatter with Line Graph!")), plotOutput(outputId = "plot2")),
               ),
               p(h6("More infomation about the data set used is avalible at: ", 
                 tags$a(href="https://support.google.com/covid19-mobility/answer/9824897?hl=en&ref_topic=9822927", strong("Click Here!")))),
               p(h6("All of the code is avalible at GitHub: ", 
                    tags$a(href="https://github.com/MrT-Stephens/R-Shiny-WebApp-Uni-Assesment", strong("Click Here!")))),
               img(src = "rshinylogo.png", height = 150, width = 450, align = "right"),
             )
           )
           ),
  tabPanel(h4(strong("Table of contents!")),
           
           # Application title.
           titlePanel(h1(strong("COVID-19 Data - Table of contents!"))),
           h5(strong("By: Tom Stephens (30048598)")),
           br(),
           h6(DT::dataTableOutput(outputId = "table")),
           h6("Positive and negative percentages indicate an increase and decrease the aseline period(median value between January 3 and Febuary 6, 2020) respectively."),
           img(src = "rshinylogo.png", height = 150, width = 450, align = "right"),
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
           aes_string(x="Date",y=input$dv, color = "Province")) + geom_line() + ylab("%change from baseline")
  })
  output$plot2 <- renderPlot({
    ggplot(filtered_data(),
           aes_string(x="Date",y=input$dv, color = "Province")) + geom_point(alpha = 0.5) + geom_line() + ylab("%change from baseline")
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
