library(shiny)
source("covid_loess.R")
info.txt <- read.delim("info.txt", header=F)[1,1]

top_countries <-
  c("United_Kingdom",
    "United_States_of_America",
    "Italy",
    "Spain",
    "France",
    "Germany",
    "China",
    "Brazil"
  )

bottom_countries <- countries[!(countries %in% top_countries)]

ui <- fluidPage(
  titlePanel("Covid Deaths"),
  
  sidebarLayout(sidebarPanel(
    selectInput(
      "country",
      "Country:",
      list(`Top` = top_countries,
           `Rest` = bottom_countries)
    ),
    tags$h3("Info"),
    info.txt,
  ),
  
  # Show plot
  mainPanel(
    tags$h3("Log scale"),
    plotOutput("deaths.plot.log"),
    tags$h3("Linear scale"),
    plotOutput("deaths.plot"),
  )))

server <- function(input, output) {
  output$deaths.plot.log <- renderPlot({
    plot_loess(input$country)
  })
  
  output$deaths.plot <- renderPlot({
    plot_loess(input$country, log_plot = FALSE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)