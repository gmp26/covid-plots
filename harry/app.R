library(shiny)

# Load ECDC dataset
ecdc.df <-
  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
ecdc.df$dateRep <- as.Date(ecdc.df$dateRep, format = "%d/%m/%Y")
ecdc.df$t <-
  as.integer(ecdc.df$dateRep) - as.integer(as.Date("2020-01-01"))

plot_loess <- function(country,
                       death_thresh = 5,
                       log_plot = TRUE) {
  #' Fit a loess for country [country]
  df <- ecdc.df[ecdc.df$countriesAndTerritories == country, ]
  start_date = min(df$dateRep[df$deaths >= death_thresh])
  stop_date = max(df$dateRep[df$deaths >= death_thresh])
  
  df <- df[df$dateRep >= start_date & df$dateRep <= stop_date, ]
  model <-
    loess(
      log(deaths + 1) ~ t,
      weights = deaths,
      span = 0.75,
      degree =
        2,
      data = df
    )
  # Calculate dispersion from the fit
  n <- model$n
  p <- model$enp
  mu <- exp(model$fitted)
  phi_hat <-
    (n - p) ^ -1 * sum((df$deaths - mu) ^ 2 / mu)
  v <- phi_hat * mu
  
  if (log_plot) {
    log_str = "y"
  } else{
    log_str = ""
  }
  
  plot(
    df$dateRep,
    df$deaths,
    log = log_str,
    pch = 16,
    cex = 0.6,
    main = country,
    xlab = "date",
    ylab = "deaths",
    xlim = c(start_date, stop_date),
    ylim = c(5, 1.2 * max(df$deaths)),
    axes = F
  )
  lines(df$dateRep, exp(model$fitted))
  axis.Date(1, at = df$dateRep, cex = 0.7)
  axis(2, cex = 0.7, las = 2)
  
  # Add standard errors
  lines(df$dateRep, mu + 1.96 * sqrt(v), lty = 2)
  lines(df$dateRep, mu - 1.96 * sqrt(v), lty = 2)
}

#########
# SHINY #
#########

countries <- unique(ecdc.df$countriesAndTerritories)
top_countries <-
  c("United_Kingdom",
    "United_States_of_America",
    "Italy",
    "Spain",
    "France",
    "Germany",
    "Brazil")

bottom_countries <- countries[!(countries %in% top_countries)]

ui <- fluidPage(
  titlePanel("Covid Deaths"),
  
  sidebarLayout(sidebarPanel(
    selectInput(
      "country",
      "Country:",
      list(`Top` = top_countries,
           `Rest` = bottom_countries)
    )
  ),
  
  # Show deaths plot
  mainPanel(
    plotOutput("deaths.plot.log"),
    plotOutput("deaths.plot")
  )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$deaths.plot.log <- renderPlot({
    plot_loess(input$country)
  })
  
  output$deaths.plot <- renderPlot({
    plot_loess(input$country, log_plot = FALSE)
  })
  
}

plot_loess("United_Kingdom")

# Run the application
shinyApp(ui = ui, server = server)
