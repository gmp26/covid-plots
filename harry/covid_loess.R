# Load ECDC dataset
ecdc.df <-
  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
ecdc.df$dateRep <- as.Date(ecdc.df$dateRep, format = "%d/%m/%Y")
# Remove the date when China reported a large number of deaths
remove_ixs <-
  ecdc.df$countriesAndTerritories == "China" &
  ecdc.df$dateRep  == as.Date("2020-04-17")
ecdc.df <- ecdc.df[!remove_ixs,]
ecdc.df$t <-
  as.integer(ecdc.df$dateRep) - as.integer(as.Date("2020-01-01"))
countries <- unique(ecdc.df$countriesAndTerritories)

# Function to plot the reported deaths
plot_loess <- function(country,
                       death_thresh = 5,
                       log_plot = TRUE) {
  df <- ecdc.df[ecdc.df$countriesAndTerritories == country,]
  start_date = min(df$dateRep[df$deaths >= death_thresh])
  stop_date = max(df$dateRep[df$deaths >= death_thresh])
  
  df <- df[df$dateRep >= start_date & df$dateRep <= stop_date,]
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
    main = sub("_", " ", country),
    xlab = "Reported date",
    ylab = "Daily reported deaths",
    xlim = c(start_date, stop_date),
    ylim = c(5, 1.2 * max(df$deaths)),
    axes = F
  )
  lines(df$dateRep, exp(model$fitted), lwd = 2)
  legend(
    min(df$dateRep),
    max(df$deaths),
    legend = c("Daily data:\nhttps://www.ecdc.europa.eu/en"),
    lwd = 2
  )
  axis.Date(1, at = df$dateRep, cex = 0.7)
  axis(2, cex = 0.7, las = 2)
  
  # Add standard errors
  lines(df$dateRep, mu + 1.96 * sqrt(v), lty = 2)
  lines(df$dateRep, mu - 1.96 * sqrt(v), lty = 2)
}