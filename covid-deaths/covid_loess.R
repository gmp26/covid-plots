# Load and process the ECDC dataset
ecdc.df <-
  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
ecdc.df$dateRep <- as.Date(ecdc.df$dateRep, format = "%d/%m/%Y")

# Remove the date when China reported a large number of deaths
exclude_rows <- rownames(ecdc.df)[(
  ecdc.df$countriesAndTerritories == "China" &
    ecdc.df$dateRep  == as.Date("2020-04-17")
) |  (
  ecdc.df$countriesAndTerritories == "United_Kingdom" &
    ecdc.df$dateRep  == as.Date("2020-04-30")
)]
exclude_rows <- as.integer(exclude_rows)

ecdc.df$t <-
  as.integer(ecdc.df$dateRep) - as.integer(as.Date("2020-01-01"))

# Filter only for countries with more than [death_thresh] deaths on at least
# [days_thresh] days
death_thresh <- 5
days_thresh <- 4
agg <-
  aggregate(ecdc.df$deaths,
            by = list(ecdc.df$countriesAndTerritories),
            FUN = function(x) sum(x > death_thresh))
countries <- as.character(agg$Group.1[agg$x > days_thresh])

# Fit all the models
dfs <- list()
mus <- list()
vs <- list()

for(country in countries){
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
      data = df[!(rownames(df) %in% exclude_rows),]
    )
  n <- model$n
  p <- model$enp
  y <- df$deaths[!(rownames(df) %in% exclude_rows)]
  mu <- exp(model$fitted)
  phi_hat <-
    (n - p) ^ -1 * sum((y - mu) ^ 2 / mu)
  mu <- exp(predict(model, newdata = df))
  v <- phi_hat * mu
  
  # Store output
  dfs <- c(dfs, list(df))
  mus <- c(mus, list(mu))
  vs <- c(vs, list(v))
}

names(dfs) <- countries
names(mus) <- countries
names(vs) <- countries

# Function to plot the reported deaths
plot_loess <- function(country,
                       log_plot = TRUE) {
  # Calculate dispersion from the fit
  
  if (log_plot) {
    log_str = "y"
  } else{
    log_str = ""
  }
  
  df <- dfs[[country]]
  mu <- mus[[country]]
  v <- vs[[country]]
  
  y_max <- 1.2 * max(df$deaths)
  plot(
    df$dateRep,
    df$deaths,
    log = log_str,
    pch = 16,
    main = sub("_", " ", country),
    xlab = "Reported date",
    ylab = "Daily reported deaths",
    ylim = c(5, y_max),
    axes = F
  )
  lines(df$dateRep, mu, lwd = 2)
  legend(
    min(df$dateRep),
    y_max,
    legend = c("Daily data:\nhttps://www.ecdc.europa.eu/en"),
    lwd = 2
  )
  axis.Date(1, at = df$dateRep, cex = 0.7)
  axis(2, cex = 0.7, las = 2)
  
  # Add standard errors
  lines(df$dateRep, mu + 1.96 * sqrt(v), lty = 2)
  lines(df$dateRep, mu - 1.96 * sqrt(v), lty = 2)
}