

countries1 <- c("United.Kingdom", "Italy","France","Germany", "Spain", "United.States")
countries2 <- c("Italy","France","Germany", "Spain", "United.Kingdom", "United.States")

# David (Italy and UK deaths)
covid.deaths <-
  read.csv("UK-italy-covid-deaths.csv") # read data into dataframe

data <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")


date <- data$date
UK.deaths <- data[[countries1[1]]] # data[[input$c1]]
It.deaths <- data[[countries2[1]]] # data[[input$c2]]

# Start from first non-zero row
last.zero <- min(which.max(It.deaths > 0), which.max(UK.deaths > 0)) - 1
UK.deaths <- UK.deaths[-(1:last.zero)]
It.deaths <- It.deaths[-(1:last.zero)]
date <- date[-(1:last.zero)]

#attach(covid.deaths)
# date <- covid.deaths$date
# UK.deaths <- covid.deaths$UK.deaths
# It.deaths <- covid.deaths$It.deaths


#date.R = as.Date(date, "%d/%m/%Y") #david
date.R = as.Date(date) #carys

# calculate oberved counts each day
n = length(UK.deaths)
UK.daily = c(0, diff(UK.deaths))
It.daily = c(0, diff(It.deaths))
days = 1:n
ylims = c(1, 2000)

###
# Some of the following preamble may need to be placed within the relevant plot function.
# I don't think all of it is relevant to all plots
###


# length of window for fitting

window = 10
current.window = (n - window + 1):n  # current UK and It lims
end.It.window = min(which(It.deaths > UK.deaths[n])) # when just exceeds current UK total
It.window = (end.It.window - window + 1):end.It.window

#################
day.weights = rep(1, n)
day.weights[33] = 0.33 # downweight obs on day 33 days ago as only 8 hours deaths
fit.UK.current = glm(UK.daily[current.window] ~ current.window,
                     family = "poisson",
                     weights = day.weights[current.window])
summary(fit.UK.current)

coeffs.UK.current = coef(summary(fit.UK.current))
overdisp.UK.current = fit.UK.current$deviance / fit.UK.current$df.residual
se.UK.current = coeffs.UK.current[2, 2] * sqrt(overdisp.UK.current)

relative.increase = exp(coeffs.UK.current[2, 1])
upper.relative.increase = exp(coeffs.UK.current[2, 1] + 1.96 * se.UK.current)
lower.relative.increase = exp(coeffs.UK.current[2, 1] - 1.96 * se.UK.current)
percent.increase.UK.current = signif(100 * (relative.increase - 1), 2)
upper.percent.increase.UK.current = signif(100 * (upper.relative.increase -
                                                    1), 2)
lower.percent.increase.UK.current = signif(100 * (lower.relative.increase -
                                                    1), 2)
#################


#################

fit.It.contemp = glm(It.daily[It.window] ~ It.window, family = "poisson")
summary(fit.It.contemp)

coeffs.It.contemp = coef(summary(fit.It.contemp))
overdisp.It.contemp = fit.It.contemp$deviance / fit.It.contemp$df.residual
se.It.contemp = coeffs.It.contemp[2, 2] * sqrt(overdisp.It.contemp)

relative.increase = exp(coeffs.It.contemp[2, 1])
upper.relative.increase = exp(coeffs.It.contemp[2, 1] + 1.96 * se.It.contemp)
lower.relative.increase = exp(coeffs.It.contemp[2, 1] - 1.96 * se.It.contemp)
percent.increase.It.contemp = signif(100 * (relative.increase - 1), 2)
upper.percent.increase.It.contemp = signif(100 * (upper.relative.increase -
                                                    1), 2)
lower.percent.increase.It.contemp = signif(100 * (lower.relative.increase -
                                                    1), 2)
#################

# compare gradients

Z = (coeffs.UK.current[2, 1] - coeffs.It.contemp[2, 1]) / sqrt(se.UK.current ^
                                                                 2 + se.It.contemp ^ 2)
P1 = 2 * min(pnorm(Z), (1 - pnorm(Z)))

predictions.UK.current = predict(
  fit.UK.current,
  type = "link",
  se.fit = T,
  dispersion = overdisp.UK.current
)
#predictions.It.current=predict( fit.It.current,  type="link",se.fit=T,dispersion=overdisp.It.current)
predictions.It.contemp = predict(
  fit.It.contemp,
  type = "link",
  se.fit = T,
  dispersion = overdisp.It.contemp
)
