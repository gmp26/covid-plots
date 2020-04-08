#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(Hmisc)
library(RColorBrewer)
library(tidyr)

######### Preamble
covid.deaths <-
    read.csv("UK-italy-covid-deaths.csv") # read data into dataframe



###
# Categorical Colour palettes
###
display.brewer.all()

# Choose up to 5 countries and allocate from the ColorBrewer Set2 palette
cols <- brewer.pal(5, "Set2")

###
#  UI related constants
###
plot.type <- list("Plot0",
                  "Plot1",
                  "Plot2",
                  "Plot3",
                  "Plot4")

scales <- list("natural", "logarithmic")

###
# Data acquisition
###

attach(covid.deaths)
date.R = as.Date(date, "%d/%m/%Y")
# calculate oberved counts each day
n = length(UK.deaths)
UK.daily = c(0, diff(UK.deaths))
It.daily = c(0, diff(It.deaths))
days = 1:n
ylims = c(1, 2000)


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



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    titlePanel("Winton Centre Covid-19 monitor"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        # Application title
        sidebarPanel(
            selectInput("variable",
                        "Select plot",
                        choices = plot.type,
                        width = 160),
            # fillRow(
            #     height = 100,
            #     fillCol(
            #         width = "90%",
            #         sliderInput(
            #             "alignment",
            #             "Align countries:",
            #             min = 0,
            #             max = 100,
            #             value = 10
            #         )
            #     ),
            #     fillCol(
            #         width = "90%",
            #         sliderInput(
            #             "bins",
            #             "Number of bins:",
            #             min = 0,
            #             max = 50,
            #             value = 30
            #         )
            #     )
            # )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(h3(fluidRow(
            plotOutput("mainPlot")
        )))
        
    )
)
plot0 <- function() {
    plot(
        date.R,
        UK.daily,
        ylim = ylims,
        type = "p",
        log = "y",
        xlab = "Date",
        ylab = "Daily number of deaths",
        pch = 19,
        cex = 0.9,
        bty = "n",
        axes = F,
        col = cols[4]
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col = cols[1])
    text(date.R[6], 20, "Italy", col = cols[1],font=2)
    text(date.R[20], 20, "UK", col = cols[4],font=2)
}

plot1 <- function() {
    plot(
        date.R,
        UK.daily,
        ylim = ylims,
        type = "p",
        log = "y",
        xlab = "Date",
        ylab = "Daily number of deaths",
        pch = 19,
        cex = 0.9,
        bty = "n",
        axes = F,
        col = cols[4]
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col=cols[1])
    text(date.R[6], 20, "Italy", col=cols[1], font=2)
    text(date.R[20], 20, "UK", col=cols[4], font=2)
    lines(date.R[current.window], exp(predictions.UK.current$fit))
    lines(date.R[It.window], exp(predictions.It.contemp$fit))
    title(
        paste(
            "Data from World in Data/ECDPC up to March 27th, fitted lines for past",
            window,
            " days\nUK: Fitted daily increase",
            percent.increase.UK.current,
            "%  (95% interval:",
            lower.percent.increase.UK.current,
            "% to",
            upper.percent.increase.UK.current,
            "%)  \n  It at Uk's stage: Fitted daily increase:",
            percent.increase.It.contemp,
            "%  (95% interval:",
            lower.percent.increase.It.contemp,
            "% to",
            upper.percent.increase.It.contemp,
            "%) \n P=",
            round(P1, 2),
            "(2-sided, Poisson regression with over-dispersion)"
        ),
        cex.main = 0.8
    )
}

plot2 <- function() {
    plot(
        date.R,
        UK.daily,
        ylim = ylims,
        type = "p",
        log = "y",
        xlab = "Date",
        ylab = "Daily number of deaths",
        pch = 19,
        cex = 0.9,
        bty = "n",
        axes = F
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 1)
    text(date.R[6], 20, "Italy")
    text(date.R[20], 20, "UK")
    lines(date.R[current.window], exp(predictions.UK.current$fit))
    lines(
        date.R[current.window],
        exp(
            predictions.UK.current$fit + 1.96 * predictions.UK.current$se.fit
        ),
        lty = 2
    )
    lines(
        date.R[current.window],
        exp(
            predictions.UK.current$fit - 1.96 * predictions.UK.current$se.fit
        ),
        lty = 2
    )
    
    #lines(date.R[current.window],exp(predictions.It.current$fit))
    #lines(date.R[current.window],exp(predictions.It.current$fit+1.96*predictions.It.current$se.fit),lty=2)
    #lines(date.R[current.window],exp(predictions.It.current$fit-1.96*predictions.It.current$se.fit),lty=2)
    
    lines(date.R[It.window], exp(predictions.It.contemp$fit))
    lines(
        date.R[It.window],
        exp(
            predictions.It.contemp$fit + 1.96 * predictions.It.contemp$se.fit
        ),
        lty = 2
    )
    lines(
        date.R[It.window],
        exp(
            predictions.It.contemp$fit - 1.96 * predictions.It.contemp$se.fit
        ),
        lty = 2
    )
    
    title(
        paste(
            "Data from World in Data/ECDPC up to March 27th, fitted lines for past",
            window,
            " days\nUK: Fitted daily increase",
            percent.increase.UK.current,
            "%  (95% interval:",
            lower.percent.increase.UK.current,
            "% to",
            upper.percent.increase.UK.current,
            "%)  \n  It at Uk's stage: Fitted daily increase:",
            percent.increase.It.contemp,
            "%  (95% interval:",
            lower.percent.increase.It.contemp,
            "% to",
            upper.percent.increase.It.contemp,
            "%) \n P=",
            round(P1, 2),
            "(2-sided, Poisson regression with over-dispersion)"
        ),
        cex.main = 0.8
    )
}

plot3 <- function() {
    fit.It.current = glm(It.daily[current.window] ~ current.window, family =
                             "poisson")
    summary(fit.It.current)
    coeffs.It.current = coef(summary(fit.It.current))
    overdisp.It.current = fit.It.current$deviance / fit.It.current$df.residual
    se.It.current = coeffs.It.current[2, 2] * sqrt(overdisp.It.current)
    
    relative.increase = exp(coeffs.It.current[2, 1])
    upper.relative.increase = exp(coeffs.It.current[2, 1] + 1.96 * se.It.current)
    lower.relative.increase = exp(coeffs.It.current[2, 1] - 1.96 * se.It.current)
    percent.increase.It.current = signif(100 * (relative.increase - 1), 2)
    upper.percent.increase.It.current = signif(100 * (upper.relative.increase -
                                                          1), 2)
    lower.percent.increase.It.current = signif(100 * (lower.relative.increase -
                                                          1), 2)
    
    # plot loess fit
    plot(
        date.R,
        UK.daily,
        ylim = ylims,
        type = "p",
        log = "y",
        xlab = "Date",
        ylab = "Daily number of deaths",
        pch = 19,
        cex = 0.9,
        bty = "n",
        axes = F
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 1)
    text(date.R[6], 20, "Italy")
    text(date.R[20], 20, "UK")
    title("Fitted loess line with 95% interval for underlying trajectory")
    
    
    It.daily.eps = It.daily + 0.0001 # add tiny bit to make log OK (weighting these obs by zero in analysis)
    UK.daily.eps = UK.daily + 0.0001
    x = 1:n
    It.loess = loess(log(It.daily.eps) ~ x , weights = It.daily.eps, span =
                         1)
    pred.It.loess = predict(It.loess, se = T)
    
    lines(date.R, exp(pred.It.loess$fit))
    lines(date.R,
          exp(pred.It.loess$fit + 1.96 * pred.It.loess$se.fit),
          lty = 2)
    lines(date.R,
          exp(pred.It.loess$fit - 1.96 * pred.It.loess$se.fit),
          lty = 2)
    
    UK.loess = loess(log(UK.daily.eps) ~ x , weights = UK.daily.eps, span =
                         1)
    pred.UK.loess = predict(UK.loess, se = T)
    lines(date.R, exp(pred.UK.loess$fit))
    lines(date.R,
          exp(pred.UK.loess$fit + 1.96 * pred.UK.loess$se.fit),
          lty = 2)
    lines(date.R,
          exp(pred.UK.loess$fit - 1.96 * pred.UK.loess$se.fit),
          lty = 2)
}

plot4 <- function() {
    # on natural scale
    plot(
        date.R,
        UK.daily,
        ylim = ylims,
        type = "p",
        xlab = "Date",
        ylab = "Daily number of deaths",
        pch = 19,
        cex = 0.9,
        bty = "n",
        axes = F
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 1)
    text(date.R[6], 20, "Italy")
    text(date.R[20], 20, "UK")
    title("Fitted loess line with 95% interval for underlying trajectory")
    
    lines(date.R, exp(pred.It.loess$fit))
    lines(date.R,
          exp(pred.It.loess$fit + 1.96 * pred.It.loess$se.fit),
          lty = 2)
    lines(date.R,
          exp(pred.It.loess$fit - 1.96 * pred.It.loess$se.fit),
          lty = 2)
    
    lines(date.R, exp(pred.UK.loess$fit))
    lines(date.R,
          exp(pred.UK.loess$fit + 1.96 * pred.UK.loess$se.fit),
          lty = 2)
    lines(date.R,
          exp(pred.UK.loess$fit - 1.96 * pred.UK.loess$se.fit),
          lty = 2)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    # output$variable <- renderText({
    #     input$variable
    # })
    # 
    # output$scale <- renderText({
    #     input$scale
    # })
    
    output$mainPlot <- renderPlot({
        if (input$variable == "Plot0") {
            plot0()
        }
        if (input$variable == "Plot1") {
            plot1()
        }
        else if (input$variable == "Plot2") {
            plot2()
        }
        else if (input$variable == "Plot3") {
            plot3()
        }
        else if (input$variable == "Plot4") {
            plot4()
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
