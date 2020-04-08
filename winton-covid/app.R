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
library(tibble)

######### Preamble

###
# Categorical Colour palettes
###
display.brewer.all()

# Choose up to 5 countries and allocate from the ColorBrewer Set2 palette
cols <- brewer.pal(5, "Set1")
It.col <- cols[2]
UK.col <- cols[1]

###
#  UI related constants
###
plot.type <- list("Plot0",
                  "Plot1",
                  "Plot2",
                  "Plot3",
                  "Plot4")

scales <- list("natural", "logarithmic")

source('david_preamble.R')


###
# Plots
###
plot0 <- function(c1,c2) {
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
        col = UK.col
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col = It.col)
    text(date.R[6], 20, c2, col = It.col,font=2)
    text(date.R[20], 20, c1, col = UK.col,font=2)
}

plot1 <- function(c1,c2) {
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
        col = UK.col
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col=It.col)
    text(date.R[6], 20, c2, col=It.col, font=2)
    text(date.R[20], 20, c1, col=UK.col, font=2)
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

plot2 <- function(c1,c2) {
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
        col = UK.col
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col=It.col)
    text(date.R[6], 20, c2,col=It.col, font=2)
    text(date.R[20], 20, c1, col=UK.col, font=2)
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

plot3 <- function(c1,c2) {
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
        axes = F,
        col = UK.col
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col=It.col)
    text(date.R[6], 20, c2, col=It.col, font=2)
    text(date.R[20], 20, c1, col=UK.col, font=2)
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

plot4 <- function(c1,c2) {
    
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
        axes = F,
        col = UK.col
    )
    axis.Date(1, at = date.R, cex = 0.7)
    axis(2, cex = 0.7, las = 2)
    points(date.R, It.daily, pch = 19, col=It.col)
    text(date.R[20], 500, c2, col=It.col, font=2)
    text(date.R[35], 400, c1, col=UK.col, font=2)
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

###
# UI
###
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
            selectInput("c1",
                        "Country 1",
                        choices = countries1,
                        width = 160),
            selectInput("c2",
                        "Country 2",
                        choices = countries2,
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
        
        # Show a plot in the main panel
        mainPanel(fluidRow(plotOutput("mainPlot")))
        
    )
)


###
# Server
###
server <- function(input, output, session) {
    # output$variable <- renderText({
    #     input$variable
    # })
    # 
    # output$scale <- renderText({
    #     input$scale
    # })
    
    output$mainPlot <- renderPlot({
        if (input$variable == "Plot0") {
            plot0(input$c1, input$c2)
        }
        if (input$variable == "Plot1") {
            plot1(input$c1, input$c2)
        }
        else if (input$variable == "Plot2") {
            plot2(input$c1, input$c2)
        }
        else if (input$variable == "Plot3") {
            plot3(input$c1, input$c2)
        }
        else if (input$variable == "Plot4") {
            plot4(input$c1, input$c2)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
