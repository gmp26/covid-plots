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

plot.type <- list(
    "Daily Deaths",
    "Daily Cases",
    "Cumulative Deaths",
    "Cumulative Cases"
);

print(length(plot.type))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),

    titlePanel("Winton Centre Covid-19 tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Application title

        sidebarPanel(
            
            selectInput("variable", "Select:",
                         #choiceNames = plot.type,
                         choices = plot.type,
                         width = 200
                        ),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
                  plotOutput("distPlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    type <- list(
        "Daily Deaths",
        "Daily Cases",
        "Cumulative Deaths",
        "Cumulative Cases"
    );
    
    output$txt <- renderText({input$variable})
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
