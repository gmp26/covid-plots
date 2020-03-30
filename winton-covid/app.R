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
    "Daily deaths",
    "Daily cases",
    "Cumulative deaths",
    "Cumulative cases"
)

countries <- list("UK only",
                  "UK+US",
                  "UK+Italy",
                  "UK+Spain",
                  "UK+France",
                  "UK+Sweden",
                  "UK+Switzerland",
                  "UK+Australia",
                  "UK+Canada",
                  "UK+Mexico",
                  "UK+Korea",
                  "UK+China",
                  "UK+Hong Kong",
                  "UK+Japan",
                  "UK+Iran",
                  "UK+India")

scales <- list("on natural scale", "on logarithmic scale")

print(length(plot.type))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),

    titlePanel("Winton Centre Covid-19 tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Application title
        tags$div(style = "margin-top:40px",
        sidebarPanel(
            
            selectInput("variable", "Show:",
                         choices = plot.type,
                         width = 200
                        ),
            selectInput("scale", "Scale:",
                        choices = scales,
                        width = 200
            ),
            selectInput("countries", "Countries:",
                        choices = countries,
                        width = 200
            ),
            sliderInput("alignment",
                        "Align countries:",
                        min = 0,
                        max = 100,
                        value = 10),
        
            sliderInput("bins",
                        "Number of bins:",
                        min = 0,
                        max = 50,
                        value = 30)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(tags$div(style = "margin-top:-49px;margin-right:15px",
                           h3(fluidRow(
                                   column(width = 3, textOutput("variable")), 
                                   column(width = 4, textOutput("scale")),
                              plotOutput("distPlot"))
                              )
                           )
                  )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$variable <- renderText({input$variable})
    
    output$scale <- renderText({input$scale})
    
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
