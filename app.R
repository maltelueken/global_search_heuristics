#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        	
            sliderInput("np",
            						"Number of populations:",
            						min = 4,
            						max = 20,
            						value = 4),
            
            sliderInput("f",
            						"Mutation strength:",
            						min = 0,
            						max = 1,
            						value = 0.5),
            
            sliderInput("cr",
            						"Crossover rate:",
            						min = 0,
            						max = 1,
            						value = 0.5),
            
            numericInput("m", "Max. generations:", 1000,
            						 min = 1, max = 10000),
            
            selectInput("example", "Choose example:",
            						choices = list("First De Jonge", "Second De Jonge", "Zimmermann's")),
            
            sliderInput("init",
            						"Initial parameter range:",
            						min = -100,
            						max = 100,
            						value = c(-25, 25)),
            
            numericInput("vtr", "Tolerance:", 1e-6,
            						 min = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
