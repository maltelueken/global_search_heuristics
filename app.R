#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

mutation <- function(G, f, i) {
	idx <- sample((1:ncol(G))[-i], size = 3, replace = FALSE)
	
	mutant <- G[,idx[1]] + f * (G[,idx[2]] - G[,idx[3]])
	
	return(mutant)
}

crossover <- function(target, mutant, CR) {
	
	randUnif <- runif(length(target), 0, 1)
	randIdx  <- sample(1:length(target), size = 1)
	idx      <- 1:length(target)
	
	trial <- ifelse(randUnif <= CR | randIdx == idx, mutant, target)
	
	return(trial)
}

selection <- function(trial, target, cost) {
	print(c(trial, cost(trial)))
	if (cost(trial) < cost(target)) {
		return(trial)
	} else {
		return(target)
	}
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Differential Evolution"),

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
            						choices = list("First De Jonge" = 1, "Second De Jonge" = 2, "Zimmermann's" = 3)),
            
            sliderInput("init",
            						"Initial parameter range:",
            						min = -100,
            						max = 100,
            						value = c(-25, 25)),
            
            numericInput("tol", "Tolerance:", 1e-6,
            						 min = 0),
            
            selectInput("dimension", "Select dimension:",
            						choices = character(0)),
            
            sliderInput("speed",
            						"Speed:",
            						min = 0.01,
            						max = 1000,
            						value = 500),
            
            actionButton("start", "Start"),
            
            actionButton("stop", "Stop"),
            
            actionButton("reset", "Reset")
        ),

        # Show a plot of the generated distribution
        mainPanel(
        	tabsetPanel(type = "tabs",
        							tabPanel("Cost function", plotOutput("costplot")),
        							
        							tabPanel("2D contour", plotOutput("contplot"))
        							)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
	
	dims <- reactive({ switch(input[["example"]],
														"1" = 3) })
	
	observe({
		
		choices <- switch(input[["example"]],
											"1" = 1:dims())
		
		updateSelectInput(session, "dimension",
											choices = choices)
		
		min <- switch(input[["example"]],
									"1" = -5.12)
		max <- switch(input[["example"]],
									"1" = 5.12)
		
		updateSliderInput(session, "init",
											min = min,
											max = max,
											value = c(min, max))
	})
	
	costfun <- reactive({
		if (input[["example"]] == 1) {
			function(x) sum(x^2)
		}
	})
	
	pops <- reactiveValues(
		G = NULL,
		i = 1,
		vtr = NULL,
		done = FALSE
	)
	
	output$costplot <- renderPlot({
		if (!is.null(pops$G)) {
			
			d <- as.numeric(input[["dimension"]])
			
			if (pops$i < 10) idx <- 1:pops$i
			else idx <- (pops$i-9):pops$i
			
			colors <- c("red", "blue", "green")
			
			cost <- apply(pops$G[,1,idx], 2, costfun())
			
			plot(pops$G[d,1,idx], cost, xlim = input[["init"]])
			lines(pops$G[d,1,idx], cost)
			mtext(paste("Iter:", pops$i))
			# seqx <- seq(input[["init"]][1], input[["init"]][2], length.out = 100)
			# lines(seqx, apply(cbind(seqx, seqx, seqx), 1, costfun()), lty = 2)
			abline(v = pops$vtr, lty = 2)
			
			for (j in 2:input[["np"]]) {
				
				cost <- apply(pops$G[,j,idx], 2, costfun())
				
				points(pops$G[d,j,idx], cost, col = colors[j-1])
				lines(pops$G[d,j,idx], cost, col = colors[j-1])
				
			}
		}
	})
	
	output$contplot <- renderPlot({
		if (!is.null(pops$G)) {
			
			if (pops$i < 10) idx <- 1:pops$i
			else idx <- (pops$i-9):pops$i
			
			colors <- c("red", "blue", "green")
			
			plot(pops$G[1,1,idx], pops$G[2,1,idx], xlim = input[["init"]], ylim = input[["init"]])
			lines(pops$G[1,1,idx], pops$G[2,1,idx])
			mtext(paste("Iter:", pops$i))

			abline(v = pops$vtr, h = pops$vtr, lty = 2)
			
			for (j in 2:input[["np"]]) {
				
				cost <- apply(pops$G[,j,idx], 2, costfun())
				
				points(pops$G[1,j,idx], pops$G[2,j,idx], col = colors[j-1])
				lines(pops$G[1,j,idx], pops$G[2,j,idx], col = colors[j-1])
				
			}
		}
	})
	
	trigger <- reactiveValues()
	trigger$timer <- reactiveTimer(Inf)
	
	observeEvent(input$start,
							 {
							 	
							 	pops$vtr <- switch(input[["example"]],
							 										 "1" = 0)
							 	
							 	if (is.null(pops$G)) {
							 		pops$G <- array(matrix(runif(input[["np"]], input[["init"]][1], input[["init"]][2]), ncol = input[["np"]], nrow = dims()), dim = c(dims(), input[["np"]], pops$i))
							 	}
							 	
							 	trigger$timer <- reactiveTimer(input[["speed"]])
							 	observeEvent(trigger$timer(),{
							 		
								 		gen <- matrix(0, ncol = input[["np"]], nrow = dims())
								 		
								 		for (j in 1:input[["np"]]) {
								 			target <- pops$G[,j,pops$i]
								 			mutant <- mutation(pops$G[,,pops$i], input[["f"]], j)
								 			trial  <- crossover(target, mutant, input[["cr"]])
								 			gen[,j]  <- selection(trial, target, costfun())
								 		}
								 		
								 		pops$i <- pops$i + 1
								 		pops$G <- array(c(pops$G, gen), dim = c(dims(), input[["np"]], pops$i))
								 		
								 		pops$done <- any(abs(apply(gen, 2, costfun())-pops[["vtr"]]) < input[["tol"]])
								 		
								 		#print(pops$G)
								 		#print(pops$i)
							 	})
							 })
	
	observeEvent(pops$done,{
		trigger$timer <- reactiveTimer(Inf)
	})
	
	observeEvent(input$stop,{
		trigger$timer <- reactiveTimer(Inf)
	})
	
	observeEvent(input$reset,{
		trigger$timer <- reactiveTimer(Inf)
		pops$G <- NULL
		pops$i <- 0
		pops$vtr <- NULL
		pops$done <- FALSE
	})
}

# Run the application 
shinyApp(ui = ui, server = server)
