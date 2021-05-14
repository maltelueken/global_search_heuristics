
# Shiny app on global search heuristics
# 
# This is a didactic tool to visualize and understand the behavior of the 
# Differential Evolution algorithm by Storn and Price (1997).
# 
# By Malte Lüken and Marta Meléndez under supervision of Francis Tuerlinckx
# 
# Contact: malte_lueken@arcor.de
#


library(shiny)
library(shinyFiles)


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
	if (cost(trial) < cost(target)) {
		return(trial)
	} else {
		return(target)
	}
}


ui <- fluidPage(
	
	tags$head(tags$style(
		HTML('
         #sidebar, #sidebar2 {
            border: 1px solid black;
        }
        body, label, input, button, select { 
          font-family: "Arial";
        }'))),
	
	titlePanel("Differential Evolution"),
	
	fluidRow(
		column(4,
					 sidebarLayout(
					 	sidebarPanel(
					 		width = 12,
					 		id = "sidebar",
					 		
					 		div(style = "float:right", actionButton("help", "Help")),
					 		
					 		h3("Input"),
					 		
					 		selectInput("example", "Choose example:",
					 								choices = list("First De Jonge (easy)" = "deJonge1",
					 															 "Griewangk's (medium)" = "griewangk",
					 															 "Ackley's Path (medium)" = "ackley",
					 															 "Rosenbrock's Valley (hard)" = "rosenbrock",
					 															 "Rastrigin's (hard)" = "rastrigin "
					 															 )
					 								),
					 		
					 		sliderInput("dims",
					 								"Number of dimensions",
					 								min = 2,
					 								max = 10,
					 								value = 2),
					 		
					 		sliderInput("np",
					 								"Number of populations:",
					 								min = 4,
					 								max = 20,
					 								value = 4),
					 		
					 		sliderInput("f",
					 								"Mutation strength:",
					 								min = 0,
					 								max = 2,
					 								value = 0.5,
					 								step = 0.1),
					 		
					 		sliderInput("cr",
					 								"Crossover rate:",
					 								min = 0,
					 								max = 1,
					 								value = 0.5),
					 		
					 		sliderInput("init",
					 								"Initial parameter range:",
					 								min = -100,
					 								max = 100,
					 								value = c(-25, 25),
					 								ticks = FALSE),
					 		
					 		numericInput("tol", "Tolerance:", 1e-6,
					 								 min = 0)
					 	),
					 	mainPanel(width = 0)
					 )	 
		),
		column(8,
					 sidebarLayout(
					 	sidebarPanel(
					 		width = 12,
					 		
					 		h3("Control"),
					 		
					 		id = "sidebar2",
					 		
					 		fluidRow(
					 			column(6,
					 						 column(4,
					 						 			 
					 						 			 actionButton("start", "Start", width = 60),
					 						 			 
					 						 			 actionButton("skip", "Skip ", width = 60)
					 						 ),
					 						 
					 						 column(4,
					 						 			 
					 						 			 actionButton("stop", "Stop", width = 60),
					 						 			 
					 						 			 numericInput("steps", NULL, value = 5, width = 60)
					 						 ),
					 						 
					 						 column(4,
					 						 			 
					 						 			 actionButton("reset", "Reset", width = 60),
					 						 			 
					 						 			 actionButton("back", "Back ", width = 60)
					 						 )
					 			)
					 			,
					 			
					 			column(6,
					 						 
					 						 sliderInput("speed",
					 						 						"Speed (in ms):",
					 						 						min = 0,
					 						 						max = 1000,
					 						 						value = 500)
					 			)
					 			
					 		)
					 	),
					 	
					 	# Show a plot of the generated distribution
					 	mainPanel(
					 		width = 12,
					 		
					 		h3("Output"),
					 		
					 		tabsetPanel(type = "tabs",
					 								tabPanel("Cost function",
					 												 fluidRow(
					 												 	column(4,
					 												 	
					 												 				 checkboxInput("showOptionsCost", "Show output options", value = FALSE),
					 												 	),
					 												 	
					 												 	column(4,
					 												 				 
					 												 				 conditionalPanel(condition = "input.dims == 2",
					 												 				 								 
					 												 				 								 checkboxInput("contourCost", "Show function contour", value = TRUE)
					 												 				 )
					 												 	),
					 												 	
					 												 	column(4,
					 												 				 
					 												 				 shinySaveButton("saveDirCost", "Save output", title = "Choose Directory", filetype = c(PNG = "png"))
					 												 	)
					 												 ),
					 												 
					 												 fluidRow(
					 												 	
					 												 	conditionalPanel(condition = "input.showOptionsCost == true",
					 												 									 column(4,
					 												 									 			 
					 												 									 			 selectInput("dimensionCost", "Select dimension:",
					 												 									 			 						choices = character(0)),
					 												 									 			 
					 												 									 			 selectInput("popsCost", "Select populations:",
					 												 									 			 						choices = character(0))
					 												 									 ),
					 												 									 
					 												 									 column(4,
					 												 									 			 
					 												 									 			 textInput("xlabCost", "X label:", value = "Parameter 1"),
					 												 									 			 
					 												 									 			 numericInput("xminCost", "X min.:", value = NULL)
					 												 									 ),
					 												 									 
					 												 									 column(4,
					 												 									 			 
					 												 									 			 textInput("ylabCost", "Y label:", value = "Cost"),
					 												 									 			 
					 												 									 			 numericInput("xmaxCost", "X max.:", value = NULL)
					 												 									 ),
					 												 	)
					 												 ),
					 												 
					 												 plotOutput("costPlot")
					 												 
					 								),
					 								
					 								tabPanel("2D plane",
					 												 fluidRow(
					 												 	
					 												 	column(4,
					 												 				 
					 												 				 checkboxInput("showOptionsPlane", "Show output options", value = FALSE),
					 												 	),
					 												 	
					 												 	column(4,
					 												 				 
					 												 				 conditionalPanel(condition = "input.dims == 2",
					 												 				 
					 												 				 								 checkboxInput("contourPlane", "Show function contour", value = TRUE)
					 												 				 )
					 												 	),
					 												 	
					 												 	column(4,
					 												 				 
					 												 				 shinySaveButton("saveDirPlane", "Save output", title = "Choose Directory", filetype = c(PNG = "png"))
					 												 	)
					 												 ),
					 												 
					 												 fluidRow(
					 												 	
					 												 	conditionalPanel(condition = "input.showOptionsPlane == true",
					 												 									 column(4,
					 												 									 			 
					 												 									 			 selectInput("dimensionPlane", "Select dimension:",
					 												 									 			 						choices = character(0)),
					 												 									 			 
					 												 									 			 selectInput("popsPlane", "Select populations:",
					 												 									 			 						choices = character(0))
					 												 									 ),
					 												 									 
					 												 									 column(4,
					 												 									 			 
					 												 									 			 textInput("xlabPlane", "X label:", value = "Parameter 1"),
					 												 									 			 
					 												 									 			 textInput("ylabPlane", "Y label:", value = "Parameter 2")
					 												 									 ),
					 												 									 
					 												 									 column(2,
					 												 									 			 
					 												 									 			 numericInput("xminPlane", "X min.:", value = NULL),
					 												 									 			 
					 												 									 			 numericInput("yminPlane", "Y min.:", value = NULL)
					 												 									 ),
					 												 									 
					 												 									 column(2,
					 												 									 			 
					 												 									 			 numericInput("xmaxPlane", "X max.:", value = NULL),
					 												 									 			 
					 												 									 			 numericInput("ymaxPlane", "Y max.:", value = NULL)
					 												 									 )	
					 												 	)
					 												 ),
					 												 
					 												 plotOutput("planePlot")
					 												 
					 								)
					 		)
					 	)
					 )
		)
	)
)

server <- function(input, output, session) {
	
	dims <- reactive({
		input[["dims"]]
	})
	
	examples <- reactive({
		list(
			deJonge1 = list(
				min = -5.12,
				max = 5.12,
				vtr = 0,
				fun = function(x) sum(x^2)
			),
			rosenbrock = list(
				min = -2.048,
				max = 2.048,
				vtr = 1,
				fun = function(x) sum(sapply(1:(length(x)-1), function(i) 100 * (x[i+1] - x[i]^2)^2 + (1 - x[i])^2))
			),
			rastrigin = list(
				min = -5.12,
				max = 5.12,
				vtr = 0,
				fun = function(x) 10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
			),
			griewangk = list(
				min = -600,
				max = 600,
				vtr = 0,
				fun = function(x) sum(x^2/4000) - prod(sapply(1:length(x), function(i) cos(x[i]/sqrt(i)))) + 1
			),
			ackley = list(
				min = -32.786,
				max = 32.786,
				vtr = 0,
				fun = function(x) -20*exp(-0.2 * sqrt(sum(x^2)/length(x))) - exp(sum(cos(2 * pi * x))/length(x)) + 20 + exp(1)
			)
		)[[input[["example"]]]]
	})
	
	observe({
		
		updateSelectInput(session, "dimensionCost",
											choices = 1:dims())
		
		combinationsPlane <- expand.grid(y = 1:dims(), x = 1:dims())
		
		choicesPlane <- apply(combinationsPlane[combinationsPlane[["x"]] != combinationsPlane[["y"]],], 1, paste, collapse = " - ")
		
		updateSelectInput(session, "dimensionPlane",
											choices = unname(choicesPlane))
		
		updateSelectInput(session, "popsCost",
											choices = 1:input[["np"]],
											selected = input[["np"]])
		
		updateSelectInput(session, "popsPlane",
											choices = 1:input[["np"]],
											selected = input[["np"]])
		
		min <- examples()[["min"]]
		max <- examples()[["max"]]
		
		updateNumericInput(session, "xminCost", value = min)
		updateNumericInput(session, "xmaxCost", value = max)
		
		updateNumericInput(session, "xminPlane", value = min)
		updateNumericInput(session, "xmaxPlane", value = max)
		updateNumericInput(session, "yminPlane", value = min)
		updateNumericInput(session, "ymaxPlane", value = max)
		
		updateSliderInput(session, "init",
											min = min,
											max = max,
											value = c(min, max),
											step = (max - min) / 10)
	})
	
	costfun <- reactive({
		examples()[["fun"]]
	})
	
	vtr <- reactive({
		examples()[["vtr"]]
	})
	
	pops <- reactiveValues(
		G = NULL,
		i = 1,
		done = FALSE
	)
	
	createCostPlot <- function() {
		if (!is.null(pops$G)) {
			
			d <- as.numeric(input[["dimensionCost"]])
			
			if (pops$i < 10) idx <- 1:pops$i
			else idx <- (pops$i-9):pops$i
			
			cost <- if(pops$i == 1) costfun()(pops$G[,1,idx]) else apply(pops$G[,1,idx], 2, costfun())
			
			xlim <- c(input[["xminCost"]], input[["xmaxCost"]])
			
			plot(pops$G[d,1,idx], cost, col = 1, xlim = xlim, 
					 xlab = input[["xlabCost"]], ylab = input[["ylabCost"]],
					 cex = 1.2, cex.lab = 1.5)
			lines(pops$G[d,1,idx], cost, col = 1)
			mtext(paste("Iteration:", pops$i), cex.main = 1.25)
			abline(v = vtr(), lty = 2)
			
			if (input[["popsCost"]] > 1) {
				for (j in 2:input[["popsCost"]]) {
					
					cost <- if(pops$i == 1) costfun()(pops$G[,1,idx]) else apply(pops$G[,1,idx], 2, costfun())
					
					points(pops$G[d,j,idx], cost, col = j, ylim = range(cost), cex = 1.2)
					lines(pops$G[d,j,idx], cost, col = j)
					
				}
			}
			
			if (dims() == 2 && input[["contourCost"]]) {
				
				n1 <- 100
				n2 <- 100
				
				xseq <- seq(xlim[1], xlim[2], length.out = n1)
				yseq <- seq(xlim[1], xlim[2], length.out = n2)
				zmat <- matrix(0, nrow = n1, ncol = n2)
				
				for (i in 1:n1) {
					for (j in 1:n2) {
						zmat[i,j] <- do.call(costfun(), list(c(xseq[i], yseq[j])))
					}
				}
				
				for (j in 1:n2) {
					lines(xseq, zmat[,j], lty = 2, col = "grey")
				}
			}
		}
	}
	
	output$costPlot <- renderPlot({
		createCostPlot()
	})
	
	createPlanePlot <- function() {
		if (!is.null(pops$G)) {
			
			if (pops$i < 10) idx <- 1:pops$i
			else idx <- (pops$i-9):pops$i
			
			d <- as.numeric(strsplit(input[["dimensionPlane"]], " - ")[[1]])
			
			xlim <- c(input[["xminPlane"]], input[["xmaxPlane"]])
			ylim <- c(input[["yminPlane"]], input[["ymaxPlane"]])
			
			plot(pops$G[d[1],1,idx], pops$G[d[2],1,idx], col = 1, xlim = xlim, ylim = ylim,
					 xlab = input[["xlabPlane"]], ylab = input[["ylabPlane"]],
					 cex = 1.2, cex.lab = 1.5)
			lines(pops$G[d[1],1,idx], pops$G[d[2],1,idx], col = 1)
			mtext(paste("Iteration:", pops$i), cex.main = 1.25)
			
			abline(v = vtr(), h = vtr(), lty = 2)
			
			if (input[["popsPlane"]] > 1) {
				for (j in 2:input[["popsPlane"]]) {
					
					points(pops$G[d[1],j,idx], pops$G[d[2],j,idx], col = j, cex = 1.2)
					lines(pops$G[d[1],j,idx], pops$G[d[2],j,idx], col = j)
					
				}
			}
			
			if (dims() == 2 && input[["contourPlane"]]) {
				
				n <- 100
				
				xseq <- seq(xlim[1], xlim[2], length.out = n)
				yseq <- seq(ylim[1], ylim[2], length.out = n)
				zmat <- matrix(0, nrow = n, ncol = n)
				
				for (i in 1:n) {
					for (j in 1:n) {
						zmat[i,j] <- do.call(costfun(), list(c(xseq[i], yseq[j])))
					}
				}
				
				zseq <- seq(log(min(zmat)), log(max(zmat)), length.out = 10)
				
				contour(x = xseq, y = yseq, z = zmat, levels = exp(zseq), drawlabels = FALSE, add = TRUE, lty = 2, col = "grey")
			}
		}
	}
	
	output$planePlot <- renderPlot({
		createPlanePlot()
	})
	
	trigger <- reactiveValues()
	trigger$timer <- reactiveTimer(Inf, session)
	
	diffEv <- function() {
		if (!is.null(pops$G)) {
			gen <- matrix(0, ncol = isolate(input[["np"]]), nrow = dims())
			
			for (j in 1:isolate(input[["np"]])) {
				target <- pops$G[,j,pops$i]
				mutant <- mutation(pops$G[,,pops$i], isolate(input[["f"]]), j)
				trial  <- crossover(target, mutant, isolate(input[["cr"]]))
				gen[,j]  <- selection(trial, target, isolate(costfun()))
			}
			
			pops$i <- pops$i + 1
			
			# print(pops$i)
			
			pops$G <- array(c(pops$G, gen), dim = c(dims(), isolate(input[["np"]]), pops$i))
			
			pops$done <- any(abs(vtr() - apply(gen, 2, costfun())) < isolate(input[["tol"]]))
		}
	}
	
	observeEvent(input$start,
							 {
							 	
							 	if (is.null(pops$G)) {
							 		pops$G <- array(matrix(runif(input[["np"]], input[["init"]][1], input[["init"]][2]), 
							 													 ncol = input[["np"]], nrow = dims()), 
							 										dim = c(dims(), input[["np"]], pops$i))
							 	}
							 	
							 	trigger$timer <- reactiveTimer(input[["speed"]], session)
							 	
							 	
							 })
	
	observeEvent(trigger$timer(), {
		diffEv()
	})
	
	observeEvent(pops$done, {
		trigger$timer <- reactiveTimer(Inf, session)
	})
	
	observeEvent(input$stop, {
		trigger$timer <- reactiveTimer(Inf, session)
	})
	
	observeEvent(input$skip, {
		for (i in 1:input$steps) {
			diffEv()
		}
	})
	
	observeEvent(input$back, {
		pops$i <- pops$i - input$steps
	})
	
	observeEvent(input$reset, {
		trigger$timer <- reactiveTimer(Inf, session)
		pops$G <- NULL
		pops$i <- 1
		pops$vtr <- NULL
		pops$done <- FALSE
	})
	
	observeEvent(input$saveDirCost, {
		if (!is.null(pops$G) && !is.integer(input$saveDirCost)) {
			
			png(filename = input$saveDirCost)
			
			createCostPlot()
			
			dev.off()
		}
	})
	
	observeEvent(input$saveDirPlane, {
		if (!is.null(pops$G) && !is.integer(input$saveDirPlane)) {
			
			png(filename = input$saveDirPlane)
			
			createPlanePlot()
			
			dev.off()
		}
	})
	
	shinyFileSave(input, "saveDirCost", session = session, roots = c('wd' = '.'), defaultRoot = "wd")
	shinyFileSave(input, "saveDirPlane", session = session, roots = c('wd' = '.'), defaultRoot = "wd")
	
	observeEvent(input$help, {
		showModal(modalDialog(
			title = "Help",
			tabsetPanel(type = "tabs",
									tabPanel("General",
													 fluidRow(
													 	column(12,
													 				 
													 				 withMathJax(),
													 				 
													 				 h4("What is Differential Evolution?"),
													 				 
													 				 "Differential Evolution (DE) is a search algorithm developed by Storn and Price (1997) that aims to find the global minimum of a function f(x).",
													 				 
													 				 h4("How does it work?"),
													 				 
													 				 "DE simulates multiple agents called populations that can communicate with each other to move efficiently through the parameter space and find the global minimum.",
													 				 
													 				 "Each population starts at a randomly chosen value within the initial parameter range of the function f(x).",
													 				 
													 				 "At every iteration of DE, each population performs three actions. The current parameter vector", tags$b("x"), "of a population is called the", tags$i("target"), "vector.",
													 				 
													 				 h5("Mutation"),
													 				 
													 				 "A new", tags$i("mutant"), "vector", tags$b("m"), "is created as a function of three randomly chosen populations other than the target:",
													 				 
													 				 "$$\\mathbf{m} = \\mathbf{x}_{p1} + F * (\\mathbf{x}_{p2} - \\mathbf{x}_{p3}).$$",
													 				 
													 				 "The mutation strength F is a parameter that amplifies the influence of the difference between populations on the mutant vectors.",
													 				 
													 				 h5("Crossover"),
													 				 
													 				 "A new", tags$i("trial"), "vector", tags$b("g"), "is created by randomly interchanging a proportion of the target vector and the mutant vector", 
													 				 
													 				 "The crossover rate (CR) indicates the proportion of swapped vector elements.",
													 				 
													 				 h5("Selection"),
													 				 
													 				 "The trial vector is compared against the target vector",
													 				 
													 				 "If the function value for the trial f(g) is smaller than the function value of the target vector f(x),",
													 				 
													 				 "the trial vector is set to be the new target vector for the next iteration. Otherwise the target vector remains the same.",
													 				 
													 				 "The algorithm stops when one of the populations has reached the global minimum plus a tolerance value.",
													 				 
													 				 h4("Why does it work?"),
													 				 
													 				 "The advantage of DE is that its agents can escape local minima.",
													 				 
													 				 "The parameter vector of the current population is changed as a function of the difference between other populations.",
													 				 
													 				 "Thus, when the populations a very separated, they will change a lot, and when they are close together, there will be small changes.",
													 				 
													 				 "Because of the selection step, only changes that further minimize the function value are accepted",
													 				 
													 				 "This makes the populations follow the population with the lowest function value.",
													 				 
													 				 "Because mutation and crossover are random, populations can also jump over 'ridges' in the function.",
													 				 
													 				 "The algorithm gets usually more efficient when more populations are simulated because the chance is higher that a population randomly moves to the",
													 				 
													 				 "global minimum.",
													 				 
													 				 h4("Reference"),
													 				 
													 				 "Storn, R. & Price, K. (1997). Differential evolution - A simple and efficient heuristic for global optimization over continuous spaces.", 
													 				 tags$i("Journal of Global Optimization, 11"),
													 				 ", 341-359."
													 				 
													 				 )
													 	)
													 ),
									tabPanel("Examples",
													 fluidRow(
													 	column(12,
													 				 
													 				 "Five different example functions are provided that can be minimized with DE.",
													 				 
													 				 h4("First De Jonge Function"),
													 				 
													 				 "This is a function that can be easiliy minimized with DE. It constitutes a multidimensional sphere.",
													 				 
													 				 "Has its global minimum at f(x) = 0.",
													 				 
													 				 output$exDeJonge1 <- renderImage(
													 				 	list(src = "examples/deJonge1.png"),
													 				 	deleteFile = FALSE
													 				 ),
													 				 
													 				 h4("Griewangk's Function"),
													 				 
													 				 "This is a function with medium difficulty. It has many regularly distributed local minima close to global minimum,",
													 				 
													 				 "but is flat far away from the global minimum.",
													 				 
													 				 "Has its global minimum at f(x) = 0.",
													 				 
													 				 output$exGriewangk <- renderImage(
													 				 	list(src = "examples/griewangk.png"),
													 				 	deleteFile = FALSE
													 				 ),
													 				 
													 				 h4("Ackley's Path"),
													 				 
													 				 "This is a function with medium difficulty. It is relatively flat far away from the global minimum,",
													 				 
													 				 "but steep with local minima close to global minimum.",
													 				 
													 				 "Has its global minimum at f(x) = 0.",
													 				 
													 				 output$exAckley <- renderImage(
													 				 	list(src = "examples/ackley.png"),
													 				 	deleteFile = FALSE
													 				 ),
													 				 
													 				 h4("Rosenbrock's Valley"),
													 				 
													 				 "This is a function that is hard to minimize with DE. It has a long, narrow, and flat valley that is easily reached",
													 				 
													 				 "but where the global minimum is difficult to find.",
													 				 
													 				 "Has its global minimum at f(x) = 1.",
													 				 
													 				 output$exRosenbrock <- renderImage(
													 				 	list(src = "examples/rosenbrock.png"),
													 				 	deleteFile = FALSE
													 				 ),
													 				 
													 				 h4("Rastrigin's Function"),
													 				 
													 				 "This is a function that is hard to minimize with DE. It has many local minima over the entire parameter space.",
													 				 
													 				 "Has its global minimum at f(x) = 0.",
													 				 
													 				 output$exRastrigin <- renderImage(
													 				 	list(src = "examples/rastrigin.png"),
													 				 	deleteFile = FALSE
													 				 ),
													 				 
													 				 h4("Links"),
													 				 
													 				 "See",
													 				 
													 				 tags$a(href="http://www.geatbx.com/docu/fcnindex-01.html#P89_3085", "this link"),
													 				 
													 				 "for more information."
													 				 
													 				 )
													 	)
									),
									tabPanel("Input",
													 fluidRow(
													 	column(12,
													 				 
													 				 h4("Choose example"),
													 				 
													 				 "Choose one of the five example functions that are currently implemented.",
													 				 
													 				 h4("Number of dimensions"),
													 				 
													 				 "Select on how many dimensions the function should be minimized.",
													 				 
													 				 "More dimensions make the minimization problem more difficult.",
													 				 
													 				 h4("Number of populations"),
													 				 
													 				 "Select the number of populations that DE uses to minimize the function.",
													 				 
													 				 "More populations make DE usually more efficient.",
													 				 
													 				 h4("Mutation strength"),
													 				 
													 				 "The influence of the difference between populations on the mutation vectors.",
													 				 
													 				 "Higher values lead the populations to change more quickly when they are far apart from each other.",
													 				 
													 				 h4("Crossover rate"),
													 				 
													 				 "The proportion of elements that are randomly swapped between trial and mutation vectors.",
													 				 
													 				 "Higher values lead the populations to change more quickly when mutation is strong.",
													 				 
													 				 h4("Initial parameter range"),
													 				 
													 				 "Range of the randomly generated starting values for the populations. Applies to each dimension.",
													 				 
													 				 h4("Tolerance"),
													 				 
													 				 "Value that must be reached to conclude that DE has converged to the global minimum."
													 				 
													 	)
													 )
									)
			),
									
			size = "l",
			fade = F,
			easyClose = TRUE
		))
	})
}

shinyApp(ui = ui, server = server)
