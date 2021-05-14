### Create Figures for Example Functions ###

examples <- list(
	deJonge1 = list(
		min = -5.12,
		max = 5.12,
		vtr = 0,
		fun = function(x) sum(x^2)
	),
	rosenbrock = list(
		min = -2.048,
		max = 2.048,
		vtr = 0,
		fun = function(x) sum(sapply(1:(length(x)-1), function(i) 100 * (x[i+1] - x[i]^2)^2 + (1 - x[i])^2))
	),
	rastrigin = list(
		min = -5.12,
		max = 5.12,
		vtr = 0,
		fun = function(x) 10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
	),
	griewangk = list(
		min = -150,
		max = 150,
		vtr = 0,
		fun = function(x) sum(x^2/4000) - prod(sapply(1:length(x), function(i) cos(x[i]/sqrt(i)))) + 1
	),
	ackley = list(
		min = -32.786,
		max = 32.786,
		vtr = 0,
		fun = function(x) -20*exp(-0.2 * sqrt(sum(x^2)/length(x))) - exp(sum(cos(2 * pi * x))/length(x)) + 20 + exp(1)
	)
)

create_3d_figure <- function(ex, name, n = 100) {
	
	xseq <- seq(ex$min, ex$max, length.out = n)
	zmat <- matrix(0, nrow = n, ncol = n)
	
	for (i in 1:n) {
		for (j in 1:n) {
			zmat[i,j] <- do.call(ex$fun, list(c(xseq[i], xseq[j])))
		}
	}
	
	jet.colors <- colorRampPalette(c("red", "gray"))
	nbcol <- 100
	color <- jet.colors(nbcol)
	
	zfacet <- zmat[-1, -1] + zmat[-1, -n] + zmat[-n, -1] + zmat[-n, -n]
	zcol <- cut(zfacet, nbcol)
	
	png(paste0("examples/", name, ".png"))
	
	persp(xseq, xseq, zmat,
				phi = 30, theta = 30,
				xlab = "Dimension 1",
				ylab = "Dimension 2",
				zlab = "Function value f(x)",
				col = color[zcol])
	
	dev.off()
	
	return()
}

for(i in 1:length(examples)) {
	create_3d_figure(examples[[i]], names(examples)[i])
}
