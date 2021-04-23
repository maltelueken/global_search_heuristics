
mutation <- function(G, f, i) {
	idx <- sample((1:ncol(G))[-3], size = 3, replace = FALSE)
	
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

differential_evolution <- function(G, np, f, CR, cost, max_gen) {
	
	for (g in 1:max_gen) {
		for (i in 1:np) {
			target <- G[,i]
			mutant <- mutation(G, f, i)
			trial  <- crossover(target, mutant, CR)
			G[,i]  <- selection(trial, target, cost)
		}
	}
	
	res <- apply(G, 2, cost)
	
	return(min(res))
}

n <- 100

dat <- rnorm(n, 0, 5)


CR <- 0.5

np <- 4

G <- matrix(c(runif(np, -25, 25),
							log(runif(np, 0, 25))),
						ncol = np, nrow = 2, byrow = TRUE)

ll <- function(x) {
	-sum(dnorm(dat, x[1], exp(x[2]), log = TRUE))
}

differential_evolution(G, np, 1, CR, ll, 100)
