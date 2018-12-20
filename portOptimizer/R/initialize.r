#initialize environment and variables
portOptimizer.env <- new.env()
assign("penaltyFactor", 25000000000, envir = portOptimizer.env)
assign("maxdelta", c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.10), envir = portOptimizer.env)
