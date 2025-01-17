
library(data.table)

setwd("C:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\M-Nodes\\BestResponseToContexts\\R")

source("BestResponseToContextDependentBeliefs.R")
set.seed(1)
P <- base_case_parameters()
P$run.length <- 1000
P$num.people <- 100
#P$num.cbeliefs <- 8
#P$inertia <- 1
#P$memory <- 0.5
#P$msne <- 0.1

MR <- sim_run_results(P=P, world_update=100)
M <- MR$model
R <- MR$results
perc_timeseries(R)

MR <- sim_run_results(P=P, timeseries_update=100)
M <- MR$model
R <- MR$results
M$plot_pop()

# Experiments
A <- experiment_inertia(num.repetitions = 2)
A
fwrite(A, "Test_Experiment_Inertia.csv")

# If M is a model, can get data on population.
pop <- M$pop()
G <- groups_info()
id <- sapply(1:dim(pop)[2], function(i) pop[,i]$who() )
x <- sapply(1:dim(pop)[2], function(i) pop[,i]$xcor() )
y <- sapply(1:dim(pop)[2], function(i) pop[,i]$ycor() )
z <- sapply(1:dim(pop)[2], function(i) {G[1 + pop[,i]$mfi_type(), lab] })
H <- data.table(id=id, x=x, y=y, z=z, key="id")

table(z)
