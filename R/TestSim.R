##############################################################################
##############################################################################

library(data.table)

##############################################################################

setwd("D:\\Share\\MNodes\\R")

setwd("C:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\M-Nodes\\BestResponseToContexts\\R")

##############################################################################

source("BestResponseToContextDependentBeliefs.R")

##############################################################################
##############################################################################
# Experiments
##############################################################################
##############################################################################
source("BestResponseToContextDependentBeliefs.R")

A <- experiment(exp.factor=c("inertia"), factor.values = c(0, 0.1, 0.3, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 1), num.repetitions = 5)
A
fwrite(A, "Test_Experiment_Inertia.csv")

A <- experiment(exp.factor=c("msne"), factor.values = (0.05 * 0:20), num.repetitions = 5)
A
fwrite(A, "Test_Experiment_MSNE.csv")

A <- experiment(exp.factor=c("memory"), factor.values = (0.05 * 0:20), num.repetitions = 5)
A
fwrite(A, "Test_Experiment_Memory.csv")

A <- experiment_memory_50(exp.factor=c("msne"), factor.values = (0.05 * 0:20), num.repetitions = 5)
A
fwrite(A, "Test_Experiment_Memory_50_MSNE.csv")

##############################################################################
##############################################################################

set.seed(1)
P <- base_case_parameters()
P$run.length <- 1000
P$num.people <- 100
#P$num.cbeliefs <- 8
#P$inertia <- 1
#P$memory <- 0.5
#P$msne <- 0.1

##############################################################################
##############################################################################

MR <- sim_run_results(P=P, world_update=100)
M <- MR$model
R <- MR$results
perc_timeseries(R)

MR <- sim_run_results(P=P, timeseries_update=100)
M <- MR$model
R <- MR$results
M$plot_pop()

##############################################################################
##############################################################################

# If M is a model, can get data on population.

##############################################################################
pop <- M$pop()
G <- groups_info()
id <- sapply(1:dim(pop)[2], function(i) pop[,i]$who() )
x <- sapply(1:dim(pop)[2], function(i) pop[,i]$xcor() )
y <- sapply(1:dim(pop)[2], function(i) pop[,i]$ycor() )
z <- sapply(1:dim(pop)[2], function(i) {G[1 + pop[,i]$mfi_type(), lab] })
H <- data.table(id=id, x=x, y=y, z=z, key="id")

table(z)

##############################################################################
##############################################################################

# Reload datafile and plot results

##############################################################################

source("BestResponseToContextDependentBeliefs.R")
B <- file_processed("Test_Experiment_Inertia.csv")
P <- plot_inertia(B)
P

source("BestResponseToContextDependentBeliefs.R")
B <- file_processed("Test_Experiment_Memory_50_MSNE.csv")
P <- plot_msne(B)
P
# Yes, the R version looks just as crazy as the NetLogo one!

