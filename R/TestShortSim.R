##############################################################################

setwd("D:\\Share\\MNodes\\R")

setwd("C:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\M-Nodes\\BestResponseToContexts\\R")

##############################################################################

source("Short_BestResponseToContextDependentBeliefs.R")

##############################################################################

set.seed(2)
P <- base_case_parameters()
P$MSNE <- 90
P$Memory <- 90
P$Inertia <- 90
P$Statistics.Retention <- 90
#P$Game.Type=-1
P$Run.Length <- 1000
system.time(MR <- sim_run_results(P))
R <- MR$Results
perc_timeseries(R)

PX <- MR$PX
PY <- MR$PY
MFI <- MR$MFI

D <- data.table(x=PX, y=PY, z=c("DD", "DH", "HD", "HH")[MFI])
plot_pop(D, Ticks=Steps)

CX <- MR$CX
CY <- MR$CY
CD <- MR$CD

MCX <- melt_by_col(CX)[,Value]
MCY <- melt_by_col(CY)[,Value]
MCD <- melt_by_col(CD)[,Value]

L <- data.table(x=MCX, y=MCY, z=categorize_by_degree(MCD, threshold=P$MSNE, margin=0.05))
plot_pop(L, Ticks=Steps)

plot_pop(rbind(D,L), Ticks=Steps)

dim(L)
head(L)
ggplot(L, aes(x=x, y=y, z=z, color=z, shape=z)) + geom_point()

head(D)

#ggplot(rbind(D,L), aes(x=x, y=y, z=z, color=z, shape=z)) + geom_point()



##############################################################################

	Na <- P$Num.Actions # Number of actions
	actions <- 1:Na # Possible actions to choose between
	outcomes <- 1:(Na * Na) # Number of possible interaction outcomes

	S <- P$Num.People # Size of population
	population <- 1:S

	Nc <- P$Num.CBeliefs # Number of contexts recognised by each person
	contexts <- 1:Nc

	m <- P$MSNE # MSNE expectation for the game
	g <- P$Game.Type # 1 if game rewards mis-matching moves, 0 if matching moves

	Memory <- P$Memory # Memory of past opponents' moves.
	Inertia <- P$Inertia # Inertia holding back repositioning of contexts
	SR <- P$Statistics.Retention # Statistical retention rate

	# Initial attributes
	random.cor <- function(x) runif(x, min=0, max=P$World.Width) # Random coordinate
	PX <- random.cor(population) # random.xcor
	PY <- random.cor(population) # random.ycor

	# Initial context-dependent belief positions and degrees
	CX <- matrix(random.cor(S * Nc), nrow=S, ncol=Nc)
	CY <- matrix(random.cor(S * Nc), nrow=S, ncol=Nc)
	CD <- matrix(m, nrow=S, ncol=Nc)

	# Number of time steps
	RL <- P$Run.Length
	
	# Frequencies of interaction outcome
	F <- matrix(0, nrow=S, ncol=length(outcomes))
	
	# Results time series
	R <- data.table(Step=integer(), DD=integer(), DH=integer(), HD=integer(), HH=integer())
	
	# Internal storage
	K <- 0 * population
	A <- 0 * population
	OT <- 0 * population
	MFI <-  0 * population
	SJX <- 0 * population
	SJY <- 0 * population


##############################################################################

		RP <- sample(population, size=S, replace=FALSE) # Random permutation (shuffle) of population
#RP <- population
		players <- RP
		opponents <- append(RP[(1+(S/2)):S], RP[1:(S/2)]) # Split and recombine halves swapped over 
#data.table(players, opponents)

		# Indentify one of most relevant contexts
		K[players] <- max.col( - distance_between(PX[opponents], PY[opponents], CX[players,], CY[players,]) )
table
		# NB: ties should be resolved randomly

		# Choose action using decision rule
		A[players] <- decision_rule(CD[players,][K[players]], m)
table(A)
head(K[players])
head(CD[players,])		
head(CD[players,][K[players]])
head(CD[players,][,K[players]])
head(CD[players,][K[players],])
head(players)
CD[3,]
CD[3,][1]
CD[3,][2]
CD[3,][3]
CD[3,][4]
CD[3,1]
CD[3,2]
CD[3,3]
CD[3,4]

CX[players[3],2] <- 9
CX[players[2:3],][K[players[2:3]]] <- c(25, 26)
CX[players[2:3],][players[2:3],K[players[2:3]]]
CX[players[2:3],][2:3,c(3,2)]
K[players[2:3]]
CX[players[2:3],][1,2]


head(sapply(players, function(a) CX[a,][K[a]]))

head(CX[players,])
head(K[players])
head(CX[players,][K[players]])
head(CX[c(players, K[players])])
head(CX[K[players]])
KK <- K[players]
head(KK)
head(t(KK))
head(t(CX[players,])[K[players]])
head(t(CX[players,])[,K[players]])
head(CXP)
head(KK)
head(CXP[,][KK])
head(t(t(CXP)[,KK]))

sample.int(20, 10, replace = TRUE)


X <- matrix(1:8, nrow=4, ncol=2)
X
I <- sample.int(4, 4, replace = FALSE)
I
J <- sample.int(4, 4, replace = FALSE)
J
C <- sample.int(2, 4, replace = TRUE)
C
C[I]
X[I,]
X[I,][C[I]]
t(X[,])
sapply(I, function(x) X[x,][C[x]]) # Offsets by row, not column
sapply(I, function(x) X[x,][C[x]] <- 2 * X[x,][C[x]]) # Generates correct data, but doesn't alter source
sapply(I, function(x) X[x,][C[x]] <<- 2 * X[x,][C[x]]) # Alters source correctly

for (i in 1:2) {
mapply(function(x, y) X[x,][C[x]] <<- 2 * X[x,][C[x]], I, J) # Alters source correctly
}
X

##############################################################################

relevance_learning(cx=-10, ax=15, matching=FALSE, inertia=90, game.type=1, min.pcor=-16, max.pcor=16)


##############################################################################

source("Short_BestResponseToContextDependentBeliefs.R")

ER <- experiment(exp.factor="MSNE", factor.values=5 * 0:20)
ER <- experiment(exp.factor="Inertia", factor.values=c(0, 20, 40, 50, 60, 65, 70, 75, 80, 85, 90, 95, 99, 100))
ER <- experiment(exp.factor="Inertia", factor.values=5 * 0:20)
ER <- experiment(exp.factor="Memory", factor.values=5 * 0:20)
ER <- experiment(exp.factor="Num.CBeliefs", factor.values=c(1, 2, 3, 4, 6, 8, 12, 16))

MER <- melt_by_mfi(ER)
dim(MER)
head(MER)

plot_perc_v_x(MER[,.(x=MSNE, y=Perc, z=MFI.Type)],xlab="MSNE")

plot_perc_v_x(MER[,.(x=Memory, y=Perc, z=MFI.Type)],xlab="Memory")

plot_perc_v_x(MER[,.(x=Inertia, y=Perc, z=MFI.Type)],xlab="Inertia")

plot_perc_v_x(MER[,.(x=Num.CBeliefs, y=Perc, z=MFI.Type)],xlab="Number of C-Beliefs")


head(ER)
dim(ER)
