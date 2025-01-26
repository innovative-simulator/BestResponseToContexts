##############################################################################
# Best-response to context-dependent beliefs
# R-like pseudo code
##############################################################################

library(data.table)

# Initialisation

Na <- 2 # Number of actions
actions <- 1:Na # Possible actions to choose between
outcomes <- 1:(Na * Na) # Number of possible interaction outcomes

S <- 200 # Size of population
population <- 1:S

Nc <- 4 # Number of contexts recognised by each person
contexts <- 1:Nc

m <- 0.9 # MSNE expectation for the game
g <- 1 # 1 if game rewards mis-matching moves, 0 if matching moves

Memory <- 0.9 # Memory of past opponents' moves.
Inertia <- 0.9 # Inertia holding back repositioning of contexts
SR <- 0.9 # Statistical retention rate

# Initial attributes
random.cor <- function(x) runif(x, min=-16.5, max=16.5) # Random coordinate
PX <- random.cor(population) # random.xcor
PY <- random.cor(population) # random.ycor

# Initial context-dependent belief positions and degrees
CX <- matrix(random.cor(S * Nc), nrow=S, ncol=Nc)
CY <- matrix(random.cor(S * Nc), nrow=S, ncol=Nc)
CD <- matrix(m, nrow=S, ncol=Nc)

# Frequencies of interaction outcome
F <- matrix(0, nrow=S, ncol=length(outcomes))

# Results time series
R <- data.table(t=integer(), DD=integer(), DH=integer(), HD=integer(), HH=integer())

##############################################################################

# Main loop

for (t in 1:2000) {
	# Pair up agents randomly
	RP <- sample(population, size=S, replace=FALSE) # Random permutation (shuffle) of population
	players <- RP
	opponents <- append(RP[(1+(S/2)):S], RP[1:(S/2)]) # Split and recombine halves swapped over 
	
	# Indentify one of most relevant contexts
	K[players] <- max.col( - distance_between(PX[opponents], PY[opponents], CX[players], CY[players]) )
	# NB: ties should be resolved randomly

	# Choose action using decision rule
	A[players] <- ifelse(
				CD[players,K[players]] < m, 
				1, 
				ifelse(
					CD[players,K[players]] > m, 
					0, 
					ifelse(
						random.float(1) < m, 
						1, 
						0
					)
				)
			)
	
	# Belief Learning
	CD[players] <- CD[players] * Memory + (1 - Memory) * A[opponents]
	
	# Relevance Learning
	SJ[players] <- shortest_jump(from=list(CX[K[players]], CY[K[players]]), to=list(PX[opponents], PY[opponents]))
	# NB: If world wraps around at edges, shortest path may be round the other way.
	(CX[players], CY[players]) <- (CX[K[players]], CY[K[players]]) + (
		Inertia * 
		SJ[players] *
		ifelse(A[players] == A[opponents], -1, 1) *
		g
	)		
	
	# Update frequency count of interaction outcome types
	F[players] <- F[players] * SR
	OT[players] <- 1 + A[opponents] + 2 * A[players] # 1+... because indexing from 1, not 0
	F[players, OT[players]] <- F[players, OT[players]] + 1 * (1 - SR)

	# Classify each agent by its most frequent interaction outcome
	MFI <- max.col(F) # For each person, index of largest value across outcomes
	# NB: ties should be resolved randomly

	# Count the number of agents with each MFI outcome type
	DD <- length(which( MFI==1 ))
	DH <- length(which( MFI==2 ))
	HD <- length(which( MFI==3 ))
	HH <- length(which( MFI==4 ))
	
	R <- rbind(R, data.table(t=t, DD=DD, DH=DH, HD=HD, HH=HH))
}

# End

##############################################################################
##############################################################################
