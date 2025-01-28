##############################################################################
# Best-response to context-dependent beliefs
# R-like pseudo code
##############################################################################

library(data.table)
library(ggplot2)

sim_run_results <- function(P=base_case_parameters()) {

	# Initialisation

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
	Min.Pcor <- 0
	Max.Pcor <- P$World.Width - Min.Pcor - 1 # Intended to match max-pxcor in NetLogo
	World.Width <- P$World.Width
	World.Height <- P$World.Width # Currently assuming square world
	random.cor <- function(x) runif(x, min=Min.Pcor - 0.5, max=Max.Pcor + 0.5) # Random coordinate
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
	
	if (RL == 0) { # Return initial system state
		return(list(
			Results=R,
			Steps=t, PX=PX, PY=PY, MFI=MFI, CX=CX, CY=CY, CD=CD, F=F
		))
	}
	
##############################################################################

	# Main loop

	for (t in 1:RL) {
		# Pair up agents randomly
		RP <- sample(population, size=S, replace=FALSE) # Random permutation (shuffle) of population
		players <- RP
		opponents <- append(RP[(1+(S/2)):S], RP[1:(S/2)]) # Split and recombine halves swapped over 
		
		# Indentify one of most relevant contexts
		K[players] <- max.col( - distance_between(PX[opponents], PY[opponents], CX[players,], CY[players,]) , ties.method="random")
		#mapply(function(a, b) K[a] <<- max.col( - distance_between(PX[b], PY[b], CX[a,], CY[a,]) , ties.method="random"), players, opponents)
		# NB: ties should be resolved randomly

		# Choose action using decision rule
		A[players] <- sapply(players, function(a) decision_rule(CD[a,][K[a]], m) )
		
		# Belief Learning
		mapply(function(a, b) {
			CD[a,][K[a]] <<- belief_learning(degree=CD[a,][K[a]], action=A[b], memory=Memory)
		}, players, opponents)
			
		# Relevance Learning
		mapply(function(a, b) {
			CX[a,][K[a]] <<- relevance_learning(
				cx=CX[a,][K[a]], 
				ax=PX[b], 
				matching=(A[a] == A[b]), 
				inertia=Inertia, 
				game.type=g, 
				min.pcor=Min.Pcor, 
				max.pcor=Max.Pcor
			)
		}, players, opponents)
		mapply(function(a, b) {
			CY[a,][K[a]] <<- relevance_learning(
				cx=CY[a,][K[a]], 
				ax=PY[b], 
				matching=(A[a] == A[b]), 
				inertia=Inertia, 
				game.type=g, 
				min.pcor=Min.Pcor, 
				max.pcor=Max.Pcor
			)
		}, players, opponents)
		
		# Update frequency count of interaction outcome types
		F[players,] <- 0.01 * F[players,] * SR
		#mapply(function(a) F[a,] <<- 0.01 * F[a,] * SR, players)
		
		OT[players] <- 1 + A[opponents] + Na * A[players] # 1+... because indexing from 1, not 0
		sapply(players, function(a) F[a,][OT[a]] <<- F[a,][OT[a]] + 0.01 * (100 - SR))

		# Classify each agent by its most frequent interaction outcome
		MFI <- max.col(F, ties.method="random") # For each person, index of largest value across outcomes
		# NB: ties should be resolved randomly

		# Count the number of agents with each MFI outcome type
		# These will work if Na=2. Na > 2 would deliver Na^2 outcome types.
		DD <- 100 * length(which( MFI==1 )) / S
		DH <- 100 * length(which( MFI==2 )) / S
		HD <- 100 * length(which( MFI==3 )) / S
		HH <- 100 * length(which( MFI==4 )) / S
		
		R <- rbind(R, data.table(Step=t, DD=DD, DH=DH, HD=HD, HH=HH))
	}

	return(list(
		Results=R,
		Steps=t, PX=PX, PY=PY, MFI=MFI, CX=CX, CY=CY, CD=CD, F=F
	))
}
# End

##############################################################################

belief_learning <- function(degree, action, memory) {
	# Assumes degree d in range [0, 100], memory [0, 100], action a {0, 1}
	0.01 * degree * memory + action * (100 - memory)
}

##############################################################################


##############################################################################

relevance_learning <- function(cx, ax, matching, inertia, game.type, min.pcor, max.pcor) {
	wrapped_cor( cx + (
		ifelse(matching, -1, 1) *
		game.type *
		0.01 * (100 - inertia) * 
		shortest_jump(from=cx, to=ax, w=(1 + max.pcor - min.pcor))
	), min.pcor, max.pcor)
}

##############################################################################

wrapped_cor <- function(xycor, minpcor, maxpcor) {
	# World wraps. Calculate new coordinate.
	if (xycor < minpcor - 0.5) {return(xycor + 1 + maxpcor - minpcor)}
	if (xycor >= maxpcor + 0.5) {return(xycor - 1 - maxpcor + minpcor)}
	xycor
}

##############################################################################

distance_between <- function(x1, y1, x2, y2, w=base_case_parameters()$World.Width, h=base_case_parameters()$World.Height) {
	# Compute distance between (x1, y1) and (x2, y2),
	# assuming world wraps horizontally and vertically,
	# where w=world.width and h=world.height.
	dx <- ifelse(x1 > x2,
		ifelse(2 * (x1 - x2) > w, x2 + w - x1, x1 - x2),
		ifelse(2 * (x2 - x1) > w, x1 + w - x2, x2 - x1)
	)
	dy <- ifelse(y1 > y2,
		ifelse(2 * (y1 - y2) > h, y2 + h - y1, y1 - y2),
		ifelse(2 * (y2 - y1) > h, y1 + h - y2, y2 - y1)
	)
	sqrt((dx ^ 2) + (dy ^ 2))
}

##############################################################################

decision_rule <- function(p, m) {
	ifelse(p < m, 1, 	# Feeling Hawkish
		ifelse(p > m, 0,	# Feeling Doveish
			ifelse(100 * runif(n=ifelse(p==p,1,1)) < m, 1, 0) # Play MSNE. NB: Use of vector p to force runif to produce vector.
		)
	)
}
	
##############################################################################

shortest_jump <- function(from.x, to.x, w=base_case_parameters()$World.Width) { 
	(
		ifelse( to.x > from.x,	# Target ahead?
			ifelse( 2 * (to.x - from.x) < w, # Would it be shorter to go backward and wrap?
				(to.x - from.x),
				(to.x - from.x) - w
			),
			ifelse( 2 * (from.x - to.x) > w, # Would it be shorter to go forward and wrap?
				w + (to.x - from.x),
				(to.x - from.x)
			)
		)
	)
}

##############################################################################
##############################################################################
##############################################################################

# Transform down

melt_by_mfi <- function(D) {
	colsA <- c("DD", "DH", "HD", "HH")
#	colsB <- c("Distance.DD", "Distance.DH", "Distance.HD", "Distance.HH")
	mfi.types <- c("DD", "DH", "HD", "HH")
	D <- melt(
			D, 
			measure.vars = list(colsA), 
#			measure.vars = list(colsA, colsB), 
			variable.name = "MFI.Type", 
#			value.name = c("Perc.of.Pop")
			value.name = c("Perc")
#			value.name = c("Perc.of.Pop", "ICB.Distance")
		)
	
	return(
		D[, MFI.Type := mfi.types[MFI.Type]]
	)
}

##############################################################################

melt_by_col <- function(D) {
	colsA <- c(1:dim(D)[2])
	col_categories <- colsA
	D <- melt(
			as.data.table(D), 
			measure.vars = list(colsA), 
			variable.name = "C_Name", 
			value.name = c("Value")
		)
	
	return(D)
	# return(
		# D[, C_Name := col_categories[C_Name]]
	# )
}

##############################################################################

categorize_by_degree <- function(D, threshold=0.5, margin=0.05) {
	return(
		ifelse(D < threshold - margin, "Hawkish",
			ifelse(D > threshold + margin, "Doveish", "Marginal")
		)
	)
}

##############################################################################

plot_perc_v_x <- function(Z, ylim=c(0,100), ylab="% of Population", zlab="MFI Type", xlim=c(0,100), xlab="Time", title="") {
	G <- groups_info()
	xbreaks <- (0:10) * max(1, integer(max(Z[,x])/10))
	ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_light(base_size = 12) +
	theme(
		plot.title = element_text(size=12), 
		axis.title.x = element_text(size=12), 
		axis.title.y = element_text(size=12),
		legend.position = c(.95, .95),
    		legend.justification = c("right", "top"),
    		legend.box.just = "right",
		legend.margin = margin(6, 6, 6, 6)
	) +
	labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab) +
	scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	#geom_point(size=3) +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab]) +
	geom_line(linewidth=1)
}

##############################################################################

groups_info <- function() {
	return(
		data.table(
			# id=1:4,
			# lab=c("DD", "DH", "HD", "HH"), 
			# lcol=c("green2", "yellow2", "blue2", "red2"),
			# pcol=c("green3", "yellow3", "blue3", "red3"),
			# shape=c(1, 2, 0, 4)
			id=1:7,
			lab=c("DD", "DH", "HD", "HH", "Doveish", "Marginal", "Hawkish"), 
			lcol=c("green2", "yellow2", "blue2", "red2", "grey1", "grey3", "grey4"),
			pcol=c("green3", "yellow3", "blue3", "red3", "gray80", "gray40", "gray20"),
			#shape=c(1, 2, 0, 4, 5, 5, 5)
			shape=c(1, 2, 0, 4, 20, 20, 20)
		)
	)
}

##############################################################################

##############################################################################

perc_timeseries <- function(R, title="") {
	plot_perc_v_x(
		melt_by_mfi(R)[, 
			.(x=Step, y=as.numeric(Perc), z=MFI.Type)
		],
		title=title
	)
}



##############################################################################

plot_pop <- function(Z, Ticks = NULL, P = base_case_parameters()) {
	G <- groups_info()
	
	# Empty sets cause problems. Remove their entry from G.
	for (g in G[,lab]) {
		if (0 == nrow(Z[z==g])) {G <- G[lab != g]}
	}
	
	ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_light(base_size = 12) +
	theme(
		plot.title = element_text(size=12), 
		axis.title.x = element_text(size=12), 
		axis.title.y = element_text(size=12),

	) +
	labs(title=paste("Ticks = ", Ticks), x="xcor", y="ycor", shape="MFI Type", color="MFI Type") +
	scale_x_continuous(limits=c(0, P$World.Width)) +
	scale_y_continuous(limits=c(0, P$World.Width)) +
	geom_point(size=3, stroke=3, show.legend = TRUE) +
	scale_color_manual(values = G[,pcol], labels=G[,lab], drop=FALSE) +
	scale_shape_manual(values = G[,shape], labels=G[,lab], drop=FALSE)
}

##############################################################################
##############################################################################

base_case_parameters <- function() {
	list(
		MSNE = 90, 		# [0,1]
		Game.Type = 1, 		# 1 for mis-matching game, -1 for matching game
		Num.Actions = 2, 	# >= 1
		Num.People = 200, 	# Even number
		Num.CBeliefs = 4, 	# >= 1
		Memory = 90, 		# [0,1]
		Inertia = 90, 		# [0,1]
		Statistics.Retention = 90, # [0,1]
		World.Width = 33,	# For comparison with NetLogo's default value
		World.Height = 33,
		Run.Length = 2000	# Number of simulation iterations, and rounds of interaction
	)
}

##############################################################################

all_combinations <- function(
	factor.names=c("V1", "V2"), 
	factor.values=list(
		V1=c("A", "B"),
		V2=c(1, 2)
	),
	wide.form=FALSE
) {
	if (length(factor.names) != length(factor.values)) {
		print("names and values lists must be of same length!")
		return(FALSE)
	}
	
	lengths <- sapply(factor.values, function(v) length(v))
	indexes <- sapply(factor.values, function(v) 1)
	
	ret=data.table()
	cycle.length <- prod(lengths)
	cur.combin <- 0
	
	while (cur.combin < cycle.length) {
		cur.values <- mapply(function(Vs, i) Vs[i], factor.values, indexes)
		cur.combin <- cur.combin + 1
		ret <- rbind(ret, data.table(
			cur.combin=cur.combin, 
			factor.name=factor.names, 
			factor.value=cur.values 
		))
		
		# Next value(s)
		flag <- TRUE
		for (i in 1:length(indexes)) {
			if (flag == TRUE) {
				indexes[i] <- indexes[i] + 1
				flag <- (indexes[i] > lengths[i])
				if (flag == TRUE) {indexes[i] <- 1}
			}				 
		}
	}
	
	if (wide.form==TRUE) {
		return(
			dcast(ret, cur.combin ~ factor.name, value.var=c("factor.value")) # Wide form
		)
	}
	
	return(ret) # Long form
	
}

##############################################################################

experiment <- function(exp.factor = "MSNE", factor.values = c(0, 1), num.repetitions = 1, P = base_case_parameters()) {
	#P$Run.Length <- 5 # For quicker tests
	#P$Memory <- 50
	#P$Num.CBeliefs <- 8
	
	results <- data.table()
	cur.run <- 0
	total.runs <- num.repetitions * length(factor.values)
	
	for (cur.rep in 1:num.repetitions) {
		for (v in factor.values) {
			cur.run <- cur.run + 1
			P[exp.factor] <- v
			#print(paste("Run ", cur.run, " of ", total.runs, " : ", exp.factor, " = ", v))
			R <- sim_run_results(P=P)$Results # sim_run_results returns a list(model, results)
			
			print(perc_timeseries(R))
			
			R <- R[Step==max(R[,Step])]
			if (0==nrow(results)) {
				results <- cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$MSNE, Inertia=P$Inertia, Memory=P$Memory, Num.CBeliefs=P$Num.CBeliefs, Num.People=P$Num.People, R)
			} else {
				results <- rbind(results, cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$MSNE, Inertia=P$Inertia, Memory=P$Memory, Num.CBeliefs=P$Num.CBeliefs, Num.People=P$Num.People, R))
			}
			
		}
	}
	#print("Experiment done!")
	results
	
}

##############################################################################
