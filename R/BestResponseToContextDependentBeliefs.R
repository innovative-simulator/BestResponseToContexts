##############################################################################
##############################################################################

library(data.table)
library(ggplot2)
library(hrbrthemes)

##############################################################################

base_case_parameters <- function() {
	list(
		msne = 0.9,
		#num.attributes = 2,
		num.cbeliefs = 4,
		num.people = 200, # Should be even number for complete pairing up.
		inertia = 0.9,
		memory = 0.9,
		stats.recency = 0.9,
		run.length = 2000,
		min.pxcor = -16,
		max.pxcor = 16,
		min.pycor = -16,
		max.pycor = 16#,
		#attribute.min = 0,
		#attribute.max = 32
	)
}

##############################################################################

new_model <- function(P = base_case_parameters()) {
	pop <- list() #initial_people(P)
	t <- 0
	freq.mfi <- replicate(4, 0)
	perc.mfi <- replicate(4, 0)
	min.xcor <- P$min.pxcor - 0.5
	max.xcor <- P$max.pxcor + 0.5
	min.ycor <- P$min.pycor - 0.5
	max.ycor <- P$max.pycor + 0.5
	world.width <- 1 + P$max.pxcor - P$min.pxcor
	world.height <- 1 + P$max.pycor - P$min.pycor
	
	list(
		setup = function() {
			pop <<- initial_people(P=P)
			t <<- 0
			freq.mfi <<- replicate(4, 0)
			perc.mfi <<- replicate(4, 0)
		},
		freq_mfi = function() freq.mfi,
		perc_mfi = function() perc.mfi,
		go = function() {
			RP <- Random_Pairing(P)
			players <- append(RP[, ego], RP[, alter])
			opponents <- append(RP[, alter], RP[, ego])
	
			mapply(function(i, j) pop[,i]$choose_cbelief(pop[,j]), players, opponents)
			mapply(function(i) pop[,i]$choose_action(), players)

			#learn opponent's action
			mapply(function(i, j) pop[,i]$learn(pop[,j]$chosen_action()), players, opponents)
			#Reposition chosen.belief
			mapply(function(i, j) pop[,i]$reposition(pop[,j]), players, opponents)
			
			#update mfi type
			mapply(function(i) pop[,i]$identify_mfi(), players)
			freq.mfi <<- sapply(0:3, function(k) {
				sum(sapply(players, function(i) pop[,i]$mfi_type() == k))
			})
			sum.freq <- sum(freq.mfi)
			perc.mfi <<- sapply(freq.mfi, function(f) 100 * f / sum.freq)
			
			t <<- t + 1
		},
		output = function() {
		},
		plot_pop = function() {
			G <- groups_info()
			id <- sapply(1:dim(pop)[2], function(i) pop[,i]$who() )
			x <- sapply(1:dim(pop)[2], function(i) pop[,i]$xcor() )
			y <- sapply(1:dim(pop)[2], function(i) pop[,i]$ycor() )
			z <- sapply(1:dim(pop)[2], function(i) {G[1 + pop[,i]$mfi_type(), lab] })
			plot.data <- data.table(id=id, x=x, y=y, z=z)
			plot_pop(Z=plot.data, Ticks=t, P=P)
		},
		population = function() {pop},
		ticks = function() {t},
		world_width = function() {world.width},
		world.height = function() {world.height},
		max_pxcor = function() {max.pxcor},
		min_pxcor = function() {min.pxcor},
		max_pycor = function() {max.pycor},
		min_pycor = function() {min.pycor}
		
	)
}

##############################################################################

plot_pop <- function(Z, Ticks = NULL, P = base_case_parameters()) {
	G <- groups_info()
	
	# Empty sets cause problems. Remove their entry from G.
	for (g in G[,lab]) {
		if (0 == nrow(Z[z==g])) {G <- G[lab != g]}
	}
	
	#print(head(Z))
	#print(str(Z))
	#print(unique(Z[,z]))
	ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_light(base_size = 12) +
	theme(
		plot.title = element_text(size=12), 
		axis.title.x = element_text(size=12), 
		axis.title.y = element_text(size=12),

	) +
	labs(title=paste("Ticks = ", Ticks), x="xcor", y="ycor", shape="MFI Type", color="MFI Type") +
	#labs(title=paste("Ticks = ", Ticks, "; MSNE = ", P$msne, "; Num.People = ", P$num.people, "; Num.CBs = ", P$num.cbeliefs, "; Memory = ", P$memory, "; Inertia = ", P$inertia), x="xcor", y="ycor", shape="MFI Type", color="MFI Type") +
# #	xlim(c(P$min.pxcor, P$max.pxcor)) +
# #	ylim(c(P$min.pycor, P$max.pycor)) +
	scale_x_continuous(limits=c(P$min.pxcor - 0.5, P$max.pxcor + 0.5)) +
	scale_y_continuous(limits=c(P$min.pycor - 0.5, P$max.pycor + 0.5)) +
	geom_point(size=3, stroke=3, show.legend = TRUE) +
	#scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	#scale_shape_manual(values = G[,shape], labels=G[,lab)
	scale_color_manual(values = G[,pcol], labels=G[,lab], drop=FALSE) +
	scale_shape_manual(values = G[,shape], labels=G[,lab], drop=FALSE)
}

##############################################################################

alt_plot_pop <- function(Z, Ticks = NULL, P = base_case_parameters()) {
	# Doesn't work!!
	G <- groups_info()
	ggplot(mapping=aes()) +
	theme_light(base_size = 12) +
	theme(
		plot.title = element_text(size=12), 
		axis.title.x = element_text(size=12), 
		axis.title.y = element_text(size=12),

	) +
	labs(title=paste("Ticks = ", Ticks), x="xcor", y="ycor", shape="MFI Type", color="MFI Type") +
# #	xlim(c(P$min.pxcor, P$max.pxcor)) +
# #	ylim(c(P$min.pycor, P$max.pycor)) +
	scale_x_continuous(limits=c(P$min.pxcor, P$max.pxcor)) +
	scale_y_continuous(limits=c(P$min.pycor, P$max.pycor)) +
	geom_point(data=Z[z=="DD"], mapping=aes(x=x, y=y, shape=as.factor(G[lab=="DD", shape]), color=G[lab=="DD", pcol]), size=3, stroke=3, show.legend = TRUE) +
	geom_point(data=Z[z=="DH"], mapping=aes(x=x, y=y, shape=as.factor(G[lab=="DH", shape]), color=G[lab=="DH", pcol]), size=3, stroke=3, show.legend = TRUE) +
	geom_point(data=Z[z=="HD"], mapping=aes(x=x, y=y, shape=as.factor(G[lab=="HD", shape]), color=G[lab=="HD", pcol]), size=3, stroke=3, show.legend = TRUE) +
	geom_point(data=Z[z=="HH"], mapping=aes(x=x, y=y, shape=as.factor(G[lab=="HH", shape]), color=G[lab=="HH", pcol]), size=3, stroke=3, show.legend = TRUE) 
	#scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	#scale_shape_manual(values = G[,shape], labels=G[,lab)
	# scale_color_manual(values = G[,pcol], labels=G[,lab], drop=FALSE) +
	# scale_shape_manual(values = G[,shape], labels=G[,lab], drop=FALSE)
}

##############################################################################

initial_people <- function(P = base_case_parameters()) {
	#if (not(is.numeric(n))) {n <- P$num.people}
	n <- P$num.people
	sapply(1:n, function(i) new_person(ID=i, P=P))
}

##############################################################################

new_person <- function(ID = NULL, P = base_case_parameters()) {
	id <- ID
	x <- runif(1, min=P$min.pxcor - 0.5, max=P$max.pxcor + 0.5)
	y <- runif(1, min=P$min.pycor - 0.5, max=P$max.pycor + 0.5)
	my.cbs <- sapply(1:P$num.cbeliefs, function(i) new_cbelief(ID=i, P=P))
	chosen.cbelief <- NULL
	chosen.action <- NULL
	freqs <- replicate(4, 0)
	mfi.type <- -1 + max_one_of(freqs) # Random ~ U[0, 3]
	
	list(
		who = function() {id},
		go_up = function(steps=1) {
			y <<- wrapped_cor(y + steps, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		go_down = function(steps=1) {
			y <<- wrapped_cor(y - steps, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		go_right = function(steps=1) {
			x <<- wrapped_cor(x + steps, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
		},
		go_left = function(steps=1) {
			x <<- wrapped_cor(x - steps, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
		},
		xcor = function() {x},
		ycor = function() {y},
		setxy = function(xcor = x, ycor = y) {
			x <<- wrapped_cor(xcor, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
			y <<- wrapped_cor(ycor, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		cbeliefs = function() {my.cbs},
		cbelief = function(ID=NULL) {my.cbs[,ID]},
		set_cbelief_degree = function(ID=NULL, p=NULL) {
			my.cbs[,ID]$set_degree(p)
		},
		distance = function(agent) {
			distancexy_wrap_h_v(x1=x, y1=y, x2=agent$xcor(), y2=agent$ycor(), w=1 + P$max.pxcor - P$min.pxcor, h=1 + P$max.pycor - P$min.pycor)
		},
		distancexy = function(xcor, ycor) {
			distancexy_wrap_h_v(x1=x, y1=y, x2=xcor, y2=ycor, w=1 + P$max.pxcor - P$min.pxcor, h=1 + P$max.pycor - P$min.pycor)
		},
		chosen_cbelief = function() {chosen.cbelief},
		choose_cbelief = function(opponent) {
			chosen.cbelief <<- min_one_of(
				sapply(1:dim(my.cbs)[2], function(i) my.cbs[,i]$distance(opponent))
			)
		},
		chosen_action = function() {chosen.action},
		choose_action = function() {
			chosen.action <<- Decision_Rule(prob=my.cbs[,chosen.cbelief]$degree(), m=P$msne)
		},
		learn = function(observation) {
			my.cbs[, chosen.cbelief]$set_degree(
				P$memory * my.cbs[, chosen.cbelief]$degree() + (1.0 - P$memory) * observation
			)
			
			# Record the interaction outcome
			outcome <- observation + 2 * chosen.action # Outcome type from 0 to 3
			freqs <<- sapply(freqs, function(f) f * P$stats.recency)
			freqs[outcome + 1] <<- freqs[outcome + 1] + (1 * (1.0 - P$stats.recency)) # Remember: Indexing from 1 to 4
		},
		reposition = function(opponent) {
			ax <- opponent$xcor()
			ay <- opponent$ycor()
			cx <- my.cbs[, chosen.cbelief]$xcor()
			cy <- my.cbs[, chosen.cbelief]$ycor()
			outcome <- ifelse(chosen.action == opponent$chosen_action(), -1, 1)
			dx <- (ax - cx) * 
				outcome * 
				ifelse(2 * abs(ax - cx) > P$max.pxcor - P$min.pxcor, -1, 1)
			dy <- (ay - cy) * 
				outcome * 
				ifelse(2 * abs(ay - cy) > P$max.pycor - P$min.pycor, -1, 1)
			my.cbs[, chosen.cbelief]$go_right(dx * (1.0 - P$inertia))
			my.cbs[, chosen.cbelief]$go_up(dy * (1.0 - P$inertia))
		},
		identify_mfi = function() {
			mfi.type <<- -1 + max_one_of(freqs)
		},
		mfi_type = function() {mfi.type}
	)
}

##############################################################################

new_cbelief <- function(ID = NULL, P = base_case_parameters()) {
	id <- ID
	x <- runif(1, min=P$min.pxcor - 0.5, max=P$max.pxcor + 0.5)
	y <- runif(1, min=P$min.pycor - 0.5, max=P$max.pycor + 0.5)
	d <- P$msne
	list(
		who = function() {id},
		go_up = function(steps=1) {
			y <<- wrapped_cor(y + steps, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		go_down = function(steps=1) {
			y <<- wrapped_cor(y - steps, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		go_right = function(steps=1) {
			x <<- wrapped_cor(x + steps, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
		},
		go_left = function(steps=1) {
			x <<- wrapped_cor(x - steps, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
		},
		xcor = function() {x},
		ycor = function() {y},
		setxy = function(xcor = x, ycor = y) {
			x <<- wrapped_cor(xcor, minpcor=P$min.pxcor, maxpcor=P$max.pxcor)
			y <<- wrapped_cor(ycor, minpcor=P$min.pycor, maxpcor=P$max.pycor)
		},
		degree = function() {d},
		set_degree = function(degree = d) {
			d <<- degree
		},
		distance = function(agent) {
			distancexy_wrap_h_v(x1=x, y1=y, x2=agent$xcor(), y2=agent$ycor(), w=1 + P$max.pxcor - P$min.pxcor, h=1 + P$max.pycor - P$min.pycor)
		},
		distancexy = function(xcor, ycor) {
			distancexy_wrap_h_v(x1=x, y1=y, x2=xcor, y2=ycor, w=1 + P$max.pxcor - P$min.pxcor, h=1 + P$max.pycor - P$min.pycor)
		}
	)	
}

##############################################################################

wrapped_cor <- function(xycor, minpcor, maxpcor) {
	# World wraps. Calculate new coordinate.
	if (xycor < minpcor - 0.5) {return(xycor + 1 + maxpcor - minpcor)}
	if (xycor >= maxpcor + 0.5) {return(xycor - 1 - maxpcor + minpcor)}
	xycor
}

##############################################################################

distancexy_wrap_h_v <- function(x1, y1, x2, y2, w, h) {
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

Random_Pairing <- function(P = Base_Case_Parameters()) {
	n <- P$num.people
	permut <- sample(1:n, replace=FALSE)
	part1 <- permut[1:(n/2)]
	part2 <- permut[(1+(n/2)):n]
	return(data.table(ego=part2, alter=part1))
}

##############################################################################

Choose_CBelief <- function(Player, Opponent) {
	Player$chosen.cbelief <- Nearest.CB(Player$cb.positions, Opponent$attribs)
	return(Player)
}

##############################################################################

Nearest.CB <- function(Player.CB.Pos, Opponent.Attribs) {
	min_one_of(
		Attrib.Distance(Player.CB.Pos, Opponent.Attribs)
	)
}

##############################################################################

Attrib.Distance <- function(A.Attribs, B.Attribs) {
	sqrt(colSums((A.Attribs - B.Attribs) ^ 2 ))
}

##############################################################################

sample.vec <- function(x, size=1, replace=T) {
	x[sample(length(x), size=size, replace=replace)]
}

##############################################################################

min_one_of <- function(X) {
	sample.vec(
		which(X == min(X)),
		size=1
	)
}

##############################################################################

max_one_of <- function(X) {
	sample.vec(
		which(X == max(X)),
		size=1
	)
}

##############################################################################

Decision_Rule <- function(prob, m) {
	ifelse(prob > m, 0, ifelse(prob < m, 1, ifelse(m > runif(1), 1, 0)))
}

##############################################################################

Outcome_Type <- function(a, b) {
	return(a + 2 * b)
}

##############################################################################

new_agent <- function() {
	x <- runif(1, min=0, max=32)
	y <- runif(1, min=0, max=32)
	list(
		go_up = function(steps=1) {
			y <<- y + steps
		},
		go_down = function() {
			y <<- y - 1
		},
		go_right = function() {
			x <<- x + 1
		},
		go_left = function() {
			x <<- x - 1
		},
		xcor = function() {x},
		ycor = function() {y},
		setxy = function(xcor=x, ycor=y) {
			x <<- xcor
			y <<- ycor
		}
	)
}

##############################################################################

results_row <- function(freqs, percs) {
	data.table(
		Freq.DD = list(freqs[1]),
		Freq.DH = list(freqs[2]),
		Freq.HD = list(freqs[3]),
		Freq.HH = list(freqs[4]),

		Perc.DD = list(percs[1]),
		Perc.DH = list(percs[2]),
		Perc.HD = list(percs[3]),
		Perc.HH = list(percs[4])
	)
}	

##############################################################################
##############################################################################
##############################################################################
##############################################################################

sim_run_results <- function(P = base_case_parameters(), return_results = TRUE, return_model = TRUE, timeseries_update = FALSE, world_update = FALSE, cur.run="", total.runs="") {
	M <- new_model(P=P)
	M$setup()
	results <- cbind(Ticks=M$ticks() , results_row(percs=M$perc_mfi(), freqs=M$freq_mfi()))
	#if (is.numeric(timeseries_update)) {print(perc_timeseries(results))} # Only 1 row

	# Print world?
	if (is.numeric(world_update)) {
		print(M$plot_pop())
	}

	for (tim in 1:P$run.length) {
		M$go()
		results <- rbind(results, cbind(Ticks=M$ticks() , results_row(percs=M$perc_mfi(), freqs=M$freq_mfi())))
		
		# Print timeseries?
		if (is.numeric(timeseries_update)) {
			if (0 == tim %% timeseries_update) {
				title <- paste("t=", tim, "; MSNE=", P$msne, "; Pop=", P$num.people, "; CBs=", P$num.cbeliefs, "; Mem=", P$memory, "; Ine=", P$inertia, "; Run=", cur.run, " of ", total.runs)
				print(perc_timeseries(results, title=title))
			}
			
		}
		
		# Print world?
		if (is.numeric(world_update)) {
			if (0 == tim %% world_update) {print(M$plot_pop())}
		}
	}
#	M
	# Update timeseries?
	if (is.numeric(timeseries_update)) {
		title <- paste("t=", tim, "; MSNE=", P$msne, "; Pop=", P$num.people, "; CBs=", P$num.cbeliefs, "; Mem=", P$memory, "; Ine=", P$inertia, "; Run=", cur.run, " of ", total.runs)
		print(perc_timeseries(results, title=title))
	}
	
	# Print world?
	if (is.numeric(world_update)) {print(M$plot_pop())}

	# Return model?
	if (return_model == TRUE) {
		if (return_results == TRUE) {return(list(model=M, results=results))} # Return both model and results in a list.
		return(M) # Return model
	}
	if (return_results == TRUE) {return(results)}	# Return results data instead
	return(0) # Return 0 to show that we did get this far.
}

##############################################################################

perc_timeseries <- function(R, title="") {
	plot_perc_v_x(
		melt_by_mfi(R)[, 
			.(x=Ticks, y=as.numeric(Perc.of.Pop), z=MFI.Type)
		],
		title=title
	)
}

##############################################################################

# Transform down

melt_by_mfi <- function(D) {
#	colsA <- c("Out_0_v_0", "Out_0_v_1", "Out_1_v_0", "Out_1_v_1")
	colsA <- c("Perc.DD", "Perc.DH", "Perc.HD", "Perc.HH")
	colsB <- c("Freq.DD", "Freq.DH", "Freq.HD", "Freq.HH")
#	colsB <- c("Distance.DD", "Distance.DH", "Distance.HD", "Distance.HH")
	mfi.types <- c("DD", "DH", "HD", "HH")
	D <- melt(
			D, 
#			measure.vars = list(colsA), 
			measure.vars = list(colsA, colsB), 
			variable.name = "MFI.Type", 
#			value.name = c("Perc.of.Pop")
			value.name = c("Perc.of.Pop", "Freq")
#			value.name = c("Perc.of.Pop", "ICB.Distance")
		)
	
	return(
		D[, MFI.Type := mfi.types[MFI.Type]]
	)
}

##############################################################################

groups_info <- function() {
	return(
		data.table(
			id=1:4,
			lab=c("DD", "DH", "HD", "HH"), 
			lcol=c("green2", "yellow2", "blue2", "red2"),
			pcol=c("green3", "yellow3", "blue3", "red3"),
			shape=c(1, 2, 0, 4)
		)
	)
}

##############################################################################
##############################################################################

##############################################################################

experiment <- function(exp.factor = "msne", factor.values = c(0, 1), num.repetitions = 1, P = base_case_parameters()) {
	P$run.length <- 5 # For quicker tests
	
	results <- data.table()
	cur.run <- 0
	total.runs <- num.repetitions * length(factor.values)
	
	for (cur.rep in 1:num.repetitions) {
		for (v in factor.values) {
			cur.run <- cur.run + 1
			P[exp.factor] <- v
			#print(paste("Run ", cur.run, " of ", total.runs, " : ", exp.factor, " = ", v))
			R <- sim_run_results(P=P, return_model=TRUE, return_results=TRUE, timeseries_update=200, cur.run=cur.run, total.runs=total.runs)$results # sim_run_results returns a list(model, results)
			R <- R[Ticks==max(R[,Ticks])]
			if (0==nrow(results)) {
				results <- cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$msne, Inertia=P$inertia, Memory=P$memory, Num.CBeliefs=P$num.cbeliefs, Num.People=P$num.people, R)
			} else {
				results <- rbind(results, cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$msne, Inertia=P$inertia, Memory=P$memory, Num.CBeliefs=P$num.cbeliefs, Num.People=P$num.people, R))
			}
			
		}
	}
	#print("Experiment done!")
	results
	
}

##############################################################################

experiment_memory_50 <- function(exp.factor = "msne", factor.values = c(0, 1), num.repetitions = 1, P = base_case_parameters()) {
	#P$run.length <- 5 # For quicker tests
	P$memory <- 0.5
	P$num.cbeliefs <- 8
	
	results <- data.table()
	cur.run <- 0
	total.runs <- num.repetitions * length(factor.values)
	
	for (cur.rep in 1:num.repetitions) {
		for (v in factor.values) {
			cur.run <- cur.run + 1
			P[exp.factor] <- v
			#print(paste("Run ", cur.run, " of ", total.runs, " : ", exp.factor, " = ", v))
			R <- sim_run_results(P=P, return_model=TRUE, return_results=TRUE, timeseries_update=200, cur.run=cur.run, total.runs=total.runs)$results # sim_run_results returns a list(model, results)
			R <- R[Ticks==max(R[,Ticks])]
			if (0==nrow(results)) {
				results <- cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$msne, Inertia=P$inertia, Memory=P$memory, Num.CBeliefs=P$num.cbeliefs, Num.People=P$num.people, R)
			} else {
				results <- rbind(results, cbind(Cur.Run=cur.run, Cur.Rep=cur.rep, MSNE=P$msne, Inertia=P$inertia, Memory=P$memory, Num.CBeliefs=P$num.cbeliefs, Num.People=P$num.people, R))
			}
			
		}
	}
	#print("Experiment done!")
	results
	
}


##############################################################################
##############################################################################
##############################################################################

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
	#ylim(ylim) +
	#scale_x_discrete() +
	#scale_x_continuous(limits=c(0, max(Z[,x])), breaks = xbreaks) +
	#scale_x_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	#scale_x_continuous(trans="log10") +
	#scale_x_log10(
	# scale_x_continuous(trans="log2",
		# breaks = scales::trans_breaks("log2", function(x) 2^x)
		# #labels = scales::trans_format("log2", scales::math_format(2^.x))
	# ) +
	scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	#geom_point(size=3) +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab]) +
#	geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
	#geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2) +
	geom_line(linewidth=1)
}


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

aggregate_over_reps <- function(D) {
	# Aggregate to compute statistics
	return(
		D[,.(
			Mean.Perc.of.Pop = mean(Perc.of.Pop),
			SD.Perc.of.Pop = sd(Perc.of.Pop),
			SE.Perc.of.Pop = sd(Perc.of.Pop) / sqrt(.N),
			Mean.Freq = mean(Freq),
			SD.Freq = sd(Freq),
			SE.Freq = sd(Freq) / sqrt(.N),
			Num.Reps = .N
		), by=.(
			MSNE=100*MSNE, 
			Num.CBeliefs, 
			Num.People, 
			Memory=100*Memory, 
			Inertia=100*Inertia, 
			#Init.Positions, 
			#Init.Attribs, 
			MFI.Type
		)]
	)
}

##############################################################################
# Plot comparable data sets
##############################################################################

# groups_info <- function() {
	# return(
		# data.table(
			# id=1:4,
			# lab=c("DD", "DH", "HD", "HH"), 
			# lcol=c("green2", "yellow2", "blue2", "red2"),
			# pcol=c("green3", "yellow4", "blue4", "red3"),
			# shape=c(1, 2, 0, 4)
		# )
	# )
# }

##############################################################################

common_plot <- function(Z,
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	P <- ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_light(base_size = 12) +
	theme(
		plot.title = element_text(size=12), 
		axis.title.x = element_text(size=12), 
		axis.title.y = element_text(size=12),
		#legend.position = "right",
		#legend.position = c(.8, .95),
   		#legend.justification = c("right", "top"),
   		legend.box.just = "right",
		legend.margin = margin(6, 6, 6, 6)
	)

	if (show_points == TRUE) {P <- P + geom_point(size=2, stroke=1)}

	if (show_errorbars == TRUE) {P <- P + 
#		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=1)
	}
	
	if (show_lines == TRUE) {P <- P + 
		geom_line(linewidth=1)
	}
	
	P
}

##############################################################################

common_plot_additions <- function(P,
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	
	P
}

##############################################################################

plot_generic <- function(Z, 
	title="", 
	zlab="MFI Type", 
	ylim=c(0,100), ylab="% of Population", 
	xlim=c(0,100), xlab="%",
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	P <- common_plot(Z, show_points=show_points, show_lines=show_lines, show_errorbars=show_errorbars) +
	labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab) +
	scale_x_continuous(limits=xlim, breaks = seq(xlim[1], xlim[2], by = xlim[2]/5)) +
	scale_y_continuous(limits=ylim, breaks = seq(ylim[1], ylim[2], by = ylim[2]/5))
	
	G <- groups_info()

	P <- P +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab])

	P
}

##############################################################################

plot_log_x <- function(Z, 
	title="", 
	zlab="MFI Type", 
	ylim=c(0,100), ylab="% of Population", 
	xlim=c(0,100), xlab="%",
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	xbreaks <- 10**(1:5 * 0.5)
	
	P <- common_plot(Z, show_points=show_points, show_lines=show_lines, show_errorbars=show_errorbars) +
	labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab) +
	
	#ylim(ylim) +
	#scale_x_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	#scale_x_continuous(trans="log10") +
	#scale_x_log10(
	scale_x_continuous(trans="log2",
		breaks = scales::trans_breaks("log2", function(x) 2^x)
		#labels = scales::trans_format("log2", scales::math_format(2^.x))
	) +
	scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20))
	
	G <- groups_info()
	P <- P +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab])

	P
}

##############################################################################

plot_msne <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.ine <- unique(D[, Inertia])
	cur.mem <- unique(D[, Memory])
	
	P <- plot_generic(
		D[,
			.(
				x=MSNE, 
				y=Mean.Perc.of.Pop,
				z=MFI.Type,
				y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
				y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
			)
		], 
		title=paste0("CBs=", cur.num.cbs, "; Ine=", cur.ine, "; Mem= ", cur.mem, "; N=", cur.pop), 
		xlab="MSNE (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = TRUE
	)
	P
}

##############################################################################

plot_cbeliefs <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.msne <- unique(D[, MSNE])
	cur.ine <- unique(D[, Inertia])
	cur.mem <- unique(D[, Memory])
	cur.init.pos <- unique(D[, Init.Positions])
	
	P <- plot_log_x(
		D[,
			.(
				x=Num.CBeliefs, 
				y=Mean.Perc.of.Pop,
				z=MFI.Type,
				y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
				y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
			)
		], 
		title=paste0("Pos=", cur.init.pos, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; Mem=", cur.mem, "; N=", cur.pop), 
		xlab="Number of C-Beliefs", 
		#xlim=c(0, max(D[, Num.CBeliefs])), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = FALSE
	)
	P
}

##############################################################################

plot_people <- function(D) {
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.msne <- unique(D[, MSNE])
	cur.ine <- unique(D[, Inertia])
	cur.mem <- unique(D[, Memory])
	
	P <- plot_log_x(
		D[,
			.(
				x=Num.People, 
				y=Mean.Perc.of.Pop,
				z=MFI.Type,
				y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
				y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
			)
		], 
		title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; Mem=", cur.mem, ""), 
		xlab="Number of People", 
		#xlim=c(0, max(D[, Num.CBeliefs])), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = FALSE
	)
	P
}

##############################################################################

plot_inertia <- function(D) {
	#cur.pop <- unique(D[, Num.People])
	#cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.msne <- unique(D[, MSNE])
	cur.mem <- unique(D[, Memory])

	P <- plot_generic(
		D[,
			.(
				x=Inertia, 
				y=Mean.Perc.of.Pop,
				z=MFI.Type,
				y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
				y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
			)
		], 
		title=paste0("MSNE=", cur.msne, "; Mem=", cur.mem), 
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Mem=", cur.mem, "; N=", cur.pop), 
		xlab="Inertia (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = TRUE
	)
	P
}

##############################################################################

plot_memory <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.msne <- unique(D[, MSNE])
	cur.ine <- unique(D[, Inertia])

	P <- plot_generic(
		D[,
			.(
				x=Memory, 
				y=Mean.Perc.of.Pop,
				z=MFI.Type,
				y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
				y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
			)
		], 
		title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; N=", cur.pop), 
		xlab="Memory (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = FALSE
	)
	P
}

##############################################################################

save_plot <- function(P, filename="test.png", dpi=150, units="px", width=750, height=500) {
	ggsave(P, filename=filename, dpi=dpi, units=units, width=width, height=height)
}

##############################################################################

file_processed <- function(
	datafilename=""
	) {
	print(paste0("Reading ", datafilename))
	D <- fread(datafilename)
	#D <- selected_fields(D)
	print(paste0("File read. dim(D) is ", dim(D)))
	print(paste0("Melt data by mfi"))
	D <- melt_by_mfi(D)
	print(paste0("Data melted. dim(D) is ", dim(D)))
	print(paste0("Aggregate data over repetitions"))
	D <- aggregate_over_reps(D)
	print(paste0("Data aggregated. dim(D) is ", dim(D)))
	return(D)
}

##############################################################################

##############################################################################

##############################################################################
##############################################################################
##############################################################################
##############################################################################
