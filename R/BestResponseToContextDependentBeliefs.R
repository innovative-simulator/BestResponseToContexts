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
		min.pxcor = 0,
		max.pxcor = 32,
		min.pycor = 0,
		max.pycor = 32#,
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
		ticks = function() {t}
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
# #	xlim(c(P$min.pxcor, P$max.pxcor)) +
# #	ylim(c(P$min.pycor, P$max.pycor)) +
	scale_x_continuous(limits=c(P$min.pxcor, P$max.pxcor)) +
	scale_y_continuous(limits=c(P$min.pycor, P$max.pycor)) +
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
	x <- runif(1, min=P$min.pxcor, max=P$max.pxcor)
	y <- runif(1, min=P$min.pycor, max=P$max.pycor)
	my.cbs <- sapply(1:P$num.cbeliefs, function(i) new_cbelief(ID=i, P=P))
	chosen.cbelief <- NULL
	chosen.action <- NULL
	freqs <- replicate(4, 0)
	mfi.type <- NULL
	
	list(
		who = function() {id},
		go_up = function(steps=1) {
			y <<- y + steps
		},
		go_down = function(steps=1) {
			y <<- y - steps
		},
		go_right = function(steps=1) {
			x <<- x + steps
		},
		go_left = function(steps=1) {
			x <<- x - steps
		},
		xcor = function() {x},
		ycor = function() {y},
		setxy = function(xcor=x, ycor=y) {
			x <<- xcor
			y <<- ycor
		},
		cbeliefs = function() {my.cbs},
		cbelief = function(ID=NULL) {my.cbs[,ID]},
		set_cbelief_degree = function(ID=NULL, p=NULL) {
			my.cbs[,ID]$set_degree(p)
		},
		distance = function(agent) {
			sqrt(
				(x - agent$xcor()) ^ 2 +
				(y - agent$ycor()) ^ 2
			)
		},
		distancexy = function(xcor, ycor) {
			sqrt(
				(x - xcor) ^ 2 +
				(y - ycor) ^ 2
			)
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
	x <- runif(1, min=P$min.pxcor, max=P$max.pxcor)
	y <- runif(1, min=P$min.pycor, max=P$max.pycor)
	d <- P$msne
	list(
		who = function() {id},
		go_up = function(steps=1) {
			y <<- y + steps
		},
		go_down = function(steps=1) {
			y <<- y - steps
		},
		go_right = function(steps=1) {
			x <<- x + steps
		},
		go_left = function(steps=1) {
			x <<- x - steps
		},
		xcor = function() {x},
		ycor = function() {y},
		setxy = function(xcor = x, ycor = y) {
			x <<- xcor
			y <<- ycor
		},
		degree = function() {d},
		set_degree = function(degree = d) {
			d <<- degree
		},
		distance = function(agent) {
			sqrt(
				(x - agent$xcor()) ^ 2 +
				(y - agent$ycor()) ^ 2
			)
		},
		distancexy = function(xcor, ycor) {
			sqrt(
				(x - xcor) ^ 2 +
				(y - ycor) ^ 2
			)
		}
	)	
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

sim_run_results <- function(P = base_case_parameters(), return_model = FALSE, timeseries_update = FALSE, world_update = FALSE) {
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
			if (0 == tim %% timeseries_update) {print(perc_timeseries(results))}
		}
		
		# Print world?
		if (is.numeric(world_update)) {
			if (0 == tim %% world_update) {print(M$plot_pop())}
		}
	}
#	M
	# Update timeseries?
	if (is.numeric(timeseries_update)) {print(perc_timeseries(results))}
	
	# Print world?
	if (is.numeric(world_update)) {print(M$plot_pop())}

	# Return model?
	if (return_model) {return(M)}
	return(results)	# Return results data instead
}

##############################################################################

perc_timeseries <- function(R) {
	plot_perc_v_x(
		melt_by_mfi(R)[, 
			.(x=Ticks, y=as.numeric(Perc.of.Pop), z=MFI.Type)
		]
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
			value.name = c("Perc.of.Pop", "Frequency")
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

experiment_inertia <- function(num.repetitions = 1, P = base_case_parameters()) {
	P$run.length <- 500 # For quick tests
	
	results <- data.table()
	cur.run <- 0
	for (rep in 1:num.repetitions) {
		for (v in c(0, 0.1, 0.3, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 1)) {
			cur.run <- cur.run + 1
			P$inertia <- v
			R <- sim_run_results(P=P, timeseries_update=100)
			R <- R[Ticks==max(R[,Ticks])]
			if (0==nrow(results)) {
				results <- cbind(cur.run=cur.run, cur.rep=rep, inertia=v, R)
			} else {
				results <- rbind(results, cbind(cur.run=cur.run, cur.rep=rep, inertia=v, R))
			}
			
		}
	}
	results
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################

plot_perc_v_perc <- function(Z, ylim=c(0,100), ylab="% of Population", zlab="MFI Type", xlim=c(0,100), xlab="%", title="") {
	G <- groups_info()

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
	scale_x_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	geom_point(size=3) +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab]) +
#	geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
	geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2) +
	geom_line(linewidth=1)
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
	geom_point(size=3) +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab]) +
#	geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
	#geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2) +
	geom_line(linewidth=1)
}

##############################################################################

survey_y_vs_x1_by_x2 <- function(D, y="Mean.Perc.of.Pop", y_label="", y_interval="SE.Perc.of.Pop", x1="Inertia", x1_label="", x2="MSNE", x2_label="") {
	vals <- sort(unique(D[, get(x2)]))
	for (v in vals) {
		p <- plot_perc_v_x(D[
			get(x2) == v, 
			.(
				x=get(x1), 
				y=get(y), 
				z=MFI.Type,
				y.lower=get(y) - get(y_interval),
				y.upper=get(y) + get(y_interval)
			)
		], title=paste0(x2_label, " = ", v), xlab=x1_label, ylab=y_label)
		print(p)
		invisible(readline(prompt=paste0(x2_label, " = ", v, ". Press [enter] to continue")))
	}
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
