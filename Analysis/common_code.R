##############################################################################
## Script to create figures from the results .csv file output by BehaviorSpace
##############################################################################

# Libraries

library(ggplot2)
library(hrbrthemes)
#install.packages("viridis") # If you don't have it already
#library(viridis) # Only if you want this colorscheme
#install.packages("xlsx") # If you don't have it already
library("xlsx")
library("data.table")

##############################################################################

# Change this to the directory on your computer. 
setwd("C:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\M-Nodes\\BestResponseToContexts\\Analysis")

##############################################################################
# Import data
##############################################################################

file_processed <- function(
	datafilename=""
	) {
	D <- read_netlogo_csv(datafilename)
	D <- selected_fields(D)
	D <- melt_by_mfi(D)
	D <- aggregate_over_reps(D)
	return(D)
}

##############################################################################

read_netlogo_csv <- function(filename) {
	# Reads in csv file output by NetLogo's BehaviorSpace
	return(
		as.data.table(
			fread(
				file=filename, 
				skip=6, 
				header=TRUE
			)
		)
	)
}

##############################################################################

selected_fields <- function(D) {
	# Identify exact dataset and relabel fields
	return(
		D[,.(
			MSNE = D$"Base-MSNE", 
			Num.CBeliefs = D$"Number-Of-C-Beliefs", 
			Num.People = D$"Number-Of-People", 
			Memory = D$"Memory", 
			Inertia = D$"Inertia", 
			Init.Degree = "Base-MSNE", # D$"Initial-Degree-Of-Belief", 
			Init.Positions = D$"Initial-C-Belief-Positions", 
			Init.Attribs = D$"Initial-Person-Attributes", 
			Response.Choice = "Best", # D$"Response-Choice", 
			Slope.Multiplier = 20, # D$"Slope-Multiplier", # Unused unless Response.Choice = "Stochastic" 
			Stats.Retention = D$"Statistics-Retention",
			Perc.DD = D$"perc-interaction-type 0",
			Perc.DH = D$"perc-interaction-type 1",
			Perc.HD = D$"perc-interaction-type 2",
			Perc.HH = D$"perc-interaction-type 3",
			Distance.DD = D$"mean-icb-distance-by-mfi 0",
			Distance.DH = D$"mean-icb-distance-by-mfi 1",
			Distance.HD = D$"mean-icb-distance-by-mfi 2",
			Distance.HH = D$"mean-icb-distance-by-mfi 3"
		),]
	)
}

##############################################################################

# Transform down

melt_by_mfi <- function(D) {
	colsA <- c("Perc.DD", "Perc.DH", "Perc.HD", "Perc.HH")
	colsB <- c("Distance.DD", "Distance.DH", "Distance.HD", "Distance.HH")
	mfi.types <- c("DD", "DH", "HD", "HH")
	D <- melt(
			D, 
			measure.vars = list(colsA, colsB), 
			variable.name = "MFI.Type", 
			value.name = c("Perc.of.Pop", "ICB.Distance")
		)
	
	return(
		D[, MFI.Type := mfi.types[MFI.Type]]
	)
}

##############################################################################

aggregate_over_reps <- function(D) {
	# Aggregate to compute statistics
	return(
		D[,.(
			Mean.Perc.of.Pop = mean(Perc.of.Pop),
			SD.Perc.of.Pop = sd(Perc.of.Pop),
			SE.Perc.of.Pop = sd(Perc.of.Pop) / sqrt(.N),
			Mean.ICB.Distance = mean(ICB.Distance),
			SD.ICB.Distance = sd(ICB.Distance),
			SE.ICB.Distance = sd(ICB.Distance) / sqrt(.N),
			Num.Reps = .N
		), by=.(
			MSNE, 
			Num.CBeliefs, 
			Num.People, 
			Memory, 
			Inertia, 
			Init.Degree,
			Init.Positions, 
			Init.Attribs, 
			Response.Choice,
			Slope.Multiplier,
			MFI.Type,
			Stats.Retention
		)]
	)
}

##############################################################################
# Plot comparable data sets
##############################################################################

groups_info <- function() {
	return(
		data.table(
			id=1:4,
			lab=c("DD", "DH", "HD", "HH"), 
			lcol=c("green2", "yellow2", "blue2", "red2"),
			pcol=c("green3", "yellow4", "blue4", "red3"),
			shape=c(1, 2, 0, 4)
		)
	)
}

##############################################################################

common_plot <- function(Z,
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	P <- ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_bw(base_size = 11) +
	theme(
		plot.title = element_text(size=11), 
		axis.title.x = element_text(size=11), 
		axis.title.y = element_text(size=11),
		panel.grid.major = element_line(color="lightgray"),
		panel.grid.minor = element_line(color="lightgray"),
		#legend.position = "right",
		#legend.position = c(.8, .95),
   		#legend.justification = c("right", "top"),
   		legend.box.just = "right",
		legend.margin = margin(6, 6, 6, 6)
	)

	if (show_points == TRUE) {P <- P + geom_point(size=1.5, stroke=0.75)}

	if (show_errorbars == TRUE) {P <- P + 
#		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=1)
	}
	
	if (show_lines == TRUE) {P <- P + 
		geom_line(linewidth=0.5)
	}
	
	P
}

##############################################################################

small_plot <- function(Z,
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE
) {
	# Smaller font sizes, smaller points, and thinner lines than common_plot
	P <- ggplot(Z, aes(x=x, y=y, z=z, shape=z, color=z)) +
	theme_bw(base_size = 10) +
	theme(
		plot.title = element_text(size=10), 
		axis.title.x = element_text(size=10), 
		axis.title.y = element_text(size=10),
		panel.grid.major = element_line(color="lightgray"),
		#panel.grid.minor = element_line(color="gray"),
		#legend.position = "right",
		#legend.position = c(.8, .95),
   		#legend.justification = c("right", "top"),
   		legend.box.just = "right",
		legend.margin = margin(6, 6, 6, 6)
	)

	if (show_errorbars == TRUE) {P <- P + 
#		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
		geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=0.25, size=1)
	}
	
	if (show_points == TRUE) {P <- P + geom_point(size=1, stroke=0.5)}

	if (show_lines == TRUE) {P <- P + 
		geom_line(linewidth=0.5)
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

grid_plot <- function(Z, 
	title="", 
	zlab="MFI Type", 
	ylim=c(0,100), ylab="% of Population", 
	xlim=c(0,100), xlab="MSNE (%)",
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE,
	log_x = FALSE
) {
	P <- small_plot(Z, show_points=show_points, show_lines=show_lines, show_errorbars=show_errorbars) +
	labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab)
	
	if (log_x == TRUE) {
		P <- P + scale_x_continuous(trans="log2",
			breaks = scales::trans_breaks("log2", function(x) 2^x)
			#labels = scales::trans_format("log2", scales::math_format(2^.x))
			)
	} else {
		P <- P + scale_x_continuous(limits=xlim, breaks = seq(xlim[1], xlim[2], by = xlim[2]/5))
	}
	
	P <- P + scale_y_continuous(limits=ylim, breaks = seq(ylim[1], ylim[2], by = ylim[2]/5))
	
	P <- P + 
	facet_grid(rows=vars(y2), cols=vars(x2), switch="both", as.table=FALSE)

	G <- groups_info()

	P <- P +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab])
	
	
	P
}

##############################################################################

wrap_plot <- function(Z, 
	title="", 
	zlab="MFI Type", 
	ylim=c(0,100), ylab="% of Population", 
	xlim=c(0,100), xlab="MSNE (%)",
	show_points = TRUE,
	show_lines = TRUE,
	show_errorbars = FALSE,
	log_x = FALSE,
	ncol=2
) {
	P <- small_plot(Z, show_points=show_points, show_lines=show_lines, show_errorbars=show_errorbars) +
	labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab)
	
	if (log_x == TRUE) {
		P <- P + scale_x_continuous(trans="log2",
			breaks = scales::trans_breaks("log2", function(x) 2^x)
			#labels = scales::trans_format("log2", scales::math_format(2^.x))
			)
	} else {
		P <- P + scale_x_continuous(limits=xlim, breaks = seq(xlim[1], xlim[2], by = xlim[2]/5))
	}
	
	P <- P + scale_y_continuous(limits=ylim, breaks = seq(ylim[1], ylim[2], by = ylim[2]/5))
	
	P <- P + 
	facet_wrap(~ x2, ncol=ncol)

	G <- groups_info()

	P <- P +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab])
	
	
	P
}

##############################################################################

##############################################################################

heatmap <- function(Z, 
	title="", 
	zlab="% of Population", 
	ylim=c(0,100), ylab="%", 
	xlim=c(0,100), xlab="MSNE",
	high.col="blue",
	low.col="white"
) {
	P <- ggplot(Z, aes(x=x, y=y, z=z, fill=z)) +
	theme_bw(base_size = 11) +
	theme(
		plot.title = element_text(size=11), 
		axis.title.x = element_text(size=11), 
		axis.title.y = element_text(size=11),
		#legend.position = "right",
		#legend.position = c(.8, .95),
   		#legend.justification = c("right", "top"),
   		legend.box.just = "right",
		legend.margin = margin(6, 6, 6, 6)
	) +
	#labs(title=title, x=xlab, y=ylab, color=zlab, shape=zlab) #+
	labs(title=title, x=xlab, y=ylab, fill=zlab) #+
	
	P <- P +
	#scale_x_discrete() +
	#scale_y_discrete()
	scale_x_continuous(limits=xlim, breaks = seq(xlim[1], xlim[2], by = (xlim[2] - xlim[1])/5)) +
	scale_y_continuous(limits=ylim, breaks = seq(ylim[1], ylim[2], by = (ylim[2] - ylim[1])/5))
	
	P <- P + geom_tile() +
	scale_fill_gradient(low=low.col, high=high.col)
	#scale_fill_distiller(palette = "RdPu")
	#scale_fill_viridis(discrete=FALSE)
	P
}

##############################################################################

# Cycle through the unique values of one parameter, 
# displaying response to other parameter.

##############################################################################

survey_y_vs_x1_by_x2 <- function(D, y="Mean.Perc.of.Pop", y_label="", y_interval="SE.Perc.of.Pop", x1="Inertia", x1_label="", x2="MSNE", x2_label="") {
	vals <- sort(unique(D[, get(x2)]))
	for (v in vals) {
		p <- plot_log_x(D[
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

survey_y_vs_x1perc_by_x2 <- function(D, y="Mean.Perc.of.Pop", y_label="", y_interval="SE.Perc.of.Pop", x1="Inertia", x1_label="", x2="MSNE", x2_label="") {
	vals <- sort(unique(D[, get(x2)]))
	for (v in vals) {
		p <- plot_generic(D[
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
		#title=paste0("CBs=", cur.num.cbs, "; Ine=", cur.ine, "; Mem= ", cur.mem, "; N=", cur.pop), 
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

plot_msne_icbd <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.ine <- unique(D[, Inertia])
	cur.mem <- unique(D[, Memory])

	P <- plot_generic(
		D[,
			.(
				x=MSNE, 
				y=Mean.ICB.Distance,
				z=MFI.Type,
				y.lower=Mean.ICB.Distance - SE.ICB.Distance,
				y.upper=Mean.ICB.Distance + SE.ICB.Distance
			)
		], 
		#title=paste0("CBs=", cur.num.cbs, "; Ine=", cur.ine, "; Mem= ", cur.mem, "; N=", cur.pop), 
		xlab="MSNE (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of World Max", 
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
		#title=paste0("Pos=", cur.init.pos, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; Mem=", cur.mem, "; N=", cur.pop), 
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

plot_cbeliefs_icbd <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.msne <- unique(D[, MSNE])
	cur.ine <- unique(D[, Inertia])
	cur.mem <- unique(D[, Memory])
	cur.init.pos <- unique(D[, Init.Positions])
	
	P <- plot_log_x(
		D[,
			.(
				x=Num.CBeliefs, 
				y=Mean.ICB.Distance,
				z=MFI.Type,
				y.lower=Mean.ICB.Distance - SE.ICB.Distance,
				y.upper=Mean.ICB.Distance + SE.ICB.Distance
			)
		], 
		#title=paste0("Pos=", cur.init.pos, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; Mem=", cur.mem, "; N=", cur.pop), 
		xlab="Number of C-Beliefs", 
		#xlim=c(0, max(D[, Num.CBeliefs])), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of World Max", 
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
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; Mem=", cur.mem, ""), 
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
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
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
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Mem=", cur.mem, "; N=", cur.pop), 
		xlab="Inertia (%)", 
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

plot_inertia_icbd <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.msne <- unique(D[, MSNE])
	cur.mem <- unique(D[, Memory])

	P <- plot_generic(
		D[,
			.(
				x=Inertia, 
				y=Mean.ICB.Distance,
				z=MFI.Type,
				y.lower=Mean.ICB.Distance - SE.ICB.Distance,
				y.upper=Mean.ICB.Distance + SE.ICB.Distance
			)
		], 
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Mem=", cur.mem, "; N=", cur.pop), 
		xlab="Inertia (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of World Max", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = FALSE
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
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; N=", cur.pop), 
		xlab="Memory (%)", 
		xlim=c(50,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of Population", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = TRUE
	)
	P
}

##############################################################################

plot_memory_icbd <- function(D) {
	cur.pop <- unique(D[, Num.People])
	cur.num.cbs <- unique(D[, Num.CBeliefs])
	cur.msne <- unique(D[, MSNE])
	cur.ine <- unique(D[, Inertia])

	P <- plot_generic(
		D[,
			.(
				x=Memory, 
				y=Mean.ICB.Distance,
				z=MFI.Type,
				y.lower=Mean.ICB.Distance - SE.ICB.Distance,
				y.upper=Mean.ICB.Distance + SE.ICB.Distance
			)
		], 
		#title=paste0("CBs=", cur.num.cbs, "; MSNE=", cur.msne, "; Ine=", cur.ine, "; N=", cur.pop), 
		xlab="Memory (%)", 
		xlim=c(0,100), 
		zlab="MFI Type", 
		ylim=c(0,100), ylab="% of World Max", 
		show_points = TRUE,
		show_lines = TRUE,
		show_errorbars = FALSE
	)
	P
}

##############################################################################

save_plot <- function(P, filename="test.png", dpi=150, units="px", width=600, height=400) {
	ggsave(P, filename=filename, dpi=dpi, units=units, width=width, height=height)
}

##############################################################################

save_grid_plot <- function(P, filename="test.png", dpi=150, units="px", width=600, height=600) {
	ggsave(P, filename=filename, dpi=dpi, units=units, width=width, height=height)
}

##############################################################################

unique_all <- function(D) {
	for (v in c(
		"MSNE",
		"Inertia",
		"Memory",
		"Num.CBeliefs",
		"Num.People",
		"Init.Attribs",
		"Init.Positions",
		"Init.Degree",
		"Stats.Retention"
	)) {
		print(v)
		print(unique(D[, get(v)]))
	}
	
}

##############################################################################

mem_mult <- function(mem=90, d0=90.0, A=c(0, 1)) {
	d <- d0
	Y <- 0 * 1:(1 + length(A))
	Y[1] <- d
	for (i in 1:(length(A))) {
		d <- 0.01 * (d * mem + (100 - mem) * 100 * A[i])
		Y[i + 1] <- d
	}
	return(Y)
}

##############################################################################

mem_roll <- function(mem.len=10, d0=90.0, A=c(0, 1)) {
	num1 <- round(0.01 * mem.len * d0)
	d <- sapply(1:num1, function(x) 1)
	d <- sample(append(d, sapply(1:(mem.len - length(d)), function(x) 0)))
	
	Y <- 0 * 1:(1 + length(A))
	Y[1] <- 100 * mean(d)
	for (i in 1:(length(A))) {
		d <- append(A[i], d[1:(mem.len - 1)])
		Y[i + 1] <- 100 * mean(d)
	}
	return(Y)
	
}

##############################################################################

plot_memory_dynamics <- function(MD) {
	P <- ggplot(MD, aes(x=x, y=y, z=z, color=z))
	P <- P + theme_light()
	P <- P + geom_line(linewidth=1)
	P <- P + labs(title="", x="Round", y="Degree of Belief", color="Method", shape="Method")
	P <- P + scale_x_continuous(limits=c(0, rl), breaks = seq(0, rl, by = rl/5))
	P <- P + scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 100/5))
	P
}

##############################################################################
