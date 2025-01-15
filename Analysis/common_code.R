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
			Init.Positions = D$"Initial-C-Belief-Positions", 
			Init.Attribs = D$"Initial-Person-Attributes", 
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
			Init.Positions, 
			Init.Attribs, 
			MFI.Type
		)]
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

# Plot comparable data sets

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

plot_perc_v_x <- function(Z, ylim=c(0,100), ylab="% of Population", zlab="MFI Type", xlim=c(0,100), xlab="%", title="") {
	G <- groups_info()
	xbreaks <- 10**(1:5 * 0.5)
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
	#scale_x_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	#scale_x_continuous(trans="log10") +
	#scale_x_log10(
	scale_x_continuous(trans="log2",
		breaks = scales::trans_breaks("log2", function(x) 2^x)
		#labels = scales::trans_format("log2", scales::math_format(2^.x))
	) +
	scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, by = 20)) +
	geom_point(size=3) +
	scale_color_manual(values = G[,pcol], labels=G[,lab]) +
	scale_shape_manual(values = G[,shape], labels=G[,lab]) +
#	geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2, position=position_dodge(0)) +
	#geom_errorbar(aes(ymin=y.lower, ymax=y.upper), width=2) +
	geom_line(linewidth=1)
}

##############################################################################

# Cycle through the unique values of one parameter, 
# displaying response to other parameter.

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

survey_y_vs_x1perc_by_x2 <- function(D, y="Mean.Perc.of.Pop", y_label="", y_interval="SE.Perc.of.Pop", x1="Inertia", x1_label="", x2="MSNE", x2_label="") {
	vals <- sort(unique(D[, get(x2)]))
	for (v in vals) {
		p <- plot_perc_v_perc(D[
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

##############################################################################

##############################################################################

##############################################################################

##############################################################################

