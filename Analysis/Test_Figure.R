##############################################################################
## Script to create figures from the results .csv file output by BehaviorSpace
##############################################################################

# Libraries

library(ggplot2)
library(hrbrthemes)
#install.packages("xlsx") # If you don't have it already
library("xlsx")
library("data.table")

##############################################################################

# Change this to the directory on your computer. 
setwd("C:\\MyDocus\\Simulation\\NetLogo\\Games\\HawkDove\\M-Nodes\\BestResponseToContexts\\Analysis")

##############################################################################

source("common_code.R")

##############################################################################
# Import data

D <- read_netlogo_csv("BestResponseToContexts experiment_Inertia_v_MSNE-table.csv")

dim(D)
head(D)
names(D)

##############################################################################
# Identify exact dataset and relabel fields

D2 <- selected_fields(D)

dim(D2)
head(D2)

##############################################################################
# Transform down

D3 <- melt_by_mfi(D2)

dim(D3)
head(D3)
#unique(D3[,MFI.Type])
D3[,.N, by=MFI.Type]

##############################################################################



##############################################################################
# Aggregate to compute statistics

D4 <- aggregate_over_reps(D3)

dim(D4)
head(D4)

##############################################################################

head(groups_info())

##############################################################################
# Plot comparable data sets

p <- plot_perc_v_perc(D4[
	MSNE == 90, 
	.(
		x=Inertia, 
		y=Mean.Perc.of.Pop, 
		z=MFI.Type,
		y.lower=Mean.Perc.of.Pop - SE.Perc.of.Pop,
		y.upper=Mean.Perc.of.Pop + SE.Perc.of.Pop
	)
], xlab="Inertia (%)", ylab="% of Population")
p

##############################################################################

#ggsave(filename="test.jpg", dpi=300, units="in", width=4, height=4)

##############################################################################
# Load common functions from script

source("common_code.R")

##############################################################################
# Load and process all the data files from NetLogo

D_Ine_Mem <- file_processed("BestResponseToContexts experiment_Inertia_v_Memory-table.csv")
D_Ine_CBe <- file_processed("BestResponseToContexts experiment_Inertia_v_CBeliefs-table.csv")
D_Ine_MSNE <- file_processed("BestResponseToContexts experiment_Inertia_v_MSNE-table.csv")

D_CBe_Mem <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_Memory-table.csv")
D_CBe_Ine <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_Inertia-table.csv")
D_CBe_MSNE <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_MSNE-table.csv")

D_CBeU_Ine <- file_processed("BestResponseToContexts experiment_CBeliefs_v_Inertia-table.csv")

D_CBeU_Pop <- file_processed("BestResponseToContexts experiment_CBeliefs_Pop-table.csv")


##############################################################################
# Cycle through the unique values of one parameter, 
# displaying response to other parameter.

# Plot MFI Types as % of Population (Perc.of.Pop)

survey_y_vs_x1perc_by_x2(D_Ine_Mem, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="Memory", 
	x2_label="Memory"
)

survey_y_vs_x1perc_by_x2(D_Ine_CBe, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="Num.CBeliefs", 
	x2_label="Number of C-Beliefs"
)

survey_y_vs_x1perc_by_x2(D_Ine_MSNE, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="MSNE", 
	x2_label="MSNE"
)

##############################################################################

# Plot Inter-C-Belief distance (ICB.Distance)

survey_y_vs_x1perc_by_x2(D_Ine_Mem, 
	y="Mean.ICB.Distance", 
	y_label="% of World Max", 
	y_interval="SE.ICB.Distance", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="Memory", 
	x2_label="Memory"
)

survey_y_vs_x1perc_by_x2(D_Ine_CBe, 
	y="Mean.ICB.Distance", 
	y_label="% of World Max", 
	y_interval="SE.ICB.Distance", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="Num.CBeliefs", 
	x2_label="Number of C-Beliefs"
)

survey_y_vs_x1perc_by_x2(D_Ine_MSNE, 
	y="Mean.ICB.Distance", 
	y_label="% of World Max", 
	y_interval="SE.ICB.Distance", 
	x1="Inertia", 
	x1_label="Inertia (%)", 
	x2="MSNE", 
	x2_label="MSNE"
)

##############################################################################

dim(D_Ine_CBe)
head(D_Ine_CBe )
unique(D_Ine_CBe[,Num.Reps]



##############################################################################

# Plot MFI Types as % of Population (Perc.of.Pop)
# C-Beliefs - Matched

source("common_code.R")
survey_y_vs_x1_by_x2(D_CBe_Mem, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs", 
	x2="Memory", 
	x2_label="Memory"
)

survey_y_vs_x1_by_x2(D_CBe_Ine, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs",
	x2="Inertia", 
	x2_label="Inertia (%)"
)

survey_y_vs_x1_by_x2(D_CBe_MSNE, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs", 
	x2="MSNE", 
	x2_label="MSNE"
)

##############################################################################

# Plot MFI Types as % of Population (Perc.of.Pop)
# C-Beliefs - Unmatched

source("common_code.R")
survey_y_vs_x1_by_x2(D_CBeU_Mem, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs", 
	x2="Memory", 
	x2_label="Memory"
)

survey_y_vs_x1_by_x2(D_CBeU_Ine, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs",
	x2="Inertia", 
	x2_label="Inertia (%)"
)

survey_y_vs_x1_by_x2(D_CBeU_MSNE, 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x1="Num.CBeliefs", 
	x1_label="Number of C-Beliefs", 
	x2="MSNE", 
	x2_label="MSNE"
)

##############################################################################
# C-Beliefs vs Pop: 

survey_y_vs_x1_by_x2(D_CBeU_Pop[Init.Attribs=="Random" & Init.Positions=="Random" & Num.CBeliefs <= Num.People], 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x2="Num.CBeliefs", 
	x2_label="Number of C-Beliefs",
	x1="Num.People", 
	x1_label="Population Size"
)

survey_y_vs_x1_by_x2(D_CBeU_Pop[Init.Attribs=="Random" & Init.Positions=="At Other Agents" & Num.CBeliefs <= Num.People], 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x2="Num.CBeliefs", 
	x2_label="Number of C-Beliefs",
	x1="Num.People", 
	x1_label="Population Size"
)

survey_y_vs_x1_by_x2(D_CBeU_Pop[Num.CBeliefs == 4 & Init.Positions=="At Other Agents" & Num.CBeliefs <= Num.People], 
	y="Mean.Perc.of.Pop", 
	y_label="% of Population", 
	y_interval="SE.Perc.of.Pop", 
	x2="Init.Attribs", 
	x2_label="Number of C-Beliefs = 4 & Initial Attributes",
	x1="Num.People", 
	x1_label="Population Size"
)

##############################################################################

