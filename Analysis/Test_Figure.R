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
# Load code in other script

source("common_code.R")

##############################################################################
# Load and process all the data files from NetLogo

D_MSNE <- file_processed("BestResponseToContexts experiment_MSNE-table.csv")
D_MSNE_Vars <- file_processed("BestResponseToContexts experiment_MSNE_Variations-table.csv")

D_Mem <- file_processed("BestResponseToContexts experiment_Memory-table.csv")

# Check for non-numeric values?
D_Pop <- file_processed("BestResponseToContexts experiment_Pop-table.csv")

D_Ine_Mem <- file_processed("BestResponseToContexts experiment_Inertia_v_Memory-table.csv")
D_Ine_CBe <- file_processed("BestResponseToContexts experiment_Inertia_v_CBeliefs-table.csv")
D_Ine_MSNE <- file_processed("BestResponseToContexts experiment_Inertia_v_MSNE-table.csv")

D_CBe_Mem <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_Memory-table.csv")
D_CBe_Ine <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_Inertia-table.csv")
D_CBe_MSNE <- file_processed("BestResponseToContexts experiment_CBeliefsMatched_v_MSNE-table.csv")

D_CBeU_Ine <- file_processed("BestResponseToContexts experiment_CBeliefs_v_Inertia-table.csv")

D_CBeU_Pop <- file_processed("BestResponseToContexts experiment_CBeliefs_Pop-table.csv")

##############################################################################

source("common_code.R")

P <- plot_msne(D_MSNE)
P
save_plot(P, filename="Fig_MSNE.png")

unique(D_MSNE_Vars[,Memory])
unique(D_MSNE_Vars[,Inertia])
unique(D_MSNE_Vars[,Num.People])
unique(D_MSNE_Vars[,Num.CBeliefs])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==16 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==4 & Num.People==50])
# OMG!!
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==16 & Num.People==200])

P <- plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
P
save_plot(P, filename="Fig_I50_M50_CB8_P200.png")


P <- plot_cbeliefs(D_CBeU_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefs.png")

P <- plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random"])
P
P <- plot_people(D_Pop)
P
save_plot(P, filename="Fig_People.png")

P <- plot_inertia(D_Ine_Mem[Memory==90])
P
save_plot(P, filename="Fig_Inertia.png")

P <- plot_memory(D_CBe_Mem[Num.CBeliefs==4])
P
P <- plot_memory(D_Ine_Mem[Inertia==90])
P
P <- plot_memory(D_Mem)
P
save_plot(P, filename="Fig_Memory.png")



##############################################################################
# Cycle through the unique values of one parameter, 
# displaying response to other parameter.
##############################################################################

# Plot MFI Types as % of Population (Perc.of.Pop)

source("common_code.R")

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

