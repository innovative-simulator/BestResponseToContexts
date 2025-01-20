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

D_Exp1 <- file_processed("BestResponseToContexts experiment_Exp1_MSNE-table.csv")
D_Exp2 <- file_processed("BestResponseToContexts experiment_Exp2_StatRet-table.csv")
D_Exp3 <- file_processed("BestResponseToContexts experiment_Exp3_CBeliefs-table.csv")
D_Exp4 <- file_processed("BestResponseToContexts experiment_Exp4_Inertia-table.csv")
D_Exp4 <- rbind(D_Exp4, D_Exp3[MSNE>=50]) # Add Inertia=100




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

# 1. Varying MSNE with Stat.Retention = 100
P <- plot_msne(D_Exp1)
P

# 2. Statistical Retention=90%
P <- plot_msne(D_Exp2)
P

# 3. CBs > 1
# CBs has no interaction with MSNE
unique(D_Exp3[,Num.CBeliefs])
plot_msne(D_Exp3[Num.CBeliefs==1])
plot_msne(D_Exp3[Num.CBeliefs==2])
plot_msne(D_Exp3[Num.CBeliefs==4])
plot_msne(D_Exp3[Num.CBeliefs==16])
plot_msne(D_Exp3[Num.CBeliefs==64])
plot_msne(D_Exp3[Num.CBeliefs==200])

unique(D_Exp3[,MSNE])
plot_cbeliefs(D_Exp3[MSNE==100])
plot_cbeliefs(D_Exp3[MSNE==90])
plot_cbeliefs(D_Exp3[MSNE==70])
plot_cbeliefs(D_Exp3[MSNE==50])
plot_cbeliefs(D_Exp3[MSNE==30])
plot_cbeliefs(D_Exp3[MSNE==10])
plot_cbeliefs(D_Exp3[MSNE==0])

# Num.CBeliefs affects inter-c-belief distance (obviously)
plot_cbeliefs_icbd(D_Exp3[MSNE==100])
plot_cbeliefs_icbd(D_Exp3[MSNE==90])
plot_cbeliefs_icbd(D_Exp3[MSNE==70])
plot_cbeliefs_icbd(D_Exp3[MSNE==50])
plot_cbeliefs_icbd(D_Exp3[MSNE==30])
plot_cbeliefs_icbd(D_Exp3[MSNE==10])
plot_cbeliefs_icbd(D_Exp3[MSNE==0])

# 4. Inertia < 100%
unique(D_Exp4[,Num.CBeliefs])
unique(D_Exp4[,MSNE])
unique(D_Exp4[,Inertia])
unique(D_Exp4[,Init.Positions])

plot_cbeliefs_icbd(D_Exp4[MSNE==100 & Inertia==30])
plot_cbeliefs_icbd(D_Exp4[MSNE==90 & Inertia==30])
plot_cbeliefs_icbd(D_Exp4[MSNE==70 & Inertia==30])

plot_cbeliefs(D_Exp4[MSNE==100 & Inertia==90])
plot_cbeliefs(D_Exp4[MSNE==90 & Inertia==90])
plot_cbeliefs(D_Exp4[MSNE==70 & Inertia==90])
plot_cbeliefs(D_Exp4[MSNE==50 & Inertia==90])

plot_inertia(D_Exp4[MSNE==50 & Num.CBeliefs==1])
plot_inertia(D_Exp4[MSNE==50 & Num.CBeliefs==2])
plot_inertia(D_Exp4[MSNE==50 & Num.CBeliefs==4])
plot_inertia(D_Exp4[MSNE==50 & Num.CBeliefs==8])
plot_inertia(D_Exp4[MSNE==50 & Num.CBeliefs==16])

plot_inertia_icbd(D_Exp4[MSNE==90 & Num.CBeliefs==1])
plot_inertia_icbd(D_Exp4[MSNE==90 & Num.CBeliefs==2])
plot_inertia_icbd(D_Exp4[MSNE==90 & Num.CBeliefs==4])
plot_inertia_icbd(D_Exp4[MSNE==90 & Num.CBeliefs==8])
plot_inertia_icbd(D_Exp4[MSNE==90 & Num.CBeliefs==16])


plot_msne(D_Exp4[Num.CBeliefs==1 & Inertia==30])
plot_msne(D_Exp4[Num.CBeliefs==2 & Inertia==30])
plot_msne(D_Exp4[Num.CBeliefs==4 & Inertia==30])
plot_msne(D_Exp4[Num.CBeliefs==8 & Inertia==30])
plot_msne(D_Exp4[Num.CBeliefs==16 & Inertia==30])
plot_msne(D_Exp4[Num.CBeliefs==200 & Inertia==30])






source("common_code.R")



P <- plot_msne(D_MSNE)
P
save_plot(P, filename="Fig_MSNE.png")
P <- plot_icbd_msne(D_MSNE)
P

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

plot_icbd_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==2 & Num.People==200])
plot_icbd_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==4 & Num.People==200])
plot_icbd_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
plot_icbd_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==16 & Num.People==200])

plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==16 & Num.People==200])

plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==16 & Num.People==200])

P <- plot_msne(D_MSNE_Vars[Inertia==50 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
P
save_plot(P, filename="Fig_I50_M50_CB8_P200.png")


P <- plot_cbeliefs(D_CBeU_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefs.png")

P <- plot_cbeliefs(D_CBe_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefsMatched.png")


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

