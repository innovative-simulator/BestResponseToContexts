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
D_Exp5 <- file_processed("BestResponseToContexts experiment_Exp5_Memory-table.csv")



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

#D_Mem50_InitDegree <- file_processed("BestResponseToContexts experiment_Mem50_MSNE_InitDeg-table.csv")
#D_Mem50_Response <- file_processed("BestResponseToContexts experiment_Mem50_MSNE_ResponseChoice-table.csv")


##############################################################################
##############################################################################

source("common_code.R")

##############################################################################

# 1. Varying MSNE with Stat.Retention = 100

P <- plot_msne(D_Exp1)
P

##############################################################################

# 2. Statistical Retention=90%

P <- plot_msne(D_Exp2)
P

##############################################################################

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

##############################################################################

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

##############################################################################

# 5. Memory <= 100%

unique(D_Exp5[,Memory])
unique(D_Exp5[,Num.CBeliefs])
unique(D_Exp5[,MSNE])
unique(D_Exp5[,Inertia])
unique(D_Exp5[,Init.Positions])

plot_msne(D_Exp5[Num.CBeliefs==1 & Memory==30])
plot_msne(D_Exp5[Num.CBeliefs==2 & Memory==30 ])
plot_msne(D_Exp5[Num.CBeliefs==4 & Memory==30 ])
plot_msne(D_Exp5[Num.CBeliefs==8 & Memory==30 ])
plot_msne(D_Exp5[Num.CBeliefs==16 & Memory==30 ])

plot_msne(D_Exp5[Num.CBeliefs==1 & Memory==50])
plot_msne(D_Exp5[Num.CBeliefs==2 & Memory==50 ])
plot_msne(D_Exp5[Num.CBeliefs==4 & Memory==50 ])
plot_msne(D_Exp5[Num.CBeliefs==8 & Memory==50 ])
plot_msne(D_Exp5[Num.CBeliefs==16 & Memory==50 ])

plot_msne(D_Exp5[Num.CBeliefs==1 & Memory==70])
plot_msne(D_Exp5[Num.CBeliefs==2 & Memory==70 ])
plot_msne(D_Exp5[Num.CBeliefs==4 & Memory==70 ])
plot_msne(D_Exp5[Num.CBeliefs==8 & Memory==70 ])
plot_msne(D_Exp5[Num.CBeliefs==16 & Memory==70 ])

plot_msne_icbd(D_Exp5[Num.CBeliefs==1 & Memory==50])
plot_msne_icbd(D_Exp5[Num.CBeliefs==2 & Memory==50 ])
plot_msne_icbd(D_Exp5[Num.CBeliefs==4 & Memory==50 ])
plot_msne_icbd(D_Exp5[Num.CBeliefs==8 & Memory==50 ])
plot_msne_icbd(D_Exp5[Num.CBeliefs==16 & Memory==50 ])

plot_memory(D_Exp5[MSNE==50 & Num.CBeliefs==1])
plot_memory(D_Exp5[MSNE==50 & Num.CBeliefs==2])
plot_memory(D_Exp5[MSNE==50 & Num.CBeliefs==4])
plot_memory(D_Exp5[MSNE==50 & Num.CBeliefs==8])
plot_memory(D_Exp5[MSNE==50 & Num.CBeliefs==16])

plot_memory(D_Exp5[MSNE==90 & Num.CBeliefs==1])
plot_memory(D_Exp5[MSNE==90 & Num.CBeliefs==2])
plot_memory(D_Exp5[MSNE==90 & Num.CBeliefs==4])
plot_memory(D_Exp5[MSNE==90 & Num.CBeliefs==8])
plot_memory(D_Exp5[MSNE==90 & Num.CBeliefs==16])

plot_cbeliefs(D_Exp5[MSNE==50 & Memory==90])
plot_cbeliefs(D_Exp5[MSNE==70 & Memory==90])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==90])
plot_cbeliefs(D_Exp5[MSNE==100 & Memory==90])

plot_cbeliefs(D_Exp5[MSNE==50 & Memory==50])
plot_cbeliefs(D_Exp5[MSNE==70 & Memory==50])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==50])
plot_cbeliefs(D_Exp5[MSNE==100 & Memory==50])

plot_cbeliefs(D_Exp5[MSNE==50 & Memory==100])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==90])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==70])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==50])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==30])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==10])
plot_cbeliefs(D_Exp5[MSNE==50 & Memory==0])

plot_cbeliefs(D_Exp5[MSNE==90 & Memory==100])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==90])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==70])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==50])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==30])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==10])
plot_cbeliefs(D_Exp5[MSNE==90 & Memory==0])






##############################################################################


source("common_code.R")

##############################################################################
##############################################################################


P <- plot_msne(D_MSNE)
P
save_plot(P, filename="Fig_MSNE.png")
P <- plot_msne_icbd(D_MSNE)
P

Mem_levels <- sapply(sort(unique(D_MSNE_Vars[, Memory]), decreasing=TRUE), function(x) paste0("Memory = ", x))
Mem_levels 
Ine_levels <- sapply(sort(unique(D_MSNE_Vars[, Inertia]), decreasing=TRUE), function(x) paste0("Inertia = ", x))
Ine_levels 
CB_levels <- sapply(sort(unique(D_MSNE_Vars[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CB_levels 
unique(D_MSNE_Vars[,Num.People])

grid_plot(D_MSNE_Vars[Memory==90 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Memory==50 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Memory==90 & Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Memory==50 & Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])

source("common_code.R")

P <- grid_plot(D_MSNE_Vars[Memory==90 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
save_grid_plot(P, filename="Fig_MSNE_Perc_Ine_CBs.png")


grid_plot(D_MSNE_Vars[Inertia==90 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Inertia==50 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Inertia==90 & Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
grid_plot(D_MSNE_Vars[Inertia==50 & Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])

P <- grid_plot(D_MSNE_Vars[Inertia==90 & Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CB_levels))])
save_grid_plot(P, filename="Fig_MSNE_Perc_Mem_CBs.png")




# Num.CBeliefs. Region of MSNE that produce Dominance broadens with CBeliefs
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==16 & Num.People==200])

# Memory==50. Changes!
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==16 & Num.People==200])

# Inertia==50. Response to MSNE not as broad as with Inertia==90
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==16 & Num.People==200])

# Pop==50
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==2 & Num.People==50])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==4 & Num.People==50])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==8 & Num.People==50])
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==16 & Num.People==50])

plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==2 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==4 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==8 & Num.People==200])
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==16 & Num.People==200])

P <- plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==8 & Num.People==200])
P
save_plot(P, filename="Fig_I90_M50_CB8_P200.png")

D_CBe_Ine_IP <- rbind(D_CBe_Ine, D_CBeU_Ine)

unique(D_CBe_Ine_IP[,Inertia])
unique(D_CBeU_Ine[,Num.CBeliefs])
unique(D_CBeU_Ine[,MSNE])

Ine_levels2 <- sapply(sort(unique(D_CBe_Ine_IP[, Inertia]), decreasing=TRUE), function(x) paste0("Inertia = ", x))
Ine_levels2 

source("common_code.R")

P <- grid_plot(D_CBe_Ine_IP[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2), x2=Init.Positions)], xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBe_Perc_IPos_Ine.png", height=800)


# Ups and downs at CB>=64
plot_cbeliefs(D_CBeU_Ine[Inertia==20])
plot_cbeliefs(D_CBeU_Ine[Inertia==40])
plot_cbeliefs(D_CBeU_Ine[Inertia==60])
plot_cbeliefs(D_CBeU_Ine[Inertia==80])
plot_cbeliefs(D_CBeU_Ine[Inertia==90])
plot_cbeliefs(D_CBeU_Ine[Inertia==95])
plot_cbeliefs(D_CBeU_Ine[Inertia==100])

P <- wrap_plot(D_CBe_Ine[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2))], xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBe_Perc_Ine.png", height=800)

P <- wrap_plot(D_CBeU_Ine[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2))], xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBeU_Perc_Ine.png", height=800)


P <- plot_cbeliefs(D_CBeU_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefs.png")

unique(D_CBe_Ine[,Inertia])
unique(D_CBe_Ine[,Num.CBeliefs])

plot_cbeliefs(D_CBe_Ine[Inertia==20])
plot_cbeliefs(D_CBe_Ine[Inertia==40])
plot_cbeliefs(D_CBe_Ine[Inertia==60])
plot_cbeliefs(D_CBe_Ine[Inertia==80])
plot_cbeliefs(D_CBe_Ine[Inertia==90])
plot_cbeliefs(D_CBe_Ine[Inertia==95])
plot_cbeliefs(D_CBe_Ine[Inertia==100])

P <- plot_cbeliefs(D_CBe_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefsMatched.png")


unique(D_CBeU_Pop[,Num.People])
unique(D_CBeU_Pop[,Num.CBeliefs])
unique(D_CBeU_Pop[,Init.Positions])
unique(D_CBeU_Pop[,Init.Attribs])

Pop_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.People]), decreasing=FALSE), function(x) paste0("Pop = ", x))
Pop_levels 

CBe_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random", .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Pop = ", Num.People), levels=Pop_levels))], xlim=c(0, 200), log_x=TRUE)
P

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random" & Init.Positions=="Random" & Num.CBeliefs %in% c(1, 2, 4, 8, 12, 16), .(x=Num.People, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_Pop_Perc_CBe.png", height=800)

# HH falls with CBeliefs
plot_people(D_CBeU_Pop[Num.CBeliefs==1 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==2 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==8 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==16 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==64 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==100 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==196 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==200 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])

# Triangular Grid and At Other Agents make little difference
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Triangular Grid" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="At Other Agents" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="At Other Agents" & Init.Attribs=="Triangular Grid" & Num.CBeliefs<Num.People])

P <- plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random"])
P
P <- plot_people(D_Pop)
P
save_plot(P, filename="Fig_People.png")



unique(D_CBe_Mem[, Memory])
unique(D_CBe_Mem[, Num.CBeliefs])
plot_memory(D_CBe_Mem[Num.CBeliefs==1])
plot_memory(D_CBe_Mem[Num.CBeliefs==2])
plot_memory(D_CBe_Mem[Num.CBeliefs==4])
plot_memory(D_CBe_Mem[Num.CBeliefs==8])
plot_memory(D_CBe_Mem[Num.CBeliefs==16])

plot_cbeliefs(D_CBe_Mem[Memory==30])
plot_cbeliefs(D_CBe_Mem[Memory==50])
plot_cbeliefs(D_CBe_Mem[Memory==70])
plot_cbeliefs(D_CBe_Mem[Memory==90])


unique(D_Ine_Mem[, Inertia])
unique(D_Ine_Mem[, Memory])

plot_inertia(D_Ine_Mem[Memory==30])
plot_inertia(D_Ine_Mem[Memory==50])
plot_inertia(D_Ine_Mem[Memory==70])
plot_inertia(D_Ine_Mem[Memory==90])

plot_inertia(D_CBe_Ine[Num.CBeliefs==1])
plot_inertia(D_CBe_Ine[Num.CBeliefs==2])
plot_inertia(D_CBe_Ine[Num.CBeliefs==4])
plot_inertia(D_CBe_Ine[Num.CBeliefs==8])
plot_inertia(D_CBe_Ine[Num.CBeliefs==16])
plot_inertia(D_CBe_Ine[Num.CBeliefs==64])
plot_inertia(D_CBe_Ine[Num.CBeliefs==100])
plot_inertia(D_CBe_Ine[Num.CBeliefs==200])

P <- plot_inertia(D_Ine_Mem[Memory==90])
P
save_plot(P, filename="Fig_Inertia.png")


plot_memory(D_Ine_Mem[Inertia==0])
plot_memory(D_Ine_Mem[Inertia==20])
plot_memory(D_Ine_Mem[Inertia==50])
plot_memory(D_Ine_Mem[Inertia==70])
plot_memory(D_Ine_Mem[Inertia==90])
plot_memory(D_Ine_Mem[Inertia==95])
plot_memory(D_Ine_Mem[Inertia==97.5])
plot_memory(D_Ine_Mem[Inertia==100])

P <- plot_memory(D_Mem)
P
save_plot(P, filename="Fig_Memory.png")


##############################################################################
# Heatmap

source("common_code.R")

names(D_Ine_Mem)
unique(D_Ine_Mem[, MFI.Type])
heatmap(D_MSNE_Vars[MFI.Type=="HD" & Memory==90 & Num.CBeliefs==4,.(x=MSNE, y=Inertia, z=Mean.Perc.of.Pop)])
heatmap(D_MSNE_Vars[MFI.Type=="HD" & Memory==90 & Inertia==90,.(x=MSNE, y=Num.CBeliefs, z=Mean.Perc.of.Pop)], ylim=c(0, 200))

heatmap(D_MSNE_Vars[MFI.Type=="HD" & Num.CBeliefs==4 & Inertia==90,.(x=MSNE, y=Memory, z=Mean.Perc.of.Pop)])
heatmap(D_MSNE_Vars[MFI.Type=="HD" & Num.CBeliefs==8 & Inertia==90,.(x=MSNE, y=Memory, z=Mean.Perc.of.Pop)])


heatmap(D_Ine_Mem[MFI.Type=="HD" & Num.CBeliefs==4 & MSNE==90,.(x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlab="Inertia", ylab="Memory")


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

