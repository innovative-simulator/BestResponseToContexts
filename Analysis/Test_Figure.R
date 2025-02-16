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
D_MSNE_Mem <- file_processed("BestResponseToContexts experiment_MSNE_Memory-table.csv")
D_MSNE_Ine <- file_processed("BestResponseToContexts experiment_MSNE_Inertia-table.csv")


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

D_Mem_CBe <- file_processed("BestResponseToContexts experiment_Memory_CBeliefs-table.csv")
D_Mem_Ine_CBe <- file_processed("BestResponseToContexts experiment_Memory_Inertia_CBeliefs_MSNE-table.csv")


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
unique(D_Exp3[,MSNE])
unique(D_Exp3[,Num.CBeliefs])
unique(D_Exp3[,Memory])
unique(D_Exp3[,Inertia])
unique(D_Exp3[,Init.Positions])
unique(D_Exp3[,Init.Attribs])

MSNE_levels <- sapply(sort(unique(D_Exp3[, MSNE]), decreasing=FALSE), function(x) paste0("MSNE = ", x))
MSNE_levels 

P <- wrap_plot(D_Exp3[, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE)
P

# Num.CBeliefs affects inter-c-belief distance (obviously)
P <- wrap_plot(D_Exp3[, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P

##############################################################################

# 4. Inertia < 100%

unique(D_Exp4[,Num.CBeliefs])
unique(D_Exp4[,MSNE])
unique(D_Exp4[,Inertia])
unique(D_Exp4[,Init.Positions])

MSNE_levels <- sapply(sort(unique(D_Exp4[, MSNE]), decreasing=FALSE), function(x) paste0("MSNE = ", x))
MSNE_levels 

P <- wrap_plot(D_Exp4[Inertia==90, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE)
P
P <- wrap_plot(D_Exp4[Inertia==70, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE)
P
P <- wrap_plot(D_Exp4[Inertia==30, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE)
P

# ICB Distance

# For MSNE=90
# ICB Distance rises to peak around CB=6-8
# then falls to a low around CB=64
# then rises again
# MSNE =100 peaks earlier, troughs later
# MSNE =50, 70 peaks later, troughs earlier

P <- wrap_plot(D_Exp4[Inertia==100, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P
P <- wrap_plot(D_Exp4[Inertia==90, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P
P <- wrap_plot(D_Exp4[Inertia==70, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P
P <- wrap_plot(D_Exp4[Inertia==30, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P
P <- wrap_plot(D_Exp4[Inertia==0, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlim=c(0, 200), log_x=TRUE, ylab="Mean ICB Distance")
P

##############################################################################

# 5. Memory <= 100%

unique(D_Exp5[,MSNE])
unique(D_Exp5[,Num.CBeliefs])
unique(D_Exp5[,Memory])
unique(D_Exp5[,Inertia])
unique(D_Exp5[,Init.Positions])
unique(D_Exp5[,Init.Attribs])

Mem_levels <- sapply(sort(unique(D_Exp5[, Memory]), decreasing=TRUE), function(x) paste0("Memory = ", x))
Mem_levels 

P <- wrap_plot(D_Exp5[Num.CBeliefs==1, .(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))])
P
P <- wrap_plot(D_Exp5[Num.CBeliefs==2, .(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))])
P
P <- wrap_plot(D_Exp5[Num.CBeliefs==4, .(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))])
P
P <- wrap_plot(D_Exp5[Num.CBeliefs==8, .(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))])
P
P <- wrap_plot(D_Exp5[Num.CBeliefs==16, .(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))])
P

# ICB Distance not relevant when Inertia=100
P <- wrap_plot(D_Exp5[Num.CBeliefs==1, .(x=MSNE, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))], ylab="Mean ICB Distance")
P

##############################################################################

# 6. Mem<100, Inertia vars, CB vars, MSNE=90,70,50

# 7. Init Pos (Init Attrib, Init Deg)

# 8. Pop vs CB


source("common_code.R")

##############################################################################
##############################################################################

### MSNE ###

P <- plot_msne(D_MSNE)
P
save_plot(P, filename="Fig_MSNE.png")
P <- plot_msne_icbd(D_MSNE)
P

unique_all(D_MSNE_Mem)
Mem_levels <- sapply(sort(unique(D_MSNE_Mem[, Memory]), decreasing=TRUE), function(x) paste0("Memory = ", x))
Mem_levels 
CBe_levels <- sapply(sort(unique(D_MSNE_Mem[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CBe_levels 
grid_plot(D_MSNE_Mem[Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))])
grid_plot(D_MSNE_Mem[Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))])
P <- grid_plot(D_MSNE_Mem[Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))])
save_plot(P, filename="Fig_MSNE_Mem_CBe.png", width=800, height=800)

grid_plot(D_MSNE_Mem[Num.People==200,.(x=MSNE, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], ylab="ICB Distance")


unique_all(D_MSNE_Ine)
Ine_levels <- sapply(sort(unique(D_MSNE_Ine[, Inertia]), decreasing=TRUE), function(x) paste0("Inertia = ", x))
Ine_levels 
CBe_levels <- sapply(sort(unique(D_MSNE_Ine[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CBe_levels 
grid_plot(D_MSNE_Ine[Num.People==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))])
P <- grid_plot(D_MSNE_Ine[Num.People==200,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))])
P
save_plot(P, filename="Fig_MSNE_Ine_CBe.png", width=800, height=800)

grid_plot(D_MSNE_Ine[Num.People==200,.(x=MSNE, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], ylab="ICB Distance")


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

# Memory==50. Changes!
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==50 & Num.CBeliefs==2 & Num.People==200])

# Inertia==50. Response to MSNE not as broad as with Inertia==90
plot_msne(D_MSNE_Vars[Inertia==50 & Memory==90 & Num.CBeliefs==2 & Num.People==200])

# Pop==50
plot_msne(D_MSNE_Vars[Inertia==90 & Memory==90 & Num.CBeliefs==2 & Num.People==50])

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

P <- grid_plot(D_CBe_Ine_IP[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2), x2=Init.Positions)], xlab="Number of C-Beliefs", xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBe_Perc_IPos_Ine.png", height=800)


# Ups and downs at CB>=64

P <- wrap_plot(D_CBe_Ine[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2))], xlab="Number of C-Beliefs", xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBe_Perc_Ine.png", height=800)

P <- wrap_plot(D_CBeU_Ine[Inertia!=40, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels2))], xlab="Number of C-Beliefs", xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBeU_Perc_Ine.png", height=800)


P <- plot_cbeliefs(D_CBeU_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefs.png")

unique(D_CBe_Ine[,Inertia])
unique(D_CBe_Ine[,Num.CBeliefs])

plot_cbeliefs(D_CBe_Ine[Inertia==20])
plot_cbeliefs(D_CBe_Ine[Inertia==100])

P <- plot_cbeliefs(D_CBe_Ine[Inertia==90])
P
save_plot(P, filename="Fig_CBeliefsMatched.png")

# CBe vs MSNE

# MSNE_levels <- sapply(sort(unique(D_CBe_MSNE[, MSNE]), decreasing=FALSE), function(x) paste0("MSNE = ", x))
# MSNE_levels 
MSNE_levels <- sapply(c(90, 70, 10, 50), function(x) paste0("MSNE = ", x))
MSNE_levels 

P <- wrap_plot(D_CBe_MSNE[MSNE!=30, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("MSNE = ", MSNE), levels=MSNE_levels))], xlab="Number of C-Beliefs", xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_CBeU_Perc_MSNE.png", height=450)


unique(D_CBeU_Pop[,Num.People])
unique(D_CBeU_Pop[,Num.CBeliefs])
unique(D_CBeU_Pop[,Init.Positions])
unique(D_CBeU_Pop[,Init.Attribs])

Pop_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.People]), decreasing=FALSE), function(x) paste0("Pop = ", x))
Pop_levels 

CBe_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random" & Num.CBeliefs<=16, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Pop = ", Num.People), levels=Pop_levels))], xlim=c(1, 16), xlab="Number of C-Beliefs", log_x=TRUE)
P

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random" & Init.Positions=="Random" & Num.CBeliefs %in% c(1, 2, 4, 8, 12, 16), .(x=Num.People, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Number of People", xlim=c(0, 200), log_x=TRUE)
P
save_plot(P, filename="Fig_Pop_Perc_CBe.png", height=800)

# HH falls with CBeliefs
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==200 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])

# Triangular Grid and At Other Agents make little difference
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random" & Num.CBeliefs<Num.People])
plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="At Other Agents" & Init.Attribs=="Triangular Grid" & Num.CBeliefs<Num.People])

source("common_code.R")

P <- plot_people(D_CBeU_Pop[Num.CBeliefs==4 & Init.Positions=="Random" & Init.Attribs=="Random"])
P
P <- plot_people(D_Pop)
P
save_plot(P, filename="Fig_People.png")


unique(D_CBe_Mem[, Memory])
unique(D_CBe_Mem[, Num.CBeliefs])

Mem_levels <- sapply(sort(unique(D_CBe_Mem[, Memory]), decreasing=FALSE), function(x) paste0("Memory = ", x))
Mem_levels

P <- wrap_plot(D_CBe_Mem[, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))], xlab="Number of C-Beliefs", log_x=TRUE, ncol=2)
P
save_plot(P, filename="Fig_CBe_Perc_Mem.png", width=700, height=700)


unique(D_Ine_Mem[, Inertia])
unique(D_Ine_Mem[, Memory])

CBe_levels <- sapply(sort(unique(D_CBe_Ine[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 

P <- wrap_plot(D_CBe_Ine[Num.CBeliefs %in% c(1, 2, 4, 8, 16, 100, 140, 180, 200), .(x=Inertia, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)", ncol=3)
P
save_plot(P, filename="Fig_Ine_Perc_CBe.png", width=750, height=800)

wrap_plot(D_CBe_Ine[Num.CBeliefs %in% c(1, 2, 4, 8, 16, 100, 140, 180, 200), .(x=Inertia, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)", ncol=3, ylab="ICB Distance")


Ine_levels <- sapply(sort(unique(D_CBe_Ine[, Inertia]), decreasing=FALSE), function(x) paste0("Inertia = ", x))
Ine_levels 

P <- wrap_plot(D_CBe_Ine[Inertia!=60, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], log_x=TRUE, xlim=c(1, 200), xlab="Number of C-Beliefs", ncol=2)
P
save_plot(P, filename="Fig_CBe_Perc_Ine.png", width=700, height=750)

wrap_plot(D_CBe_Ine[Inertia!=60, .(x=Num.CBeliefs, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], log_x=TRUE, xlim=c(1, 200), xlab="Number of C-Beliefs", ncol=2, ylab="ICB Distance")


P <- wrap_plot(D_CBeU_Ine[Inertia!=60, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], log_x=TRUE, xlim=c(1, 200), xlab="Number of C-Beliefs", ncol=2)
P
save_plot(P, filename="Fig_CBeU_Perc_Ine.png", width=700, height=750)

P <- plot_inertia(D_Ine_Mem[Memory==90])
P
save_plot(P, filename="Fig_Inertia.png")


Mem_levels <- sapply(sort(unique(D_Ine_Mem[, Memory]), decreasing=FALSE), function(x) paste0("Memory = ", x))
Mem_levels

P <- wrap_plot(D_Ine_Mem[, .(x=Inertia, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels))], xlab="Inertia (%)", ncol=2)
P
save_plot(P, filename="Fig_Ine_Perc_Mem.png", width=700, height=700)

P <- plot_log_x(D_Mem[Memory >= 50,.(x=(101-Memory), y=Mean.Perc.of.Pop, z=MFI.Type)])
P
P <- plot_generic(D_Mem[Memory >= 50,.(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type)], xlim=c(50, 100))
P

P <- plot_memory(D_Mem)
P
save_plot(P, filename="Fig_Memory.png")

unique_all(D_Mem_CBe)
CBe_levels <- sapply(sort(unique(D_Mem_CBe[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 

P <- wrap_plot(D_Mem_CBe[, .(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Memory (%)", xlim=c(50, 100), log_x=FALSE)
P
save_plot(P, filename="Fig_Memory_CBeliefs.png")

wrap_plot(D_Mem_CBe[, .(x=Memory, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Memory (%)", xlim=c(50, 100), log_x=FALSE, ylab="ICB Distance")


unique_all(D_Mem_Ine_CBe)
Ine_levels <- sapply(sort(unique(D_Mem_Ine_CBe[, Inertia]), decreasing=FALSE), function(x) paste0("Inertia = ", x))
Ine_levels 
CBe_levels <- sapply(sort(unique(D_Mem_Ine_CBe[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CBe_levels 

grid_plot(D_Mem_Ine_CBe[MSNE==50, .(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels), x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], xlim=c(50, 100), xlab="Memory (%)")
P <- grid_plot(D_Mem_Ine_CBe[MSNE==90, .(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels), x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], xlim=c(50, 100), xlab="Memory (%)")
P
save_plot(P, filename="Fig_Memory_Inertia_CBeliefs.png", width=800, height=800)

grid_plot(D_Mem_Ine_CBe[MSNE==90, .(x=Memory, y=Mean.ICB.Distance, z=MFI.Type, y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels), x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels))], xlim=c(50, 100), xlab="Memory", ylab="ICB Distance")


##############################################################################

source("common_code.R")

##############################################################################

q <- 0.1
rl <- 100
R <- 0:rl
A <- ifelse(q > runif(1:rl), 1, 0)
A
msne <- 20
d0 <- 100
mem_mult(mem=95, d0=d0, A=A)
mem_roll(mem.len=10, d0=d0, A=A)

MD <- data.table(x=R, y=sapply(R, function(x) msne), z="MSNE")
MD <- rbind(MD, data.table(x=R, y=mem_mult(mem=95, d0=d0, A=A), z="Mem = 95"))
MD <- rbind(MD, data.table(x=R, y=mem_mult(mem=90, d0=d0, A=A), z="Mem = 90"))
MD <- rbind(MD, data.table(x=R, y=mem_mult(mem=70, d0=d0, A=A), z="Mem = 70"))
plot_memory_dynamics(MD)
save_plot(P, filename="Fig_BeliefLearning_Mult.png", width=600, height=400)


MD <- data.table(x=R, y=sapply(R, function(x) msne), z="MSNE")
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=10, d0=d0, A=A), z="Len = 10"))
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=20, d0=d0, A=A), z="Len = 20"))
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=50, d0=d0, A=A), z="Len = 50"))
plot_memory_dynamics(MD)
save_plot(P, filename="Fig_BeliefLearning_Roll.png", width=600, height=400)

##############################################################################

