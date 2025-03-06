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
D_MSNE_Mem <- file_processed("BestResponseToContexts experiment_MSNE_Memory-table.csv")
D_MSNE_Ine <- file_processed("BestResponseToContexts experiment_MSNE_Inertia-table.csv")

D_CBe_IP_Ine <- file_processed("BestResponseToContexts experiment_CBeliefs_IP_Inertia-table.csv")

# Check for non-numeric values?
D_CBeU_Pop <- file_processed("BestResponseToContexts experiment_CBeliefs_Pop-table.csv")
D_Pop <- file_processed("BestResponseToContexts experiment_Pop-table.csv")

D_Ine <- file_processed("BestResponseToContexts experiment_Inertia-table.csv")
D_Ine_Mem_CBe_MSNE <- file_processed("BestResponseToContexts experiment_Inertia_Memory_CBeliefs_MSNE-table.csv")

D_Mem <- file_processed("BestResponseToContexts experiment_Memory-table.csv")
D_MSNE75_Mem <- file_processed("BestResponseToContexts experiment_MSNE75_Mem-table.csv")
D_Mem50_MSNE75 <- file_processed("BestResponseToContexts experiment_Mem50_MSNE75-table.csv")

D_Ine_Mem_CBe <- file_processed("BestResponseToContexts experiment_Inertia_Memory_CBe-table.csv")

##############################################################################
##############################################################################

source("common_code.R")

##############################################################################
##############################################################################

### MSNE ###

P <- plot_msne(D_MSNE)
P
save_plot(P, filename="Fig_MSNE.png")
P <- plot_msne_icbd(D_MSNE)
P


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


### C-Beliefs ###

unique_all(D_CBe_IP_Ine)

P <- plot_cbeliefs(D_CBe_IP_Ine[Inertia==90 & Init.Positions=="Random"])
P
save_plot(P, filename="Fig_CBeliefs.png")

P <- plot_cbeliefs(D_CBe_IP_Ine[Inertia==90 & Init.Positions=="At Other Agents"])
P
save_plot(P, filename="Fig_CBeliefsMatched.png")

#D_CBe_IP_Ine
Ine_levels <- sapply(sort(unique(D_CBe_IP_Ine[, Inertia]), decreasing=FALSE), function(x) paste0("Inertia = ", x))
Ine_levels 
P <- grid_plot(D_CBe_IP_Ine[,.(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), x2=paste0("Initial Positions ", Init.Positions))], log_x=TRUE, xlab="Number of C-Beliefs")
P
P <- grid_plot(D_CBe_IP_Ine[Inertia %in% c(20, 60, 90, 100),.(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, y2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), x2=paste0("Initial Positions ", Init.Positions))], log_x=TRUE, xlab="Number of C-Beliefs")
P
save_plot(P, filename="Fig_CBe_IP_Ine.png", width=750, height=800)


### People ###


unique_all(D_CBeU_Pop)

Pop_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.People]), decreasing=FALSE), function(x) paste0("Pop = ", x))
Pop_levels 

CBe_levels <- sapply(sort(unique(D_CBeU_Pop[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random" & Num.CBeliefs<=16, .(x=Num.CBeliefs, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Pop = ", Num.People), levels=Pop_levels))], xlim=c(1, 16), xlab="Number of C-Beliefs", log_x=TRUE)
P

P <- wrap_plot(D_CBeU_Pop[Init.Attribs=="Random" & Init.Positions=="Random" & Num.CBeliefs %in% c(1, 2, 4, 6, 8, 16), .(x=Num.People, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Number of People", xlim=c(0, 200), log_x=TRUE)
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


### Inertia ###

P <- plot_inertia(D_Ine)
P
save_plot(P, filename="Fig_Inertia.png")

unique_all(D_Ine_Mem_CBe_MSNE)
Mem_levels <- sapply(sort(unique(D_Ine_Mem_CBe_MSNE[, Memory]), decreasing=TRUE), function(x) paste0("Memory = ", x))
Mem_levels 
CBe_levels <- sapply(sort(unique(D_Ine_Mem_CBe_MSNE[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CBe_levels 
grid_plot(D_Ine_Mem_CBe_MSNE[MSNE==50,.(x=Inertia, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)")
P <- grid_plot(D_Ine_Mem_CBe_MSNE[MSNE==90,.(x=Inertia, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)")
P
save_plot(P, filename="Fig_Ine_Mem_CBe_MSNE90.png", width=800, height=800)

grid_plot(D_Ine_Mem_CBe_MSNE[MSNE==90,.(x=Inertia, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)", ylab="ICB Distance")
grid_plot(D_Ine_Mem_CBe_MSNE[MSNE==50,.(x=Inertia, y=Mean.ICB.Distance, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlab="Inertia (%)", ylab="ICB Distance")



### Memory ###

sort(unique(D_Mem[,Memory]))
P <- plot_memory(D_Mem)
P
save_plot(P, filename="Fig_Memory.png")

unique_all(D_MSNE75_Mem)
# At MSNE==75, Memory==50 is very different from Memory>=52
plot_generic(D_MSNE75_Mem[,.(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type)], xlim=c(50, 100), xlab="Memory")
plot_generic(D_MSNE75_Mem[,.(x=Memory, y=Mean.ICB.Distance, z=MFI.Type)], xlim=c(50, 100), xlab="Memory", ylab="ICB Distance")


# What happens when Memory==50 and MSNE==75?!
unique_all(D_Mem50_MSNE75)
plot_generic(D_Mem50_MSNE75[MSNE==75,.(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type)], xlim=c(45, 55), xlab="Memory")
plot_generic(D_Mem50_MSNE75[MSNE==75,.(x=Memory, y=Mean.ICB.Distance, z=MFI.Type)], xlim=c(45, 55), xlab="Memory", ylab="ICB Distance")

plot_generic(D_Mem50_MSNE75[Memory==50,.(x=MSNE, y=Mean.Perc.of.Pop, z=MFI.Type)], xlim=c(70, 80), xlab="MSNE (%)")
plot_generic(D_Mem50_MSNE75[Memory==50,.(x=MSNE, y=Mean.ICB.Distance, z=MFI.Type)], xlim=c(70, 80), xlab="MSNE (%)", ylab="ICB Distance")



### Heatmap Ine vs Mem, by CBe

source("common_code.R")

#D_Ine_Mem_CBe
unique_all(D_Ine_Mem_CBe)
CBe_levels <- sapply(sort(unique(D_Ine_Mem_CBe[, Num.CBeliefs]), decreasing=FALSE), function(x) paste0("CBs = ", x))
CBe_levels 
Perc_levels <- c("DD", "DH", "HD", "HH")

heatmap(D_Ine_Mem_CBe[Num.CBeliefs==2 & MFI.Type=="HH" & Memory<100,.(x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% HH", high.col="darkred", low.col="white")
heatmap(D_Ine_Mem_CBe[Num.CBeliefs==4 & MFI.Type=="HH" & Memory<100,.(x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% HH", high.col="darkred", low.col="white")
heatmap(D_Ine_Mem_CBe[Num.CBeliefs==8 & MFI.Type=="HH" & Memory<100,.(x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% HH", high.col="darkred", low.col="white")
heatmap(D_Ine_Mem_CBe[Num.CBeliefs==16 & MFI.Type=="HH" & Memory<100,.(x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% HH", high.col="darkred", low.col="white")

P <- heatmap(D_Ine_Mem_CBe[MFI.Type=="HH" & Memory<100,.(x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels), x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% HH", low.col="white", high.col="darkred")
P <- P + facet_wrap(~ x2, ncol=2)
P
save_plot(P, filename="Fig_HH_Ine_Mem_CBe.png", width=700, height=700)

P <- heatmap(D_Ine_Mem_CBe[MFI.Type %in% c("HD", "DH"),.(x2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels), x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% DH or HD", low.col="white", high.col="black")
P <- P + facet_wrap(~ x2, ncol=2)
P
P <- heatmap(D_Ine_Mem_CBe[Num.CBeliefs==2,.(x2=factor(MFI.Type, levels=Perc_levels), x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% Pop", low.col="white", high.col="black")
P <- P + facet_wrap(~ x2, ncol=2)
P
P <- heatmap(D_Ine_Mem_CBe[Num.CBeliefs==4,.(x2=factor(MFI.Type, levels=Perc_levels), x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% Pop", low.col="white", high.col="black")
P <- P + facet_wrap(~ x2, ncol=2)
P
P <- heatmap(D_Ine_Mem_CBe[Num.CBeliefs==8,.(x2=factor(MFI.Type, levels=Perc_levels), x=Inertia, y=Memory, z=Mean.Perc.of.Pop)], xlim=c(70,100), ylim=c(70,100), xlab="Inertia (%)", ylab="Memory (%)", zlab="% Pop", low.col="white", high.col="black")
P <- P + facet_wrap(~ x2, ncol=2)
P

# Alternative to Heatmap

Ine_levels <- sapply(sort(unique(D_Ine_Mem_CBe[, Inertia]), decreasing=FALSE), function(x) paste0("Inertia = ", x))
Ine_levels 
Mem_levels <- sapply(sort(unique(D_Ine_Mem_CBe[, Memory]), decreasing=FALSE), function(x) paste0("Memory = ", x))
Mem_levels 
CBe_levels <- sapply(sort(unique(D_Ine_Mem_CBe[, Num.CBeliefs]), decreasing=TRUE), function(x) paste0("CBs = ", x))
CBe_levels 
P <- grid_plot(D_Ine_Mem_CBe[Memory %in% c(70, 80, 90, 98),.(x=Inertia, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Memory = ", Memory), levels=Mem_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlim=c(70,100), xlab="Inertia (%)")
P
save_plot(P, filename="Fig_Ine_Mem_CBe_MSNE90.png", width=800, height=800)
P <- grid_plot(D_Ine_Mem_CBe[Inertia %in% c(70, 80, 90, 98),.(x=Memory, y=Mean.Perc.of.Pop, z=MFI.Type, x2=factor(paste0("Inertia = ", Inertia), levels=Ine_levels), y2=factor(paste0("CBs = ", Num.CBeliefs), levels=CBe_levels))], xlim=c(70,100), xlab="Memory (%)")
P
save_plot(P, filename="Fig_Mem_Ine_CBe_MSNE90.png", width=800, height=800)

##############################################################################

source("common_code.R")

##############################################################################

### Demonstrate Learning Methods ###

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
P <- plot_memory_dynamics(MD)
P
save_plot(P, filename="Fig_BeliefLearning_Mult.png", width=600, height=400)


MD <- data.table(x=R, y=sapply(R, function(x) msne), z="MSNE")
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=10, d0=d0, A=A), z="Len = 10"))
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=20, d0=d0, A=A), z="Len = 20"))
MD <- rbind(MD, data.table(x=R, y=mem_roll(mem.len=50, d0=d0, A=A), z="Len = 50"))
P <- plot_memory_dynamics(MD)
P
save_plot(P, filename="Fig_BeliefLearning_Roll.png", width=600, height=400)

##############################################################################

