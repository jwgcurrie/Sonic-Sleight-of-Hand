## Consequential sound induces illusory distortions in the perception and prediction of robot motion
## Experiment 1b
## Preprocessing and Analysis
## 24/05/2023

# Mr Joel Currie
# University of Aberdeen
# Scotland

rm(list = ls())

library(dplyr)
library(vctrs)
library(lme4)
library(hdrcde)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ez) 
library(psychReport)

di <- getwd()
setwd(di)

Expt1a <- read.csv("Experiment_1/data_Experiment_1a_processed.csv")
Expt1b <- read.csv("Experiment_1/data_Experiment_1b_processed.csv")

Expt1a['Expt'] <- '1a'
Expt1b['Expt'] <- '1b'

Expt1b$ID <- Expt1b$ID + max(Expt1a$ID)

Expts_combined <- data.frame(rbind(Expt1a, Expt1b))

Expts_combined$Action_length <- as.factor(data$Action_length)

x_combined_expt_compare <- ezANOVA(data=Expts_combined, 
                              dv=x_disp, 
                              wid=.(ID), 
                              within=.(Action_Dir, Action_length, Sound),
                              between=. (Expt),
                              type = 3,
                              detailed = TRUE)

x_combined_expt_compare

aovEffectSize(x_combined_expt_compare, effectSize = "pes")