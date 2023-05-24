## Experiment 2a && 2b Demmand Effects Comparison:

di <- getwd()
setwd(di)

rm(list = ls())

library(dplyr)
library(vctrs)
library(lme4)
library(hdrcde)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(patchwork)
library(ez) 
library(psychReport)
library(zoo)
library(ggdist)
library(tidyquant)


demmand_2a <- "Experiment_2/Demmand_Combined/demmand_2a.csv"
demmand_2b <- "Experiment_2//Demmand_Combined/demmand_2b.csv"

demmand_2a <- read.csv(demmand_2a)
demmand_2b <- read.csv(demmand_2b)

shapiro.test(demmand_2a$RM)

z_pre_demmand_2a <- subset(demmand_2a, select = -c(ID) )
z_pre_demmand_2b <- subset(demmand_2b, select = -c(ID) )



z_2a <- data.frame(sapply(z_pre_demmand_2a, function(data) (data-mean(data))/sd(data)))
z_2b <- data.frame(sapply(z_pre_demmand_2b, function(data) (data-mean(data))/sd(data)))

z_comb <- data.frame(rbind(z_2a, z_2b))


cor.test(z_comb$Hyp_score, z_comb$RM, method= "pearson")

cor.test(z_comb$SD, z_comb$RM, method= "pearson")

hyp_linear_model <- lm(RM ~ Hyp_score, data = z_comb)

sound_detect_linear_model <- lm(RM ~ SD_Score, data = z_comb)

summary(hyp_linear_model)

summary(sound_detect_linear_model)

non_normalized <- data.frame(rbind(z_pre_demmand_2a, z_pre_demmand_2b))

cor.test(non_normalized$Hyp_score, non_normalized$RM, method= "pearson")

cor.test(non_normalized$SD, z_comb$RM, method= "pearson")

non_normalized_hyp_linear_model <- lm(RM ~ Hyp_score, data = non_normalized)

non_normalized_sound_detect_linear_model <- lm(RM ~ SD_Score, data = non_normalized)

summary(non_normalized_hyp_linear_model)

summary(non_normalized_sound_detect_linear_model)

                             