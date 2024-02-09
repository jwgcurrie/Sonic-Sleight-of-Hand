

#rm(list = ls())

library(tidyverse) # data wrangling and visualization
library(sjPlot)    # to visualizing mixed-effects models
library(effects)   # to visualizing mixed-effects models
library(lme4)      # "golden standard" for mixed-effects modelling in R (no p-values)
library(lmerTest)  # p-values for MEMs based on the Satterthwaite approximation
library(report)    # mainly for an "report" function
library(emmeans)   # post-hoc analysis
library(knitr)     # beautifying tables
library(sjstats)   # ICC - intraclass-correlation coefficient
#library(caret)     # ML, model comparison & utility functions

BR_2049_colors <- c("#f78b04", "#027f93", "#a30502", "#2b1718", "#153a42")
BR_2049_colors2 <- c("#a30502", "#2b1718")


data <- read.csv("Experiment_2/data_2b_training.csv")

data['probe_vec'] <- 0
data$probe_vec[data$Probe_dir == '-'] <- -data$Probe_dist[data$Probe_dir == '-']
data$probe_vec[data$Probe_dir == '+'] <- data$Probe_dist[data$Probe_dir == '+']


# Changing probe factor
data['Prob_dirContrast'] <- 0
data$Prob_dirContrast[data$Probe_dir == '-'] <- -0.5
data$Prob_dirContrast[data$Probe_dir == '+'] <- +0.5

# Changing probe factor
data['Probe_distContrast'] <- 0
data$Probe_distContrast[data$Probe_dist == '1'] <- -0.5
data$Probe_distContrast[data$Probe_dist == '3'] <- 0.5


data['frames'] <- 0
data$frames[data$Action_pos == 'C'] <- -0.5
data$frames[data$Action_pos == 'M'] <- 0
data$frames[data$Action_pos == 'O'] <- 0.5

data['SoundContrast'] <- 0
data$SoundContrast[data$Sound == '+ 100 ms'] <- .5
data$SoundContrast[data$Sound == '- 100 ms'] <- -.5

data['ActionContrast'] <- 0
data$ActionContrast[data$Action_Dir == 'W'] <- -0.5
data$ActionContrast[data$Action_Dir == 'R'] <- 0.5

data['Exposure'] <- 0
data$Exposure <- (round(data$trial_number/32) - 4.5)/4.5


data['Probe_responseContrast'] <- 0
data$Probe_responseContrast[data$Probe_response == 0] <- -0.5
data$Probe_responseContrast[data$Probe_response == 1] <- 0.5


#mod_tr <- glmer(Probe_response ~ Action_Dir * frames + SoundContrast * probe_vec
#                + Exposure + Exposure:SoundContrast:probe_vec
#                +(1 + Action_Dir + probe_vec + frames + Exposure| ID),
#                data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#
#summary(mod_tr)

#mod_tr_pb <- glmer(Probe_response ~ Action_Dir * frames + SoundContrast * probe_vec
#                + Exposure + Exposure:SoundContrast:probe_vec + I(probe_vec^2)
#                +(1| ID),
#                data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))

#mod_tr_pb2 <- glmer(Probe_response ~ Action_Dir * frames + SoundContrast * probe_vec
#                   + Exposure + Exposure:SoundContrast:probe_vec + I(probe_vec^2)
#                   +(1 + Action_Dir + probe_vec + frames + Exposure| ID),
#                   data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))

mod_tr_pb3 <- glmer(Probe_response ~ ActionContrast * frames + SoundContrast * probe_vec
                    + Exposure + Exposure:SoundContrast:probe_vec + probe_vec:Exposure + I(probe_vec^2)
                    +(1 + ActionContrast + probe_vec + frames + Exposure| ID),
                    data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
tab_model(mod_tr_pb3)

plot_model(mod_tr_pb3, type = "pred")

plot_model(mod_tr_pb3, type = "pred", terms = c("Exposure", "probe_vec"))

#mod_tr_pb4 <- glmer(Probe_response ~ ActionContrast * frames + SoundContrast * probe_vec 
#                    + Exposure + Exposure*SoundContrast*probe_vec + I(probe_vec^2)
#                    +(1 + ActionContrast + I(probe_vec^2) + probe_vec + frames + Exposure| ID),
#                    data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))#




#mod_tr_pb5 <- glmer(Probe_response ~ ActionContrast * frames + SoundContrast * probe_vec # best fit
#                    + Exposure* I(probe_vec^2) + Exposure*SoundContrast*probe_vec 
#                    +(1 + ActionContrast + I(probe_vec^2) + probe_vec + frames + Exposure| ID),
#data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#

#tab_model(mod_tr_pb5)



#mod_tr_probe <- glmer(Probe_response ~ ActionContrast * frames + SoundContrast * Prob_dirContrast * Probe_distContrast * Exposure
#+(1 + ActionContrast + Probe_distContrast + Prob_dirContrast + frames + Exposure| ID),
#                    data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"))




