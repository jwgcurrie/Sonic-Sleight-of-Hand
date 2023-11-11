

rm(list = ls())

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


data <- read.csv("Experiment_1/data_Experiment_1b_processed_RT.csv")

data['frames'] <- data$Action_length

data['SoundContrast'] <- 0
data$SoundContrast[data$Sound == 'S2'] <- .5
data$SoundContrast[data$Sound == 'S1'] <- -.5

data['ActionContrast'] <- 0
data$ActionContrast[data$Action_Dir == 'W'] <- -0.5
data$ActionContrast[data$Action_Dir == 'R'] <- 0.5




data['blocks'] <- 0
data$blocks <- round(data$trial_number/13) + 1

mod0 <- lmer(x_disp ~ ActionContrast * frames * SoundContrast
                    + blocks + blocks:SoundContrast:ActionContrast
                    +(1 + ActionContrast + frames + blocks|ID),
                    data = data, control = lmerControl(optimizer = "bobyqa"))

#mod1 <- lmer(x_disp ~ Action_Dir * frames * SoundContrast
#             + blocks + blocks:SoundContrast:Action_Dir
#             +(1 + Action_Dir + frames + blocks|ID)
#             + (Action_Dir|frames),
#             data = data, control = lmerControl(optimizer = "bobyqa"))

tab_model(mod0)

plot(mod0)
qqnorm(residuals(mod0))

sjPlot::plot_model(mod0)
