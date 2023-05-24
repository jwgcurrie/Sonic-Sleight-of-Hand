## Consequential sound induces illusory distortions in the perception and prediction of robot motion
## Experiment 1a
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
dataset <- read.csv("Experiment_1/data_Experiment_1a.csv")

# Renaming data for convenience
dataset$blockcode[dataset$blockcode == 'reach_block_1'] <- 'block_R1'
dataset$blockcode[dataset$blockcode == 'reach_block_11'] <- 'block_R2'
dataset$blockcode[dataset$blockcode == 'reach_block_12'] <- 'block_R3'

dataset$blockcode[dataset$blockcode == 'withdraw_block_1'] <- 'block_W3'
dataset$blockcode[dataset$blockcode == 'withdraw_block_11'] <- 'block_W1'
dataset$blockcode[dataset$blockcode == 'withdraw_block_12'] <- 'block_W2'

delims_list <- filter(dataset, trialcode == 'debrief2')
delims_list$time # Unique time for each participant, unique time at trial debrief2 means participant completed the experiment

data = filter(dataset, time %in% delims_list$time)


# Find measurements:
data <- filter(dataset, trialcode == 'Mouse_test')

# Exclude non mouse measurements:
data <- filter(data, computer.platform == 'win' | computer.platform == 'mac')

# Exclude practice trials:
data <- filter(data, trial_number > 0)

# Scale for display size:
data$mousex = ((1920/data$display.width) * data$mousex)
data$mousey = ((1080/data$display.height) * data$mousey)

# Index participants:
data['ID'] <- vec_rep_each(1:(nrow(data)/120), 120)

# Code Action Direction:
data['Action_Dir'] <- (data$blockcode[] == 'block_R1'| data$blockcode[] == 'block_R2'| data$blockcode[] == 'block_R3') # True == Reach, False == Withdraw
data$Action_Dir[data$Action_Dir == TRUE] <- 'R'
data$Action_Dir[data$Action_Dir == FALSE] <- 'W'

# Code Action Length:
data['Action_length'] <- replicate(nrow(data), 'n')
data$Action_length[data$blockcode == 'block_R1' | data$blockcode == 'block_W1'] <- '1'
data$Action_length[data$blockcode == 'block_R2' | data$blockcode == 'block_W2'] <- '2'
data$Action_length[data$blockcode == 'block_R3' | data$blockcode == 'block_W3'] <- '3'

# Group audio_condition_select:
data['Sound'] <- (data$audio_condition_select[] == 1)
data$Sound[data$Sound == TRUE] <- 'S1'
data$Sound[data$Sound == FALSE] <- 'S2'

# Dataframe for disappearance positions:
position_label <- c('R1', 'R2', 'R3', 'W1', 'W2', 'W3')
disappearance_x <- c(870, 863, 850, 1154, 1176, 1182)
disappearance_y <- c(571, 564, 565, 634, 645, 650)
disaparance_positions <- data.frame(position_label, disappearance_x, disappearance_y)

# Calculate participant displacement:
data['x_disp'] <- replicate(nrow(data), 0)
data$x_disp[data$blockcode == 'block_R1'] <- data$mousex[data$blockcode == 'block_R1'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'R1']
data$x_disp[data$blockcode == 'block_R2'] <- data$mousex[data$blockcode == 'block_R2'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'R2']
data$x_disp[data$blockcode == 'block_R3'] <- data$mousex[data$blockcode == 'block_R3'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'R3']
data$x_disp[data$blockcode == 'block_W1'] <- data$mousex[data$blockcode == 'block_W1'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'W1']
data$x_disp[data$blockcode == 'block_W2'] <- data$mousex[data$blockcode == 'block_W2'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'W2']
data$x_disp[data$blockcode == 'block_W3'] <- data$mousex[data$blockcode == 'block_W3'] - disaparance_positions$disappearance_x[disaparance_positions$position_label == 'W3']

data['y_disp'] <- replicate(nrow(data), 0)
data$y_disp[data$blockcode == 'block_R1'] <- data$mousey[data$blockcode == 'block_R1'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'R1']
data$y_disp[data$blockcode == 'block_R2'] <- data$mousey[data$blockcode == 'block_R2'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'R2']
data$y_disp[data$blockcode == 'block_R3'] <- data$mousey[data$blockcode == 'block_R3'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'R3']
data$y_disp[data$blockcode == 'block_W1'] <- data$mousey[data$blockcode == 'block_W1'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'W1']
data$y_disp[data$blockcode == 'block_W2'] <- data$mousey[data$blockcode == 'block_W2'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'W2']
data$y_disp[data$blockcode == 'block_W3'] <- data$mousey[data$blockcode == 'block_W3'] - disaparance_positions$disappearance_y[disaparance_positions$position_label == 'W3']
data$y_disp = -data$y_disp


data <- data.frame(ID = data$ID, Action_Dir = data$Action_Dir, Action_length = data$Action_length, Sound = data$Sound, x_disp = data$x_disp, y_disp = data$y_disp)
# Exclude Participants by displacement:
excl_by_participant <- data
excl_by_participant <- excl_by_participant %>% 
  group_by(ID) %>% 
  summarize(x_disp = mean(x_disp),
            y_disp = mean(y_disp))

data <- filter(data, ID %in% excl_by_participant$ID)

# Exclude trials by Euclidean displacement :

excl_by_participant['eucl_disp'] = sqrt(excl_by_participant$x_disp^2 + excl_by_participant$y_disp^2)
excl_by_participant <- filter(excl_by_participant,eucl_disp < 96)
data <- filter(data, ID %in% excl_by_participant$ID)

data['eucl_disp'] = sqrt(data$x_disp^2 + data$y_disp^2)
excl_disp_3SD <- median(data$eucl_disp) + 3*sd(data$eucl_disp)
data <- filter(data, eucl_disp <= excl_disp_3SD)

## Summarize data for statistical treatment
sum_data <- data %>% 
  group_by(ID, Action_Dir, Action_length, Sound) %>% 
  summarize(x_disp = mean(x_disp),
            y_disp = mean(y_disp))


## Statistical Analysis
# Within participant, 3 x 2 x 2 ANOVA for x-axis data
x_disp_model <- ezANOVA(data=sum_data, 
                        dv=x_disp, 
                        wid=.(ID), 
                        within=.(Action_Dir, Action_length, Sound),
                        type = 3,
                        detailed = TRUE)

aovEffectSize(x_disp_model, effectSize = "pes")
# Within participant, 3 x 2 x 2 ANOVA for y-axis data
y_disp_model <- ezANOVA(data=sum_data, 
                        dv=y_disp, 
                        wid=.(ID), 
                        within=.(Action_Dir, Action_length, Sound),
                        type = 3,
                        detailed = TRUE)

aovEffectSize(y_disp_model, effectSize = "pes")

