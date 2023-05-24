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
  dataset <- read.csv("Experiment_1/data_Experiment_1b.csv")
  
  # Filter incomplete experiments:
  delims_list <- filter(dataset, trialcode == 'debrief2')
  delims_list$time # Unique time for each participant, unique time at trial debrief2 means participant completed the experiment
  
  data = filter(dataset, time %in%  delims_list$time)
  
  # Find measurements:
  data <- filter(dataset, trialcode == 'Mouse_test')
  
  # Exclude non-mouse measurements:
  data <- filter(data, computer.platform == 'win' | computer.platform == 'mac')
  
  # Exclude practice trials:
  data <- filter(data, trial_number > 0)
  
  # Scale for display size:
  data$mousex = ((1920/data$display.width) * data$mousex)
  data$mousey = ((1080/data$display.height) * data$mousey)
  
  
  # Index participants:
  data['ID'] <- vec_rep_each(1:(nrow(data)/120), 120)
  data_orig <- data
  # Code Action Direction:
  data['Action_Dir'] <- (data$blockcode[] == 'block_R1'| data$blockcode[] == 'block_R2'| data$blockcode[] == 'block_R3') # True == Reach, False == Withdraw
  data$Action_Dir[data$Action_Dir == FALSE] <- 'W'
  data$Action_Dir[data$Action_Dir == TRUE] <- 'R'
  
  # Code Position:
  data['Action_length'] <- replicate(nrow(data), 'n')
  data$Action_length[data$blockcode == 'block_R1' | data$blockcode == 'block_W1'] <- '1'
  data$Action_length[data$blockcode == 'block_R2' | data$blockcode == 'block_W2'] <- '2'
  data$Action_length[data$blockcode == 'block_R3' | data$blockcode == 'block_W3'] <- '3'
  
  # Group audio_condition_select:
  data['Sound'] <- (data$audio_condition_select[] == 1| data$audio_condition_select[] == 3|data$audio_condition_select[] == 5) # True == -100ms, False == +100ms
  data$Sound[data$Sound == TRUE] <- 'S1'
  data$Sound[data$Sound == FALSE] <- 'S2'
  
  # Dataframe for disappearance positions:
  position_label <- c('R1', 'R2', 'R3', 'W1', 'W2', 'W3')
  disappearance_x <- c(899, 879, 864, 1117, 1153, 1177)
  disappearance_y <- c(595, 577, 568, 634, 635, 646)
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
  
  data <- data.frame(ID = data$ID, RT = data$response_time, Action_Dir = data$Action_Dir, Action_length = data$Action_length, Sound = data$Sound, x_disp = data$x_disp, y_disp = data$y_disp)
  
  # Exclude Participants by displacement:
  excl_by_participant <- data
  excl_by_participant <- excl_by_participant %>% 
    group_by(ID) %>% 
    summarize(x_disp = mean(x_disp),
              y_disp = mean(y_disp),
              RT = mean(RT))
  
  # Exclude trials by Euclidian displacement:
  data['eucl_disp'] = sqrt(data$x_disp^2 + data$y_disp^2)
  excl_disp_3SD <- median(data$eucl_disp) + 3*sd(data$eucl_disp)
  data <- filter(data, eucl_disp <= excl_disp_3SD)
  excl_by_participant['eucl_disp'] = sqrt(excl_by_participant$x_disp^2 + excl_by_participant$y_disp^2)
  excl_by_participant <- filter(excl_by_participant,eucl_disp < 96)
  # Exclude participants by RT:
  excl_by_participant <- filter(excl_by_participant, RT > 200 &RT < 4500) # Response after 4500 means majority of responses were at 5000ms
  data <- filter(data, ID %in% excl_by_participant$ID)
  # Exclude trials by RT:
  excl_RT_3SD <- median(data$RT) + 3*sd(data$RT)
  data <- filter(data, RT > 200 & RT < excl_RT_3SD)
  

## Summarize data for satistical treatment 
  sum_data <- data %>% 
    group_by(ID, Action_Dir, Action_length, Sound) %>% 
    summarize(x_disp = mean(x_disp),
              y_disp = mean(y_disp))


  # ANOVA model for x-axis:
  x_disp_model <- ezANOVA(data=sum_data, 
                             dv=x_disp, 
                             wid=.(ID), 
                             within=.(Action_Dir, Action_length, Sound),
                             type = 3,
                             detailed = TRUE)
  

  aovEffectSize(x_disp_model, effectSize = "pes")
  
  # ANOVA model for y-axis:
  y_disp_model <- ezANOVA(data=sum_data, 
                          dv=y_disp, 
                          wid=.(ID), 
                          within=.(Action_Dir, Action_length, Sound),
                          type = 3,
                          detailed = TRUE)
  
  aovEffectSize(y_disp_model, effectSize = "pes")
  
  
  aovEffectSize(x_disp_model, effectSize = "pes")
  
  
  # Demand Effect Preprocessing:
  data_orig <- filter(data_orig, ID %in% data$ID)
  Questionaire_data <- filter(dataset, trialcode == 'debrief2')
  Questionaire_data <- filter(Questionaire_data, time %in%  data_orig$time)
  Questionaire_data['ID'] <- data_orig$ID[data_orig$trial_number == 1]
  
  demand_scores <- read.csv("Experiment_1/data_Experiment_1b_demand_scores.csv")
  demand_scores['Hyp_score'] <- 0
  demand_scores['Det_sound_score'] <- 0

  demand_scores$Hyp_score <- (demand_scores$Hyp_score1 + demand_scores$Hyp_score2)/2
  demand_scores$Sound_det_score <- (demand_scores$Det_sound_score1 + demand_scores$Det_sound_score2)/2
  demand_scores <- data.frame(ID = demand_scores$ID, Hyp_score = demand_scores$Hyp_score, Sound_det_score = demand_scores$Sound_det_score)

  sum_data_Demand <- sum_data


  sum_data_Demand <- merge(sum_data_Demand, demand_scores, by.x = "ID")
  

  
  sum_data_Demand_corr <- sum_data_Demand %>% 
    group_by(ID, Action_Dir, Sound) %>% 
    summarize(x_disp = mean(x_disp),
              Hyp_score = mean(Hyp_score),
              Det_sound_score = mean(Sound_det_score))
  
  
  # Hypothesis guessing linear model 
  hyp_linear_model_full <- lm(x_disp ~  Hyp_score + Action_Dir + Sound + Sound:Hyp_score  + (1 | ID), data = sum_data_Demand_corr) 
  summary(hyp_linear_model_full)
  
  
  sum_data_Demand_corr_diff <- data.frame(ID = filter(sum_data_Demand_corr, Sound == 'S1')$ID, Action_Dir = filter(sum_data_Demand_corr, Sound == 'S1')$Action_Dir, Hyp_score = filter(sum_data_Demand_corr, Sound == 'S1')$Hyp_score, Det_sound_score = filter(sum_data_Demand_corr, Sound == 'S1')$Det_sound_score , x_S1 = filter(sum_data_Demand_corr, Sound == 'S1')$x_disp, x_S2 =   sum_data_Demand_corr_S2 <- filter(sum_data_Demand_corr, Sound == 'S2')$x_disp)
  sum_data_Demand_corr_diff['Sound_diff_score'] <- 0
  sum_data_Demand_corr_diff$Sound_diff_score[sum_data_Demand_corr_diff$Action_Dir == 'W'] <- sum_data_Demand_corr_diff$x_S2[sum_data_Demand_corr_diff$Action_Dir == 'W'] - sum_data_Demand_corr_diff$x_S1[sum_data_Demand_corr_diff$Action_Dir == 'W']
  sum_data_Demand_corr_diff$Sound_diff_score[sum_data_Demand_corr_diff$Action_Dir == 'R'] <- -(sum_data_Demand_corr_diff$x_S2[sum_data_Demand_corr_diff$Action_Dir == 'R'] - sum_data_Demand_corr_diff$x_S1[sum_data_Demand_corr_diff$Action_Dir == 'R'])
  
  
  
  sum_data_Demand_corr_diff <- sum_data_Demand_corr_diff %>% 
    group_by(ID) %>% 
    summarize(Hyp_score = mean(Hyp_score),
              Det_sound_score = mean(Det_sound_score),
              Sound_diff_score = mean(Sound_diff_score))
  
  
  
  # Hypothesis testing correlation 
  
  cor.test(sum_data_Demand_corr_diff$Sound_diff_score, sum_data_Demand_corr_diff$Hyp_score, method= "pearson")
  
  # Hypothesis testing linear model 
  
  hyp_linear_model <- lm(Sound_diff_score ~ Hyp_score, data = sum_data_Demand_corr_diff)
  summary(hyp_linear_model)
  
  # Sound Detection score linear model 

  SD_linear_model <- lm(Sound_diff_score ~ Det_sound_score, data = sum_data_Demand_corr_diff)
  summary(SD_linear_model)
  
  # Sound Detection score correlation
  
  cor.test(sum_data_Demand_corr_diff$Sound_diff_score, sum_data_Demand_corr_diff$Det_sound_score, method=c("pearson", "kendall", "spearman"))

  
  
  ID_part_0_hyp <- filter(sum_data_Demand_corr_diff, Hyp_score == 0)
    
  sum_data_0_hyp <- filter(sum_data, ID %in% ID_part_0_hyp$ID)
    
  # ANOVA model with only participants of hypothesis guessing score of 0
  
  x_disp_model_0_hyp <- ezANOVA(data=sum_data_0_hyp, 
                          dv=x_disp, 
                          wid=.(ID), 
                          within=.(Action_Dir, Action_length, Sound),
                          type = 3,
                          detailed = TRUE)
  
  x_disp_model_0_hyp
  
  aovEffectSize(x_disp_model_0_hyp, effectSize = "pes")
  