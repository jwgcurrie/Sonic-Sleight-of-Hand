## Consequential sound induces illusory distortions in the perception and prediction of robot motion
## Experiment 2a and 2b
## Preprocessing, Analysis and Visualisation
## 24/05/2023

# Mr Joel Currie
# University of Aberdeen
# Scotland



di <- getwd()
setwd(di)

  #rm(list = ls())
  
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

  BR_2049_colors <- c("#f78b04", "#027f93", "#a30502", "#2b1718", "#153a42")
  BR_2049_colors2 <- c("#a30502", "#2b1718")
  
  
  RT_analysis_switch = TRUE
  EXPT <- '2a'
  

  dataset_expt2a <- "Experiment_2/data_Experiment_2a.csv"
  dataset_expt2b <- "Experiment_2/data_Experiment_2b.csv"
  
  if(EXPT == '2a') 
  {dataset <- read.csv(dataset_expt2a)
  }else if(EXPT == '2b')
    {dataset <- read.csv(dataset_expt2b)}
  
  delims_list <- filter(dataset, trialcode == 'debrief2')
  data = filter(dataset, time %in% delims_list$time)
  
  # Find measurements:
  
  data <- filter(data, trialcode == 'Probe_Measure')
  
  
  
  # Exclude practice trials:
  data <- filter(data, trial_number > 0)
  
  
  
  # Index participants:
  data['ID'] <- vec_rep_each(1:(nrow(data)/288), 288)
  
  
  # Code Action Direction:
  data['Action_Dir'] <- (data$blockcode[] == 'block_R1S1'|data$blockcode[] == 'block_R1S2'| data$blockcode[] == 'block_R2S1'|data$blockcode[] == 'block_R2S2'| data$blockcode[] == 'block_R3S1' |data$blockcode[] == 'block_R3S2') # True == Reach, False == Withdraw
  data$Action_Dir[data$Action_Dir == FALSE] <- 'W'
  data$Action_Dir[data$Action_Dir == TRUE] <- 'R'
  
  
  # Code Position:
  data['Action_pos'] <- replicate(nrow(data), 'n')
  data$Action_pos[data$blockcode == 'block_R1S1' | data$blockcode == 'block_W1S1'|data$blockcode == 'block_R1S2' | data$blockcode == 'block_W1S2'] <- 'C'
  data$Action_pos[data$blockcode == 'block_R2S1' | data$blockcode == 'block_W2S1'|data$blockcode == 'block_R2S2' | data$blockcode == 'block_W2S2'] <- 'M'
  data$Action_pos[data$blockcode == 'block_R3S1' | data$blockcode == 'block_W3S1'|data$blockcode == 'block_R3S2' | data$blockcode == 'block_W3S2'] <- 'O'
  
  # Group audio_condition_select:
  data['Sound'] <- (data$audio_condition_select[] == 1| data$audio_condition_select[] == 3|data$audio_condition_select[] == 5) # True == -100ms, False == +100ms
  data$Sound[data$Sound == TRUE] <- '- 100 ms'
  data$Sound[data$Sound == FALSE] <- '+ 100 ms'
  
  data['Probe_response'] <- (data$trial.Probe_Measure.response[] == 57)
  data$Probe_response[data$Probe_response == TRUE] <- 1
  

  
  tot_sum_data_conditions <- data %>% 
    group_by(ID, Action_Dir, Sound, probe_displacement) %>% 
    dplyr::summarize(sum_resp = mean(Probe_response))
  
  
  diff_scores_conditions <- data.frame(ID = tot_sum_data_conditions$ID,Action_Dir = tot_sum_data_conditions$Action_Dir, Sound = tot_sum_data_conditions$Sound, Probe_disp = tot_sum_data_conditions$probe_displacement, diff_scores = tot_sum_data_conditions$sum_resp)
  diff_scores_conditions['same_score'] <- 1 - diff_scores_conditions$diff_scores
  diff_scores_conditions['probe_weights'] <- diff_scores_conditions$Probe_disp * diff_scores_conditions$same_score
  diff_scores_conditions$prop_probe_weights <- (diff_scores_conditions$probe_weights / mean(diff_scores_conditions$same_score))
  

  
  # Prepare data for ANOVA 
  
  tot_sum_data_conditions <- data %>% 
    group_by(ID, Action_Dir, Action_pos, Sound, probe_displacement) %>% 
    dplyr::summarize(sum_resp = n())
  
  # Weighted means calculation
  diff_scores_conditions <- data.frame(ID = tot_sum_data_conditions$ID,Action_Dir = tot_sum_data_conditions$Action_Dir, Action_pos = tot_sum_data_conditions$Action_pos, Sound = tot_sum_data_conditions$Sound, Probe_disp = tot_sum_data_conditions$probe_displacement, diff_scores = tot_sum_data_conditions$sum_resp)
  diff_scores_conditions['same_score'] <- 1 - diff_scores_conditions$diff_scores
  diff_scores_conditions['probe_weights'] <- diff_scores_conditions$Probe_disp * diff_scores_conditions$same_score
  diff_scores_conditions$prop_probe_weights <- (diff_scores_conditions$probe_weights/3)
  
  

  data['Probe_dist'] <- 0
  data['Probe_dir'] <- 0
  
  data$Probe_dist[data$probe_displacement == -3 | data$probe_displacement == 3] <- 3
  data$Probe_dist[data$probe_displacement == -1 | data$probe_displacement == 1] <- 1
  data$Probe_dir[data$probe_displacement == -1 | data$probe_displacement == -3] <- '-'
  data$Probe_dir[data$probe_displacement == 1 | data$probe_displacement == 3] <- '+'
  
  
  
  ANOVA_w_probe <- data %>% 
    group_by(ID, Probe_dist, Probe_dir, Action_Dir, Sound, Action_pos) %>% 
    summarize(Y = mean(Probe_response))

  
  ## Exclusion:
  # Exclude Participants with greater than 10% 'different' score for inner than outer probes:
  
  data_incl_parts <- ANOVA_w_probe %>%
    group_by(ID) %>%
    summarize(Y = mean(Y))
  data_incl_parts['Include'] <- 0
  data_incl_parts$Include[data_incl_parts$Y > 0.1 & data_incl_parts$Y < 0.9] <- TRUE
  
  participants_include <- data.frame(ID = data_incl_parts$ID[data_incl_parts$Include == 1])
  
  data <- filter(data,ID %in% participants_include$ID)

  
  data_incl_parts <- ANOVA_w_probe %>%
    group_by(ID, Probe_dist) %>%
    summarize(Y = mean(Y))
  
  excl_var_outer_inner_probes = 0.1
  
  data_incl_parts_outer <- filter(data_incl_parts, Probe_dist == 3) 
  data_inlc_parts_innter <- filter(data_incl_parts, Probe_dist == 1)
  data_incl_outer_inner <- data.frame(ID = data_incl_parts_outer$ID, probes_outer = data_incl_parts_outer$Y, probes_innner = data_inlc_parts_innter$Y)
  data_incl_outer_inner['Include'] <- 0
  data_incl_outer_inner$Include[data_incl_outer_inner$probes_outer > data_incl_outer_inner$probes_innner + excl_var_outer_inner_probes & data_incl_outer_inner$probes_outer != 1] <- TRUE
  
  participants_include <- data.frame(ID = data_incl_outer_inner$ID[data_incl_outer_inner$Include == 1])

  data <- filter(data,ID %in% participants_include$ID)
  
  
  # Probe_plot
  diff_scores <- data %>% 
    group_by(ID, Sound, probe_displacement, Action_Dir) %>% 
    dplyr::summarize(m_response = 1 - mean(Probe_response),
                     SD = sd(Probe_response),
                     CI_upper = m_response + 0.95 * SD/sqrt(nrow(participants_include)),
                     CI_lower = m_response - 0.95 * SD/sqrt(nrow(participants_include)))
  
  
  RM_error <- diff_scores %>% 
    group_by(ID, Sound, Action_Dir) %>% 
    dplyr::summarize(m_response = m_response,
                     SD = sd(m_response))
  
  RM_error_S1 <- filter(RM_error, Sound == '- 100 ms')
  RM_error_S2 <- filter(RM_error, Sound == '+ 100 ms')
  
  RM_error <- data.frame(ID = RM_error_S1$ID, S1 = RM_error_S1$m_response, S2 = RM_error_S2$m_response)
  RM_error['Sound_diff'] <- RM_error$S2 - RM_error$S1
  
  m_RM <- mean(RM_error$Sound_diff)
  SD_RM <- sd(RM_error$Sound_diff)
  RM_error <- sd(RM_error$Sound_diff)/sqrt(nrow(delims_list))
  
  
  diff_scores <- data %>% 
    group_by(Sound, probe_displacement, Action_Dir) %>% 
    dplyr::summarize(m_response = 1 - mean(Probe_response),
                     SD = sd(Probe_response),
                     CI_upper = m_response + 0.95 * RM_error,
                     CI_lower = m_response - 0.95 * RM_error)
  
  
  R_plot <- ggplot(data=filter(diff_scores, Action_Dir == 'R'), aes(x=probe_displacement, y=m_response, group=Sound))+
    scale_x_continuous(breaks = seq(-3, 3, 2)) + 
    geom_line(aes(color = Sound),linetype = "dashed")+
    geom_point(aes(color = Sound,shape=Sound))+
    scale_linetype_manual(values=c("twodash", "dashed"))+ 
    labs(title="(e) Action Direction - Reach",x="Probe Stimulus", y = "Proportion of 'Same' responses (%)") + 
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, color = Sound), width=.2,
                  position=position_dodge(0.05)) + 
    scale_color_manual(values = BR_2049_colors)
  
  
  W_plot <-ggplot(data=filter(diff_scores, Action_Dir == 'W'), aes(x=probe_displacement, y=m_response, group=Sound))+
    scale_x_continuous(breaks = seq(-3, 3, 2)) + 
    geom_line(aes(color = Sound),linetype = "dashed", width = 0.5)+
    geom_point(aes(color = Sound,shape=Sound))+
    scale_linetype_manual(values=c("twodash", "dashed")) + 
    labs(title="(f) Action Direction - Withdraw",x="Probe Stimulus", y = "Proportion of 'Same' responses (%)") +
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, color = Sound), width=.2,
                  position=position_dodge(0.05)) +
    scale_color_manual(values = BR_2049_colors) 
    
  
 probe_plots <- R_plot / W_plot
  
  
  
  ## calculate proportional probe weights:
  RM_by_condition <- data %>% 
    group_by(ID, Action_Dir,Action_pos, Sound, Probe_dist, Probe_dir) %>%
    summarize(m_resp = 1 - mean(Probe_response))
  
  RM_by_condition['Weighted_response'] <- RM_by_condition$m_resp

  RM_by_condition$Weighted_response[RM_by_condition$Probe_dist == 3] <- RM_by_condition$m_resp[RM_by_condition$Probe_dist == 3] * RM_by_condition$Probe_dist[RM_by_condition$Probe_dist == 3]
  RM_by_condition$Weighted_response[RM_by_condition$Probe_dir == '-'] <- -RM_by_condition$Weighted_response[RM_by_condition$Probe_dir == '-']
  

  
  RM_by_condition_weighted <- RM_by_condition %>%
    group_by(ID, Action_Dir, Sound, Action_pos) %>%
    summarize(RM_weighted_means = sum(Weighted_response)/sum(m_resp))
  
  Error_RM_by_condition_weighted <- RM_by_condition %>%
    group_by(ID, Action_Dir, Sound) %>%
    summarize(RM_weighted_means = sum(Weighted_response)/sum(m_resp))
  
    Error_RM_by_condition_weighted_S1 <- filter(Error_RM_by_condition_weighted, Sound == '- 100 ms')
    Error_RM_by_condition_weighted_S2 <- filter(Error_RM_by_condition_weighted, Sound == '+ 100 ms')
    Error_RM_by_condition_weighted <- data.frame(Action_Dir = Error_RM_by_condition_weighted_S1$Action_Dir, RM_weighted_means = Error_RM_by_condition_weighted_S2$RM_weighted_means - Error_RM_by_condition_weighted_S1$RM_weighted_means)
    
    
    m_RM <- mean(Error_RM_by_condition_weighted$RM_weighted_means)
    SD_RM <- sd(Error_RM_by_condition_weighted$RM_weighted_means)
    RM_error <- sd(Error_RM_by_condition_weighted$RM_weighted_means)/sqrt(nrow(delims_list))
    
    

    
  plot_RM_by_condition_weighted <- RM_by_condition %>%
    group_by(Action_Dir, Sound) %>%
    summarize(RM_weighted_means = sum(Weighted_response)/sum(m_resp),
              CI2_upper = RM_weighted_means + 0.95 * RM_error,
              CI2_lower = RM_weighted_means - 0.95 * RM_error)
  



  plot_RM_by_condition_weighted$Action_Dir[plot_RM_by_condition_weighted$Action_Dir == 'R'] <- 'Reach'
  plot_RM_by_condition_weighted$Action_Dir[plot_RM_by_condition_weighted$Action_Dir == 'W'] <- 'Withdraw'

plot_RM_weighted <- ggplot(plot_RM_by_condition_weighted, aes(x=Action_Dir, y=RM_weighted_means, fill=Sound)) + 
    geom_bar(stat="identity", width=.6, position=position_dodge(width=.6)) +
    ggtitle("(g) Overestimation over probe positions") + 
    xlab("Action Direction") + ylab("Motion overestimation (weighted means)") + 
    geom_errorbar(aes(ymin=CI2_lower, ymax=CI2_upper), width=.3,
                position=position_dodge(.6)) + 
                scale_fill_manual(values = BR_2049_colors)

# Raincloud visualisation:
data_raincloud_RM_weighted <- RM_by_condition_weighted %>% group_by(ID, Action_Dir, Sound) %>%
summarize(RM_weighted_means = mean(RM_weighted_means))



data_raincloud_RM_weighted <- data.frame(S1 = filter(data_raincloud_RM_weighted, Sound == '- 100 ms')$RM_weighted_means, S2 = filter(data_raincloud_RM_weighted, Sound == '+ 100 ms')$RM_weighted_means, Action_Dir = filter(data_raincloud_RM_weighted, Sound == '- 100 ms')$Action_Dir)


data_raincloud_RM_weighted['Sound_diff'] <- data_raincloud_RM_weighted$S2 - data_raincloud_RM_weighted$S1
data_raincloud_RM_weighted$Action_Dir[data_raincloud_RM_weighted$Action_Dir == 'W'] <- 'Withdraw'
data_raincloud_RM_weighted$Action_Dir[data_raincloud_RM_weighted$Action_Dir == 'R'] <- 'Reach'



plot_raincloud_RM_weighted <- ggplot(data_raincloud_RM_weighted, aes(x = Action_Dir, y=Sound_diff, alpha = 0.6, fill = Action_Dir)) + 
    ggdist::stat_halfeye(
      # custom bandwidth 
      adjust = 0.5,
      # move geom to right
      justification = -0,
      # remove slab interval 
      .width = 0,
      point_colour = NA) +
      geom_jitter(width = .05, alpha = .3) + 
      #+ geom_smooth(span = 1) + 
      ggtitle("(h) Weighted response distribution") + 
      xlab("Proportion of sample") + ylab("Motion overestimation (weighted means)") + 
      theme(legend.position = "none") + 
      geom_hline(yintercept=0, linetype="dashed") +
      scale_fill_manual(values = BR_2049_colors2) 

  
    # 
    ANOVA_w_probe = filter(ANOVA_w_probe, ID %in% participants_include$ID)
  
  
    # Weighted means ANOVA analysis
    model_weighted_means <- ezANOVA(data=RM_by_condition_weighted, 
                             dv=RM_weighted_means, 
                             wid=.(ID), 
                             within=.(Action_Dir, Action_pos, Sound),
                             type = 3,
                             detailed = TRUE)
  
  
    aovEffectSize(model_weighted_means, effectSize = "pes")
  
    
  ## final visualitaiton
  pub_plots <- (probe_plots | plot_RM_weighted) + plot_raincloud_RM_weighted
  

  
  if(EXPT == '2a')
    {pub_plots + plot_annotation('Experiment 2a', caption = '', 
                                 theme = theme(plot.title = element_text(size = 18))) 

    }else if(EXPT == '2b')
    {
      pub_plots + plot_annotation('Experiment 2b', caption = '', 
                                  theme = theme(plot.title = element_text(size = 18))) 

    }

  


  