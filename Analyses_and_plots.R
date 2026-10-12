library(afex)
library(emmeans)
library(readr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library (patchwork)

## Load and wrangle data

Mean_Frames <- read_csv("Mean_Frames_Familiar_Faces_Filtered.csv")
Mean_Frames_long <- melt(Mean_Frames, 
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("Participant", "Age"),
                         # The source columns
                         measure.vars=c("Unfamiliar_Upright", "Unfamiliar_Inverted", 
                                        "Familiar_Upright", "Familiar_Inverted", 
                                        "Self_Upright", "Self_Inverted"),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Condition",
                         value.name="Mean_N_Frames"               
                         
)

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Orientation = case_when(Condition == 'Unfamiliar_Upright' ~ 'Upright',
                                 Condition == 'Familiar_Upright' ~ 'Upright',
                                 Condition == 'Self_Upright' ~ 'Upright',
                                 TRUE ~ 'Inverted'))

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Familiarity = case_when(Condition == 'Unfamiliar_Upright' ~ 'Unfamiliar',
                                 Condition == 'Unfamiliar_Inverted' ~ 'Unfamiliar',
                                 Condition == 'Familiar_Upright' ~ 'Familiar',
                                 Condition == 'Familiar_Inverted' ~ 'Familiar',
                                 TRUE ~ 'Self'))

Mean_Frames_long$Mean_time <- Mean_Frames_long$Mean_N_Frames*(1000/120)
Mean_Frames_long$Age_c <- Mean_Frames_long$Age - mean(Mean_Frames_long$Age)

## Reaction times 

Mean_RTs <- read_csv("Mean_Reaction_Times_Familiar_Faces_Filtered.csv")
Mean_RTs_long <- melt(Mean_RTs, 
                      # ID variables - all the variables to keep but not split apart on
                      id.vars=c("Participant", "Age"),
                      # The source columns
                      measure.vars=c("Unfamiliar_Upright", "Unfamiliar_Inverted", 
                                     "Familiar_Upright", "Familiar_Inverted", 
                                     "Self_Upright", "Self_Inverted"),
                      # Name of the destination column that will identify the original
                      # column that the measurement came from
                      variable.name="Condition",
                      value.name="Mean_RT_secs"               
                      
)

Mean_RTs_long <- Mean_RTs_long %>%
  mutate(Orientation = case_when(Condition == 'Unfamiliar_Upright' ~ 'Upright',
                                 Condition == 'Familiar_Upright' ~ 'Upright',
                                 Condition == 'Self_Upright' ~ 'Upright',
                                 TRUE ~ 'Inverted'))

Mean_RTs_long <- Mean_RTs_long %>%
  mutate(Familiarity = case_when(Condition == 'Unfamiliar_Upright' ~ 'Unfamiliar',
                                 Condition == 'Unfamiliar_Inverted' ~ 'Unfamiliar',
                                 Condition == 'Familiar_Upright' ~ 'Familiar',
                                 Condition == 'Familiar_Inverted' ~ 'Familiar',
                                 TRUE ~ 'Self'))

Mean_RTs_long$Mean_RT_ms <- Mean_RTs_long$Mean_RT_secs*(1000)
Mean_RTs_long$Age_c <- Mean_RTs_long$Age - mean(Mean_RTs_long$Age)


##  Do ANOVA analysis of ratings - don't GG correct and use partial eta squared
RecognitionTimes_ANOVA <- aov_ez("Participant", "Mean_time", Mean_Frames_long, 
                                 within=c("Orientation", "Familiarity"),
                                 factorize = FALSE, covariate = "Age_c",  
                                 anova_table = list(correction = "none", es = "pes")) 


EM_Means_RecognitionTimes_Interaction <- emmeans (RecognitionTimes_ANOVA, ~ Orientation|Familiarity) # Get EM Means for interaction
EM_Means_RecognitionTimes_Familiarity <- emmeans (RecognitionTimes_ANOVA, ~ Familiarity) # Get EM Means

posthoc_RecognitionTimes_Familiarity <- pairs(EM_Means_RecognitionTimes_Familiarity, adjust="bon")


RTs_ANOVA <- aov_ez("Participant", "Mean_RT_ms", Mean_RTs_long, within=c("Orientation", "Familiarity"),
                    factorize = FALSE, covariate = "Age_c", anova_table = list(correction = "none", es = "pes")) # Do ANOVA - don't GG correct and use partial eta squared

EM_Means_RTs <- emmeans (RTs_ANOVA, ~ Orientation|Familiarity) # Get EM Means

##  Use afex_plot to plot these simply
recTimesPlot <-  afex_plot(RecognitionTimes_ANOVA,  ~ Familiarity,  ~ Orientation, error = "within")

RTsPlot <-  afex_plot(RTs_ANOVA,  ~ Familiarity,  ~ Orientation, error = "within")

recTimesPlot + labs(x = 'Familiarity', y = 'Mean Recognition Time (ms)') + theme_classic()

RTsPlot + labs(x = 'Familiarity', y = 'Mean Reaction Time (ms)') + theme_classic()

