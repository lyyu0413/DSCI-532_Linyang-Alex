# Data_cleaning.R
# Group: Lab2 Alex and Linyang
# Data: January 14, 2019

# This is the script reads the raw data and cleans the raw data

# Usage:
# Rscript src/Data_cleaning.R data/MH_survey.csv data/cleaned_data.csv

library(tidyverse)


suppressPackageStartupMessages(library(tidyverse))

# Read in command line argument
args <- commandArgs(trailingOnly = TRUE)
input_file_path <- args[1]
output_file_path <- args[2]


data <- read.csv(input_file_path)

data[is.na(data)] <- 0

data_cleaned <- data %>% 
  filter(tech_company == 'Yes') %>%
  filter(Age > 5 & Age < 100) %>%
  mutate(self_employed_score = ifelse(self_employed=='Yes'|self_employed=='yes' , 1,
                                      ifelse(self_employed=='No'|self_employed=='no', -1, 0))) %>%
  mutate(benefits_score = ifelse(benefits == 'Yes', 1,
                                 ifelse(benefits =='No', -1, 0))) %>%
  mutate(remote_work_score = ifelse(remote_work == 'Yes', 1,
                                    ifelse(remote_work == 'No', -1,0)))%>%
  mutate(leave_score = ifelse(leave == 'Yes',1,
                              ifelse(leave == 'No', -1,0))) %>%
  mutate(wellness_program_score = ifelse(wellness_program == 'Yes', 1,
                                         ifelse(wellness_program =='No', -1,0))) %>%
  mutate(work_freedom = self_employed_score + wellness_program_score + benefits_score + remote_work_score + leave_score) %>%
  
  mutate(family_history_score = ifelse(family_history == 'Yes'|family_history == 'yes', 1,
                                       ifelse(self_employed == 'No'|self_employed == 'no', -1, 0))) %>%
  
  mutate(anonymity_score = ifelse(anonymity == 'Yes', 1,
                                  ifelse(anonymity == 'No', -1, 0)))

data_cleaned$Gender <- str_replace(data_cleaned$Gender, pattern = '^m', replacement = 'M')
data_cleaned$Gender <- str_replace(data_cleaned$Gender, pattern = '^M', replacement = 'M')
data_cleaned$Gender <- str_replace(data_cleaned$Gender, pattern = '^f', replacement = 'F')
data_cleaned$Gender <- str_replace(data_cleaned$Gender, pattern = '^F', replacement = 'F')

data_cleaned <- data_cleaned %>%
  mutate(Gender = ifelse(Gender == 'Male'|Gender == 'M', 'M',
                         ifelse(Gender =='Female'|Gender = 'F', 'Others')))

data_cleaned <- data_cleaned %>%
  mutate(treatment_score = ifelse(treatment == 'Yes', 1,
                                  ifelse(treatment == 'No', -1, 0))) %>%
  mutate(work_interfere_score = ifelse(work_interfere == 'Never', 0,
                                       ifelse(work_interfere == 'Rarely', 1,
                                              ifelse(work_interfere == 'Sometimes', 2,
                                                     ifelse(work_interfere == 'often', 3, 0))))) %>%
  mutate(care_options_score = ifelse(care_options == 'Yes', 1,
                                     ifelse(care_options == 'No', -1,0))) %>%
  mutate(seek_help_score = ifelse(seek_help == 'Yes', 1,
                                  ifelse(seek_help == 'No', -1, 0))) %>%
  mutate(mental_health_interview_score = ifelse(mental_health_interview == 'Yes', 1,
                                                ifelse(mental_health_interview == 'No', -1, 0))) %>%
  mutate(phys_health_interview_score = ifelse(phys_health_interview == 'Yes', 1,
                                              ifelse(phys_health_interview == 'No', -1,0)))


cc <- gapminder %>% select(country, continent) %>% unique()
cc <- cc %>% mutate(Country = country) 
data_cleaned <- left_join(data_cleaned, cc, by='Country')

data_cleaned_select <- data_cleaned %>% 
  group_by(continent, Country) %>% 
  unique() %>%
  summarise(sum_c = n()) %>%
  filter(sum_c > 10)

data_cleaned <- data_cleaned %>% filter(Country %in% data_cleaned_select$Country)


data_cleaned <- data_cleaned %>%
  select(Age, Gender, Country, continent, self_employed, self_employed_score, benefits, benefits_score, remote_work, remote_work_score, leave, leave_score, wellness_program, wellness_program_score, work_freedom, family_history, family_history_score, anonymity, anonymity_score, treatment, treatment_score, work_interfere, work_interfere_score, care_options, care_options_score, seek_help, seek_help_score, mental_health_interview, mental_health_interview_score, phys_health_interview, phys_health_interview_score)
write.csv(data_cleaned, file = output_file_path)