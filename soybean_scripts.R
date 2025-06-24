#load soybean data from github
soy_data <- read.csv("https://raw.githubusercontent.com/WinnieOnyango/Tooker_lab/refs/heads/main/Soy_bean_data.csv")

#data cu-ration

library(tidyverse)
library(dplyr)
library(stringr)
#fixing all the spaces, capitaizing only the 1st letter of the 1st word of a sentence
soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = pest_pathogen_sp %>%
           str_trim() %>%                        # remove leading/trailing spaces
           str_squish() %>%                      # collapse multiple spaces into one
           str_replace_all("-", " ") %>%         # replace hyphens with spaces
           str_to_lower() %>%                    # make all lowercase
           str_replace("^\\w", toupper),         # capitalize first letter only
  pest_pathogen = pest_pathogen %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace("^\\w", toupper),
  county = county %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace("^\\w", toupper)
    
         )

unique(soy_data$pest_pathogen_sp)
#filter only insect data
insect_data <- soy_data %>%
  filter(pest_pathogen == "Insect")
#filter only pathogen data
pathogen_dt <- soy_data %>% 
  filter(pest_pathogen == "Pathogen")
#filter slug data
slug_dt <- soy_data %>%
  filter(pest_pathogen == "Slug")
#filter snail dta
snail_dt <- soy_data %>%
  filter(pest_pathogen == "Snail")
#filter mammal data
mammal_dt <- soy_data %>%
  filter(pest_pathogen == "Mammal")

usethis::use_git()


