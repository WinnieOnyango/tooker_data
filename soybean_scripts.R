#load soybean data from github
soy_data <- read.csv("https://raw.githubusercontent.com/WinnieOnyango/tooker_data/refs/heads/master/Soy_bean_data.csv")
write.csv(soy_data, "soy_data.csv", row.names = FALSE)

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

library(dplyr)

# 1. Define classification lists
insects <- c(
  "Aphid", "Aphids", "Armyworm", "Bean leaf beetle", "Black cutworm", "Blister beetle",
  "Brown marmorated stink bug", "Brown stink bug", "Corn rootworm", "Cucumber beetle",
  "Flea beetle", "Grasshopper", "Grasshopper beetle", "Green cloverworm", "Green cloverworn",
  "Green cloverworn caterpillar", "Green stink bug", "Japanese beetle", "Locust leafminer",
  "Mexican bean beetle", "Mexican been beetle", "Painted lady catepillar", "Potato leaf hopper",
  "Potato leafhopper", "Read headed flee beetle", "Red headed flea beetle", "Red shouldered stink bug",
  "Silver spotted skipper", "Silver spotted skipper catepillar", "Silver spotted skipper caterpillar", "Spoted cucumber beetle",
  "Silver-spotted skipper caterpillar",  "Green marmoratted stink bug" , "True armyworm", "Thisttle caterpillar",
  "Soybean aphid", "Soybean leaf miner", "Soybean leafminer", "Soybean leafminor", "Soybean looper",
  "Soybean stink bug", "Soybean thrips", "Spotted cucumber beetle", "Stink bug", "Stinkbug",
  "Tarnished plant bug", "Thisttle caterpillar", "Thistle caterpillar", "Thrip", "Thrips",
  "Velvet bean caterpillar", "Velvet bean caterpillars", "Velvetbean caterpillar",
  "White flies"
)

pathogens <- c(
  "Alfalafa mosaic virus", "Alfalfa mosaic virus", "Alfalfa mosiac virus", "Anthracnose",
  "Bacterial blight", "Bacterial leaf blight", "Bacterial leaf wilt", "Bacterial pustule",
  "Bacterial pustules", "Bean pod mottle virus", "Brown leaf spot", "Brown stem rot",
  "Cercospora", "Cercospora leaf blight", "Damping off disease", "Downy mildew",
  "Frog eye leaf spot", "Frogeye leaf spot", "Frogeye leafspot", "Pathogen", "Phompsis pod rot/stem blight",
  "Phyllostica leaf spot", "Phyllosticta leaf spot", "Phytophtora root and stem root", "Phytophthora root & stem root", "Phytophtora root and stem rot" , "Phytophthora root and stem rot",
  "Powdery mildew", "Septoria brown spot",
  "Phomopsis pod rot & stem blight", 
  "Septoria leaf blight", "Septoria leaf spot", "Soybean bacterial pustule",
  "Soybean blight", "Soybean brown spot", "Soybean leafminer", "Soybean mosaic virus",
  "Soybean pod and stem blight", "Soybean purple seed stain", "Soybean sudden death sundrome",
  "Soybean sudden death syndrome", "Soybean sudden deathsyndrome", "Soybean vein necrosis",
  "Soybean vein necrosis virus", "Stem canker", "Sudden death syndrome", "Target spot",
  "White mold", "White mould", "Anthracnose stem blight", "Soybean mosaic", "Purple seed stain",         
  "Bean pod mottle", "Diaporthe/Phomopsis complex", "Pythium seedling blight", "Diaporthe/phomopsis complex"  
)

mammals <- c("Deer", "Ground hog", "Groundhog")
slugs   <- c("Slug")
snails  <- c("Snail")
arachnids <- c("Spider mite", "Two spotted spider mite","Two spotted spider mites")

# 2. Clean pest_pathogen_sp (optional: standardize formatting)
soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = trimws(pest_pathogen_sp))  # remove extra spaces

# 3. Assign each to a group
soy_data <- soy_data %>%
  mutate(pest_pathogen = case_when(
    pest_pathogen_sp %in% insects    ~ "Insect",
    pest_pathogen_sp %in% pathogens  ~ "Pathogen",
    pest_pathogen_sp %in% mammals    ~ "Mammal",
    pest_pathogen_sp %in% slugs      ~ "Slug",
    pest_pathogen_sp %in% snails     ~ "Snail",
    pest_pathogen_sp %in% arachnids  ~ "Arachnid",
    TRUE                             ~ "Unknown"
  ))
#data cleaning
soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = str_trim(pest_pathogen_sp),
         pest_pathogen_sp = case_when(
           pest_pathogen_sp %in% c("Alfalafa mosaic virus", "Alfalfa mosiac virus") ~ "Alfalfa mosaic virus",
           pest_pathogen_sp == "Anthracnose" ~ "Anthracnose stem blight",
           pest_pathogen_sp %in% c("Bacterial leaf blight", "Bacterial leaf wilt", "Bacterial  blight") ~ "Bacterial blight",
           pest_pathogen_sp %in% c("Brown leaf spot", "Septoria leaf blight", "Septoria leaf spot", "Soybean brown spot") ~ "Septoria brown spot",
           pest_pathogen_sp %in% c("Bean pod mottle virus", "Bean pod mottle") ~ "Bean pod mottle",
           pest_pathogen_sp == "Damping off disease" ~ "Pythium seedling blight",
           pest_pathogen_sp %in% c("Bacterial pustules", "Bacterial pustule") ~ "Soybean bacterial pustule",
           pest_pathogen_sp %in% c("Frogeye leaf spot", "Frogeye leafspot", "Frog eye leaf spot") ~ "Frogeye leaf spot",
           pest_pathogen_sp %in% c("Cercospora", "Soybean blight") ~ "Cercospora leaf blight",
           pest_pathogen_sp %in% c("Phompsis pod rot/stem blight", "Phomopsis pod rot & stem blight", "Soybean pod and stem blight") ~ "Diaporthe/Phomopsis complex",
           pest_pathogen_sp == "Phyllostica leaf spot" ~ "Phyllosticta leaf spot",
           pest_pathogen_sp %in% c("Phytophtora root and stem root", "Phytophthora root & stem root", "Phytophtora root and stem rot") ~ "Phytophthora root and stem rot",
           pest_pathogen_sp == "Soybean bacterial pustule" ~ "Bacterial pustule",
           pest_pathogen_sp == "Soybean mosaic virus" ~ "Soybean mosaic",
           pest_pathogen_sp == "Soybean purple seed stain" ~ "Purple seed stain",
           pest_pathogen_sp %in% c("Soybean sudden deathsyndrome", "Soybean sudden death sundrome", "Sudden death syndrome", "Soybean sudden death syndrome") ~ "Sudden death syndrome",
           pest_pathogen_sp == "White mould" ~ "White mold",
           pest_pathogen_sp == "Soybean vein necrosis virus" ~ "Soybean vein necrosis",
           TRUE ~ pest_pathogen_sp
         ))

soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = case_when( 
    pest_pathogen_sp == "Ground hog" ~ "Groundhog",
    pest_pathogen_sp == "Snail" ~ "Snail",
    pest_pathogen_sp == "Slug" ~ "Slug",
    TRUE~ pest_pathogen_sp))
soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = case_when(
    pest_pathogen_sp %in% c("Aphid", "Aphids") ~ "Soybean aphid",
    pest_pathogen_sp == "Armyworm" ~ "True armyworm",
    pest_pathogen_sp == "Cucumber beetle" ~ "Spoted cucumber beetle",
    pest_pathogen_sp == "Flea beetle" ~ "Red headed flea beetle",
    pest_pathogen_sp %in% c("Green cloverworn", "Green cloverworn caterpillar") ~"Green cloverworm",
    pest_pathogen_sp == "Locust leafminer" ~ "Soybean leafminer",
    pest_pathogen_sp =="Mexican been beetle" ~"Mexican bean beetle",
    pest_pathogen_sp == "Painted lady catepillar" ~ "Thisttle caterpillar",
    pest_pathogen_sp == "Potato leaf hopper" ~ "Potato leafhopper",
    pest_pathogen_sp %in% c("Red headed flee beetle", "Read headed flee beetle") ~ "Red headed flea beetle",
    pest_pathogen_sp %in% c("Silver spotted skipper","Silver spotted skipper catepillar", "Silver spotted skipper caterpillar") ~ "Silver-spotted skipper caterpillar",
    pest_pathogen_sp %in% c("Soybean leaf miner", "Soybean leafminor") ~ "Soybean leafminer",
    pest_pathogen_sp == "Spoted cucumber beetle" ~"Spotted cucumber beetle",
    pest_pathogen_sp %in% c("Stink bug", "Stinkbug") ~ "Green marmoratted stink bug",
    pest_pathogen_sp %in% c("Thrips", "Thrip") ~ "Soybean thrips",
    pest_pathogen_sp == "Thisttle caterpillar" ~ "Thistle caterpillar",
    pest_pathogen_sp %in% c("Velvet bean caterpillar", "Velvet bean caterpillars") ~ "Velvetbean caterpillar",
    TRUE ~ pest_pathogen_sp
  ))

soy_data <- soy_data %>%
  mutate(pest_pathogen_sp = case_when(
    pest_pathogen_sp %in% c("Spider mite", "Two spotted spider mites") ~ "Two spotted spider mite",
    TRUE ~ pest_pathogen_sp
    
  ))
setdiff(unique(soy_data$pest_pathogen_sp), c(insects, pathogens, mammals, slugs, snails, arachnids))


#data cleaning
library(dplyr)

# Vector of misclassified pathogens

#all unique entries in pest_pathogen column
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
arachnid_dt <- soy_data %>%
  filter(pest_pathogen == "Arachnid")


#usethis::use_git()
#saving files to:"C:/Users/Winnie Onyango/Documents/Tooker/tooker_data" 
write.csv(insect_data, "insect_data.csv", row.names = FALSE)
write.csv(pathogen_dt, "pathogen_data.csv", row.names = FALSE)
write.csv(slug_dt, "slug_data.csv", row.names = FALSE)
write.csv(snail_dt, "snail_data.csv", row.names = FALSE)
write.csv(mammal_dt, "mammal_data.csv", row.names = FALSE)
write.csv(arachnid_dt, "arachnid_data.csv", row.names = FALSE)
#making changes in grouping of pes_pathogen_sp

#usethis::use_git_config(user.name = "Winnie Onyango", user.email = "wao5041@psu.edu")

#filter all unique insect sp entries
insect_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Insect"]))))
pathogen_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Pathogen"]))))
slug_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Slug"]))))
snail_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Snail"]))))
mammal_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Mammal"]))))
arachnid_df <- as.data.frame(table(sort(unique(soy_data$pest_pathogen_sp[soy_data$pest_pathogen == "Arachnid"]))))
write.csv(pathogen_df,"pathogen_df.csv", row.names = FALSE)
write.csv(insect_df,"insect_df.csv", row.names = FALSE)

