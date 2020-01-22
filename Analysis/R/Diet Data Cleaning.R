#This R script is for data cleaning of diet data collected during summer 2018 for my undergrad thesis.
#See .Rmd file for explanation of calculations and manipulations.


#Load Required Packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#Import Data
ffg <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "FFG")
sizes <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Size")
Diets <- readxl::read_xlsx("./Data/2018 Diets.xlsx")
bugs <- readxl::read_xlsx("../Bugs/Data/2017-18 Bugs.xlsx")

# Fill missing family's with order
Diets$Family[is.na(Diets$Family)] <- Diets$Order[is.na(Diets$Family)]

# Merge diets, sizes and FFG's
Diets <- merge(Diets, sizes, by.x = "Family", by.y = "Taxon", all.x = TRUE) %>% 
  merge(., ffg, by.x = "Family", by.y = "Taxon", all.x = TRUE)

# Aggregate by rep
Diets.fish <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size, Rep) %>%
  summarise_at("Count", funs(sum))

# Aggregate by reach
Diets.reach <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size) %>%
  summarise_at("Count", funs(sum))






