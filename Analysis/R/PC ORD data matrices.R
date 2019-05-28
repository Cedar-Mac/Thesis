# Make some long style matrix with species as cols and sites as rows.  
# Make matrix for family, genera, family difference, genera difference, and diets

# Load packages
x <- c("tidyverse", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

# Read in data
bugs <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Cleaned")

# Calculate densities
bugs$Density <- bugs$Count / bugs$PercentSub / .09

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

## BY GENERA

bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Taxon) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3

bugs.agg <- spread(bugs.agg, key = "Taxon", value = "Density") %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  gather(key = "Taxon", value = "Density", 4:ncol(.))

bugs.reach.diff <- bugs.agg %>% select(-c("Density", "CollDate")) %>% unique()

bugs.reach.diff$Diff <- bugs.agg$Density[bugs.agg$CollDate == "2018"] -     
  bugs.agg$Density[bugs.agg$CollDate == "2017"]

bugs.diff <- bugs.reach.diff %>% 
  spread(key = "Taxon", value = "Diff") %>%
  mutate_if(is.numeric , replace_na, replace = 0)

## BY FAMILY

bugs.family <- bugs %>%
  group_by(CollDate, Stream, Treatment, Family) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.family$Density <- bugs.family$Density / 3

bugs.family <- spread(bugs.family, key = "Family", value = "Density") %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  gather(key = "Family", value = "Density", 4:ncol(.))

bugs.fam.diff <- bugs.family %>% select(-c("Density", "CollDate")) %>% unique()

bugs.fam.diff$Diff <- bugs.family$Density[bugs.family$CollDate == "2018"] -     
  bugs.family$Density[bugs.family$CollDate == "2017"]

bugs.fam.diff <- bugs.fam.diff %>% spread(key = "Family", value = "Diff") %>%
  mutate_if(is.numeric , replace_na, replace = 0)

bugs.family <- spread(bugs.family, key = "Family", value = "Density") %>% 
  mutate_if(is.numeric , replace_na, replace = 0)

## For DIETS

Diets <- readxl::read_xlsx("../Diets/Data/2018 Diets.xlsx")
Diets$Family[is.na(Diets$Family)] <- Diets$Order[is.na(Diets$Family)]

Diets.reach <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin) %>%
  summarise_at("Count", funs(sum)) %>% ungroup

diets_family <- Diets.reach %>% spread(key = "Family", value = "Count") %>%
  mutate_if(is.numeric , replace_na, replace = 0)

diets_aquatic <- Diets.reach %>% spread(key = "Family", value = "Count") %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>% filter(Origin == "A")


## DIETS and BENTHIC combined

bugndiet <- Diets.reach %>% filter(Origin == "A") %>% 
  merge(bugs.family, all.x = TRUE, all.y = TRUE) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  mutate_at(vars(CollDate), replace_na, replace = "2018") %>%
  mutate_at(vars(Origin), replace_na, replace = "A")

comb_diet <- bugndiet %>% select(-c(Density)) %>% 
  spread(key = Family, value = Count) %>%  
  mutate_if(is.numeric, replace_na, replace = 0)

comb_benthic <- bugndiet %>% select(-c(Count)) %>% 
  spread(key = Family, value = Density) %>%  
  mutate_if(is.numeric, replace_na, replace = 0)

combined <- rbind(comb_benthic, comb_diet)

bugs.fam.diff <- bugs.fam.diff %>% 
  gather(key = Family, value = Density, "Ameletidae":"Uenoidae")

bugdiffdiet <- Diets.reach %>% 
  filter(Origin == "A") %>% 
  select(-c("Origin")) %>% 
  merge(bugs.fam.diff, all.x = TRUE, all.y = TRUE) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) 

comb_diet_diff <- bugdiffdiet %>% select(-c(Density)) %>% 
  spread(key = Family, value = Count) %>%  
  mutate_if(is.numeric, replace_na, replace = 0)

comb_benthic_diff <- bugdiffdiet %>% select(-c(Count)) %>% 
  spread(key = Family, value = Density) %>%  
  mutate_if(is.numeric, replace_na, replace = 0)

combined_diff <- rbind(comb_benthic_diff, comb_diet_diff)


## Write csv's

write_csv(bugs.family, "~/Desktop/bugs_family.csv")
write_csv(bugs.fam.diff, "~/Desktop/bugs_family.diff.csv")
write_csv(bugs.diff, "~/Desktop/bugs.diff.csv")
write_csv(diets_family, "~/Desktop/diets_all.csv")
write_csv(diets_aquatic, "~/Desktop/diets_aquatic.csv")
write_csv(combined, "~/Desktop/combined.csv")
write_csv(combined_diff, "~/Desktop/combined_diff.csv")