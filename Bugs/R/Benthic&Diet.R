# The purpose of this script is to: 
#     1. create a dataframe with benthic and diet communities
#     2. Calculate proportions of each taxa in benthic versus diet communites
#     3. Plot resulting proportions



# Load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

# Import Data
Diets.fish <- readRDS("./Data/Diets.fish")
Diets.reach <- readRDS("./Data/Diets.reach")
bugs.family <- readRDS("./Data/bugs.family")

bugndiet <- merge(Diets.reach, bugs.family) %>% filter(Origin == "A") 

# Calculate proportions of each taxa in the diet and benthic communites
familyndiet <- bugndiet %>% 
  group_by(Stream, Treatment, Family) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density))

# Calculate proportions of each FFG in diet and Benthic communities
ffgndiet <- bugndiet %>% 
  group_by(Stream, Treatment, FFG) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density))

# Calculate proportions of each size class in diet and Benthic communities
sizendiet <- bugndiet %>% 
  group_by(Stream, Treatment, Size) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density))

# Plot proportion of a taxon in the benthic community versus proporion of a taxon in the diet community
ggplot(data = familyndiet) +
  geom_point(mapping = aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_text(aes(x = frac.benthic, y = frac.diet, label = Family), size = 2, nudge_y = .02) +
  facet_wrap("Stream")

# Plot proportion of a FFG in the benthic community versus proporion of a FFG in the diet community
ggplot(data = ffgndiet) +
  geom_point(mapping = aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
  geom_text(aes(x = frac.benthic, y = frac.diet, label = FFG), size = 2, nudge_y = .02) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap("Stream")


ggplot(data = sizendiet) +
  geom_point(mapping = aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
  geom_text(aes(x = frac.benthic, y = frac.diet, label = Size), size = 2, nudge_y = .02) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap("Stream")



