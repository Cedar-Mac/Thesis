# Load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

# Import Data
Diets.fish <- readRDS("./Data/Diets.fish")
Diets.reach <- readRDS("./Data/Diets.reach")



# Calculate average count of each Origin by stream and reach
Diets.origins <- Diets.fish %>%
  group_by(Stream, Treatment, Origin) %>%
  mutate(AdjCount = Count / max(Rep)) %>% 
  summarise_at(vars(AdjCount), funs(sum)) 

ggplot(data = Diets.origins) +
  geom_col(mapping = aes(x = Treatment, y = AdjCount, fill = Origin), position = "dodge") +
  facet_wrap("Stream")



# Calculate average count of each Family by stream and reach
Diets.fam <- Diets.fish %>%
  group_by(Stream, Treatment, Family) %>%
  mutate(AdjCount = Count / max(Rep)) %>% 
  summarise_at(vars(AdjCount), funs(sum)) 


ggplot(data = Diets.fam) +
  geom_col(mapping = aes(x = Family, y = AdjCount, fill = Treatment), position = "dodge") +
  facet_wrap("Stream") +
  coord_flip()


# Collapse counts into FFG's
Diets.ffg <- Diets.reach %>%
  group_by(Stream, Treatment, FFG) %>%
  summarise_at(vars(Count), funs(sum))

# FFG's in diets by stream and reach
ggplot(data = Diets.ffg) + 
  geom_jitter(mapping = aes(x = FFG, y = Count, color = Treatment, shape = Stream), width = .2, height = .2)



# Filter for only terrestrials
terrestrials <- Diets.reach %>% group_by(Stream, Treatment, Origin) %>%
  summarise_at(vars(Count), funs(sum)) %>%
  filter(Origin == "T")

ggplot(data = terrestrials) + 
  geom_col(mapping = aes(x = Treatment, y = Count, fill = Stream), 
           position = "dodge") 


terrestrials.fam <- filter(Diets.reach, Origin == "T")

ggplot(data = terrestrials.fam) + 
  geom_jitter(mapping = aes(x = Family, y = Count, color = Stream, shape = Treatment)) +
  coord_flip()

