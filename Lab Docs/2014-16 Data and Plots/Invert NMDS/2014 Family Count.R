library(tidyverse)
library(vegan)

Density <- read_csv("2014_density.csv")
Enviro <- (read_csv("2014_enviro.csv")) %>% mutate_if(is.character, as.factor)


#Create a dataframe of environmental variables and families w/ a count of each family.
invert_2014 <- bind_cols(Enviro, Density) 
invert_2014 <- invert_2014 %>% gather(17:55, key = "Family", value = "Count")


#plot stuff
ggplot(data = invert_2014) +
  geom_point(mapping = aes(x = `Gravel Ave (%)`, y = Count, color = Family)) +
  facet_wrap(~ Reach)
  

