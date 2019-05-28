#load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#import data
bugs.by.year <- readRDS("./Data/bugs.by.year")


#Aggtegate by size class (Need to do more research, use the Poff trait database)
bugs.size <- aggregate(Diff ~ Size + Stream + CollDate, sum, data = bugs.by.year)

ggplot(data = bugs.size) + 
  geom_jitter(mapping = aes(x = Size, y = Diff, color = CollDate, shape = Stream), width = .2, height = .2)