#load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#import data
bugs.by.year <- readRDS("./Data/bugs.by.year")
bugs18 <- readRDS("./Data/bugs18")



#Aggregate to get density differences of FFG's
bugs.ffg <- bugs.by.year %>% 
  group_by(FFG, Stream, CollDate) %>%
  summarise_at("Diff", funs(sum)) %>% ungroup

#Add total SC, this throws off counts (scrapers doubly reprented)
extradf <- bugs.ffg[bugs.ffg$FFG == "SCe" | bugs.ffg$FFG == "SCi",]
extradf$FFG <- "SC"
extradf <- aggregate(Diff ~ FFG + Stream + CollDate, sum, data = extradf)
ffg.SC <- rbind(bugs.ffg, extradf) 

ggplot(data = ffg.SC) + 
  geom_jitter(mapping = aes(x = FFG, y = Diff, color = CollDate, shape = Stream), width = .2, height = .2)


bugs18.ffg <- bugs18 %>%
  group_by(Stream, Treatment, FFG) %>%
  summarise_at(vars(Density), funs(sum))

ggplot(data = bugs18.ffg) + 
  geom_jitter(mapping = aes(x = FFG, y = Density, color = Treatment, shape = Stream), width = .2, height = .2)
