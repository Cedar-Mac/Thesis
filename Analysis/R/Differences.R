#load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#import data
bugs.by.year <- readRDS("./Data/bugs.by.year")

# Plot density differences
ggplot(data = bugs.by.year) + 
  geom_col(mapping = aes(x = Taxon, y = Diff, fill = CollDate), position = "dodge") +
  facet_wrap(c("Stream"), ncol = 4) +
  coord_flip()

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
  geom_point(mapping = aes(x = FFG, y = Diff, color = CollDate, shape = Stream))


