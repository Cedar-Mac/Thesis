#load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#import data
bugs.by.year <- readRDS("./Data/bugs.by.year")
bugs18 <- readRDS("./Data/bugs18")

# Plot density differences
ggplot(data = bugs.by.year) + 
  geom_col(mapping = aes(x = Taxon, y = Diff, fill = CollDate), position = "dodge") +
  facet_wrap(c("Stream")) +
  coord_flip()

# Filter for only scraper taxa
scraper.taxa.diff <- filter(bugs.by.year, FFG == "SCe" | FFG == "SCi")
scraper.taxa18 <- filter(bugs18, FFG == "SCe" | FFG == "SCi")

ggplot(data = scraper.taxa.diff) + 
  geom_jitter(mapping = aes(x = Taxon, y = Diff, color = CollDate, shape = Stream), 
              width = .2, height = .2) +
  coord_flip()

ggplot(data = scraper.taxa18) + 
  geom_jitter(mapping = aes(x = Taxon, y = Density, color = Treatment, shape = Stream), 
              width = .2, height = .2) +
  coord_flip()



# Linear model of diff with CollDate as explanatory variable
plot(lm(Diff ~ CollDate, data = bugs.by.year))

anova(lm(Diff ~ CollDate, data = bugs.by.year))
