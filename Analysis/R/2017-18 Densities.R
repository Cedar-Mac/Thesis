x <- c("tidyverse", "vegan", "lubridate")
lapply(x, library, character.only = TRUE)

# Import Data
bugs <- readxl::read_xlsx("../Data/2017-18 bugs.xlsx", sheet = "Cleaned")
ffg <- readxl::read_xlsx("../Data/2017-18 bugs.xlsx", sheet = "FFG")
str(bugs)


# Adjust individual count values by multiplying by inverse 
# of subsample and dividing by area sampled
bugs$Density <- bugs$Count * (1 / bugs$PercentSub) / .09

# Aggregate and divide by three (number of 
# replicate samples pooled) to get true density
bugs.agg <- aggregate(Density ~ CollDate + Stream + 
                        Treatment + Family, sum, data = bugs)
bugs.agg$Density <- bugs.agg$Density / 3

# Spread to get Family matrix
bugs.agg.spread <- spread(bugs.agg, key = "Family", value = "Density")

# Gather to fill in missing family values
bugs.agg.filled <- gather(bugs.agg.spread, key = "Family", value = "Density", 4:30)

# Change CollDate to 'Factor' instead of 'Date'
bugs.agg.filled$CollDate <- as.factor(year(bugs.agg.filled$CollDate))

#Assign Functional Feeding Groups
bugs.agg.filled <- merge(bugs.agg.filled, ffg)



# Get ratio of Treatement over Control
bugs.treated <- bugs.agg.filled %>% filter(Treatment == "Y")
bugs.not.treated <- bugs.agg.filled %>% filter(Treatment == "N")
bugs.ratio <- bugs.treated[,c("Stream", "CollDate", "FFG", "Family")]
bugs.ratio$DensityRatio <- bugs.treated$Density / bugs.not.treated$Density
bugs.ratio$DensityDiff <- bugs.treated$Density - bugs.not.treated$Density




# Bar plots of Family Densities
#############################

# Plot bug densities
ggplot(data = bugs.agg.filled) + 
  geom_col(mapping = aes(x = Family, y = Density, fill = Treatment), position = "dodge") +
  facet_wrap(c("Stream", "CollDate"), ncol = 4) +
  coord_flip()

# Plot density ratios
ggplot(data = bugs.ratio) + 
  geom_col(mapping = aes(x = Family, y = DensityRatio, fill = CollDate), position = "dodge") +
  facet_wrap(c("Stream"), ncol = 4) +
  coord_flip()

# Plot density differences
ggplot(data = bugs.ratio) + 
  geom_col(mapping = aes(x = Family, y = DensityDiff, fill = CollDate), position = "dodge") +
  facet_wrap(c("Stream"), ncol = 4) +
  coord_flip()

# Log of ratio
bugs.ratio$LogDensityRatio <- log(bugs.ratio$DensityRatio + 1)
# Plot log of density ratio's
ggplot(data = bugs.ratio) + 
  geom_col(mapping = aes(x = Family, y = LogDensityRatio, fill = CollDate), position = "dodge") +
  facet_wrap(c("Stream"), ncol = 4) +
  coord_flip()



#############################
# Plots of Functional Feeding groups
##############################

bugs.ffg <- aggregate(DensityDiff ~ FFG + Stream + CollDate, sum, data = bugs.ratio)

ffg_name <- unique(bugs.ffg$FFG) 

for(i in 1:length(ffg_name)) {
    bugs.ffg$Var[bugs.ffg$FFG == ffg_name[i]] <- 
     var(bugs.ffg$DensityDiff[bugs.ffg$FFG == ffg_name[i]])
}


ggplot(data = ffg.SC) + 
  geom_jitter(mapping = aes(x = FFG, y = Diff, color = CollDate, shape = Stream), width = .2, height = .2)




Diets.fish <- Diets_fish
Diets.fish  <-Diets.fish %>% 
  group_by(Stream, Treatment, Family) %>%
  summarise_at(vars(Count), funs(sum)) %>% ungroup

Diets.fish <- Diets.fish %>% select("Stream", "Treatment", "Family", "Count")
Diets.fish$CollDate <- 2018 




bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Family) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3

combined <- merge(Diets.fish, bugs.agg)


bugs.agg <- spread(bugs.agg, key = "Family", value = "Density") %>% 
  mutate_if(is.numeric , replace_na, replace = 0) 




combined <-rbind.na(bugs.agg, Diets.fish)



















