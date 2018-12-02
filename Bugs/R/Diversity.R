#Load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

#import data
bugs <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Cleaned")

#Spread to get family matrix
bugs.div <- aggregate(Count ~ CollDate + Stream + Treatment + Taxon, sum, data = bugs) %>% spread(key = "Taxon", value = "Count")
bugs.div[is.na(bugs.div)] <- 0
bugs.div$Shannon <- diversity(bugs.div[ , -c(1:3)], MARGIN = 1, "shannon")
bugs.div$Simpson <- diversity(bugs.div[ , -c(1:3)], MARGIN = 1, "simpson")
plot(Simpson ~ CollDate, data = bugs.div)
