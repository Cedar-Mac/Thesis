# The purpose of this script is to create complete aggregated dataframes of Family densities
# with NA's and with NA's filled with zero


# Load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl")
lapply(x, library, character.only = TRUE)

# Import Data
bugs <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Cleaned")
ffg <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "FFG")
sizes <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Size")

#Replace Ssp. in Taxon with Family
bugs$Taxon[bugs$Taxon == "Ssp."] <- bugs$Family[bugs$Taxon == "Ssp."]


#Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- aggregate(Density ~ CollDate + Stream + Treatment + Taxon, sum, data = bugs) 

bugs.agg$Density <- bugs.agg$Density / 3

# Spread and then gather to fill missing Genus's with NA values
bugs.agg.na <- spread(bugs.agg, key = "Taxon", value = "Density") %>% 
  gather(key = "Taxon", value = "Density", 4:ncol(.))

# Spread and then gather to fill missing Genus's with 0's
bugs.agg.zero <- spread(bugs.agg, key = "Taxon", value = "Density") %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  gather( key = "Taxon", value = "Density", 4:ncol(.))

# Create list of CollDate, Stream and Genus
bugs.by.year <- unique(bugs.agg.na[ , -c(3, 5)])

# Calculate Ratios
bugs.by.year$Ratio <- bugs.agg.na$Density[bugs.agg.na$Treatment == "Y"] /   
  bugs.agg.na$Density[bugs.agg.na$Treatment == "N"]

# Calculate Differences
bugs.by.year$Diff <- bugs.agg.zero$Density[bugs.agg.zero$Treatment == "Y"] -     
  bugs.agg.zero$Density[bugs.agg.zero$Treatment == "N"]

bugs.by.year <- merge(bugs.by.year, sizes) %>% merge(., ffg)
bugs.agg.zero <- merge(bugs.agg.zero, sizes) %>% merge(., ffg)


write_csv(bugs.by.year, "./Data/bugs.by.year.csv")

write_csv(bugs.agg.zero, "./Data/bugs.agg.zero.csv")
