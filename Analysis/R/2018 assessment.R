x <- c("vegan", "tidyverse", "mosaic")
lapply(x, library, character.only = TRUE)

mcte.pre <- readxl::read_xlsx("~/Desktop/OneDrive - Oregon State University/Stream Ecology/2018 bugs/2018 pre assessment.xlsx", n_max = 1010)
bugs2017 <- readxl::read_xlsx("~/Desktop/OneDrive - Oregon State University/Stream Ecology/2017 Bugs/2017 bugs.xlsx")
####################################################################################################

# Look for differences between reaches
mcte.reach <- aggregate(Count ~ `Coll. Date` + Stream + Reach + Order + Family, data = mcte.pre, sum) 
mcte.reach$Count[mcte.reach$Reach == "DS"] <- mcte.reach$Count[mcte.reach$Reach == "DS"]/2

mcte.reach %<>% group_by(Reach) %>% mutate(`Rel Abundance` = Count / Count[Family == "Chloroperlidae"])

ggplot(data = mcte.reach) + 
  geom_col(mapping = aes(x = `Family`, y = Count)) +
  facet_wrap("Reach") +
  coord_flip()

####################################################################################################

# Look for differences between meter marks in the treatment reach
mcte.meter <- aggregate(Count ~ `Coll. Date` + Stream + Reach + Meter + Order + Family, data = mcte.pre, sum) %>% 
  filter(Reach == "DS")

mcte.meter %<>% group_by(Meter) %>% mutate(`Rel Abundance` = Count / Count[Family == "Chloroperlidae"])

ggplot(data = mcte.meter) + 
  geom_col(mapping = aes(x = `Family`, y = Count)) +
  facet_wrap("Meter") +
  coord_flip()

####################################################################################################

# Are samples large enough to have good precision
mcte.meter$D <- c(0)
family_names <- unique(mcte.meter$Family) 

for(i in 1:length(family_names)) {
  mcte.meter$D[mcte.meter$Family == family_names[i]] <- 
    sqrt(var(mcte.meter$Count[mcte.meter$Family == family_names[i]])) / 
    mean(mcte.meter$Count[mcte.meter$Family == family_names[i]])
}
okay2018bugs <- mcte.meter %>% filter(D < .45)

bugs2017UP <- aggregate(Count ~ `Coll. Date` + Stream + Reach + Meter + Order + Family, data = bugs2017, sum) %>% filter(Stream == "MCTE" & Reach == "UP") 
bugs2017UP$D <- c(0)
family_names <- unique(bugs2017UP$Family) 
for(i in 1:length(family_names)) {
  bugs2017UP$D[bugs2017UP$Family == family_names[i]] <- 
    sqrt(var(bugs2017UP$Count[bugs2017UP$Family == family_names[i]])) / 
    mean(bugs2017UP$Count[bugs2017UP$Family == family_names[i]])
}
okay2017bugs <- bugs2017UP %>% filter(D < .45)

(bestbugs <- intersect(okay2017bugs$Family, okay2018bugs$Family))








fam_precision <- function (df) {
  df$D <- c(1)
  family_names <- unique(df$Family) 
  for(i in 1:length(family_names)) {
    df$D[df$Family == family_names[i]] <- sqrt(var(df$Count[df$Family == family_names[i]])) / 
      mean(df$Count[df$Family == family_names[i]])
  }
}
fam_precision(mcte.meter)


