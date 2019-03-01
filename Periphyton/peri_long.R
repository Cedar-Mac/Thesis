# Prep Periphyton data for PC ORD
x <- c("tidyverse", "vegan", "lubridate", "readxl", "ggthemes", "viridis", "ggrepel")
lapply(x, library, character.only = TRUE)

reach.peri <- read_csv("../Periphyton/Data/Reach_Algae_DiatomsSpecified_pca_data.csv")
meter.peri <- read_csv("../Periphyton/Data/Meter_Algae_DiatomsSpecified_pca_data.csv")
 
meter.algae <- meter.peri %>% filter(str_detect(Portion.Taxon, "Algae"))

reach.cell <- reach.peri %>% 
  select(-c("Sum_Count", "Sum_Het")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach")) 


reach.het <- reach.peri %>% 
  select(-c("Sum_Count", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Het) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach"))
                           
           
reach.count <-  reach.peri %>% 
  select(-c("Sum_Het", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Count) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach"))


meter.count <-  meter.peri %>% 
  select(-c("Sum_Het", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Count) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach.Meter) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach.Meter"))
  
meter.count <-  meter.algae %>% 
  select(-c("Sum_Het", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Count) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach.Meter) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach.Meter"))



meter.het <-  meter.peri %>% 
  select(-c("Sum_Count", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Het) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach.Meter) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach.Meter"))

meter.cell <-  meter.peri %>% 
  select(-c("Sum_Het", "Sum_Count")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  group_by(Time.Reach.Meter) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-c("X1", "Time.Reach.Meter"))

reach.env <- reach.peri %>% 
  select(-c("Sum_Count", "Sum_Het")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  select(Time.Reach) %>% unique()

meter.env <- meter.peri %>% 
  select(-c("Sum_Het", "Sum_Count")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  select(Time.Reach.Meter) %>% unique()



#peri.list <- list(meter.cell = meter.cell, meter.het = meter.het, meter.count = meter.count, 
#                  reach.cell = reach.cell, reach.het = reach.het, reach.count = reach.count, 
#                  meter.env = meter.env, reach.env = reach.env)

#mapply(write.csv, peri.list, file = paste0(names(peri.list), '.csv'))


#### By METER

Density <- beals(meter.count)
Enviro <- meter.env
sol <- metaMDS(Density, distance = "bray", k = 2, trymax = 100)

NMDS <- data.frame(x = sol$points[, 1], y = sol$points[ ,2], 
                   Time.Reach.Meter = select(Enviro, Time.Reach.Meter))


ggplot(data = NMDS, aes(x, y, color = Time.Reach.Meter)) +
  annotate("text", x = (NMDS$x), y = (NMDS$y), label = NMDS$Time.Reach.Meter, size = 2) +
  geom_point() +
  ggtitle("NMDS of Periphyton Community By Meter") +
  scale_color_viridis_d()

#### By REACH

Density <- reach.count
Enviro <- reach.env
sol <- metaMDS(Density, distance = "bray", k = 2, trymax = 100)

NMDS <- data.frame(x = sol$points[, 1], y = sol$points[ ,2], 
                   Time.Reach = select(Enviro, Time.Reach))

plot.new()
ord <- ordiellipse(sol, NMDS$Time.Reach, display = "sites", kind = "sd", conf = 0.95, label = TRUE)
dev.off()

df_ell <- data.frame()
for(g in NMDS$Time.Reach){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Time.Reach == g,],
                                                   vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                ,Time.Reach = g))
}

ggplot(data = NMDS, aes(x, y, color = Time.Reach)) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2)) +
  annotate("text", x = (NMDS$x), y = (NMDS$y) + .04, label = NMDS$Time.Reach, size = 2) +
  geom_point() +
  ggtitle("NMDS of Periphyton Community By Reach") +
  scale_color_viridis_d()

