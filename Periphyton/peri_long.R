# Prep Periphyton data for PC ORD
x <- c("tidyverse", "vegan", "lubridate", "readxl", "ggthemes", "vegan", "viridis", "DT")
lapply(x, library, character.only = TRUE)

reach.peri <- read_csv("../Periphyton/Data/Reach_Algae_DiatomsSpecified_pca_data.csv")
meter.peri <- read_csv("../Periphyton/Data/Meter_Algae_DiatomsSpecified_pca_data.csv")
 
reach.cell <- reach.peri %>% 
  select(-c("Sum_Count", "Sum_Het")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame

reach.het <-  reach.peri %>% 
  select(-c("Sum_Count", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Het) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame                         
           
reach.count <-  reach.peri %>% 
  select(-c("Sum_Het", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Count) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame    

meter.count <-  meter.peri %>% 
  select(-c("Sum_Het", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Count) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame 

meter.het <-  meter.peri %>% 
  select(-c("Sum_Count", "Sum_CellCount")) %>% 
  spread(key = Portion.Taxon, value = Sum_Het) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame

meter.cell <-  meter.peri %>% 
  select(-c("Sum_Het", "Sum_Count")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  mutate_if(is.numeric , replace_na, replace = 0) %>% as.data.frame

meter.env <- select(meter.cell, c("X1", "Time.Reach.Meter"))
reach.env <- select(reach.cell, c("X1", "Time.Reach"))



peri.list <- list(meter.cell = meter.cell, meter.het = meter.het, meter.count = meter.count, 
                  reach.cell = reach.cell, reach.het = reach.het, reach.count = reach.count, 
                  meter.env = meter.env, reach.env = reach.env)

mapply(write.csv, peri.list, file = paste0(names(peri.list), '.csv'))
