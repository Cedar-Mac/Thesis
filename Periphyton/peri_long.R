# Prep Periphyton data for PC-ORD
x <- c("tidyverse", "vegan", "lubridate", "readxl", "ggthemes", "viridis", "ggrepel")
lapply(x, library, character.only = TRUE)

reach.peri <- read_csv("../Periphyton/Data/Reach_Algae_DiatomsSpecified_pca_data.csv")
meter.peri <- read_csv("../Periphyton/Data/Meter_Algae_DiatomsSpecified_pca_data.csv")
 
meter.algae <- meter.peri %>% filter(str_detect(Portion.Taxon, "Algae"))
reach.algae <- reach.peri %>% filter(str_detect(Portion.Taxon, "Algae"))

# Function to convert data to long output and fill missing values with 0
long_output <- function(x, dat, key, value, group) {
  x %>%
    select(dat) %>%
    spread(key = key, value = value) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    group_by_(.dots = group) %>%
    summarise_if(is.numeric, funs(sum)) %>%
    select(-c(group))
}


reach.het <- long_output(x = reach.peri, dat = c("Time.Reach","Portion.Taxon", "Sum_Het"), 
                         key = "Portion.Taxon", value = "Sum_Het", group = "Time.Reach")
  
reach.cell <- long_output(x = reach.peri, dat = c("Time.Reach","Portion.Taxon", "Sum_CellCount"), 
                          key = "Portion.Taxon", value = "Sum_CellCount", group = "Time.Reach")

reach.count <- long_output(x = reach.peri, dat = c("Time.Reach","Portion.Taxon", "Sum_Count"), 
                            key = "Portion.Taxon", value = "Sum_Count", group = "Time.Reach")

meter.count <-  long_output(x = meter.peri, dat = c("Time.Reach.Meter","Portion.Taxon", "Sum_Count"), 
                            key = "Portion.Taxon", value = "Sum_Count", group = "Time.Reach.Meter")

meter.het <-  long_output(x = meter.peri, dat = c("Time.Reach.Meter","Portion.Taxon", "Sum_Het"), 
                          key = "Portion.Taxon", value = "Sum_Het", group = "Time.Reach.Meter")

meter.cell <-  long_output(x = meter.peri, dat = c("Time.Reach.Meter","Portion.Taxon", "Sum_CellCount"), 
                           key = "Portion.Taxon", value = "Sum_CellCount", group = "Time.Reach.Meter")

reach.count.algae <- long_output(x = reach.algae, dat = c("Time.Reach","Portion.Taxon", "Sum_Count"), 
                           key = "Portion.Taxon", value = "Sum_Count", group = "Time.Reach")

meter.count.algae <-  long_output(x = meter.algae, dat = c("Time.Reach.Meter","Portion.Taxon", "Sum_Count"), 
                            key = "Portion.Taxon", value = "Sum_Count", group = "Time.Reach.Meter")

reach.env <- reach.peri %>% 
  select(-c("Sum_Count", "Sum_Het")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  select(Time.Reach) %>% unique()

meter.env <- meter.peri %>% 
  select(-c("Sum_Het", "Sum_Count")) %>% 
  spread(key = Portion.Taxon, value = Sum_CellCount) %>% 
  select(Time.Reach.Meter) %>% unique()

# No algae at post_2_treatment_15
meter.env.algae <- meter.env[-10,]


# Make list and write csv's
peri.list <- list(meter.cell = meter.cell, meter.het = meter.het, meter.count = meter.count, 
                reach.cell = reach.cell, reach.het = reach.het, reach.count = reach.count, 
                 meter.env = meter.env, reach.env = reach.env, reach.count.algae = reach.count.algae, 
                meter.count.algae = meter.count.algae, meter.env.algae = meter.env.algae)

mapply(write.csv, peri.list, file = paste0(names(peri.list), '.csv'))

#### By METER

Density <- beals(meter.count.algae)
Enviro <- meter.env[-10,]
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

ggplot(data = NMDS, aes(x, y, color = Time.Reach)) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2)) +
  annotate("text", x = (NMDS$x), y = (NMDS$y) + .04, label = NMDS$Time.Reach, size = 2) +
  geom_point() +
  ggtitle("NMDS of Periphyton Community By Reach") +
  scale_color_viridis_d()

