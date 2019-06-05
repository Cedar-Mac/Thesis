library(tidyverse)
library(dplyr)
library(ggplot2)
library(grid)
library(cowplot)
library(readxl)

Bentho <- read_xlsx(here::here("Analysis", "Data", "2018 Tiles Allison's Computer.xlsx"), 
                    sheet = "2017 and 2018")
Snails <- read_xlsx(here::here("Analysis", "Data", "2018 Tiles Allison's Computer.xlsx"),
                        sheet = "Snails", range = "Q1:T21")

Mean_Chla <- Bentho %>% group_by(Stream, Treatment, Year, Meter) %>%
  summarise_at(vars(BenthoTotal), mean)

ref <- Mean_Chla %>% filter(Treatment == "N")
trt <- Mean_Chla %>% filter(Treatment == "Y")
treat.wide <- merge(ref, trt, by = c("Stream", 'Year', "Meter"))
treat.wide$d_Chla <- (treat.wide$BenthoTotal.y - treat.wide$BenthoTotal.x)

diffs <- treat.wide %>% select(Stream, Year, Meter, d_Chla) %>% filter(Stream == "W-100" | Stream == "W-113")

pre <- diffs %>% filter(Year == 2017)
post <- diffs %>% filter(Year == 2018)
year.wide <- merge(pre, post, by = c("Stream", "Meter"))
year.wide$D_Chla <- (year.wide$d_Chla.y - year.wide$d_Chla.x)

Dubs <- year.wide %>% select(Stream, Meter, D_Chla)

snail_vars <- merge(Dubs, Snails, by = c("Stream", "Meter"))

snail_vars$Gap <- c('Before Gap', "Before Gap", "Before Gap", "Before Gap", "Gap", "Gap", "Gap", "After Gap", "After Gap", "After Gap")



ggplot(data = snail_vars, aes(x = D_Chla, y = Snail)) +
  labs(title = "Snails by Chla",
       x = "Chla",
       y = "Snails",
       caption = "BACI Response of Snails Compared to BACI Response of Chla") +
  theme_bw(base_size = 14) +
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream, color = Gap), size = 2)

ggplot(data = snail_vars, aes(x = Meter, y = Snail)) +
  labs(title = "Snails by Meter",
       x = "Meter",
       y = "Snails", 
       caption = "BACI of Snails at Each Meter") +
  theme_bw(base_size = 14) +
  geom_smooth(method = 'loess', size = .25, se = T) + 
  geom_point(aes(shape = Stream), size = 2)

ggplot(data = snail_vars, aes(x = Meter, y = D_Chla)) +
  labs(title = "Chla by Meter",
       x = "Meter",
       y = "Chla",
       caption = "BACI of Chla at Each Meter") +
  theme_bw(base_size = 14) +
  geom_smooth(method = 'lm', size = .25, se = T) + 
  geom_point(aes(shape = Stream), size = 2)



mod_snail <- lm(Snail ~ D_Chla, data = snail_vars)
summary(mod_snail)

