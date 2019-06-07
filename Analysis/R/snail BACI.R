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

all.snails <- read_xlsx(here::here("Analysis", "Data", "2018 Tiles Allison's Computer.xlsx"),
                        sheet = "Snails", range = "A1:F81")

reach.snail <- read_xlsx(here::here("Analysis", "Data", "2018 Tiles Allison's Computer.xlsx"),
                         sheet = "Snails", range = "W1:Z41")

temp_light <- read.csv(here::here("Analysis", "Data", "canopy_light.csv"))

temp_light <- dplyr::rename(temp_light, Year = Year.2, Treatment = Reach.2)
temp_light$Year <- temp_light$Year %>% recode(Post = 2018, Pre = 2017)
temp_light$Treatment <- temp_light$Treatment %>% recode(Reference = "N", Treatment = "Y")
temp_light <- temp_light %>% select(c("Stream", "Treatment", "Year", "PAR", "Max7MovingAMaxT"))


Mean_Chla <- Bentho %>% group_by(Stream, Treatment, Year, Meter) %>%
  summarise_at(vars(BenthoTotal), mean) %>% filter(Stream == "W-100" | Stream == "W-113")

Mean_Chla <- merge(Mean_Chla, all.snails, by = c("Stream", "Treatment", "Year", "Meter"))

Mean_Chla <- merge(Mean_Chla, temp_light, by = c("Stream", "Treatment", "Year", "Meter"))

pre <- Mean_Chla %>% filter(Year == 2017)
post <- Mean_Chla %>% filter(Year == 2018)
year.wide <- merge(pre, post, by = c("Stream", "Meter", "Treatment"))
year.wide$d_Chla <- (year.wide$BenthoTotal.y - year.wide$BenthoTotal.x)
year.wide$d_Snail <- (year.wide$Snail.y - year.wide$Snail.x)

diffs <- year.wide

ref <- diffs %>% filter(Treatment == "N")
trt <- diffs %>% filter(Treatment == "Y")
treat.wide <- merge(ref, trt, by = c("Stream", "Meter"))
treat.wide$D_Chla <- (treat.wide$d_Chla.y - treat.wide$d_Chla.x)
treat.wide$D_Snail <- (treat.wide$d_Snail.y - treat.wide$d_Snail.x)


Dubs <- treat.wide %>% select(Stream, Meter, D_Chla, D_Snail)

snail_vars <- Dubs

snail_vars$Gap <- c('Before Gap', "Before Gap", "Gap", "Gap", "Gap", "Gap", "After Gap", "After Gap", "After Gap", "After Gap")


ggplot(data = snail_vars, aes(x = D_Chla, y = D_Snail)) +
  labs(title = "Snails by Chla",
       x = "Chla",
       y = "Snails",
       caption = "BACI Response of Snails Compared to BACI Response of Chla") +
  theme_bw(base_size = 14) +
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream, color = Gap), size = 2)

ggplot(data = snail_vars, aes(x = Meter, y = D_Snail)) +
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



mod_snail <- lm(D_Snail ~ D_Chla, data = snail_vars)
summary(mod_snail)

t.test(Snail ~ Treatment, data = reach.snail, paired = T)



pre.reach <- Mean_Chla %>% filter(Year == 2017)
post.reach <- Mean_Chla %>% filter(Year == 2018)
year.wide <- merge(pre.reach, post.reach, by = c("Stream", 'Treatment', "Meter"))
year.wide$d_Chla <- (year.wide$BenthoTotal.y - year.wide$BenthoTotal.x)

reach.diffs <- year.wide %>% select(Stream, Treatment, Meter, d_Chla) %>% filter(Stream == "W-100" | Stream == "W-113")

reach.snails <- merge(reach.snail, reach.diffs, by = c("Stream", "Treatment", "Meter"))

reach.snails$Gap <- c(rep("Ref", 10), 'Before Gap', "Before Gap", "Gap", "Gap", "Gap", "Gap", "Gap", "After Gap", "After Gap", "After Gap")


ggplot(data = reach.snails, aes(x = d_Chla, y = Snail)) +
  labs(title = "Snails by Chla",
       x = "Chla",
       y = "Snails",
       caption = "Snail Difference between years Compared to Chla difference between years") +
  theme_bw(base_size = 14) +
  geom_smooth(method = 'lm', size = .25, se = T) +
  geom_point(aes(color = Gap), size = 2) +
    facet_wrap("Stream")





