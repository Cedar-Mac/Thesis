library(readxl)
library(dplyr)
library(ggplot2)

FL <- read_excel("~/Google Drive/Stream Ecology/2018 Field Data/2018_Fluorescein.xlsx", sheet = "Compiled")

FL$Stream<- as.factor(FL$Stream)
FL$Reach<-as.factor(FL$Reach)
FL$Meter<-as.factor(FL$Meter)
FL$PAR<- as.numeric(FL$PAR)

PAR.mean <- aggregate(PAR ~ Stream + Reach + Meter, mean, data = FL)
write.csv(PAR.mean, file = "2018_5mMean_PAR.csv",row.names=FALSE)

ReachMean<- aggregate(PAR~ Stream + Reach, mean, data = FL)
write.csv(ReachMean, file = "2018_Mean_PAR.csv",row.names=FALSE)


#Setup ggplot theme
my_theme <- theme_bw(base_size = 18, base_family = "Microsoft Sans Serif") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
        legend.title = element_text(colour = "black", size = 18, face = "bold"),
        legend.text = element_text(colour = "black", size = 12))

theme_set(my_theme)

PAR.mean %>% filter(Stream != "W-122") %>%

ggplot(aes(x = Meter, y = PAR, color = Reach, group = Reach)) +
  geom_line(size = 3) +
  ylab("PAR (mol/m^2 day)")+ scale_x_discrete(breaks = seq(0, 90, by = 15)) +
  labs(title = "Post Treatment") + ylim(0,18) +
  facet_wrap("Stream") +
  scale_color_viridis_d(option = "E")


  
  
