# Load packages
library(readxl)
library(dplyr)
library(ggplot2)

# Read in data
Bentho<- read_xlsx("~/Desktop/2018 Tiles Allison's Computer.xlsx", sheet = "Tiles")


# Make ggplot theme 
my_theme <- theme_bw(base_size = 18, base_family = "Microsoft Sans Serif") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
        legend.title = element_text(colour = "black", size = 18, face = "bold"),
        legend.text = element_text(colour = "black", size = 12))
theme_set(my_theme)

Bentho$Stream<- as.factor(Bentho$Stream)
Bentho$Treatment<-as.factor(Bentho$Treatment)
Bentho$Meter<-as.factor(Bentho$Meter)
Bentho$BenthoTotal<- as.numeric(Bentho$BenthoTotal)

# Calculate mean of the three tiles for each meter
bentho.mean <- aggregate(BenthoTotal ~ Stream + Treatment + Meter, mean, data = Bentho)

ggplot(data = bentho.mean) +
  geom_line(mapping = aes(x = Meter, y = BenthoTotal, color = Treatment, group = Treatment), size =1.25) +
  ylab("Chlorophyll a (ug/cm^2)")+ scale_x_discrete(breaks = seq(0, 90, by = 15)) +
  scale_color_viridis_d(option = "E") +
  facet_wrap("Stream")

Both_years<- read_excel("~/Desktop/2018 Tiles Allison's Computer.xlsx", sheet = "2017 and 2018")

Both_years$Stream<- as.factor(Both_years$Stream)
Both_years$Year<-as.factor(Both_years$Year)
Both_years$Year.Treatment<-as.factor(Both_years$Year.Treatment)
Both_years$Treatment<-as.factor(Both_years$Treatment)
Both_years$Meter<-as.factor(Both_years$Meter)
Both_years$BenthoTotal<- as.numeric(Both_years$BenthoTotal)

bentho_both.mean <- aggregate(BenthoTotal ~ Stream + Year.Treatment + Meter + Year, mean, data = Both_years)


ggplot(data = bentho_both.mean) +
  geom_line(mapping = aes(x = Meter, y = BenthoTotal, color = Year.Treatment, 
                linetype = Year, size = Year, group = Year.Treatment), size =1.25) +
  ylab("Chlorophyll a (ug/cm^2)")+ scale_x_discrete(breaks = seq(0, 90, by = 15)) +
  scale_color_viridis_d(option = "E") +
  facet_wrap("Stream")

