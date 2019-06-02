rm(list=ls()) 
library(dplyr)
library(ggplot2)
library(grid)
library(cowplot)
library(readxl)

Bentho <- read_xlsx("./Data/2018 Tiles Allison's Computer.xlsx", 
                     sheet = "2017 and 2018")
single_Q_estimates <- read.csv("./Data/discharge.csv")
temp_light <- read.csv("./Data/canopy_light.csv") 
ffgs <- read_xlsx("./Data")

Bentho$PostTRTvar<- as.factor(paste(Bentho$Year, Bentho$Treatment, sep = ".")) #need this for relaxation of constant variance. 
 
temp_light$Mean_GSFcc <- (1 - temp_light$Mean_GSF )
temp_light <- rename(temp_light, Year = Year.2, Treatment = Reach.2)
temp_light$Year <- temp_light$Year %>% recode(Post = 2018, Pre = 2017)
temp_light$Treatment <- temp_light$Treatment %>% recode(Reference = "N", Treatment = "Y")
PAR <- temp_light %>% select(c("PAR", "Stream", "Treatment", "Year"))

Q <- filter(single_Q_estimates, filter_variable == "1")
Q <- filter(Q, !Stream == "W-122")


Reach_Means <- Bentho %>%
  group_by(Stream, Year, Treatment, PostTRTvar) %>%
  summarise(Mean_Chla = mean(BenthoTotal), SD_Chla = sd(BenthoTotal,na.rm = TRUE)/3)

exp_vars <- merge(PAR, Reach_Means, by = c( "Stream", "Year", "Treatment"))
exp_vars <- filter(Chla_addition, !Stream == "W-122")


Refs <- filter(exp_vars,  Treatment == "N")
Trts <- filter(exp_vars, Treatment == "Y")
Diffs <- merge(Refs, Trts, by = c( "Stream", "Year"))

Diffs$d_PAR<- (Diffs$PAR.y - Diffs$PAR.x)
Diffs$d_Chla<- (Diffs$Mean_Chla.y - Diffs$Mean_Chla.x)
Diffs$d_BugDensity<- (Diffs$Density.y - Diffs$Density.x)
Diffs$d_BuglogDensity<- (Diffs$logDensity.y - Diffs$logDensity.x)
Diffs$d_ScDensity<- (Diffs$Sc_Density.y - Diffs$Sc_Density.x)
Diffs$d_SclogDensity<- (Diffs$Sc_logDensity.y - Diffs$Sc_logDensity.x)


Diffs_only <- Diffs %>% select(Stream, Year, d_PAR, d_Chla)


Pre <- filter(Diffs_only, Year == 2017)
Post <- filter(Diffs_only, Year == 2018)
Dubs <- merge(Pre, Post, by = "Stream")

Dubs$D_PAR <- (Dubs$d_PAR.y - Dubs$d_PAR.x)
Dubs$D_Chla <- (Dubs$d_Chla.y - Dubs$d_Chla.x)
Dubs$D_BugDensity <- (Dubs$d_BugDensity.y - Dubs$d_BugDensity.x)
Dubs$D_BuglogDensity <- (Dubs$d_BuglogDensity.y - Dubs$d_BuglogDensity.x)
Dubs$D_ScDensity <- (Dubs$d_ScDensity.y - Dubs$d_ScDensity.x)
Dubs$D_SclogDensity <- (Dubs$d_SclogDensity.y - Dubs$d_SclogDensity.x)


Dubs_only <- select(as_data_frame(Dubs),Stream, D_PAR, D_Chla)



Dubs_only$Stream<-factor(Dubs_only$Stream, level=c("MCTE","W-113","LOON","CHUCK","W-100","W-122"))
Q$Stream<-factor(Q$Stream, level = c("MCTE","W-113","LOON","CHUCK","W-100","W-122"), labels = c("McTE","W-113","Loon","Chucksney","W-100","W-122"))

Dubs_only$Q <- Q$Q
Dubs_only$Bankful<-Q$Bankful
Dubs_only$GapEllipse<-Q$GapEllipse
Dubs_only$light_meters<-Q$light_meters

(Bankful<- ggplot(data = Dubs_only, aes(x=Bankful, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression('Bankfull Width (m)'))+labs(y="")+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T) + geom_point(aes(shape = Stream),size =2) )

(Q<- ggplot(data = Dubs_only, aes(x=Q, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression(paste('Discharge (L',~s^-1,')')))+labs(y=expression('Sc Density Response'))+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T)+ geom_point(aes(shape = Stream),size =2) )

(Gap<- ggplot(data = Dubs_only, aes(x=GapEllipse, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression(paste('Gap Area (',m^2,')')))+labs(y="")+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T) + geom_point(aes(shape = Stream),size =2) )

(PAR<- ggplot(data = Dubs_only, aes(x=D_PAR, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression('Light Response'))+labs(y=expression('Sc Density Response'))+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T)+ geom_point(aes(shape = Stream),size =2) )

(Tmax<- ggplot(data = Dubs_only, aes(x=D_7Max, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression(paste('T'['7DayMax'], ' Response')))+labs(y=expression('Sc Density Response'))+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T) + geom_point(aes(shape = Stream),size =2) )

(Chla<- ggplot(data = Dubs_only, aes(x=D_Chla, y=D_SclogDensity))+#scale_y_continuous(limits = c(-.1,.51),breaks=c(-.1,0,.1,.2,.3,.4,.5))+
    labs(x=expression(paste('Chla Response')))+labs(y="")+
    theme_bw(base_size = 14) +geom_smooth(method='lm',size = .25, se= T)+ geom_point(aes(shape = Stream),size =2) )

leg<- get_legend(Q)
(exp_vars<- plot_grid(PAR+theme(legend.position="none"),Gap+theme(legend.position="none"),
                      Q+theme(legend.position="none"),Bankful+theme(legend.position="none"),
                      Tmax+theme(legend.position="none"),Chla+theme(legend.position="none"),
                      labels = c("a","b","c","d","e","f"),nrow = 3) )
(exp_vars_leg<-plot_grid(exp_vars,leg, nrow=1,rel_widths = c(1,.15)) )

ggsave("Explanatory_variables_Sc_Log_Density_6.png",exp_vars_leg, 
       path="/Users/allisonswartz/Google Drive (swartza@oregonstate.edu)/Warren Lab Shared Folder/2018 Summer Data/Tiles/Thesis_figures/", 
       width=10, height = 10)

mod_light<- lm(D_ScDensity ~ D_PAR, data = Dubs_only)
summary(mod_light)

mod_gap<- lm(D_Chla ~ GapEllipse, data = Dubs_only)
summary(mod_gap)

mod_Q<- lm(D_Chla ~ Q, data = Dubs_only)
summary(mod_Q)

mod_Bankful<- lm(D_Chla ~ Bankful, data = Dubs_only)
summary(mod_Bankful)

mod_Tmax<- lm(D_Chla ~ D_7Max, data = Dubs_only)
summary(mod_Tmax)

mod_Tmean<- lm(D_Chla ~ D_7Mean, data = Dubs_only)
summary(mod_Tmean)



