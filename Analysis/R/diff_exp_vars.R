
x <- c("tidyverse", "vegan", "lubridate", "readxl", "ggthemes", "vegan", "viridis", "DT", "ggrepel", 
       "here", "cowplot", "grid", "here")
lapply(x, library, character.only = TRUE)


Bentho <- read_xlsx(here::here("Analysis", "Data", "2018 Tiles Allison's Computer.xlsx"), 
                     sheet = "2017 and 2018")
ffgs <- read_xlsx(here::here("Analysis", "Data", "2017-18 bugs.xlsx"),
                  sheet = "Density Ratio's", range = "D1:H121")
single_Q_estimates <- read.csv(here::here("Analysis", "Data", "discharge.csv"))
temp_light <- read.csv(here::here("Analysis", "Data", "canopy_light.csv"))



ffgs <- dplyr::rename(ffgs, Year = CollDate)

Bentho$PostTRTvar<- as.factor(paste(Bentho$Year, Bentho$Treatment, sep = ".")) #need this for relaxation of constant variance. 


temp_light$Mean_GSFcc <- (1 - temp_light$Mean_GSF )
temp_light <- dplyr::rename(temp_light, Year = Year.2, Treatment = Reach.2)
temp_light$Year <- temp_light$Year %>% recode(Post = 2018, Pre = 2017)
temp_light$Treatment <- temp_light$Treatment %>% recode(Reference = "N", Treatment = "Y")
temp_light <- temp_light %>% select(c("Stream", "Treatment", "Year", "PAR", "Max7MovingAMaxT"))

Q <- filter(single_Q_estimates, filter_variable == "1")
Q <- Q %>% filter(Stream != "W-122")


Reach_Means <- Bentho %>% dplyr::group_by(Stream, Year, Treatment, Year.Treatment) %>%
  dplyr::summarise(Mean_Chla = mean(BenthoTotal), SD_Chla = sd(BenthoTotal, na.rm = TRUE))


# Fabricating data... Missing CHUCK 2017 N Chla Data, I am making conservative estimate that 
# Chla in the reference reach was 10% higher than the treatment reach
Reach_Means[24, ] <- list("CHUCK", "2017", "N", "2017 N", 0.0459259259259259, 0.080634945)

exp_vars <- merge(temp_light, Reach_Means, by = c( "Stream", "Year", "Treatment"))
exp_vars <- filter(exp_vars, Stream != "W-122")

# Spread so each FFG is a column
ffgs.wide <- spread(ffgs, key = FFG, value = Density)
# Calculate total invert density by adding all FFG's
ffgs.wide$All <- ffgs.wide$CF + ffgs.wide$CG + ffgs.wide$SCe + ffgs.wide$SCi + ffgs.wide$P + ffgs.wide$SH

#Calculate total SC density by adding SCi and SCe
ffgs.wide$SC <- ffgs.wide$SCe + ffgs.wide$SCi

bug_exp <- merge(exp_vars, ffgs.wide, by = c("Stream", "Year", "Treatment"))


####### REACH DIFFS ###################
Refs <- filter(bug_exp,  Treatment == "N")
Trts <- filter(bug_exp, Treatment == "Y")
Diffs <- merge(Refs, Trts, by = c( "Stream", "Year"))

Diffs$d_PAR <- (Diffs$PAR.y - Diffs$PAR.x)
Diffs$d_Chla <- (Diffs$Mean_Chla.y - Diffs$Mean_Chla.x)
Diffs$d_SC <- (Diffs$SC.y - Diffs$SC.x)
Diffs$d_SCe <- (Diffs$SCe.y - Diffs$SCe.x)
Diffs$d_SCi <- (Diffs$SCi.y - Diffs$SCi.x)
Diffs$d_All <- (Diffs$All.y - Diffs$All.x)
Diffs$d_CF <- (Diffs$CF.y - Diffs$CF.x)
Diffs$d_P <- (Diffs$P.y - Diffs$P.x)
Diffs$d_SH <- (Diffs$SH.y - Diffs$SH.x)
Diffs$d_CG <- (Diffs$CG.y - Diffs$CG.x)
Diffs$d_7max <- (Diffs$Max7MovingAMaxT.y - Diffs$Max7MovingAMaxT.x)

Diffs_only <- Diffs %>% select(Stream, Year, d_PAR, d_Chla, d_CF, d_All, d_SCe, d_SCi, d_SC, d_7max, d_P, d_SH, d_CG)

Diffs_only$EPT <- c(2, 0, 1, -3, -1, -1, 5, -2, -6, -1)

####### YEAR DOUBLE DIFF ###############
Pre <- filter(Diffs_only, Year == 2017)
Post <- filter(Diffs_only, Year == 2018)
Dubs <- merge(Pre, Post, by = "Stream")

Dubs$D_PAR <- (Dubs$d_PAR.y - Dubs$d_PAR.x)
Dubs$D_Chla <- (Dubs$d_Chla.y - Dubs$d_Chla.x)
Dubs$D_All <- (Dubs$d_All.y - Dubs$d_All.x)
Dubs$D_SC <- (Dubs$d_SC.y - Dubs$d_SC.x)
Dubs$D_SCi <- (Dubs$d_SCi.y - Dubs$d_SCi.x)
Dubs$D_SCe <- (Dubs$d_SCe.y - Dubs$d_SCe.x)
Dubs$D_CF <- (Dubs$d_CF.y - Dubs$d_CF.x)
Dubs$D_7max <- (Dubs$d_7max.y - Dubs$d_7max.x)

Dubs_only <- select(as_data_frame(Dubs),Stream, D_PAR, D_Chla, D_All, D_SC, D_SCe, D_SCi, D_CF, D_7max)

Dubs_only$Q <- Q$Q
Dubs_only$Bankful<-Q$Bankful
Dubs_only$GapEllipse<-Q$GapEllipse
Dubs_only$light_meters<-Q$light_meters

Dubs_only$EPT <- c(-2,-4,0,-7,5)



####### ALL SCRAPER PLOTS #############
(Q <- ggplot(data = Dubs_only, aes(x = Q, y = D_SC)) +
    labs(x = expression(paste('Discharge')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Tmax <- ggplot(data = Dubs_only, aes(x = D_7max, y = D_SC)) +
    labs(x = expression(paste('Max Temperature Response')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(PAR <- ggplot(data = Dubs_only, aes(x = D_PAR, y = D_SC)) +
    labs(x = expression(paste('PAR Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Chla <- ggplot(data = Dubs_only, aes(x = D_Chla, y = D_SC)) +
    labs(x = expression(paste('Chla Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )


leg <- get_legend(Q)

(SC_reg <- plot_grid(PAR + theme(legend.position = "none"),
                      Q + theme(legend.position = "none"),
                      Chla + theme(legend.position = "none"),
                      Tmax + theme(legend.position = "none"),
                      labels = c("a", "b", "c", "d"), nrow = 2) )

SC_reg_leg<-plot_grid(SC_reg, leg, nrow = 1, rel_widths = c(1, 0.15))

ggsave("SC Exp Var Regressions.png",SC_reg_leg, 
       path = "~/Desktop/", width = 10, height = 10)

###### SCe PLOTS ########
(Q <- ggplot(data = Dubs_only, aes(x = Q, y = D_SCe)) +
    labs(x = expression(paste('Discharge')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Tmax <- ggplot(data = Dubs_only, aes(x = D_7max, y = D_SCe)) +
    labs(x = expression(paste('Max Temperature Response')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(PAR <- ggplot(data = Dubs_only, aes(x = D_PAR, y = D_SCe)) +
    labs(x = expression(paste('PAR Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Chla <- ggplot(data = Dubs_only, aes(x = D_Chla, y = D_SCe)) +
    labs(x = expression(paste('Chla Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

leg <- get_legend(Q)

(SCe_reg <- plot_grid(PAR + theme(legend.position = "none"),
                     Q + theme(legend.position = "none"),
                     Chla + theme(legend.position = "none"),
                     Tmax + theme(legend.position = "none"),
                     labels = c("a", "b", "c", "d"), nrow = 2) )

SCe_reg_leg <- plot_grid(SCe_reg, leg, nrow = 1, rel_widths = c(1, 0.15))

ggsave("SCe Exp Var Regressions.png", SC_reg_leg, 
       path = "~/Desktop/", width = 10, height = 10)


###### SCi PLOTS #########
(Q <- ggplot(data = Dubs_only, aes(x = Q, y = D_SCi)) +
    labs(x = expression(paste('Discharge')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Tmax <- ggplot(data = Dubs_only, aes(x = D_7max, y = D_SCi)) +
    labs(x = expression(paste('Max Temperature Response')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(PAR <- ggplot(data = Dubs_only, aes(x = D_PAR, y = D_SCi)) +
    labs(x = expression(paste('PAR Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Chla <- ggplot(data = Dubs_only, aes(x = D_Chla, y = D_SCi)) +
    labs(x = expression(paste('Chla Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

leg <- get_legend(Q)

(SCi_reg <- plot_grid(PAR + theme(legend.position = "none"),
                     Q + theme(legend.position = "none"),
                     Chla + theme(legend.position = "none"),
                     Tmax + theme(legend.position = "none"),
                     labels = c("a", "b", "c", "d"), nrow = 2) )

SCi_reg_leg<-plot_grid(SCi_reg, leg, nrow = 1, rel_widths = c(1, 0.15))

ggsave("SCi Exp Var Regressions.png",SC_reg_leg, 
       path = "~/Desktop/", width = 10, height = 10)




####### CF PLOTS ###################
(Q <- ggplot(data = Dubs_only, aes(x = Q, y = D_CF)) +
    labs(x = expression(paste('Discharge')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Tmax <- ggplot(data = Dubs_only, aes(x = D_7max, y = D_CF)) +
    labs(x = expression(paste('Max Temperature Response')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(PAR <- ggplot(data = Dubs_only, aes(x = D_PAR, y = D_CF)) +
    labs(x = expression(paste('PAR Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Chla <- ggplot(data = Dubs_only, aes(x = D_Chla, y = D_CF)) +
    labs(x = expression(paste('Chla Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

leg <- get_legend(Q)

(CF_reg <- plot_grid(PAR + theme(legend.position = "none"),
                     Q + theme(legend.position = "none"),
                     Chla + theme(legend.position = "none"),
                     Tmax + theme(legend.position = "none"),
                     labels = c("a", "b", "c", "d"), nrow = 2) )

CF_reg_leg<-plot_grid(CF_reg, leg, nrow = 1, rel_widths = c(1, 0.15))

ggsave("CF Exp Var Regressions.png", CF_reg_leg, 
       path = "~/Desktop/", width = 10, height = 10)


####### ALL BUGS #########
(Q <- ggplot(data = Dubs_only, aes(x = Q, y = D_All)) +
    labs(x = expression(paste('Discharge')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Tmax <- ggplot(data = Dubs_only, aes(x = D_7max, y = D_All)) +
    labs(x = expression(paste('Max Temperature Response')),
         y = "") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(PAR <- ggplot(data = Dubs_only, aes(x = D_PAR, y = D_All)) +
    labs(x = expression(paste('PAR Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

(Chla <- ggplot(data = Dubs_only, aes(x = D_Chla, y = D_All)) +
    labs(x = expression(paste('Chla Response')),
         y = "Total Scraper Density Response") +
    theme_bw(base_size = 14) + 
    geom_smooth(method = 'lm', size = .25, se = T) + 
    geom_point(aes(shape = Stream), size = 2) )

leg <- get_legend(Q)

(All_reg <- plot_grid(PAR + theme(legend.position = "none"),
                     Q + theme(legend.position = "none"),
                     Chla + theme(legend.position = "none"),
                     Tmax + theme(legend.position = "none"),
                     labels = c("a", "b", "c", "d"), nrow = 2) )

All_reg_leg<-plot_grid(All_reg, leg, nrow = 1, rel_widths = c(1, 0.15))

ggsave("All Exp Var Regressions.png", All_reg_leg, 
       path = "~/Desktop/", width = 10, height = 10)




##### LINEAR MODELS ########

mod_light<- lm(D_SC ~ D_PAR, data = Dubs_only)
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





####### T-TESTS ##########
t.test(d_Chla ~ Year, data = Diffs_only, paired = T)
t.test(d_PAR ~ Year, data = Diffs_only, paired = T)

t.test(d_SC ~ Year, data = Diffs_only, paired = T)
t.test(d_SCe ~ Year, data = Diffs_only, paired = T)
t.test(d_SCi ~ Year, data = Diffs_only, paired = T)
t.test(d_P ~ Year, data = Diffs_only, paired = T)
t.test(d_SH ~ Year, data = Diffs_only, paired = T)
t.test(d_CG ~ Year, data = Diffs_only, paired = T)

t.test(d_All ~ Year, data = Diffs_only, paired = T)



###### PLOT OF BACI RESPONSE FOR CHLA, PAR, AND TOTAL INVERTS #####
PAR <- tibble("Mean_PAR" = 
                c(mean(Diffs_only$d_PAR[Diffs_only$Year == 2017]),
                     mean(Diffs_only$d_PAR[Diffs_only$Year == 2018])), 
               "SD_PAR" = c(sd(Diffs_only$d_PAR[Diffs_only$Year == 2017]), 
                               sd(Diffs_only$d_PAR[Diffs_only$Year == 2018])),
               "Year" = as.factor(c(2017, 2018))
              )

Chla <- tibble("Mean_Chla" = 
                 c(mean(Diffs_only$d_Chla[Diffs_only$Year == 2017]),
                      mean(Diffs_only$d_Chla[Diffs_only$Year == 2018])), 
               "SD_Chla" = c(sd(Diffs_only$d_Chla[Diffs_only$Year == 2017]), 
                               sd(Diffs_only$d_Chla[Diffs_only$Year == 2018])),
               "Year" = as.factor(c(2017, 2018))
)

All <- tibble("Mean_All" = 
                c(mean(Diffs_only$d_All[Diffs_only$Year == 2017]),
                     mean(Diffs_only$d_All[Diffs_only$Year == 2018])), 
              "SD_All" = c(sd(Diffs_only$d_All[Diffs_only$Year == 2017]), 
                              sd(Diffs_only$d_All[Diffs_only$Year == 2018])),
              "Year" = as.factor(c(2017, 2018))
)

SC <- tibble("Mean_SC" = 
                c(mean(Diffs_only$d_SC[Diffs_only$Year == 2017]),
                     mean(Diffs_only$d_SC[Diffs_only$Year == 2018])), 
              "SD_SC" = c(sd(Diffs_only$d_SC[Diffs_only$Year == 2017]), 
                              sd(Diffs_only$d_SC[Diffs_only$Year == 2018])),
              "Year" = as.factor(c(2017, 2018))
)

EPT <- tibble("Mean_EPT" = 
               c(mean(Diffs_only$EPT[Diffs_only$Year == 2017]),
                 mean(Diffs_only$EPT[Diffs_only$Year == 2018])), 
             "SD_EPT" = c(sd(Diffs_only$EPT[Diffs_only$Year == 2017]), 
                         sd(Diffs_only$EPT[Diffs_only$Year == 2018])),
             "Year" = as.factor(c(2017, 2018))
)


(PAR_plot <- ggplot(data = PAR, aes(x = Year, y = Mean_PAR, group = Year)) +
   labs(x = expression(paste('Year')),
        y = bquote("PAR " (mol ~m^-2 ~day^-1))) +
   theme_bw(base_size = 14) + 
   geom_col(aes(fill = Year)) +
   geom_errorbar(aes(x = Year, ymin = Mean_PAR - ((SD_PAR*2.345)/sqrt(5)), 
                     ymax = Mean_PAR + ((SD_PAR*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    theme_bw(base_family = "LM Roman 10") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
          legend.title = element_text(face = "bold")) + 
    scale_fill_viridis_d(option = "E", end = .9) +
    scale_color_viridis_d(option = "E", end = .9) )

(Chla_plot <- ggplot(data = Chla, aes(x = Year, y = Mean_Chla)) +
    labs(x = expression(paste('Year')),
         y = bquote("Chla " (ug ~cm^-2))) +
    theme_bw(base_size = 14) + 
    geom_col(aes(fill = Year)) +
    geom_errorbar(aes(x = Year, ymin = Mean_Chla - ((SD_Chla*2.345)/sqrt(5)), 
                      ymax = Mean_Chla + ((SD_Chla*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    theme_bw(base_family = "LM Roman 10") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
          legend.title = element_text(face = "bold")) + 
    scale_fill_viridis_d(option = "E", end = .9) +
    scale_color_viridis_d(option = "E", end = .9) )

(All_plot <- ggplot(data = All, aes(x = Year, y = Mean_All)) +
    labs(x = expression(paste('Year')),
         y = bquote("Total Invertebrate Density " (~n m^-2))) +
    theme_bw(base_size = 14) + 
    geom_col(aes(fill = Year)) +
    geom_errorbar(aes(x = Year, ymin = Mean_All - ((SD_All*2.345)/sqrt(5)), 
                      ymax = Mean_All + ((SD_All*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    theme_bw(base_family = "LM Roman 10") +
    theme_bw(base_family = "LM Roman 10") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
          legend.title = element_text(face = "bold")) + 
    scale_fill_viridis_d(option = "E", end = .9) +
    scale_color_viridis_d(option = "E", end = .9) )

(SC_plot <- ggplot(data = SC, aes(x = Year, y = Mean_SC)) +
    labs(x = expression(paste('Year')),
         y = "Total Scrapers") +
    theme_bw(base_size = 14) + 
    geom_col(aes(fill = Year)) +
    geom_errorbar(aes(x = Year, ymin = Mean_SC - ((SD_SC*2.345)/sqrt(5)), 
                      ymax = Mean_SC + ((SD_SC*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    theme_bw(base_family = "LM Roman 10") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
          legend.title = element_text(face = "bold")) + 
    scale_fill_viridis_d(option = "E", end = .9) +
    scale_color_viridis_d(option = "E", end = .9) )

(EPT_plot <- ggplot(data = EPT, aes(x = Year, y = Mean_EPT)) +
    labs(x = expression(paste('Year')),
         y = "EPT Index") +
    theme_bw(base_size = 14) + 
    geom_col(aes(fill = Year)) +
    geom_errorbar(aes(x = Year, ymin = Mean_EPT - ((SD_EPT*2.345)/sqrt(5)), 
                      ymax = Mean_EPT + ((SD_EPT*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    theme_bw(base_family = "LM Roman 10") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
          legend.title = element_text(face = "bold")) + 
    scale_fill_viridis_d(option = "E", end = .9) +
    scale_color_viridis_d(option = "E", end = .9) )

baci_leg <- get_legend(PAR_plot)

(baci_plots <- plot_grid(PAR_plot + theme(legend.position = "none"),
                      Chla_plot + theme(legend.position = "none"),
                      All_plot + theme(legend.position = "none"),
                      EPT_plot + theme(legend.position = "none"),
                      labels = c("a", "b", "c", "d"), nrow = 1) )

All_baci <- plot_grid(baci_plots, baci_leg, nrow = 1, rel_widths = c(8,1))

ggsave("Vars_Reach_Diffs.png", All_baci, 
       path = "~/Desktop/", width = 8, height = 4)









