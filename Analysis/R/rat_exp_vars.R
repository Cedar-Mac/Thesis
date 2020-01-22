
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


####### REACH rat ###################
Refs <- filter(bug_exp,  Treatment == "N")
Trts <- filter(bug_exp, Treatment == "Y")
rat <- merge(Refs, Trts, by = c( "Stream", "Year"))

rat$d_PAR <- (rat$PAR.y / rat$PAR.x)
rat$d_Chla <- (rat$Mean_Chla.y / rat$Mean_Chla.x)
rat$d_SC <- (rat$SC.y / rat$SC.x)
rat$d_SCe <- (rat$SCe.y / rat$SCe.x)
rat$d_SCi <- (rat$SCi.y / rat$SCi.x)
rat$d_All <- (rat$All.y / rat$All.x)
rat$d_CF <- (rat$CF.y / rat$CF.x)
rat$d_P <- (rat$P.y / rat$P.x)
rat$d_SH <- (rat$SH.y / rat$SH.x)
rat$d_CG <- (rat$CG.y / rat$CG.x)
rat$d_7max <- (rat$Max7MovingAMaxT.y / rat$Max7MovingAMaxT.x)

rat_only <- rat %>% select(Stream, Year, d_PAR, d_Chla, d_CF, d_All, d_SCe, d_SCi, d_SC, d_7max, d_P, d_SH, d_CG)

rat_only$EPT <- c(1.1250000, 1.0000000, 1.0769231, 0.8000000, 0.9500000, 0.9333333, 1.3846154, 0.8666667, 0.6842105, 0.9375000)


## Log transform data using log(n. + min(n)) where n is a column
log_rat <- rat_only %>% mutate_at(vars(contains('d_')), funs(log(. + 1)))


####### T-TESTS ##########
t.test(d_Chla ~ Year, data = rat_only, paired = T)
t.test(d_PAR ~ Year, data = rat_only, paired = T)
t.test(d_SC ~ Year, data = rat_only, paired = T)
t.test(d_SCe ~ Year, data = rat_only, paired = T)
t.test(d_SCi ~ Year, data = rat_only, paired = T)
t.test(d_P ~ Year, data = rat_only, paired = T)
t.test(d_SH ~ Year, data = rat_only, paired = T)
t.test(d_CG ~ Year, data = rat_only, paired = T)

t.test(d_All ~ Year, data = rat_only, paired = T)



###### PLOT OF BACI RESPONSE FOR CHLA, PAR, AND TOTAL INVERTS #####
PAR <- tibble("Mean_PAR" = 
                c(mean(rat_only$d_PAR[rat_only$Year == 2017]),
                  mean(rat_only$d_PAR[rat_only$Year == 2018])), 
              "SD_PAR" = c(sd(rat_only$d_PAR[rat_only$Year == 2017]), 
                           sd(rat_only$d_PAR[rat_only$Year == 2018])),
              "Year" = as.factor(c(2017, 2018))
)

Chla <- tibble("Mean_Chla" = 
                 c(mean(rat_only$d_Chla[rat_only$Year == 2017]),
                   mean(rat_only$d_Chla[rat_only$Year == 2018])), 
               "SD_Chla" = c(sd(rat_only$d_Chla[rat_only$Year == 2017]), 
                             sd(rat_only$d_Chla[rat_only$Year == 2018])),
               "Year" = as.factor(c(2017, 2018))
)

All <- tibble("Mean_All" = 
                c(mean(rat_only$d_All[rat_only$Year == 2017]),
                  mean(rat_only$d_All[rat_only$Year == 2018])), 
              "SD_All" = c(sd(rat_only$d_All[rat_only$Year == 2017]), 
                           sd(rat_only$d_All[rat_only$Year == 2018])),
              "Year" = as.factor(c(2017, 2018))
)

SC <- tibble("Mean_SC" = 
               c(mean(rat_only$d_SC[rat_only$Year == 2017]),
                 mean(rat_only$d_SC[rat_only$Year == 2018])), 
             "SD_SC" = c(sd(rat_only$d_SC[rat_only$Year == 2017]), 
                         sd(rat_only$d_SC[rat_only$Year == 2018])),
             "Year" = as.factor(c(2017, 2018))
)

EPT <- tibble("Mean_EPT" = 
                c(mean(rat_only$EPT[rat_only$Year == 2017]),
                  mean(rat_only$EPT[rat_only$Year == 2018])), 
              "SD_EPT" = c(sd(rat_only$EPT[rat_only$Year == 2017]), 
                           sd(rat_only$EPT[rat_only$Year == 2018])),
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
    ylim(-.5,6) +
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
    ylim(-.5,6) +
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
         y = bquote("Total Invertebrate Density " (~m^-2))) +
    theme_bw(base_size = 14) + 
    geom_col(aes(fill = Year)) +
    geom_errorbar(aes(x = Year, ymin = Mean_All - ((SD_All*2.345)/sqrt(5)), 
                      ymax = Mean_All + ((SD_All*2.345)/sqrt(5)), color = Year)) +
    geom_hline(yintercept = 0, color = "black", size = .5) +
    ylim(-.5,6) +
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
    ylim(-.5,6) +
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
    ylim(-.5,6) +
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

ggsave("Vars_Reach_rat.png", All_baci, 
       path = "~/Desktop/", width = 8, height = 4)




