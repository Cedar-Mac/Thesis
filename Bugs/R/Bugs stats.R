# Statistical Analyses

# load required packages
x <- c("tidyverse", "vegan", "lubridate", "readxl", "ggthemes", "vegan", "viridis", 
       "DT", "ggrepel", "lme4", "simr", "gmodels", "nlme")
lapply(x, library, character.only = TRUE)

# Import Data
bugs <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Cleaned", 
                          col_types = c("date", "guess", "guess", "guess","guess", "guess", "guess",
                                        "guess", "guess", "guess","guess", "guess"))
ffg <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "FFG")
sizes <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Size")
Diets <- readxl::read_xlsx("./Data/2018 Diets.xlsx")
Bentho <- readxl::read_xlsx("./Data/2018 Tiles Allison's Computer.xlsx", sheet = "2017 and 2018")
FL <- readxl::read_xlsx("./Data/2018_Fluorescein.xlsx", sheet = "Compiled")
Enviro <- readxl::read_xlsx("./Data/benthic2.xlsx")
#Replace Ssp. in Taxon with Family
bugs$Taxon[bugs$Taxon == "Ssp."] <- bugs$Family[bugs$Taxon == "Ssp."]


########## DATA WRANGLING  #########


###### PAR & Chla ######

# PAR from fluorecein
FL$Stream <- recode(FL$Stream, Loon = "LOON", MCTE = "MCTE", `W-100` = "W-100", `W-113` = "W-113", 
                    Chucksney = "CHUCK")
FL$Stream <- factor(FL$Stream, levels = c("LOON", "MCTE", "W-113", "W-100", "CHUCK", "W-122"))
FL$Reach<-as.factor(FL$Reach)
FL$Meter<-as.numeric(FL$Meter)
FL$PAR<- as.numeric(FL$PAR)

PAR.mean <- aggregate(PAR ~ Stream + Reach + Meter, mean, data = FL)

PAR.mean <- PAR.mean %>% filter(Stream != "W-122")

#Chla from bentho torch of tiles

# Change variables to factors.  Re-order with CHUCK at the end
Bentho$Stream <- factor(Bentho$Stream, levels = c("LOON", "MCTE", "W-113", "W-100", "CHUCK", "W-122"))
Bentho$Treatment <- as.factor(Bentho$Treatment)
Bentho$Meter <- as.factor(Bentho$Meter)
Bentho$BenthoTotal <- as.numeric(Bentho$BenthoTotal)

# Drop W-122
Bentho <- Bentho %>% filter(Stream != "W-122")

# Calculate mean of the three tiles for each meter
bentho.mean <- aggregate(BenthoTotal ~ Stream + Year.Treatment + Meter, mean, data = Bentho)


##### Inverts ######

# Invert Density Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09  #Calculate total in pooled sample

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Taxon) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3    #Divide by number of samples pooled

# Spread and then gather to fill missing Taxons with 0's
bugs.agg <- spread(bugs.agg, key = "Taxon", value = "Density") %>% #bugs.agg is benthic samples
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  gather(key = "Taxon", value = "Density", 4:ncol(.))

# Add FFG and size classes
bugs.agg <- merge(bugs.agg, sizes) %>% merge(., ffg)

bugs.agg <- bugs.agg %>% mutate(StreamTreat = interaction(Stream, Treatment, sep = " "))
bugs.agg <- bugs.agg %>% mutate(YearTreat = interaction(CollDate, Treatment, sep = " "))


# Create list of CollDate, Stream and Taxon
bugs.reach.diff <- bugs.agg %>% select(-c("Density", "CollDate")) %>% unique()

# Calculate Differences
bugs.reach.diff$Diff <- bugs.agg$Density[bugs.agg$CollDate == "2018"] -     
  bugs.agg$Density[bugs.agg$CollDate == "2017"]

# Calculate ratios
bugs.reach.diff$Ratio <- bugs.agg$Density[bugs.agg$Treatment == "Y"] / bugs.agg$Density[bugs.agg$Treatment == "N"]

#Aggregate to get density differences of FFG's
bugs.ffg <- bugs.reach.diff %>% 
  group_by(FFG, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% ungroup

#Add total SC, this throws off counts (scrapers doubly reprented)
ffg.SC <- filter(bugs.ffg, FFG %in% c("SCi", "SCe"))
ffg.SC$FFG <- "SC"
ffg.SC <- ffg.SC %>% group_by(FFG, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% ungroup
ffg.SC <- rbind(bugs.ffg, ffg.SC) 


##### Trout Diets #####

# Fill missing family's with order
Diets$Family[is.na(Diets$Family)] <- Diets$Order[is.na(Diets$Family)]

# Merge diets, sizes and FFG's
Diets <- merge(Diets, sizes, by.x = "Family", by.y = "Taxon") %>% 
  merge(., ffg, by.x = "Family", by.y = "Taxon")

# Aggregate by rep
Diets.by.fish <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size, Rep) %>%
  summarise_at("Count", funs(sum)) %>% ungroup

# Aggregate by reach
Diets.reach <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size) %>%
  summarise_at("Count", funs(sum)) %>% ungroup

# Merge benthic and trout diet data
bugs.family <- bugs %>% 
  filter(CollDate == "2018") %>%
  select(-Taxon) %>% 
  group_by(Stream, Treatment, Family, CollDate) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugndiet <- merge(Diets.reach, bugs.family, all.x = TRUE, all.y = TRUE) %>% 
  filter(Origin == "A") %>%
  mutate_if(is.numeric, replace_na, replace = 0)

Diets.reach$Stream.Treat <- interaction(Diets.reach$Treatment, Diets.reach$Stream)



########## STATISTICS #############

##### PAR & Chla #####

# PAR

# Chla

# Regression
PAR.evens <- PAR.mean %>% filter(Meter %% 2 == 0)
lm(bentho.mean$BenthoTotal ~ PAR.evens)


##### Inverts #####

# Get totals of all bugs and of FFG's
bug_totals <- bugs.agg %>% group_by(Stream, Treatment, CollDate, YearTreat, StreamTreat) %>% 
  summarise_at(vars(Density), sum)
ffg_totals <- bugs.agg %>% group_by(Stream, Treatment, CollDate, YearTreat, StreamTreat, FFG) %>% 
  summarise_at(vars(Density), sum)

# Log transform to improve residuals
bug_totals <- bug_totals %>% mutate(logDensity = log(Density))
ffg_totals <- ffg_totals %>% mutate(logDensity = log(Density))

# 

ffg_scrapers <- ffg_totals %>% filter(FFG == c("SCi", "SCe"))
ffg_SC <- ffg_scrapers %>% group_by(Stream, Treatment, YearTreat, StreamTreat, CollDate) %>% 
  summarise_at(vars(Density, logDensity), sum)
ffg_SC$FFG <- "SC"
ffg_other <- ffg_totals %>% filter(FFG == "P" | FFG == "SH" | FFG == "CF" | FFG == "CG")
ffg_totals <- rbind(ffg_SC, ffg_other)

ffg_scraper_totals <- ffg_scrapers %>% group_by(Stream, Treatment, CollDate, StreamTreat, YearTreat, FFG) %>% 
  summarise_at(vars(Density), sum)

ffg_scraper_totals <- ffg_scraper_totals %>% mutate(logDensity = log(Density))


# Linear model of invert totals. #######
# 1: weight variances by CollDate
# 2: weight variances by Treatment
# 3: No weight of group variances

lmBug1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment , 
            random = ~1|Stream/StreamTreat,
            weights = varIdent(form = ~1|CollDate),
            data = bug_totals) 

lmBug2 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment , 
              random = ~1|Stream/StreamTreat,
              weights = varIdent(form = ~1|Treatment), 
              data = bug_totals) 

lmBug3 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment , 
              random = ~1|Stream/StreamTreat, data = bug_totals)

summary(lmBug1)
anova(lmBug1)
plot(lmBug1)
bug_totals$res1 <- residuals(lmBug1, type = "pearson")

summary(lmBug2)
anova(lmBug2)
plot(lmBug2)
bug_totals$res2 <- residuals(lmBug2, type = "pearson")

summary(lmBug3)
anova(lmBug3)
plot(lmBug3)
bug_totals$res3 <- residuals(lmBug3, type = "pearson")

bug_totals_mean <- data_summary(data = bug_totals, varname = "Density", 
                                groupnames = c("Treatment", "CollDate"))
ffg_totals_mean <- data_summary(data = ffg_totals, varname = "Density", 
                                groupnames = c("Treatment", "CollDate", "FFG"))
ffg_SC_mean <- data_summary(data = ffg_scrapers, varname = "Density", 
                            groupnames = c("Treatment", "CollDate", "FFG"))


gbugs = ggplot(bug_totals_mean, aes(y = Mean, x = CollDate, linetype = Treatment)) +
    # Define stock as group this week as well as set x and y axes
    geom_point(position = position_dodge(width = .75)) + # Add points, dodge by group
    geom_errorbar(aes(ymax = Mean - SD, ymin = Mean + SD),
                  position = position_dodge(width = .75)) + # Add errorbars, dodge by group
    theme_bw() +
    labs(y = "Difference in Density",
         x = "Year") +
    scale_linetype_manual(values = c(N = "twodash", Y = "solid"),
                          name = "", # Change names in legend
                          labels = c("Control", "Treated")) +
    geom_rect(xmax = Inf, xmin = -Inf, ymax = .25, ymin = -.25,
              fill = "grey54", alpha = .05) + # Add grey rectangle
    theme(legend.position = c(.25, .8), # change legend position
          legend.direction = "horizontal", # make legend horiz
          panel.grid.minor = element_blank()) # Remove gridlines
plot(gbugs)

gffg = ggplot(ffg_totals_mean, aes(y = Mean, x = CollDate, linetype = Treatment)) +
  # Define stock as group this week as well as set x and y axes
  geom_point(position = position_dodge(width = .75)) + # Add points, dodge by group
  geom_errorbar(aes(ymax = Mean - SD, ymin = Mean + SD),
                position = position_dodge(width = .75)) + # Add errorbars, dodge by group
  theme_bw() +
  labs(y = "Difference in Density",
       x = "Year") +
  scale_linetype_manual(values = c(N = "twodash", Y = "solid"),
                        name = "", # Change names in legend
                        labels = c("Control", "Treated")) +
  geom_rect(xmax = Inf, xmin = -Inf, ymax = .25, ymin = -.25,
            fill = "grey54", alpha = .05) + # Add grey rectangle
  theme(legend.position = c(.25, .8), # change legend position
        legend.direction = "horizontal", # make legend horiz
        panel.grid.minor = element_blank()) + # Remove gridlines
  facet_wrap("FFG")
plot(gffg)

gSC = ggplot(ffg_SC_mean, aes(y = Mean, x = CollDate, linetype = Treatment)) +
  # Define stock as group this week as well as set x and y axes
  geom_point(position = position_dodge(width = .75)) + # Add points, dodge by group
  geom_errorbar(aes(ymax = Mean - SD, ymin = Mean + SD),
                position = position_dodge(width = .75)) + # Add errorbars, dodge by group
  theme_bw() +
  labs(y = "Difference in Density",
       x = "Year") +
  scale_linetype_manual(values = c(N = "twodash", Y = "solid"),
                        name = "", # Change names in legend
                        labels = c("Control", "Treated")) +
  geom_rect(xmax = Inf, xmin = -Inf, ymax = .25, ymin = -.25,
            fill = "grey54", alpha = .05) + # Add grey rectangle
  theme(legend.position = c(.25, .8), # change legend position
        legend.direction = "horizontal", # make legend horiz
        panel.grid.minor = element_blank()) + # Remove gridlines
  facet_wrap("FFG")
plot(gSC)


#Linear models of ffg totals
#1: log transform
#2: raw data
#3: Weight variances of Treatement groups, log transform
#4: Weight variances of Treatement groups, raw
lmSC1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
        random = ~1|Stream/StreamTreat,
        subset = FFG == "SC", data = ffg_totals) 
summary(lmSC1)
plot(lmSC1)

lmSCi1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
             random = ~1|Stream/StreamTreat,
             subset = FFG == "SCi", data = ffg_totals) 
summary(lmSCi1)
plot(lmSCi1)

lmCG1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
             random = ~1|Stream/StreamTreat,
             subset = FFG == "CG", data = ffg_totals) 
summary(lmCG1)
plot(lmCG1)

lmP1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
            random = ~1|Stream/StreamTreat,
            subset = FFG == "P", data = ffg_totals) 
summary(lmP1)
plot(lmP1)

lmSH1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
             random = ~1|Stream/StreamTreat,
             subset = FFG == "SH", data = ffg_totals) 
summary(lmSH1)
plot(lmSH1)

lmCF1 <- lme(logDensity ~ CollDate + Treatment + CollDate:Treatment, 
            random = ~1|Stream/StreamTreat,
            subset = FFG == "CF", data = ffg_totals) 
summary(lmCF1)
plot(lmCF1)

# linear model with FFG explanatory variable
lmFFG1 <- lme(logDensity ~ CollDate + Treatment + FFG + CollDate:Treatment, 
            random = ~1|Stream/StreamTreat, data = ffg_totals) 
summary(lmFFG1)
plot(lmFFG1)

#



# T-tests, bar charts using mean values of streams #######
bugs_18 <- bug_totals %>% filter(CollDate == 2018)
bugs_17 <- bug_totals %>% filter(CollDate == 2017)
bdiff <- bugs_18$Density - bugs_17$Density
bug_diffs <- bugs_18 %>% ungroup() %>% select(Stream, Treatment, StreamTreat) 
bug_diffs <- cbind(bug_diffs, bdiff)

# Summary function that provides mean and SD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      SD = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = "Mean"))
  return(data_sum)
}

bug_mean <- data_summary(bug_diffs, varname = "bdiff", groupnames = "Treatment")

ggplot(data = bug_mean, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width =.2,
                position = position_dodge(.9))


# Same as above, but for FFG's
ffg_18 <- ffg_totals %>% filter(CollDate == 2018)
ffg_17 <- ffg_totals %>% filter(CollDate == 2017)
fdiff <- ffg_18$Density - ffg_17$Density
ffg_diffs <- ffg_18 %>% ungroup() %>% select(Stream, Treatment, StreamTreat, FFG) 
ffg_diffs <- cbind(ffg_diffs, fdiff)
ffg_diffs <- ffg_diffs %>% mutate(FFGTreat = interaction(Treatment, FFG, sep = " "))

ffg_mean <- data_summary(ffg_diffs, varname = "fdiff", groupnames = c("FFGTreat", "Treatment"))

ggplot(data = ffg_mean, aes(x = FFGTreat, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width =.2,
                position = position_dodge(.9))


lmBugMeans <- lm(bdiff ~ Treatment, data = bug_diffs)
summary(lmBugMeans)
plot(lmBugMeans)

lmFFGMeans <- lm(fdiff ~ FFG + Treatment, data = ffg_diffs)
summary(lmFFGMeans)

ffg_mean <- ffg_mean %>% mutate(FFG = substr(FFGTreat, start = 3, stop = 7))

sc_diffs <- ffg_diffs %>% filter(FFG == "SC")
t.test(fdiff ~ Treatment, data = sc_diffs)

CF_diffs <- ffg_diffs %>% filter(FFG == "CF")
t.test(fdiff ~ Treatment, data = CF_diffs)


