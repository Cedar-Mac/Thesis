bugndiet.ffg.rm <- bugndiet.fam %>% filter(Ratio < 500)


# D, Difference selectivity index
bugndiet.ffg.rm <- bugndiet.ffg.rm %>% mutate(D = 
                                (frac.diet - frac.benthic) / (frac.diet + frac.benthic - 2*frac.diet*frac.benthic))


# Calculate average proportion of FFG's in benthos and diet of all streams and avg index values
avg.ffg.diet.rm <- ddply(bugndiet.ffg.rm, .(Treatment, FFG), summarize,
                      Mean.benthic = mean(frac.benthic),
                      SD.benthic = sd(frac.benthic),
                      Mean.diet = mean(frac.diet),
                      SD.diet = sd(frac.diet),
                      Mean.D = mean(D),
                      SD.D = sd(D))



ggplot(aes(x = FFG, y = Mean.D, color = Treatment), data = avg.ffg.diet.rm) +
  geom_point(position = position_dodge(width = .6)) +
  geom_errorbar(aes(ymin = Mean.D - SD.D, ymax = Mean.D + SD.D, color = Treatment), position = position_dodge(width = .6)) +
  labs(
    x = "Functional Feeding Group",
    y = "Difference Selection Index") +
  scale_color_viridis_d(option = "E", end = .9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white", color = "grey"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
        legend.title = element_text(face = "bold"),
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

