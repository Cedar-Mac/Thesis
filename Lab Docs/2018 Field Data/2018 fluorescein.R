#Load required packages
x <- c("tidyverse", "mosaic", "readxl", "xlsx", "plyr")
lapply(x, library, character.only = TRUE)

#Load data from folder on MY computer, need to change file path otherwise
fl.value <- read_excel("~/Google Drive/Stream Ecology/2018 Field Data/2018 Fluorescein.xlsx", 
                       col_types = c("numeric", "guess", "numeric"), sheet = "Fluorescein Value")
fl.meter <- read_excel("~/Google Drive/Stream Ecology/2018 Field Data/2018 Fluorescein.xlsx", 
                      sheet = "Meter")

#Assign mean values for the controls of each batch from excel sheet
batch.1.control <- 394.035
batch.3.control <- 390.866
batch.4.control <- 411.96

#Create single dataframe with meter and stream info and Fl value
fl <- full_join(fl.meter, fl.value)

#Filter Fl values greater than one sd away from other two reps.
#Currently not sure exactly what functionis doing
fl.test <- ddply(fl, as.quoted(c("Stream", "Reach", "Meter")), function(z) {
  idx <- abs(z$Fl - mean(z$Fl)) < sd(z$Fl)
  z[idx, ]})

#Compute the mean of three reps for each meter
fl.mean <- aggregate(Fl ~ Stream + Reach + Meter, mean, data = fl)
colnames(fl.mean)[4] <- "mean.fl"
fl.mean$sd <- aggregate(Fl ~ Stream + Reach + Meter, sd, data = fl)
#Adjust mean Fl values at each meter to the  variation of the controls
fl.mean$fl_adj <- ifelse(fl.mean$Fl[fl.mean$Stream == "LOON" || fl.mean$Stream == "CHUCK"], 
                  batch.1.control - fl.mean$Fl[fl.mean$Stream == "LOON" || fl.mean$Stream == "CHUCK"],
              ifelse(fl.mean$Fl[fl.mean$Stream == "W-122" || fl.mean$Stream == "W-100"], 
              batch.3.control - fl.mean$Fl[fl.mean$Stream == "W-122" || fl.mean$Stream == "W-100"], 
              batch.4.control - fl.mean$Fl[fl.mean$Stream == "MCTE"]))

#Plot adjusted fl value for each stream
ggplot(data = fl.mean) +
  geom_point(mapping = aes(x = Meter, y = fl_adj, color = Reach)) +
  facet_wrap("Stream") 
  
#Save file
xlsx::write.xlsx(fl, file = paste0(path.expand("~/Desktop/"),"2018 Fluorescein.xlsx"))

fl.sd <- full_join(fl, fl.mean)
abs(fl.sd$Fl - fl.sd$mean.fl) < fl.sd$sd
