rm(list=ls()) 
library(dplyr)
library(ggplot2)
library(readxl)
setwd("/Users/allisonswartz/Google Drive (swartza@oregonstate.edu)/Warren Lab Shared Folder/2018 Summer Data/Periphyton/")
data<- read_xlsx("Oregon State University N-fixation Study 2018 Data.xlsx")
data$Reach.2<- as.factor(data$Reach.2)
data$Time<- as.factor(data$Time)
data$Meter.2<- as.factor(data$Meter.2)
data$Portion.Taxon<- paste(data$Portion,data$Taxon)
data2<- select(as_data_frame(data),Date,Time, Reach.2,Meter.2,Time.Reach, Time.Reach.Meter,Portion,Taxon,Portion.Taxon,Count_final,CellCount_final,HeterocystCount_final,DiatomCyanoCellCount,ProportionExamined,Comment) 
data2$Date<-as.factor(as.character(data2$Date))
data2$Portion<-as.factor(data2$Portion)
data2$Taxon<-as.factor(data2$Taxon)
data2$Time.Reach<- as.factor(data2$Time.Reach)
data2$Time.Reach.Meter<- as.factor(data$Time.Reach.Meter)

data3_all_rmAlDs<-filter(data2, !Portion == "Algae"|!Taxon %in% c("Diatoms","Empty diatom cells"))
data4_Algae_only<- filter(data2, Portion == "Algae")

Alg_Diatoms<- data3_all_rmAlDs %>%
  group_by(Time.Reach,Portion.Taxon) %>%
  summarise(Sum_Count = sum(Count_final),
            Sum_CellCount = sum(CellCount_final),
            Sum_Het = sum(HeterocystCount_final))

Algae<- data4_Algae_only %>%
  group_by(Time.Reach,Portion,Taxon) %>%
  summarise(Sum_Count = sum(Count_final),
            Sum_CellCount = sum(CellCount_final),
            Sum_Het = sum(HeterocystCount_final))

write.csv(Alg_Diatoms,"Reach_Algae_DiatomsSpecified_pca_data.csv")
write.csv(Algae,"Reach_Algae_portionOnly_pca_data.csv")

Alg_Diatoms<- data3_all_rmAlDs %>%
  group_by(Time.Reach.Meter,Portion,Taxon) %>%
  summarise(Sum_Count = sum(Count_final),
            Sum_CellCount = sum(CellCount_final),
            Sum_Het = sum(HeterocystCount_final))

Algae<- data4_Algae_only %>%
  group_by(Time.Reach.Meter,Portion,Taxon) %>%
  summarise(Sum_Count = sum(Count_final),
            Sum_CellCount = sum(CellCount_final),
            Sum_Het = sum(HeterocystCount_final))

