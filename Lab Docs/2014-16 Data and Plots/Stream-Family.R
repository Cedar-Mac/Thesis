install.packages("vegan")
library(tidyverse)
library(mosaic)
library(magrittr)
library(vegan)

##################################################################
master <- INVERT_MASTER_2014_CEDAR

CHUCK <- dplyr::filter(master, Stream == "CHUCK")
COOK <- dplyr::filter(master, Stream == "COOK")
FRITZ <- dplyr::filter(master, Stream == "FRITZ")
IND <- dplyr::filter(master, Stream == "IND")
LO701 <- dplyr::filter(master, Stream == "LO701")
LO703 <- dplyr::filter(master, Stream == "LO703")
LOON <- dplyr::filter(master, Stream == "LOON")
MACK <- dplyr::filter(master, Stream == "MACK")
MCT_E <- dplyr::filter(master, Stream == "MCT_E")
MCT_W <- dplyr::filter(master, Stream == "MCT_W")
MR404 <- dplyr::filter(master, Stream == "MR404")
STR <- dplyr::filter(master, Stream == "STR")

ggplot(data = MR404) + 
  geom_bar(mapping = aes(x = Family, fill = Reach), position = "fill") +
  coord_flip()


  facet_wrap(~Reach, ncol = 1) +
  coord_flip()
  
##################################################################
bugs <- INVERT_MASTER_2014_CEDAR
  
COOK_OG <- dplyr::filter(COOK, Stream == "COOK" & Reach == "OG")   
  COOK_OG <- table(COOK_OG[, 9] )
  
COOK_SG <- dplyr::filter(COOK, Stream == "COOK" & Reach == "SG")   
  COOK_SG <- table(COOK_SG[, 9] )
  
FRITZ_OG <- dplyr::filter(FRITZ, Stream == "FRITZ" & Reach == "OG")
  FRITZ_OG <- table(FRITZ_OG[, 9])
  
FRITZ_SG <- dplyr::filter(FRITZ, Stream == "FRITZ" & Reach == "SG")
  FRITZ_SG <- table(FRITZ_SG[, 9])
  
LO701_OG <- dplyr::filter(LO701, Stream == "LO701" & Reach == "OG")
  LO701_OG <- table(LO701_OG[, 9])
  
LO701_SG <- dplyr::filter(LO701, Stream == "LO701" & Reach == "SG")
  LO701_SG <- table(LO701_SG[, 9])
  
LO703_OG <- dplyr::filter(LO703, Stream == "LO703" & Reach == "OG")
  LO703_OG <- table(LO703_OG[, 9])

LO703_SG <- dplyr::filter(LO703, Stream == "LO703" & Reach == "SG")
  LO701_SG <- table(LO703_SG[, 9])

MACK_OG <- dplyr::filter(MACK, Stream == "MACK" & Reach == "OG")
  MACK_OG <- table(MACK_OG[, 9])
  
MACK_SG <- dplyr::filter(MACK, Stream == "MACK" & Reach == "SG")
  MACK_SG <- table(MACK_SG[, 9])
  
MCT_E_OG <- dplyr::filter(MCT_E, Stream == "MCT_E" & Reach == "OG")
  MCT_E_OG <- table(MCT_E_OG[, 9])
  
MCT_E_SG <- dplyr::filter(MCT_E, Stream == "MCT_E" & Reach == "SG")
  MCT_E_SG <- table(MCT_E_SG[, 9])
  
MCT_E_TRANS <- dplyr::filter(MCT_E, Stream == "MCT_E" & Reach == "TRANS")
  MCT_E_TRANS <- table(MCT_E_TRANS[, 9])
  
MCT_W_OG <- dplyr::filter(MCT_W, Stream == "MCT_W" & Reach == "OG")
  MCT_W_OG <- table(MCT_W_OG[, 9])
  
MCT_W_SG <- dplyr::filter(MCT_W, Stream == "MCT_W" & Reach == "SG")
  MCT_W_SG <- table(MCT_W_SG[, 9])
 
MR404_OG <- dplyr::filter(MR404, Stream == "MR404" & Reach == "OG")
  MR404_OG <- table(MR404_OG[, 9])
  
MR404_SG <- dplyr::filter(MR404, Stream == "MR404" & Reach == "SG")
  MR404_SG <- table(MR404_SG[, 9])
  
STR_OG <- dplyr::filter(STR, Stream == "STR" & Reach == "OG")
  STR_OG <- table(STR_OG[, 9])
  
STR_SG <- dplyr::filter(STR, Stream == "STR" & Reach == "SG")
  STR_SG <- table(STR_SG[, 9])
  
STR_SG
  
  
a <- as.tibble(STR_SG, MR404_SG, MCT_E_SG, MCT_W_SG, MACK_SG, 
      LO701_SG, LO703_SG, COOK_SG, FRITZ_SG)

a <- t(a)
colnames(a) <- a[1,]
a <- a[2,]
a

b <- as.tibble(STR_OG, MR404_OG, MCT_E_OG, MCT_W_OG, MACK_OG, 
               LO701_OG, LO703_OG, COOK_OG, FRITZ_OG)

b <- t(b)
colnames(b) <- b[1,]
b <- b[2,]
b

c <- cbind(STR[,9], MR404[,9], MCT_E[,9], MCT_W[,9], MACK[,9], 
          LO701[,9], LO703[,9], COOK[,9], FRITZ[,9])
