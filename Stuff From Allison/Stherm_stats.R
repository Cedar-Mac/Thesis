###########Stream Thermal Metrics##########
rm(list=ls()) 
library(nlme)
library(gmodels)
library(dplyr)
STherm_summary <- read.csv("~/Desktop/STherm_Mag_SUMMER_allsites_2-18-19.csv")
STherm_summary$Stream<-factor(STherm_summary$Stream,level=c("MCTE","W-113","LOON","CHUCK","W-100","W-122"))
STherm_summary$Year.2<-factor(STherm_summary$Year.2,level=c("Pre","Post"))
STherm_summary$PostTRTvar<- as.factor(paste(STherm_summary$Year.2, STherm_summary$Reach.2, sep = ".")) #need this for relaxation of constant variance. 
STherm_summary$Unique.Reach<- as.factor(paste(STherm_summary$Stream, STherm_summary$Reach.2, sep = "."))

STherm_90m<- STherm_summary %>% filter(Meter == 90)


(model3_relaxVar = update(model3, weights = varIdent(form = ~ 1|PostTRTvar) ) ) #relaxes assumption of unequal veriance
plot(ACF(model3))

###### MAX #######
model4<- lme(Max7MovingAMaxT ~ Year.2 + Reach.2 + Year.2:Reach.2 , random = ~1|Stream, data = STherm_90m)
summary(model4)
plot(model4)
(model4_relaxVar = update(model4, weights = varIdent(form = ~ 1|PostTRTvar) ) ) #relaxes assumption of unequal veriance
summary(model4_relaxVar)
plot(model4_relaxVar)

#model 5 is the winner!!!!!!!!!!
model5<-lme(Max7MovingAMaxT ~ Year.2 + Reach.2 + Year.2:Reach.2 , random = ~1|Stream/Unique.Reach, data = STherm_90m) 
#the forward slash with the unique reach factor is correct- see lab 5 pdf #random = ~1|watershed/stand where stand is unique combo of watershed and stand. 
plot(model5)
summary(model5)
#decided residuals for this are worse (more patterned), than 
model5_relaxVar<- update(model5, weights = varIdent(form = ~ 1|PostTRTvar) )
summary(model5_relaxVar)
plot(model5_relaxVar)

#Model 6 is for the max 7 day moving means 
model6<-lme(Max7MovingAMeanT ~ Year.2 + Reach.2 + Year.2:Reach.2 , random = ~1|Stream/Unique.Reach,  data = STherm_90m) 
plot(model6)
summary(model6)
(model6_relaxVar = update(model6, weights = varIdent(form = ~ 1|PostTRTvar) ) ) #relaxes assumption of unequal veriance
summary(model6_relaxVar)
plot(model6_relaxVar)

#just to test
model5a<-lme(Max7MovingAMaxT ~ Year.2*Reach.2 , random = ~1|Stream/Unique.Reach, data = STherm_90m) #this is the same as model5 (just differnet sytax)
summary(model5a)

model5_relaxVar<- update(model4, weights = varIdent(form = ~ 1|PostTRTvar) ) 
plot(model5_relaxVar) #this starts to look worse... I think the residuals were fine before. 
summary(model5_relaxVar)






