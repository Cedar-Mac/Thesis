---
title: "Diet Analyses"
author: "Cedar Mackaness"
date: "11/21/2018"
output:
  html_document:
    theme: sandstone
    highlight: tango
    keep_md: true
    includes:
      before_body: header.html
      after_body: footer.html
      css: styles.css
    code_folding: "hide"
---

### Purpose of This Document
***

The goals here are to:

- Calculate the percent of each taxa in each fish's diet 

- Calculate the percent of fish with each specific taxa in its diet

- Do some NMDS up in heya

- Do some statistical analysis (TBD)

### Setup
***









```r
#Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Taxon) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3
```

Here we divide Count by percent subsampled (actually a fraction) to get a count for the total sample taken.  We then divide by .09 which is the area of the surber sampler (in m$^2$). After these adjustments have been made and the data aggregated (remember the data is structured so that each individual bug gets its own row, i.e. multiple occurances of "*Baetis*") we divide by three because at this point the three samples per reach in 2017 have been aggregated together and 2018 samples have been adjusted to the same value when dividing by percent sub.  (There were three samples taken per reach).













## Plots of Benthic Vs. Diet Community Composition {.tabset .tabset-fade .tabset-pills}
***

### By Family
***

![](Diets_files/figure-html/Plot Taxa Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual taxon in the benthic community versus the proportional abundance of the aggregate diets for that reach. 

Here we see that Chironomidae typically make up the greatest abundance of both the fish diets and the benthic community, with LOON being the exception (Brachycentridae are equally represented in the diets of control reach and over represented in the treatment). In MCTE, Juga compose a large part of the diets relative to their abundance due to one outlier fish. 

There are no overarching differences in how a taxa maps out given the treatment, but the difference in Brachycentridae in LOON between treatment and control is interesting.

### By FFG
***

![](Diets_files/figure-html/Plot FFG Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual FFG in the benthic community versus the proportional abundance of the aggregate diets for that reach.

We see that Collector Gatherers and Shredders are the most abundant both in diets and in the benthic community.  There aren't any over arching trends in how FFG's fall out by treatment.


### By Size Class
***

![](Diets_files/figure-html/Plot Size Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual size class in the benthic community versus the proportional abundance of the aggregate diets for that reach.


### Costello method plot
***




![](Diets_files/figure-html/Plot Costello-1.png)<!-- -->

The Costello method plots % occurance of a taxon in fish versus % of aggregate diet. With the adjusted method the aggregate diet is only of fish that had the taxon present (ignoring zero values).  We see that in MCTE and in W-100 there seem to be just a couple fish that specifically target Juga and ELmidae respectively. In MCTE, the taxa from treatment diets seem to consistently constitute a smaller portion of the diet than would be expected given how many fish they occur in. 



## NMDS :metal:
***

Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570.  I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.








```
## Wisconsin double standardization
## Run 0 stress 0.09340359 
## Run 1 stress 0.09340359 
## ... Procrustes: rmse 3.488825e-06  max resid 7.331493e-06 
## ... Similar to previous best
## Run 2 stress 0.09340359 
## ... New best solution
## ... Procrustes: rmse 1.165284e-06  max resid 2.430271e-06 
## ... Similar to previous best
## Run 3 stress 0.2523908 
## Run 4 stress 0.09340359 
## ... Procrustes: rmse 1.708905e-05  max resid 2.591004e-05 
## ... Similar to previous best
## Run 5 stress 0.09340359 
## ... Procrustes: rmse 3.974729e-07  max resid 6.55094e-07 
## ... Similar to previous best
## Run 6 stress 0.09340359 
## ... Procrustes: rmse 7.439396e-07  max resid 1.260407e-06 
## ... Similar to previous best
## Run 7 stress 0.09340359 
## ... Procrustes: rmse 7.572334e-07  max resid 1.609224e-06 
## ... Similar to previous best
## Run 8 stress 0.09340359 
## ... New best solution
## ... Procrustes: rmse 6.541678e-07  max resid 9.932951e-07 
## ... Similar to previous best
## Run 9 stress 0.2074019 
## Run 10 stress 0.09340359 
## ... Procrustes: rmse 1.285544e-06  max resid 2.525399e-06 
## ... Similar to previous best
## Run 11 stress 0.09340359 
## ... Procrustes: rmse 1.11413e-06  max resid 1.634846e-06 
## ... Similar to previous best
## Run 12 stress 0.09340359 
## ... Procrustes: rmse 5.855227e-07  max resid 9.205662e-07 
## ... Similar to previous best
## Run 13 stress 0.1908636 
## Run 14 stress 0.2155995 
## Run 15 stress 0.1588546 
## Run 16 stress 0.1588546 
## Run 17 stress 0.09340359 
## ... Procrustes: rmse 1.190975e-06  max resid 2.123139e-06 
## ... Similar to previous best
## Run 18 stress 0.1588546 
## Run 19 stress 0.1612039 
## Run 20 stress 0.09340359 
## ... Procrustes: rmse 5.928013e-07  max resid 9.332766e-07 
## ... Similar to previous best
## *** Solution reached
```










The "display" argument can be set to "site" or "species" depending on what you want to group. "kind" can be standard deviation or standard error, not sure which to use here.


```r
# Fit the ellipse function to actual data
df_ell <- data.frame()
for(g in NMDS$Treatment){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Treatment == g,],
                  vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                ,Treatment = g))
}
```

not going to pretend like I know how this works, got it from https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo 
but I do know that changing the column selected from NMDS changes which variable is used for grouping.

![](Diets_files/figure-html/NMDS Plot6-1.png)<!-- -->

There seems to be total overlap of the diet communities in the control versus treatment reaches.  This seems to be consistent with what we saw in other plots.