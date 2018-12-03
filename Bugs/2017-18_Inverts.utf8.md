---
title: "2017-18 Benthic Community Analysis"
author: "Cedar Mackaness"
date: "12/01/2018"
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
## Purpose of This Document 
***
Working with 2017-18 benthic invertebrate data to calculate and plot:

- Ratio's and Differences of density by family

- Ratio's and Differences of density by FFG

- Changes in scaper taxa community

- NMDS analysis

## Sections {.tabset .tabset-fade .tabset-pills}
***
<br>






### Setup

Do some basic math to calculate density of each taxon...


```r
#Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Taxon) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3
```

Here we divide Count by percent subsampled (actually a fraction) to get a count for the total sample taken.  We then divide by .09 which is the area of the surber sampler (in m$^2$). We then aggregate and divide by three (There were three samples taken per reach).







A quick taste of the finished data table product:

preserve6793f2026f8fbb9e


### Taxa
***
![](2017-18_Inverts_files/figure-html/Plot Taxon Density Diffs-1.png)<!-- -->

Plot of density differences between reaches for both 2017 and 2018. As seen, there are no ubiquitous taxa that consistently increase or decrease across streams. Can also see how shitty the plot is, too many taxa...


### FFG's
***






![CF = Collector Filterer, CG = Collector Gatherer, SC = Scrapers (a composite of SCe and SCi), SCe = Scrapers that are edible, SCi = Scrapers that are inedible, P = Predators](2017-18_Inverts_files/figure-html/Plot FFG Density Differences-1.png)

Here we plot density differences of FFG's.  

 
Notice the only group with consistently elevated differences in the post gap year (2018) is Scrapers.


![](2017-18_Inverts_files/figure-html/Plot 2018 FFG Densities-1.png)<!-- -->

Here we can see that Collector Gatherers are the most abundant taxa, but the aggregate of the two scraper categories (SCe and SCi) is also up there. 

When comparing between the reaches we see elevated abundances of both scraper groups (except for SCi in CHUCK).  Collector Gatherers are also consistently elevated (except in CHUCK), which seems contradictory to the between year comparison (Control reach has always had elevated abundances of CG's?)

- Will these functional feeding groups be important in diets? 

- Does the relative change in abundance of a taxa group change fish selection  (i.e. now that there are more scrapers in the treatment reach are the fish going to town on Scrapers?)

- TBD. (see other.md file in the Diets project)


![](2017-18_Inverts_files/figure-html/Scraper Taxa Density Differences-1.png)<!-- -->

Here we see that the Scraper taxa with the greatest change are: 

- Micrasema

- Juga

- Glossosoma

- Drunella

- Heptageniidae taxa


![](2017-18_Inverts_files/figure-html/Plot 2018 Scraper Taxa-1.png)<!-- -->






![](2017-18_Inverts_files/figure-html/Plot Size Differences-1.png)<!-- -->

Two size classes are plotted, small (S) and large (L) based on nothing, just observation... Hopefully I will soon incorporate some information from the Poff database about final larval instar size and have three size classes: small, medium, and large.


### NMDS 	:metal:
***
Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570.  I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.








```
## Square root transformation
## Wisconsin double standardization
## Run 0 stress 0.1726121 
## Run 1 stress 0.1726121 
## ... New best solution
## ... Procrustes: rmse 1.424336e-05  max resid 3.192359e-05 
## ... Similar to previous best
## Run 2 stress 0.1726121 
## ... Procrustes: rmse 3.807849e-06  max resid 1.206738e-05 
## ... Similar to previous best
## Run 3 stress 0.2544354 
## Run 4 stress 0.1726121 
## ... Procrustes: rmse 1.554826e-05  max resid 3.523409e-05 
## ... Similar to previous best
## Run 5 stress 0.1786933 
## Run 6 stress 0.1726121 
## ... Procrustes: rmse 2.053972e-05  max resid 4.950695e-05 
## ... Similar to previous best
## Run 7 stress 0.1726121 
## ... Procrustes: rmse 7.488264e-06  max resid 1.605228e-05 
## ... Similar to previous best
## Run 8 stress 0.1726121 
## ... Procrustes: rmse 4.657109e-05  max resid 0.0001128104 
## ... Similar to previous best
## Run 9 stress 0.1726123 
## ... Procrustes: rmse 0.0001063858  max resid 0.0002334147 
## ... Similar to previous best
## Run 10 stress 0.1726121 
## ... Procrustes: rmse 4.810629e-05  max resid 0.0001144586 
## ... Similar to previous best
## Run 11 stress 0.1738211 
## Run 12 stress 0.1726121 
## ... Procrustes: rmse 6.173488e-06  max resid 1.639791e-05 
## ... Similar to previous best
## Run 13 stress 0.1726121 
## ... Procrustes: rmse 1.083619e-05  max resid 2.241171e-05 
## ... Similar to previous best
## Run 14 stress 0.1726121 
## ... Procrustes: rmse 4.214216e-06  max resid 8.744222e-06 
## ... Similar to previous best
## Run 15 stress 0.1726121 
## ... Procrustes: rmse 1.07112e-05  max resid 2.377102e-05 
## ... Similar to previous best
## Run 16 stress 0.1726121 
## ... Procrustes: rmse 1.594841e-05  max resid 5.036414e-05 
## ... Similar to previous best
## Run 17 stress 0.1726121 
## ... Procrustes: rmse 1.840912e-05  max resid 4.120888e-05 
## ... Similar to previous best
## Run 18 stress 0.1738211 
## Run 19 stress 0.1726121 
## ... Procrustes: rmse 3.199781e-05  max resid 6.98212e-05 
## ... Similar to previous best
## Run 20 stress 0.1726121 
## ... Procrustes: rmse 9.438625e-06  max resid 1.809173e-05 
## ... Similar to previous best
## *** Solution reached
```

This is a pretty good stress value, who knows how that will change when I start doing this right...
Can see the transfrmations applied at the top of the output, this was done automatically, I wouldn't know what to do. Used the Bray-Curtis distance metric, max tries is set to 100 with dimensions equal to 2.









The "display" argument can be set to "site" or "species" depending on what you want to group. "kind" can be standard deviation or standard error, not sure which to use here.


```r
# Fit the ellipse function to actual data
df_ell <- data.frame()
for(g in NMDS$YearTreat){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$YearTreat == g,],
                  vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                ,YearTreat = g))
}
```

not going to pretend like I know how this works, got it from https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo 
but I do know that changing the column selected from NMDS changes which variable is used for grouping.

![](2017-18_Inverts_files/figure-html/Plot Benthic NMDS-1.png)<!-- -->

Actually maybe cool result, all of the treatment reaches now plot closer post-gap year. The controls show a similar pattern though maybe a little less pronounced. Everything also shifted left between the two years, although this isn't because of the treatment, just annual changes. Maybe the gap amplified whatever effect the year had, extra, extra light or temperature maybe.



```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.14
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2    DT_0.5            viridis_0.5.1    
##  [4] viridisLite_0.3.0 ggthemes_4.0.1    readxl_1.1.0     
##  [7] lubridate_1.7.4   vegan_2.5-2       lattice_0.20-35  
## [10] permute_0.9-4     forcats_0.3.0     stringr_1.3.1    
## [13] dplyr_0.7.5       purrr_0.2.5       readr_1.1.1      
## [16] tidyr_0.8.1       tibble_1.4.2      ggplot2_3.0.0    
## [19] tidyverse_1.2.1  
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.3.1       jsonlite_1.5     modelr_0.1.2     shiny_1.1.0     
##  [5] assertthat_0.2.0 highr_0.7        cellranger_1.1.0 yaml_2.1.19     
##  [9] pillar_1.2.3     backports_1.1.2  glue_1.2.0       digest_0.6.15   
## [13] promises_1.0.1   rvest_0.3.2      colorspace_1.3-2 htmltools_0.3.6 
## [17] httpuv_1.4.3     Matrix_1.2-14    plyr_1.8.4       psych_1.8.4     
## [21] pkgconfig_2.0.1  broom_0.4.4      haven_1.1.1      xtable_1.8-2    
## [25] scales_1.0.0     later_0.7.3      mgcv_1.8-24      withr_2.1.2     
## [29] lazyeval_0.2.1   cli_1.0.0        mnormt_1.5-5     magrittr_1.5    
## [33] crayon_1.3.4     mime_0.5         evaluate_0.10.1  nlme_3.1-137    
## [37] MASS_7.3-50      xml2_1.2.0       foreign_0.8-70   tools_3.5.1     
## [41] hms_0.4.2        munsell_0.5.0    cluster_2.0.7-1  compiler_3.5.1  
## [45] rlang_0.2.1      grid_3.5.1       rstudioapi_0.7   htmlwidgets_1.3 
## [49] crosstalk_1.0.0  labeling_0.3     rmarkdown_1.10   gtable_0.2.0    
## [53] reshape2_1.4.3   R6_2.2.2         gridExtra_2.3    knitr_1.20      
## [57] bindr_0.1.1      rprojroot_1.3-2  stringi_1.2.3    parallel_3.5.1  
## [61] Rcpp_0.12.17     tidyselect_0.2.4
```

