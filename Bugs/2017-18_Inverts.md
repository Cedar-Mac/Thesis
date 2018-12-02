2017-18 Densities
================
Cedar Mackaness
12/01/2018

Benthic Community Analysis
--------------------------

Working with 2017-18 benthic invertebrate data to calculate and plot:

-   Ratio's and Differences of density by family

-   Ratio's and Differences of density by FFG

-   Changes in scaper taxa community

-   NMDS analysis

``` r
#Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- aggregate(Density ~ CollDate + Stream + Treatment + Taxon, sum, data = bugs) 

bugs.agg$Density <- bugs.agg$Density / 3
```

Here we divide Count by percent subsampled (actually a fraction) to get a count for the total sample taken. We then divide by .09 which is the area of the surber sampler (in m**<sup>2</sup>). After these adjustments have been made and the data aggregated (remember the data is structured so that each individual bug gets its own row, i.e. multiple occurances of "*Baetis*") we divide by three because at this point the three samples per reach in 2017 have been aggregated together and 2018 samples have been adjusted to the same value when dividing by percent sub. (There were three samples taken per reach).

Taxa
----

![](2017-18_Inverts_files/figure-markdown_github/Plot%20Taxon%20Density%20Diffs-1.png)

Plot of density differences between reaches for both 2017 and 2018. As seen, there are no ubiquitous taxa that consistently increase or decrease across streams. Can also see how shitty the plot is, too many taxa...

FFG's
-----

![](2017-18_Inverts_files/figure-markdown_github/Plot%20FFG%20Density%20Differences-1.png)

Here we plot density differences of FFG's.
- CF = Collector Filterer

-   CG = Collector Gatherer

-   SC = Scrapers (a composite of SCe and SCi)

-   SCe = Scrapers that are edible

-   SCi = Scrapers that are inedible

-   P = Predators

Notice the only group with consistently elevated differences in the post gap year (2018) is Scrapers.

``` r
bugs18.ffg <- bugs18 %>%
  group_by(Stream, Treatment, FFG) %>%
  summarise_at(vars(Density), funs(sum))
```

![](2017-18_Inverts_files/figure-markdown_github/Plot%202018%20FFG%20Densities-1.png)

Here we can see that Collector Gatherers are the most abundant taxa, but the aggregate of the two scraper categories (SCe and SCi) is also up there.

When comparing between the reaches we see elevated abundances of both scraper groups (except for SCi in CHUCK). Collector Gatherers are also consistently elevated (except in CHUCK), which seems contradictory to the between year comparison (Control reach has always had elevated abundances of CG's?)

-   Will these functional feeding groups be important in diets?

-   Does the relative change in abundance of a taxa group change fish selection (i.e. now that there are more scrapers in the treatment reach are the fish going to town on Scrapers?)

-   TBD. (see other.md file in the Diets project)

![](2017-18_Inverts_files/figure-markdown_github/Scraper%20Taxa%20Density%20Differences-1.png)

Here we see that the Scraper taxa with the greatest change are:

-   Micrasema

-   Juga

-   Glossosoma

-   Drunella

-   Heptageniidae taxa

![](2017-18_Inverts_files/figure-markdown_github/Plot%202018%20Scraper%20Taxa-1.png)

![](2017-18_Inverts_files/figure-markdown_github/Plot%20Size%20Differences-1.png)

Two size classes are plotted, small (S) and large (L) based on nothing, just observation... Hopefully I will soon incorporate some information from the Poff database about final larval instar size and have three size classes: small, medium, and large.

NMDS :metal:
------------

Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570. I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1726121 
    ## Run 1 stress 0.1738211 
    ## Run 2 stress 0.1738211 
    ## Run 3 stress 0.1726121 
    ## ... Procrustes: rmse 3.46017e-05  max resid 7.697395e-05 
    ## ... Similar to previous best
    ## Run 4 stress 0.1726121 
    ## ... Procrustes: rmse 1.691063e-05  max resid 3.709724e-05 
    ## ... Similar to previous best
    ## Run 5 stress 0.2544314 
    ## Run 6 stress 0.1726121 
    ## ... Procrustes: rmse 2.560024e-05  max resid 6.579595e-05 
    ## ... Similar to previous best
    ## Run 7 stress 0.1726121 
    ## ... Procrustes: rmse 1.622728e-05  max resid 4.047531e-05 
    ## ... Similar to previous best
    ## Run 8 stress 0.1726121 
    ## ... Procrustes: rmse 4.181483e-05  max resid 0.0001047041 
    ## ... Similar to previous best
    ## Run 9 stress 0.1726121 
    ## ... Procrustes: rmse 0.0001001377  max resid 0.0002283191 
    ## ... Similar to previous best
    ## Run 10 stress 0.1726121 
    ## ... Procrustes: rmse 1.907343e-05  max resid 5.466964e-05 
    ## ... Similar to previous best
    ## Run 11 stress 0.1726121 
    ## ... New best solution
    ## ... Procrustes: rmse 6.219637e-06  max resid 1.726012e-05 
    ## ... Similar to previous best
    ## Run 12 stress 0.1738211 
    ## Run 13 stress 0.1786995 
    ## Run 14 stress 0.1726121 
    ## ... New best solution
    ## ... Procrustes: rmse 3.397357e-06  max resid 1.149766e-05 
    ## ... Similar to previous best
    ## Run 15 stress 0.1726121 
    ## ... Procrustes: rmse 5.784547e-05  max resid 0.0001311794 
    ## ... Similar to previous best
    ## Run 16 stress 0.1738211 
    ## Run 17 stress 0.2476795 
    ## Run 18 stress 0.1726121 
    ## ... Procrustes: rmse 8.842029e-06  max resid 2.123548e-05 
    ## ... Similar to previous best
    ## Run 19 stress 0.1738211 
    ## Run 20 stress 0.1726125 
    ## ... Procrustes: rmse 0.00010333  max resid 0.0002618653 
    ## ... Similar to previous best
    ## *** Solution reached

This is a pretty good stress value, who knows how that will change when I start doing this right... Can see the transfrmations applied at the top of the output, this was done automatically, I wouldn't know what to do. Used the Bray-Curtis distance metric, max tries is set to 100 with dimensions equal to 2.

The "display" argument can be set to "site" or "species" depending on what you want to group. "kind" can be standard deviation or standard error, not sure which to use here.

``` r
# Fit the ellipse function to actual data
df_ell <- data.frame()
for(g in NMDS$YearTreat){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$YearTreat == g,],
                  vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                ,YearTreat = g))
}
```

not going to pretend like I know how this works, got it from <https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo> but I do know that changing the column selected from NMDS changes which variable is used for grouping.

![](2017-18_Inverts_files/figure-markdown_github/Plot%20Benthic%20NMDS-1.png)

Actually maybe cool result, all of the treatment reaches now plot closer post-gap year. The controls show a similar pattern though maybe a little less pronounced. Everything also shifted left between the two years, although this isn't because of the treatment, just annual changes. Maybe the gap amplified whatever effect the year had, extra, extra light or temperature maybe.
