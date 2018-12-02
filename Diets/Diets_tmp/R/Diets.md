Diets
================
Cedar Mackaness
11/21/2018

The goals here are to:

-   Calculate the percent of each taxa in each fish's diet

-   Calculate the percent of fish with each specific taxa in its diet

-   Do some NMDS up in heya

![](Diets_files/figure-markdown_github/Plot%20Taxa%20Proportions-1.png)

Plot of proportional abundance of an individual taxon in the benthic community versus the proportional abundance of the aggregate diets for that reach.

![](Diets_files/figure-markdown_github/Plot%20FFG%20Proportions-1.png)

Plot of proportional abundance of an individual FFG in the benthic community versus the proportional abundance of the aggregate diets for that reach.

![](Diets_files/figure-markdown_github/Plot%20Size%20Proportions-1.png)

Plot of proportional abundance of an individual size class in the benthic community versus the proportional abundance of the aggregate diets for that reach.

NMDS :metal:
------------

Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570. I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.

    ## Wisconsin double standardization
    ## Run 0 stress 0.09340359 
    ## Run 1 stress 0.2191531 
    ## Run 2 stress 0.09340359 
    ## ... Procrustes: rmse 2.045826e-06  max resid 3.845805e-06 
    ## ... Similar to previous best
    ## Run 3 stress 0.09340359 
    ## ... Procrustes: rmse 1.898151e-06  max resid 4.125266e-06 
    ## ... Similar to previous best
    ## Run 4 stress 0.09340359 
    ## ... Procrustes: rmse 1.461843e-06  max resid 2.310075e-06 
    ## ... Similar to previous best
    ## Run 5 stress 0.09340359 
    ## ... Procrustes: rmse 8.698214e-07  max resid 1.72023e-06 
    ## ... Similar to previous best
    ## Run 6 stress 0.1629444 
    ## Run 7 stress 0.09340359 
    ## ... Procrustes: rmse 4.420085e-07  max resid 7.030427e-07 
    ## ... Similar to previous best
    ## Run 8 stress 0.1588546 
    ## Run 9 stress 0.1629444 
    ## Run 10 stress 0.1908625 
    ## Run 11 stress 0.09340359 
    ## ... New best solution
    ## ... Procrustes: rmse 4.426046e-07  max resid 8.184235e-07 
    ## ... Similar to previous best
    ## Run 12 stress 0.1805111 
    ## Run 13 stress 0.09340359 
    ## ... Procrustes: rmse 1.277975e-06  max resid 2.525653e-06 
    ## ... Similar to previous best
    ## Run 14 stress 0.09340359 
    ## ... New best solution
    ## ... Procrustes: rmse 4.720988e-07  max resid 7.014626e-07 
    ## ... Similar to previous best
    ## Run 15 stress 0.09340359 
    ## ... Procrustes: rmse 1.009671e-06  max resid 2.093006e-06 
    ## ... Similar to previous best
    ## Run 16 stress 0.09340359 
    ## ... Procrustes: rmse 3.619863e-07  max resid 6.185915e-07 
    ## ... Similar to previous best
    ## Run 17 stress 0.09340359 
    ## ... Procrustes: rmse 1.713787e-06  max resid 3.628154e-06 
    ## ... Similar to previous best
    ## Run 18 stress 0.2155995 
    ## Run 19 stress 0.1588546 
    ## Run 20 stress 0.09340359 
    ## ... New best solution
    ## ... Procrustes: rmse 2.632597e-07  max resid 3.728607e-07 
    ## ... Similar to previous best
    ## *** Solution reached

The "display" argument can be set to "site" or "species" depending on what you want to group. "kind" can be standard deviation or standard error, not sure which to use here.

``` r
# Fit the ellipse function to actual data
df_ell <- data.frame()
for(g in NMDS$Treatment){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Treatment == g,],
                  vegan:::veganCovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale)))
                                ,Treatment = g))
}
```

not going to pretend like I know how this works, got it from <https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo> but I do know that changing the column selected from NMDS changes which variable is used for grouping.

![](Diets_files/figure-markdown_github/NMDS%20Plot6-1.png)

There seems to be total overlap of the diet communities in the control versus treatment reaches. This seems to be consistent with what we saw in other plots.
