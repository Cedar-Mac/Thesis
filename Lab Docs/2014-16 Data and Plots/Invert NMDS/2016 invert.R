invert_2016 <- read_csv("~/Desktop/OneDrive - Oregon State University/Stream Ecology/Invert NMDS/2016 Density.csv")
enviro_2016 <- read_csv("~/Desktop/OneDrive - Oregon State University/Stream Ecology/Invert NMDS/2016 enviro.csv")

library(tidyverse)
library(vegan)

Density <- as.tibble(invert_2016)
Enviro <- enviro_2016 %>% mutate_if(is.character, as.factor)
attach(Enviro)

grouping <- select(Enviro, Stream, Reach, Watershed)
meta_data <- Enviro[,5:16] 
meta_data[3] <- NULL
#Get MDS for invert density
sol <- metaMDS(Density, distance = "bray", k = 2, trymax = 100)

#set up NMDS with dimensions of sol and env factors from "Enviro". 
NMDS <- data.frame(x = sol$points[, 1], y = sol$points[ ,2], Stream = select(grouping, Stream), 
                   Reach = select(grouping, Reach), Watershed = select(grouping, Watershed))

#Get mean x,y values 
NMDS.mean <- select(NMDS, x, y) %>% aggregate(grouping, mean)

#Load ellipse for NMDS
plot.new()
ord <- ordiellipse(sol, NMDS$Reach, display = "sites", kind = "sd", conf = 0.95, label = TRUE)
dev.off()

#Function for how vegan draws the cov ellipse?
CovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov, pivot = TRUE)))
}

#Fit the ellipse function to actual data
df_ell <- data.frame()
for(g in NMDS$Reach){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Reach == g,],
                                                   CovEllipse(ord[[g]]$cov, ord[[g]]$center, ord[[g]]$scale))),
                                Reach = g))
}

#Plot the points of streams with shape of watershed and include the cov error ellipse
p <- ggplot(data = NMDS, aes(x, y, colour = Reach))
p <- p + geom_point(aes(shape = Reach)) 
p <- p + annotate("text", x = (NMDS$x), y = (NMDS$y), label = NMDS$Stream, size = 2)
p <- p + geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2), size = 1, linetype = 2)


print(p)

cmethod <- "pearson" #Correlation method to use: pearson, pearman, kendall
fmethod <- "bray" #Fixed distance method: euclidean, manhattan, gower, altGower, canberra, bray, kulczynski, morisita,horn, binomial, and cao
vmethod <- "bray" #Variable distance method: euclidean, manhattan, gower, altGower, canberra, bray, kulczynski, morisita,horn, binomial, and cao
nmethod <- "bray" #NMDS distance method:  euclidean, manhattan, gower, altGower, canberra, bray, kulczynski, morisita,horn, binomial, and cao


res <- bio.env(wisconsin(Density), meta_data, fix.dist.method = fmethod, var.dist.method = vmethod, 
               correlation.method = cmethod, scale.fix = FALSE, scale.var = TRUE) 

#Get the 10 best subset of environmental variables

envNames <- colnames(meta_data)
bestEnvFit <- ""
for(i in (1:length(res$order.by.best$var.incl))){
  bestEnvFit[i] <- paste(paste(envNames[as.numeric(unlist(strsplit(res$order.by.best$var.incl[i], split = ",")))], collapse = ' + '), " = ",res$order.by.best$rho[i], sep = "")
}
bestEnvFit <- data.frame(bestEnvFit)
colnames(bestEnvFit) <- "Best combination of environmental variables with similarity score"

# get 10 best subset of taxa
res.bv.step.bio <- bv.step(wisconsin(Density), wisconsin(Density), 
                           fix.dist.method = fmethod, var.dist.method = vmethod,correlation.method=cmethod,
                           scale.fix = FALSE, scale.var = FALSE, 
                           max.rho = 0.95, min.delta.rho = 0.001,
                           random.selection = TRUE,
                           prop.selected.var = 0.3,
                           num.restarts = 10,
                           output.best = 10,
                           var.always.include = NULL) 

taxaNames <- colnames(Density)
bestTaxaFit <- ""
for(i in (1:length(res.bv.step.bio$order.by.best$var.incl))){
  bestTaxaFit[i] <- paste(paste(taxaNames[as.numeric(unlist(strsplit(res.bv.step.bio$order.by.best$var.incl[i], split=",")))], collapse = ' + '), " = ", res.bv.step.bio$order.by.best$rho[i], sep = "")
}
bestTaxaFit <- data.frame(bestTaxaFit)
colnames(bestTaxaFit) <- "Best combination of taxa with similarity score"

##
MDS_res = metaMDS(Density, distance = nmethod, k = 2, trymax = 50)

bio.keep <- as.numeric(unlist(strsplit(res.bv.step.bio$order.by.best$var.incl[5], ",")))
bio.fit <- envfit(MDS_res, Density[,bio.keep,drop = F], perm = 999)

#use the best set of environmental variables in env.keep
eval(parse(text = paste("env.keep <- c(", res$order.by.best$var.incl[5], ")", sep="")))
env.fit <- envfit(MDS_res, meta_data[, env.keep,drop = F], perm = 999) 

#Get site information
df <- scores(MDS_res,display = c("sites"))

#Add grouping information
df_label <- data.frame(df, Type = grouping)

#Get the vectors for bioenv.fit
df_biofit <- scores(bio.fit,display = c("vectors"))
df_biofit <- df_biofit * vegan:::ordiArrowMul(df_biofit)
df_biofit <- as.data.frame(df_biofit)

#Get the vectors for env.fit
df_envfit <- scores(env.fit, display = c("vectors"))
df_envfit <- df_envfit * vegan:::ordiArrowMul(df_envfit)
df_envfit <- as.data.frame(df_envfit)

#Draw samples
p <- ggplot()
p <- p + geom_point(data = df_label, aes(NMDS1, NMDS2, colour = Type.Reach))
#Draw taxas
p <- p + geom_segment(data = df_biofit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                      arrow = arrow(length = unit(0.2, "cm")), color="#808080", alpha=0.5)

p <- p + geom_text(data = as.data.frame(df_biofit*.8), aes(NMDS1, NMDS2, label = rownames(df_biofit)), color="#808080", alpha=0.5)
#Draw environmental variables
p <- p + geom_segment(data = df_envfit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                      arrow = arrow(length = unit(0.2, "cm")), color="#4C005C", alpha=0.5)

p <- p + geom_text(data = as.data.frame(df_envfit*.7), aes(NMDS1, NMDS2, label = rownames(df_envfit)), color="#4C005C", alpha=0.5)
p <- p + theme_bw()

print(p)

?geom_text
