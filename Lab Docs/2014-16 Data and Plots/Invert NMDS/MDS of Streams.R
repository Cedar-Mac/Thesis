?read_csv
# NMDS with Error Ellipse for grouping by reach in ggplot2
############
library(tidyverse)
library(vegan)

Density <- read_csv("2014_density.csv")
Enviro <- (read_csv("2014_enviro.csv")) %>% mutate_if(is.character, as.factor)
dattach(Enviro)

grouping <- select(Enviro, Stream, Reach, Watershed)
meta_data <- Enviro[,4:16]
meta_data[19,12] <- 100

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
p <- p + annotate("text", x = (NMDS$x), y = (NMDS$y), label = NMDS$Stream, size = 2)
p <- p + geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2), size = 1, linetype = 2)
p <- p + geom_point(aes(shape = Reach)) 

print(p)


############

# bv.step and bio.env functions
############

bv.step <- function(fix.mat, var.mat, 
                    fix.dist.method="bray", var.dist.method="euclidean", correlation.method="spearman",
                    scale.fix=FALSE, scale.var=TRUE,
                    max.rho=0.95,
                    min.delta.rho=0.001,
                    random.selection=TRUE,
                    prop.selected.var=0.2,
                    num.restarts=10,
                    var.always.include=NULL,
                    var.exclude=NULL,
                    output.best=10
){
  
  if(dim(fix.mat)[1] != dim(var.mat)[1]){stop("fixed and variable matrices must have the same number of rows")}
  if(sum(var.always.include %in% var.exclude) > 0){stop("var.always.include and var.exclude share a variable")}
  require(vegan)
  
  if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
  if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}
  
  fix.dist <- vegdist(as.matrix(fix.mat), method=fix.dist.method)
  
  #an initial removal phase
  var.dist.full <- vegdist(as.matrix(var.mat), method=var.dist.method)
  full.cor <- suppressWarnings(cor.test(fix.dist, var.dist.full, method=correlation.method))$estimate
  var.comb <- combn(1:ncol(var.mat), ncol(var.mat)-1)
  RES <- data.frame(var.excl=rep(NA,ncol(var.comb)), n.var=ncol(var.mat)-1, rho=NA)
  for(i in 1:dim(var.comb)[2]){
    var.dist <- vegdist(as.matrix(var.mat[,var.comb[,i]]), method=var.dist.method)
    temp <- suppressWarnings(cor.test(fix.dist, var.dist, method=correlation.method))
    RES$var.excl[i] <- c(1:ncol(var.mat))[-var.comb[,i]]
    RES$rho[i] <- temp$estimate
  }
  delta.rho <- RES$rho - full.cor
  exclude <- sort(unique(c(RES$var.excl[which(abs(delta.rho) < min.delta.rho)], var.exclude)))
  
  if(random.selection){
    num.restarts=num.restarts
    prop.selected.var=prop.selected.var
    prob<-rep(1,ncol(var.mat))
    if(prop.selected.var< 1){
      prob[exclude]<-0
    }
    n.selected.var <- min(sum(prob),prop.selected.var*dim(var.mat)[2])
  } else {
    num.restarts=1
    prop.selected.var=1  
    prob<-rep(1,ncol(var.mat))
    n.selected.var <- min(sum(prob),prop.selected.var*dim(var.mat)[2])
  }
  
  RES_TOT <- c()
  for(i in 1:num.restarts){
    step=1
    RES <- data.frame(step=step, step.dir="F", var.incl=NA, n.var=0, rho=0)
    attr(RES$step.dir, "levels") <- c("F","B")
    best.comb <- which.max(RES$rho)
    best.rho <- RES$rho[best.comb]
    delta.rho <- Inf
    selected.var <- sort(unique(c(sample(1:dim(var.mat)[2], n.selected.var, prob=prob), var.always.include)))
    while(best.rho < max.rho & delta.rho > min.delta.rho & RES$n.var[best.comb] < length(selected.var)){
      #forward step
      step.dir="F"
      step=step+1
      var.comb <- combn(selected.var, RES$n.var[best.comb]+1, simplify=FALSE)
      if(RES$n.var[best.comb] == 0){
        var.comb.incl<-1:length(var.comb)
      } else {
        var.keep <- as.numeric(unlist(strsplit(RES$var.incl[best.comb], ",")))
        temp <- NA*1:length(var.comb)
        for(j in 1:length(temp)){
          temp[j] <- all(var.keep %in% var.comb[[j]]) 
        }
        var.comb.incl <- which(temp==1)
      }
      
      RES.f <- data.frame(step=rep(step, length(var.comb.incl)), step.dir=step.dir, var.incl=NA, n.var=RES$n.var[best.comb]+1, rho=NA)
      for(f in 1:length(var.comb.incl)){
        var.incl <- var.comb[[var.comb.incl[f]]]
        var.incl <- var.incl[order(var.incl)]
        var.dist <- vegdist(as.matrix(var.mat[,var.incl]), method=var.dist.method)
        temp <- suppressWarnings(cor.test(fix.dist, var.dist, method=correlation.method))
        RES.f$var.incl[f] <- paste(var.incl, collapse=",")
        RES.f$rho[f] <- temp$estimate
      }
      
      last.F <- max(which(RES$step.dir=="F"))
      RES <- rbind(RES, RES.f[which.max(RES.f$rho),])
      best.comb <- which.max(RES$rho)
      delta.rho <- RES$rho[best.comb] - best.rho 
      best.rho <- RES$rho[best.comb]
      
      if(best.comb == step){
        while(best.comb == step & RES$n.var[best.comb] > 1){
          #backward step
          step.dir="B"
          step <- step+1
          var.keep <- as.numeric(unlist(strsplit(RES$var.incl[best.comb], ",")))
          var.comb <- combn(var.keep, RES$n.var[best.comb]-1, simplify=FALSE)
          RES.b <- data.frame(step=rep(step, length(var.comb)), step.dir=step.dir, var.incl=NA, n.var=RES$n.var[best.comb]-1, rho=NA)
          for(b in 1:length(var.comb)){
            var.incl <- var.comb[[b]]
            var.incl <- var.incl[order(var.incl)]
            var.dist <- vegdist(as.matrix(var.mat[,var.incl]), method=var.dist.method)
            temp <- suppressWarnings(cor.test(fix.dist, var.dist, method=correlation.method))
            RES.b$var.incl[b] <- paste(var.incl, collapse=",")
            RES.b$rho[b] <- temp$estimate
          }
          RES <- rbind(RES, RES.b[which.max(RES.b$rho),])
          best.comb <- which.max(RES$rho)
          best.rho<- RES$rho[best.comb]
        }
      } else {
        break()
      }
      
    }
    
    RES_TOT <- rbind(RES_TOT, RES[2:dim(RES)[1],])
    print(paste(round((i/num.restarts)*100,3), "% finished"))
  }
  
  RES_TOT <- unique(RES_TOT[,3:5])
  
  
  if(dim(RES_TOT)[1] > output.best){
    order.by.best <- RES_TOT[order(RES_TOT$rho, decreasing=TRUE)[1:output.best],]
  } else {
    order.by.best <-  RES_TOT[order(RES_TOT$rho, decreasing=TRUE), ]
  }
  rownames(order.by.best)<-NULL
  
  order.by.i.comb <- c()
  for(i in 1:length(selected.var)){
    f1 <- which(RES_TOT$n.var==i)
    f2 <- which.max(RES_TOT$rho[f1])
    order.by.i.comb <- rbind(order.by.i.comb, RES_TOT[f1[f2],])
  }
  rownames(order.by.i.comb)<-NULL
  
  if(length(exclude)<1){var.exclude=NULL} else {var.exclude=exclude}
  out <- list(
    order.by.best=order.by.best,
    order.by.i.comb=order.by.i.comb,
    best.model.vars=paste(colnames(var.mat)[as.numeric(unlist(strsplit(order.by.best$var.incl[1], ",")))], collapse=","),
    best.model.rho=order.by.best$rho[1],
    var.always.include=var.always.include,
    var.exclude=var.exclude
  )
  out
  
}

bio.env <- function(fix.mat, var.mat, 
                    fix.dist.method = "bray", var.dist.method = "euclidean", correlation.method = "spearman",
                    scale.fix = FALSE, scale.var = TRUE,
                    output.best = 10,
                    var.max = ncol(var.mat)
){
  if(dim(fix.mat)[1] != dim(var.mat)[1]){stop("fixed and variable matrices must have the same number of rows")}
  if(var.max > dim(var.mat)[2]){stop("var.max cannot be larger than the number of variables (columns) in var.mat")}
  
  require(vegan)
  
  combn.sum <- sum(factorial(ncol(var.mat))/(factorial(1:var.max)*factorial(ncol(var.mat)-1:var.max)))
  
  if(scale.fix){fix.mat<-scale(fix.mat)}else{fix.mat<-fix.mat}
  if(scale.var){var.mat<-scale(var.mat)}else{var.mat<-var.mat}
  fix.dist <- vegdist(fix.mat, method=fix.dist.method)
  RES_TOT <- c()
  best.i.comb <- c()
  iter <- 0
  for(i in 1:var.max){
    var.comb <- combn(1:ncol(var.mat), i, simplify=FALSE)
    RES <- data.frame(var.incl=rep(NA, length(var.comb)), n.var=i, rho=0)
    for(f in 1:length(var.comb)){
      iter <- iter+1
      var.dist <- vegdist(as.matrix(var.mat[,var.comb[[f]]]), method=var.dist.method)
      temp <- suppressWarnings(cor.test(fix.dist, var.dist, method=correlation.method))
      RES$var.incl[f] <- paste(var.comb[[f]], collapse=",")
      RES$rho[f] <- temp$estimate
      if(iter %% 100 == 0){print(paste(round(iter/combn.sum*100, 3), "% finished"))}
    }
    
    order.rho <- order(RES$rho, decreasing=TRUE)
    best.i.comb <- c(best.i.comb, RES$var.incl[order.rho[1]])
    if(length(order.rho) > output.best){
      RES_TOT <- rbind(RES_TOT, RES[order.rho[1:output.best],])
    } else {
      RES_TOT <- rbind(RES_TOT, RES)
    }
  }
  rownames(RES_TOT)<-NULL
  
  if(dim(RES_TOT)[1] > output.best){
    order.by.best <- order(RES_TOT$rho, decreasing=TRUE)[1:output.best]
  } else {
    order.by.best <- order(RES_TOT$rho, decreasing=TRUE)
  }
  OBB <- RES_TOT[order.by.best,]
  rownames(OBB) <- NULL
  
  order.by.i.comb <- match(best.i.comb, RES_TOT$var.incl)
  OBC <- RES_TOT[order.by.i.comb,]
  rownames(OBC) <- NULL
  
  out <- list(
    order.by.best=OBB,
    order.by.i.comb=OBC,
    best.model.vars=paste(colnames(var.mat)[as.numeric(unlist(strsplit(OBB$var.incl[1], ",")))], collapse=",") ,
    best.model.rho=OBB$rho[1]
  )
  out
}
############

#NMDS with environmental factors fitted
#########
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
p <- p + geom_point(data = df_label, aes(NMDS1, NMDS2, colour = Type.Watershed))
#Draw taxas
p <- p + geom_segment(data = df_biofit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                  arrow = arrow(length = unit(0.2, "cm")), color="#808080", alpha=0.5)

p <- p + geom_text(data = as.data.frame(df_biofit*.7), aes(NMDS1, NMDS2, label = rownames(df_biofit)), color="#808080", alpha=0.5)
#Draw environmental variables
p <- p + geom_segment(data = df_envfit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                  arrow = arrow(length = unit(0.2, "cm")), color="#4C005C", alpha=0.5)

p <- p + geom_text(data = as.data.frame(df_envfit*.8), aes(NMDS1, NMDS2, label = rownames(df_envfit)), color="#4C005C", alpha=0.5)
p <- p + theme_bw()

print(p)

############

#CCA
#########
#Convert to relative frequencies
relative_density <- Density / rowSums(Density)

#Use adonis to find significant environmental variables
abund_table.adonis <- adonis(relative_density ~ ., data = meta_data)

(bestEnvVariables <- rownames(abund_table.adonis$aov.tab)[abund_table.adonis$aov.tab$"Pr(>F)"<= 0.01])
bestEnvVariables<-bestEnvVariables[!is.na(bestEnvVariables)]
(sol_cca <- cca(Density ~ ., data = meta_data))
scrs <- scores(sol_cca, display = c("sp","wa","lc","bp","cn"))

df_sites <- data.frame(scrs$sites,t(as.data.frame(strsplit(rownames(scrs$sites), "_"))))
colnames(df_sites) <- c("x", "y", "Stream")

p <- ggplot()
p <- p + geom_point(data = df_sites, aes(x, y, colour = Stream))

#Draw biplots
multiplier <- vegan:::ordiArrowMul(scrs$biplot)

df_arrows <- scrs$biplot*multiplier
colnames(df_arrows) <- c("x","y")
df_arrows = as.data.frame(df_arrows)

p <- p + geom_segment(data = df_arrows, aes(x = 0, y = 0, xend = x, yend = y),
                  arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha = 0.5)

p <- p + geom_text(data = as.data.frame(df_arrows * 1.1), aes(x, y, label = rownames(df_arrows)), color = "#808080", alpha = 0.5)

# Draw species
df_species<- as.data.frame(scrs$species)
colnames(df_species)<-c("x","y")

p<-p+geom_point(data=df_species,aes(x,y,shape="Species"))+scale_shape_manual("",values=2)
plot(p)

#########

#Richness
########
# Calculate species richness
N <- rowSums(Density)
S <- specnumber(Density)
S.rar <- rarefy(Density, min(N))

# Regression of S.rar against meta_table
S.lm <- lm(S.rar ~ ., data = meta_data)
summary(S.lm)

#Format the data for ggplot

df_S <- NULL
for (i in 1:dim(meta_data)[2]){
  tmp <- data.frame(row.names = NULL, Sample = as.character(grouping$Stream), Richness = S.rar, Env = meta_data[,i], Label = rep(colnames(meta_data)[i], length(meta_data[,i])))
  if (is.null(df_S)){
    df_S = tmp
  }else{
    df_S <- rbind(df_S, tmp)
  }
}

#Get grouping information
grouping_info<-data.frame(row.names=rownames(df_S),t(as.data.frame(strsplit(as.character(df_S[,"Sample"]),"_"))))
colnames(grouping_info)<-c("Countries","Latrine","Depth")

#Merge this information with df_S
df_S<-cbind(df_S,grouping_info)

formatPvalues <- function(pvalue) {
  ra<-""
  if(pvalue <= 0.1) ra<-"."
  if(pvalue <= 0.05) ra<-"*"
  if(pvalue <= 0.01) ra<-"**"
  if(pvalue <= 0.001) ra<-"***"
  return(ra)
}

#Now use ddply to get correlations
library(plyr)
cors<-ddply(df_S,.(Label), summarise, cor=round(cor(Richness,Env),2), sig=formatPvalues(cor.test(Richness,Env)$p.value))

facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

#Now convert your labels
wrap_labels<-do.call(paste,c(cors[c("Label")],"(",cors[c("cor")]," ",cors[c("sig")],")",sep=""))


p<-ggplot(df_S,aes(Env,Richness)) + 
  geom_point(aes(colour=Countries)) +
  geom_smooth(,method="lm", size=1, se=T) +
  facet_wrap( ~ Label , scales="free", ncol=3) +theme_bw() 
p<-facet_wrap_labeller(p,labels=wrap_labels)
print(p)
dev.off()



########


# NMDS for density base vegan plots
############ 

library(tidyverse)
library(vegan)

Density <- read_csv("2014_density.csv")
Enviro <- (read_csv("2014_enviro.csv")) %>% mutate_if(is.character, as.factor)
attach(Enviro)

ord_density <- metaMDS(Density, autotransform = TRUE)

plot_NMDS <- function(group){
  plot(ord_density, disp = "sites", type = "n")
  ordihull(ord_density, group, col = 1:2, lwd = 3)
  ordiellipse(ord_density, group, col = 1:2, kind = "ehull", lwd = 3)
  ordiellipse(ord_density, group, col = 1:2, draw ="polygon")
  ordispider(ord_density, group, col = 1:2, label = TRUE)
  points(ord_density, disp = "sites", pch = 18, col = "steelblue", bg="yellow", cex = 1.3)
}

plot_NMDS(Reach)
plot_NMDS(Stream)
plot_NMDS(Watershed)

ord_fit <- function(env_fctr){
  envfit(ord_density ~ env_fctr, data = Density, perm = 999)
}
  
plot(ord_fit(`Gradient (%)`), label = "Gradient (%)")
plot(ord_fit(`Wetted Average (m)`), label = "Wetted Average (m)")


############


# NMDS by biomass base vegan plots
#############
Biomass <- biomass[, 2:39]
ord_biomass <- metaMDS(Biomass)
ordiellipse(ord_biomass, Stream, col=1:9, draw="polygon")
plot(ord_fit_biomass, type = "p")





#############


