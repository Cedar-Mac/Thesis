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
Working with 2017-18 benthic invertebrate and fish diet data to calculate and plot:

- Changes in scaper taxa community

- NMS analysis

- Diet NMS

- Diet 1:1 and costello plot

- Analysis of diet and benthic overlay

- Effect of treatment on benthic and diet communities using MRPP or perMANOVA?

## Benthic {.tabset .tabset-fade .tabset-pills}
***
<br>

### Load Packages
*** 





```r
# import data
bugs <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Cleaned")
ffg <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "FFG")
sizes <- readxl::read_xlsx("./Data/2017-18 bugs.xlsx", sheet = "Size")
Diets <- readxl::read_xlsx("./Data/2018 Diets.xlsx")
Bentho <- read_xlsx("./Data/2018 Tiles Allison's Computer.xlsx", sheet = "Tiles")
Both_years<- read_excel("./Data/2018 Tiles Allison's Computer.xlsx", sheet = "2017 and 2018")

#Replace Ssp. in Taxon with Family
bugs$Taxon[bugs$Taxon == "Ssp."] <- bugs$Family[bugs$Taxon == "Ssp."]
```



```r
#Setup ggplot theme
my_theme <- theme_bw(base_size = 18, base_family = "Microsoft Sans Serif") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey"),
                legend.title = element_text(colour = "black", size = 18, face = "bold"),
                legend.text = element_text(colour = "black", size = 12))
                
                
theme_set(my_theme)
```


### Chlorophyll *a*
***


```r
# Change variables to factors.  Re-order with CHUCK at the end
Bentho$Stream <- factor(Bentho$Stream, levels = c("LOON", "MCTE", "W-113", "W-100", "CHUCK", "W-122"))
Bentho$Treatment <- as.factor(Bentho$Treatment)
Bentho$Meter <- as.factor(Bentho$Meter)
Bentho$BenthoTotal <- as.numeric(Bentho$BenthoTotal)

# Drop W-122
Bentho <- Bentho %>% filter(Stream != "W-122")

# Calculate mean of the three tiles for each meter
bentho.mean <- aggregate(BenthoTotal ~ Stream + Treatment + Meter, mean, data = Bentho)

ggplot(data = bentho.mean) +
  geom_line(mapping = aes(x = Meter, y = BenthoTotal, color = Treatment, group = Treatment), size = 3) +
  ylab("Chlorophyll a (ug/cm^2)")+ scale_x_discrete(breaks = seq(0, 90, by = 15)) +
  scale_color_viridis_d(option = "E") +
  facet_wrap("Stream")
```

![](index_files/figure-html/Bentho torch 2018-1.png)<!-- -->



```r
# Change variables to factors.  Re-order with CHUCK at the end
Both_years$Stream <- factor(Both_years$Stream, levels = c("LOON", "MCTE", "W-113", "W-100", "CHUCK", "W-122"))
Both_years$Year <- factor(Both_years$Year, levels = c("2018", "2017"))
Both_years$Year.Treatment <- as.factor(Both_years$Year.Treatment)
Both_years$Treatment <- as.factor(Both_years$Treatment)
Both_years$Meter <- as.factor(Both_years$Meter)
Both_years$BenthoTotal <- as.numeric(Both_years$BenthoTotal)

# Drop W-122.
Both_years <- Both_years %>% filter(Stream != "W-122")

# Calculate mean of the three tiles for each meter
bentho_both.mean <- aggregate(BenthoTotal ~ Stream + Treatment + Year.Treatment + Meter + Year, mean, data = Both_years)
bentho.reach <- aggregate(BenthoTotal ~ Stream + Treatment + Year.Treatment + Year, mean, data = Both_years)
write_csv(bentho.reach, "./Data/bentho.reach.csv")

ggplot(data = bentho_both.mean) +
  geom_line(mapping = aes(x = Meter, y = BenthoTotal, color = Treatment, 
  linetype = Year, group = Year.Treatment), size = 3) +
  ylab("Chlorophyll a (ug/cm^2)") + 
  scale_x_discrete(breaks = seq(0, 90, by = 15)) +
  scale_color_viridis_d(option = "E") +
  facet_wrap("Stream") +
  theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/Bentho torch Both Years-1.png)<!-- -->


### Setup
***

In order to calculate density we must take into account that our count values are from a subsample of three pooled samples.


```r
#Calculations...
bugs$Density <- bugs$Count / bugs$PercentSub / .09  #Calculate total in pooled sample

bugs$CollDate <- as.Date(bugs$CollDate, format = "%m/%d/%y") %>% year() %>% as.factor()

bugs.agg <- bugs %>%
  group_by(CollDate, Stream, Treatment, Taxon) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugs.agg$Density <- bugs.agg$Density / 3    #Divide by number of samples pooled
```

***

We divide Count by percent subsampled (actually a fraction) to get a count for the total sample taken.  We then divide by .09 which is the area of the surber sampler (in m$^2$). We then aggregate and divide by three (There were three samples taken per reach).



```r
# Spread and then gather to fill missing Taxons with 0's
bugs.agg <- spread(bugs.agg, key = "Taxon", value = "Density") %>% #bugs.agg is benthic samples
  mutate_if(is.numeric , replace_na, replace = 0) %>% 
  gather(key = "Taxon", value = "Density", 4:ncol(.))
```



```r
# Add FFG and size classes
bugs.agg <- merge(bugs.agg, sizes) %>% merge(., ffg)

# Create list of CollDate, Stream and Taxon
bugs.reach.diff <- bugs.agg %>% select(-c("Density", "CollDate")) %>% unique()

# Calculate Differences
bugs.reach.diff$Diff <- bugs.agg$Density[bugs.agg$CollDate == "2018"] -     
  bugs.agg$Density[bugs.agg$CollDate == "2017"]

# Filter for only 2018 bugs
bugs18 <- filter(bugs.agg, CollDate == "2018")
```

A quick taste of the finished data table product:


```r
# Datatable with filters and data ranges
datatable(bugs.agg, rownames = FALSE, class = "hover", filter = "top", options = list(pageLength = 10, scrollX = T)) %>% formatRound("Density", digits = 2, interval = 3, 
                                          mark = ",", dec.mark = getOption("OutDec"))
```

<!--html_preserve--><div id="htmlwidget-8adcc68b70029cd7c68e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8adcc68b70029cd7c68e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;2017&quot;,&quot;2018&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"3333.33333333333\" data-scale=\"15\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Ameletus","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Amphizoa","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Ampumixis","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Antocha","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Aquarius","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atheryx","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Atrichopogon","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Baetis","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Brachycentrus","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Calineuria","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Caudatella","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Ceratapogonidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chironomidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Chloroperlidae","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Cinygmula","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Clam","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Coleoptera","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Collembola","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Copepod","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Corydalidae","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Crayfish","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Despaxia","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranopselaphus variegatus","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dicranota","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixella","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Dixidae","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Doronueria","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Drunella","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dubiraphia","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Dyticidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Elmidae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Empididae","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Epeorus","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerella","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Ephemerellidae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Gerridae","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Glossosoma","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Heptageniidae","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hesperoperla","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hexatoma","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydrophilidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Hydropsychidae","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Ironodes","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Juga","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lara","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Lepidostoma","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leptophlebiidae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Leuctridae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Limnephilidae","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Malenka","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Meringodixa","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Micrasema","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Mite","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Moselia","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Narpus","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemoura","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Nemouridae","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Neophylax","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Octogomphus","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Orohermes","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Ostrocod","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Paraleptophlebia","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Parapsyche","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Pelecorhynchidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Peltoperlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Perlidae","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philocasca","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Philopotamidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychodidae","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Psychoglypha","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcidae","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Pteronarcys","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rhyacophila","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Rithrogena","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Sialis","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Simuliidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Smithirnidae","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Sweltsa","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tabanidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Tipulidae","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Treptobates","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Uenoidae","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Wormalia","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Yoraperla","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada","Zapada"],["2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2018","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2018","2017","2017","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2018","2018","2017","2017","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2017","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2017","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2018","2017","2017","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2017","2017","2018","2018","2017","2017","2018","2017","2017","2018","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2017","2017","2018","2018","2017","2017","2018","2017","2017","2018","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2018","2017","2017","2018","2018","2018","2018","2018","2018","2017","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2018","2017","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2017","2017","2018","2018","2017","2018","2017","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2017","2017","2018","2018","2017","2018","2017","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2018","2017","2018","2017","2018","2018","2017","2018","2018","2018","2018","2017","2017","2018","2017","2017","2018","2017","2017","2018","2017","2018","2017","2017","2018","2018","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2017","2018","2018","2018","2018","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2017","2018","2018","2018","2018","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2018","2017","2017","2017","2017","2018","2017","2017","2017","2018","2018","2017","2018","2018","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2017","2018","2017","2017","2017","2017","2018","2017","2018","2018","2017","2017","2018","2018","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2018","2018","2017","2018","2018","2017","2017","2018","2017","2017","2017","2017","2018","2017","2018","2018","2017","2017","2018","2018","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2017","2018","2018","2018","2018","2017","2017","2018","2018","2018","2018","2017","2017","2018","2018","2017","2018","2017","2017","2017","2017","2017","2018","2017","2018","2018","2017","2018","2018","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2018","2018","2018","2017","2017","2017","2018","2018","2017","2018","2018","2017","2017","2017","2017","2018","2017","2018","2018","2017","2018","2018","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2018","2018","2018","2017","2017","2017","2018","2018","2017","2018","2018","2017","2017","2017","2017","2018","2017","2018","2018","2017","2018","2018","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2018","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2018","2017","2017","2018","2017","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2018","2017","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2018","2017","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2018","2018","2018","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2017","2017","2017","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018"],["W-113","W-100","CHUCK","CHUCK","W-100","W-113","MCTE","MCTE","LOON","LOON","LOON","MCTE","CHUCK","LOON","W-113","W-113","CHUCK","MCTE","W-100","W-100","CHUCK","CHUCK","LOON","LOON","W-113","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","MCTE","W-113","MCTE","W-100","LOON","MCTE","W-100","MCTE","W-100","LOON","MCTE","MCTE","MCTE","W-100","LOON","W-100","W-113","W-113","W-100","CHUCK","CHUCK","LOON","LOON","W-113","W-113","CHUCK","CHUCK","CHUCK","MCTE","CHUCK","LOON","W-113","W-113","LOON","W-100","W-100","W-113","W-113","W-100","CHUCK","CHUCK","MCTE","LOON","MCTE","MCTE","LOON","W-100","MCTE","W-100","W-100","MCTE","W-100","W-100","W-113","W-113","W-113","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","CHUCK","MCTE","LOON","LOON","MCTE","W-113","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","CHUCK","MCTE","W-100","W-100","MCTE","W-113","MCTE","W-100","W-100","MCTE","W-100","W-100","W-113","W-113","W-113","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","CHUCK","MCTE","LOON","LOON","MCTE","W-113","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","CHUCK","MCTE","W-100","W-100","MCTE","W-113","MCTE","W-100","W-100","MCTE","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","W-113","CHUCK","CHUCK","LOON","LOON","LOON","W-113","CHUCK","CHUCK","CHUCK","LOON","MCTE","W-100","MCTE","W-113","LOON","LOON","W-113","W-113","W-113","CHUCK","W-100","CHUCK","CHUCK","W-100","LOON","MCTE","MCTE","W-100","W-113","W-100","W-113","CHUCK","W-100","W-100","W-113","W-113","LOON","CHUCK","CHUCK","CHUCK","MCTE","MCTE","LOON","LOON","MCTE","MCTE","W-100","LOON","MCTE","W-100","LOON","LOON","MCTE","CHUCK","CHUCK","W-100","W-113","W-113","MCTE","CHUCK","LOON","LOON","MCTE","W-113","W-100","W-100","W-113","CHUCK","W-113","W-100","W-113","CHUCK","W-100","W-100","W-113","W-113","LOON","CHUCK","CHUCK","CHUCK","MCTE","MCTE","LOON","LOON","MCTE","MCTE","W-100","LOON","MCTE","W-100","LOON","LOON","MCTE","CHUCK","CHUCK","W-100","W-113","W-113","MCTE","CHUCK","LOON","LOON","MCTE","W-113","W-100","W-100","W-113","CHUCK","W-113","W-100","W-113","CHUCK","W-100","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","LOON","LOON","LOON","MCTE","LOON","W-100","W-113","MCTE","W-100","LOON","MCTE","W-113","LOON","W-113","CHUCK","CHUCK","CHUCK","W-100","W-100","CHUCK","MCTE","W-113","W-113","CHUCK","W-113","CHUCK","CHUCK","LOON","W-100","W-113","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","LOON","MCTE","W-100","W-113","W-100","MCTE","MCTE","W-100","W-100","W-113","CHUCK","CHUCK","LOON","W-100","W-113","MCTE","W-100","LOON","LOON","MCTE","W-113","CHUCK","LOON","W-113","CHUCK","W-113","CHUCK","W-113","CHUCK","CHUCK","LOON","W-100","W-113","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","LOON","MCTE","W-100","W-113","W-100","MCTE","MCTE","W-100","W-100","W-113","CHUCK","CHUCK","LOON","W-100","W-113","MCTE","W-100","LOON","LOON","MCTE","W-113","CHUCK","LOON","W-113","CHUCK","W-113","CHUCK","W-113","CHUCK","CHUCK","LOON","W-100","W-100","W-113","W-113","LOON","LOON","CHUCK","LOON","W-100","W-100","MCTE","MCTE","MCTE","MCTE","MCTE","MCTE","W-100","W-113","CHUCK","W-113","W-100","CHUCK","MCTE","W-100","LOON","MCTE","CHUCK","LOON","LOON","W-100","W-113","W-113","LOON","CHUCK","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","W-113","LOON","LOON","MCTE","MCTE","MCTE","MCTE","W-100","W-100","W-113","W-100","W-100","W-113","W-100","MCTE","W-100","W-113","MCTE","W-113","CHUCK","LOON","LOON","W-113","MCTE","W-100","W-100","LOON","CHUCK","W-113","CHUCK","CHUCK","MCTE","LOON","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","W-113","LOON","LOON","MCTE","MCTE","MCTE","MCTE","W-100","W-100","W-113","W-100","W-100","W-113","W-100","MCTE","W-100","W-113","MCTE","W-113","CHUCK","LOON","LOON","W-113","MCTE","W-100","W-100","LOON","CHUCK","W-113","CHUCK","CHUCK","MCTE","LOON","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","W-113","W-100","W-113","W-113","LOON","MCTE","MCTE","LOON","W-100","W-100","W-100","MCTE","MCTE","LOON","MCTE","W-100","W-113","W-113","CHUCK","W-100","W-100","CHUCK","MCTE","LOON","LOON","MCTE","LOON","W-113","MCTE","W-100","W-113","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","CHUCK","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","MCTE","W-100","W-113","W-100","W-100","W-113","W-113","W-100","W-113","W-100","W-100","MCTE","W-113","W-113","MCTE","W-113","LOON","LOON","MCTE","CHUCK","W-100","W-100","W-113","LOON","CHUCK","CHUCK","CHUCK","LOON","MCTE","CHUCK","CHUCK","LOON","LOON","CHUCK","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","MCTE","W-100","W-113","W-100","W-100","W-113","W-113","W-100","W-113","W-100","W-100","MCTE","W-113","W-113","MCTE","W-113","LOON","LOON","MCTE","CHUCK","W-100","W-100","W-113","LOON","CHUCK","CHUCK","CHUCK","LOON","MCTE","CHUCK","CHUCK","LOON","LOON","CHUCK","LOON","LOON","MCTE","CHUCK","W-113","W-113","MCTE","MCTE","MCTE","W-100","W-100","W-113","W-113","W-100","W-100","W-113","W-113","LOON","W-100","MCTE","LOON","W-100","W-100","MCTE","CHUCK","W-100","LOON","W-113","MCTE","W-113","CHUCK","MCTE","CHUCK","CHUCK","LOON","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","MCTE","W-100","CHUCK","MCTE","W-100","W-100","W-113","W-100","W-113","W-113","CHUCK","W-113","W-113","W-113","W-100","W-100","W-100","MCTE","W-113","CHUCK","W-100","MCTE","CHUCK","CHUCK","CHUCK","W-113","LOON","LOON","LOON","LOON","MCTE","MCTE","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","MCTE","W-100","CHUCK","MCTE","W-100","W-100","W-113","W-100","W-113","W-113","CHUCK","W-113","W-113","W-113","W-100","W-100","W-100","MCTE","W-113","CHUCK","W-100","MCTE","CHUCK","CHUCK","CHUCK","W-113","LOON","LOON","LOON","LOON","MCTE","MCTE","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","MCTE","W-100","CHUCK","W-100","MCTE","W-100","W-100","W-113","W-113","W-113","CHUCK","W-113","W-113","W-113","LOON","MCTE","W-100","W-100","MCTE","MCTE","W-113","W-100","W-100","W-113","CHUCK","CHUCK","LOON","CHUCK","LOON","CHUCK","MCTE","LOON","CHUCK","LOON","CHUCK","LOON","MCTE","MCTE","LOON","MCTE","MCTE","W-100","W-100","LOON","W-100","W-100","W-113","W-113","W-113","W-113","CHUCK","CHUCK","W-113","MCTE","W-100","W-100","W-100","W-113","W-113","CHUCK","CHUCK","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","MCTE","LOON","W-100","MCTE","CHUCK","LOON","CHUCK","LOON","MCTE","MCTE","LOON","MCTE","MCTE","W-100","W-100","LOON","W-100","W-100","W-113","W-113","W-113","W-113","CHUCK","CHUCK","W-113","MCTE","W-100","W-100","W-100","W-113","W-113","CHUCK","CHUCK","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","LOON","MCTE","LOON","W-100","MCTE","CHUCK","LOON","CHUCK","LOON","MCTE","MCTE","LOON","MCTE","MCTE","W-100","W-100","LOON","W-113","W-113","W-100","W-100","W-113","CHUCK","CHUCK","W-113","W-113","W-113","W-100","LOON","MCTE","W-100","LOON","MCTE","W-100","W-113","CHUCK","W-100","W-113","CHUCK","CHUCK","MCTE","CHUCK","LOON","LOON","MCTE","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","MCTE","W-100","W-100","W-113","CHUCK","W-113","W-113","W-113","LOON","LOON","CHUCK","CHUCK","W-113","W-113","W-100","W-100","CHUCK","W-113","MCTE","CHUCK","CHUCK","LOON","LOON","CHUCK","LOON","LOON","MCTE","W-113","MCTE","MCTE","W-100","W-100","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","MCTE","W-100","W-100","W-113","CHUCK","W-113","W-113","W-113","LOON","LOON","CHUCK","CHUCK","W-113","W-113","W-100","W-100","CHUCK","W-113","MCTE","CHUCK","CHUCK","LOON","LOON","CHUCK","LOON","LOON","MCTE","W-113","MCTE","MCTE","W-100","W-100","LOON","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","MCTE","W-100","W-100","W-113","CHUCK","W-113","CHUCK","W-113","W-113","LOON","CHUCK","LOON","CHUCK","CHUCK","MCTE","LOON","LOON","MCTE","LOON","MCTE","MCTE","W-100","W-100","W-113","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","W-113","W-113","W-100","W-100","W-113","CHUCK","CHUCK","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","MCTE","MCTE","LOON","LOON","W-113","W-100","W-100","CHUCK","CHUCK","MCTE","LOON","LOON","MCTE","LOON","MCTE","MCTE","W-100","W-100","W-113","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","W-113","W-113","W-100","W-100","W-113","CHUCK","CHUCK","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","MCTE","MCTE","LOON","LOON","W-113","W-100","W-100","CHUCK","CHUCK","MCTE","LOON","LOON","MCTE","LOON","MCTE","MCTE","W-100","W-100","W-113","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","W-113","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","W-113","MCTE","MCTE","LOON","LOON","W-100","W-100","CHUCK","LOON","MCTE","W-100","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","W-113","W-113","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-113","MCTE","W-100","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","W-113","W-113","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-113","MCTE","W-100","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","LOON","MCTE","CHUCK","MCTE","MCTE","W-100","W-100","W-113","W-113","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113","CHUCK","CHUCK","LOON","LOON","MCTE","MCTE","W-100","W-100","W-113","W-113"],["Y","N","N","Y","Y","N","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","Y","N","Y","N","Y","N","N","Y","Y","N","Y","N","Y","Y","N","Y","Y","N","Y","N","N","N","Y","Y","N","N","N","Y","Y","N","Y","N","Y","Y","N","Y","N","N","Y","Y","Y","N","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","Y","Y","Y","N","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","N","Y","N","N","Y","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","N","N","Y","Y","N","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","N","Y","N","N","Y","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","N","N","Y","Y","N","N","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","N","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","Y","N","N","Y","Y","N","N","Y","N","N","Y","N","Y","Y","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","N","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","Y","N","N","Y","Y","N","N","Y","N","N","Y","N","Y","Y","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","N","N","Y","N","Y","Y","N","Y","Y","Y","Y","N","N","N","N","Y","N","Y","N","Y","Y","N","Y","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","N","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","N","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","N","N","Y","N","Y","N","N","Y","Y","N","Y","N","Y","Y","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","Y","Y","N","Y","N","N","N","Y","N","N","Y","N","Y","Y","N","N","Y","N","Y","N","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","Y","Y","N","Y","N","N","N","Y","N","N","Y","N","Y","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","N","Y","Y","Y","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","N","N","Y","N","N","Y","N","Y","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","Y","Y","N","Y","N","N","Y","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","Y","Y","N","Y","N","N","Y","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","Y","Y","N","Y","Y","N","Y","N","N","N","Y","Y","Y","N","Y","N","Y","Y","N","Y","N","N","N","N","Y","N","Y","N","Y","Y","N","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","N","Y","N","Y","Y","N","Y","N","Y","N","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","Y","Y","N","N","N","Y","N","Y","N","N","Y","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","Y","N","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","Y","Y","N","Y","N","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","Y","N","Y","N","N","Y","N","N","Y","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","N","Y","Y","N","Y","Y","Y","N","Y","N","N","Y","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","Y","N","Y","N","Y","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","N","N","Y","N","Y","N","Y","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y","N","Y"],[0,0,0,22.2222222226144,0,0,15.8730158756428,0,0,0,3.7037037037037,11.1111111111111,0,0,0,0,0,0,0,3.7037037037037,12.3456790123457,0,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,0,7.40740740740741,0,0,0,0,33.3333333333333,0,0,0,0,18.5185185185185,0,29.6296296296296,22.2222222222222,0,44.4444444462222,0,0,0,12.3456790123457,66.6666666933333,88.8888888924445,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12.3456790123457,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24.6913580246914,0,0,0,0,0,0,0,0,0,377.777777828283,481.481481481482,49.3827160493827,37.037037037037,257.671957712236,666.666666693333,277.777777777778,222.222222231111,244.444444542222,159.259259259259,271.604938271605,381.481481481481,59.2592592592593,74.0740740740741,159.259259259259,129.62962962963,429.62962962963,396.296296296296,174.074074074074,259.259259259259,3.7037037037037,0,3.7037037037037,0,0,0,444.444444444445,0,0,66.6666666933333,3.7037037037037,0,3.7037037037037,0,0,3.7037037037037,24.6913580246914,0,0,0,222.222222254063,98.7654320987654,378.600823045268,222.222222222222,63.4920635063875,320.000000012448,25.9259259259259,135.802469135802,377.777777928889,66.6666666666667,14.8148148148148,209.876543209877,266.666666677333,40.7407407407407,92.5925925925926,103.703703703704,70.3703703703704,25.9259259259259,18.5185185185185,96.2962962962963,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,0,185.185185185185,18.5185185185185,49.3827160493827,12.3456790123457,47.6190476279833,3.7037037037037,7.40740740740741,22.2222222231111,88.8888889244444,44.4444444462222,3.7037037037037,11.1111111119122,0,11.1111111111111,0,3.7037037037037,0,22.2222222222222,0,37.037037037037,37.037037037037,992.592592592593,18.5185185185185,48.1481481481481,277.777777777778,3066.66666678933,3111.11111235556,2088.88888897244,1049.38271604938,88.8888888888889,617.283950617284,266.666666709341,88.8888888888889,159.259259259259,18.5185185185185,133.333333333333,1015.87301610382,2666.66666666667,2037.03703703704,1567.9012345679,0,0,0,0,0,0,7.40740740740741,0,22.2222222311111,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,18.5185185185185,7.40740740740741,3.7037037037037,18.5185185185185,0,0,18.5185185185185,22.2222222231111,0,0,12.3456790123457,0,14.8148148148148,7.40740740740741,0,0,3.7037037037037,0,0,0,0,0,0,22.2222222311111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,15.8730158747896,0,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,12.3456790123457,0,63.4920635042547,0,0,0,0,0,37.037037037037,0,44.4444444622222,22.2222222231111,0,0,0,0,0,0,0,0,0,0,0,0,24.6913580246914,24.6913580246914,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,79.3650793852405,0,0,44.4444444622222,7.40740740740741,311.111111123556,18.5185185185185,22.2222222222222,3.7037037037037,0,7.40740740740741,0,22.2222222249243,37.037037037037,61.7283950617284,3.7037037037037,7.40740740740741,11.1111111111111,0,24.6913580246914,0,0,0,0,3.7037037037037,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,4.62962962962963,0,0,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,11.1111111118122,0,0,0,0,0,0,0,31.7460317493772,0,0,0,0,22.2222222222222,0,0,0,0,0,0,0,0,37.037037037037,0,0,3.7037037037037,0,0,0,3.7037037037037,0,49.3827160493827,0,0,0,44.4444444510385,3.7037037037037,12.3456790123457,0,0,0,11.1111111111111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15.8730158738018,0,14.8148148148148,37.037037037037,185.185185185185,0,311.111111123556,88.8888888888889,37.037037037037,244.444444454222,74.0740740740741,18.5185185185185,12.3456790123457,3.7037037037037,22.2222222222222,61.7283950617284,7.40740740740741,31.7460317560898,159.259259259259,7.40740740740741,37.037037037037,366.66666672543,0,11.1111111136521,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24.6913580246914,0,12.3456790123457,0,0,0,0,0,12.3456790123457,12.3456790123457,0,3.7037037037037,3.7037037037037,0,0,0,0,37.037037037037,37.037037037037,29.6296296296296,51.8518518518518,14.8148148148148,11.1111111111111,22.2222222231111,22.2222222231111,66.6666666933333,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,11.1111111114922,0,0,12.3456790123457,11.1111111111111,3.7037037037037,7.40740740740741,0,0,44.4444444462222,0,0,3.7037037037037,18.5185185185185,3.7037037037037,14.8148148148148,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12.3456790123457,15.8730158744754,0,0,0,0,0,0,0,0,0,0,0,0,0,11.1111111135721,0,0,0,44.4444444622222,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37.037037037037,0,0,98.7654320987654,111.111111111111,15.8730158798858,0,0,3.7037037037037,0,0,77.7777777903648,111.111111111111,55.5555555555556,222.222222231111,133.333333386667,11.1111111111111,14.8148148148148,40.7407407407407,185.185185185185,177.777777784889,355.555555697778,311.111111123556,218.518518518519,222.222222222222,200.000000008,11.1111111111111,29.6296296296296,123.456790123457,25.9259259259259,44.4444444444444,3.7037037037037,62.962962962963,244.444444474177,7.40740740740741,12.3456790123457,111.111111111111,0,29.6296296296296,95.2380952602097,222.222222222222,0,0,0,0,0,0,0,0,0,0,0,0,0,148.148148148148,0,0,0,0,0,0,0,0,3.7037037037037,7.40740740740741,32.4074074074074,0,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,22.2222222222222,7.40740740740741,0,0,0,0,0,0,0,0,0,0,0,0,14.8148148148148,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,166.666666666667,0,0,0,0,0,0,0,0,59.2592592592593,0,0,0,0,0,0,0,22.2222222231111,11.1111111111111,0,0,0,600.000000024,25.9259259259259,14.8148148148148,88.8888888888889,1155.55555560178,488.888889084444,0,24.6913580246914,44.4444444497686,22.2222222222222,0,3.7037037037037,0,12.3456790123457,18.5185185185185,0,95.2380952599852,0,1171.2962962963,259.259259259259,0,0,0,0,0,0,0,0,0,3.7037037037037,33.3333333333333,12.3456790123457,7.40740740740741,22.2222222222222,74.0740740740741,44.4444444462222,88.8888888924445,0,0,66.6666666933333,0,0,3.7037037037037,0,14.8148148148148,0,0,0,18.5185185185185,0,0,0,0,0,7.40740740740741,74.0740740740741,77.7777777928047,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,88.8888888924445,0,0,0,0,0,37.037037037037,33.3333333402863,49.3827160493827,37.037037037037,0,0,0,0,88.8888889244444,15.8730158738243,37.037037037037,0,22.2222222231111,7.40740740740741,0,0,0,0,0,0,0,11.1111111111111,3.7037037037037,0,0,0,0,0,0,0,0,37.037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22.2222222222222,0,0,0,0,0,0,0,0,0,31.7460317532386,148.148148148148,92.5925925925926,0,0,7.40740740740741,3.7037037037037,0,0,0,0,0,0,0,55.5555555595909,12.3456790123457,22.2222222222222,55.5555555555556,125.925925925926,14.8148148148148,48.1481481481481,70.3703703703704,246.913580246914,269.841269898341,3333.33333333333,129.62962962963,177.777777784889,66.6666666933333,18.5185185185185,307.407407407407,22.2222222222222,85.1851851851852,296.296296296296,100.00000000779,567.901234567901,355.555555569778,222.222222231111,44.4444444444444,222.222222222222,25.9259259259259,172.839506172839,111.111111126081,37.037037037037,125.925925925926,96.2962962962963,0,166.666666666667,126.209599479951,244.444444444444,523.809523929122,629.62962962963,654.320987654321,481.481481481482,177.777777848889,518.518518518519,488.888888908444,0,0,11.1111111111111,0,0,14.8148148148148,12.3456790123457,47.6190476314406,37.037037037037,0,22.2222222231111,0,0,3.7037037037037,0,0,0,11.1111111126121,0,22.2222222231111,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,160.493827160494,0,0,0,0,0,0,0,0,0,61.7283950617284,11.1111111121522,12.3456790123457,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,22.2222222311111,0,0,12.3456790123457,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.4444444462222,0,3.7037037037037,0,0,33.3333333400962,10.5820105820106,0,0,0,14.8148148148148,7.40740740740741,0,0,0,0,0,0,0,0,0,22.2222222231111,0,0,0,0,0,22.2222222311111,0,0,0,14.8148148148148,0,7.40740740740741,0,31.7460317533059,4.11522633744856,4.62962962962963,66.6666666693333,0,31.1111111120036,0,0,0,0,0,0,0,0,0,0,3.7037037037037,962.962962962963,322.222222269512,0,14.8148148148148,70.3703703703704,0,0,25.9259259259259,18.5185185185185,0,592.592592592593,0,12.3456790123457,428.571428682449,0,0,33.3333333333333,22.2222222222222,40.7407407407407,0,22.2222222222222,162.962962962963,25.9259259259259,174.603174640534,148.148148148148,0,22.2222222231111,88.8888889244444,133.333333338667,3.7037037037037,3.7037037037037,0,24.6913580246914,22.2222222239343,37.037037037037,74.0740740740741,0,0,0,0,7.40740740740741,0,3.7037037037037,3.7037037037037,22.2222222222222,3.7037037037037,0,0,0,0,0,0,111.111111111111,22.2222222231111,311.111111235555,111.111111115556,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12.3456790123457,22.2222222246843,0,0,0,0,55.5555555555556,22.2222222231111,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,0,0,0,0,0,12.3456790123457,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11.1111111116322,0,0,0,0,0,0,0,0,0,0,3.7037037037037,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12.3456790123457,11.1111111115222,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,14.8148148148148,0,0,0,25.9259259259259,3.7037037037037,12.3456790123457,66.6666666761628,5.29100529100529,0,0,8.23045267489712,0,0,177.777777848889,0,51.8518518518518,140.740740740741,11.1111111111111,18.5185185185185,40.7407407407407,14.8148148148148,7.40740740740741,159.259259259259,59.2592592592593,29.6296296296296,160.493827160494,244.444444473377,37.037037037037,37.037037037037,111.111111139377,226.337448559671,185.185185185185,88.8888888924445,222.222222311111,102.222222225783,0,0,0,0,3.7037037037037,0,0,0,0,0,12.3456790123457,11.1111111112022,0,0,0,0,0,0,22.2222222311111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15.8730158777081,185.185185185185,0,0,0,0,0,3.7037037037037,0,18.5185185185185,3.7037037037037,55.5555555555556,0,0,0,0,12.3456790123457,55.5555555625507,0,0,31.7460317543612,0,18.5185185185185,22.2222222231111,0,0,0,0,0,0,0,0,0,0,0,0,0,11.1111111118822,0,0,0,0,0,0,0,0,162.962962962963,233.333333333333,44.4444444444444,51.8518518518518,351.851851851852,81.4814814814815,66.6666666666667,77.7777777777778,51.8518518518518,214.814814814815,148.148148148148,66.6666666775827,49.3827160493827,24.6913580246914,238.095238146809,222.222222222222,166.666666666667,44.4444444462222,66.6666666933333,44.4444444462222,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.40740740740741,11.1111111111111,0,14.8148148148148,22.2222222222222,7.40740740740741,14.8148148148148,18.5185185185185,0,0,37.037037037037,44.4444444513885,37.037037037037,49.3827160493827,47.6190476294201,37.037037037037,37.037037037037,22.2222222231111,0,22.2222222231111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,18.5185185185185,0,0,0,3.7037037037037,0,0,0,0,0,0,0,3.7037037037037,0,0,0,0,0,0,0,0,0,0,0,3.7037037037037,3.7037037037037,7.40740740740741,0,3.7037037037037,0,0,25.9259259259259,0,3.7037037037037,24.6913580246914,0,0,0,0,0,37.037037037037,0,0,0,11.1111111111111,18.5185185185185,0,0,903.703703703704,118.518518518519,0,0,59.2592592592593,18.5185185185185,24.6913580246914,133.333333346496,0,0,841.269841457086,407.407407407407,0,0,0,0,40.7407407407407,51.8518518518518,14.8148148148148,77.7777777777778,140.740740740741,159.259259259259,7.40740740740741,70.3703703703704,33.3333333333333,3.7037037037037,61.7283950617284,177.777777802584,74.0740740740741,37.037037037037,238.095238144788,481.481481481482,129.62962962963,66.6666666693333,311.111111235555,400.000000016],["S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S"],["SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","SCe","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","CG","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","SCi","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","CF","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH","SH"]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Taxon<\/th>\n      <th>CollDate<\/th>\n      <th>Stream<\/th>\n      <th>Treatment<\/th>\n      <th>Density<\/th>\n      <th>Size<\/th>\n      <th>FFG<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-right","targets":4}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 4, 2, 3, ',', '.');\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


### Taxa
***

```r
# Plot density differences
ggplot(data = bugs.reach.diff, aes(x = Taxon, y = Diff, fill = Treatment)) + 
  geom_col(position = "dodge") +
  facet_wrap(c("Stream")) +
  ggtitle("Differences in Taxa Abundances Between Reaches Pre and Post Gap Years") +
  scale_fill_viridis_d(option = "E") +
  coord_flip()
```

![](index_files/figure-html/Plot Taxon Density Diffs-1.png)<!-- -->

Plot of density differences between 2017 and 2018 for both reaches. As seen, there are no ubiquitous taxa that consistently increase or decrease across streams. 


### FFG's
***

```r
#Aggregate to get density differences of FFG's
bugs.ffg <- bugs.reach.diff %>% 
  group_by(FFG, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% ungroup
```



```r
#Add total SC, this throws off counts (scrapers doubly reprented)
ffg.SC <- filter(bugs.ffg, FFG %in% c("SCi", "SCe"))
ffg.SC$FFG <- "SC"
ffg.SC <- ffg.SC %>% group_by(FFG, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% ungroup
ffg.SC <- rbind(bugs.ffg, ffg.SC) 
```



```r
ggplot(data = ffg.SC, aes(x = FFG, y = Diff, color = Treatment, shape = Stream)) + 
  geom_jitter(width = .2, height = .2, size = 4) +
  labs(title = "FFG Differences Between Years (Treatment and Control Reaches)", 
       caption = "CF = Collector Filterer, CG = Collector Gatherer,  SC = Scrapers (a composite of SCe and SCi), \n         SCe = Scrapers that are edible, SCi = Scrapers that are inedible, P = Predators") +
  scale_color_viridis_d(option = "E") +
  coord_flip()
```

![](index_files/figure-html/Plot FFG Density Differences-1.png)<!-- -->

Here we plot density differences of FFG's.  

 
Notice the only group with consistently elevated differences in the post gap year (2018) is Scrapers.



```r
bugs18 %>%
  group_by(Stream, Treatment, FFG) %>%
  summarise_at(vars(Density), funs(sum)) %>% 
  ungroup() %>%
  
  ggplot(aes(x = FFG, y = Density, color = Treatment, shape = Stream)) + 
    geom_jitter(width = .2, height = .2, size = 4) + 
    ggtitle("2018 FFG Densities (Control & Treatment)")  +
    scale_color_viridis_d(option = "E") +
    coord_flip()
```

![](index_files/figure-html/Plot 2018 FFG Densities-1.png)<!-- -->

Here we can see that Collector Gatherers are the most abundant taxa, but the aggregate of the two scraper categories (SCe and SCi) is also up there. 

When comparing between the reaches we see elevated abundances of both scraper groups (except for SCi in CHUCK).  Collector Gatherers are also consistently elevated (except in CHUCK), which seems contradictory to the between year comparison (Control reach has always had elevated abundances of CG's?)

- Will these functional feeding groups be important in diets? 

- Does the relative change in abundance of a taxa group change fish selection  (i.e. now that there are more scrapers in the treatment reach are the fish going to town on Scrapers?)

- TBD. (see other.md file in the Diets project)



```r
bugs.reach.diff %>% filter(FFG == "SCe" | FFG == "SCi") %>% 
  ggplot(aes(x = Taxon, y = Diff, color = Treatment, shape = Stream)) + 
    geom_jitter(width = .2, height = .2, size = 4) +
    ggtitle("Diff. Between Reaches of Scraper Taxa (2017 & 2018") +
    scale_color_viridis_d(option = "E") +
    coord_flip()
```

![](index_files/figure-html/Scraper Taxa Density Differences-1.png)<!-- -->

Here we see that the Scraper taxa with the greatest change are: 

- Micrasema

- Juga

- Glossosoma

- Drunella

- Heptageniidae taxa



```r
bugs18 %>% filter(FFG == "SCe" | FFG == "SCi") %>%
  ggplot(aes(x = Taxon, y = Density, color = Treatment, shape = Stream)) +
    geom_jitter(width = .2, height = .2, size = 4) +
    ggtitle("Density of Scraper Taxa in 2018") +
    scale_color_viridis_d(option = "E") +
    coord_flip()
```

![](index_files/figure-html/Plot 2018 Scraper Taxa-1.png)<!-- -->



```r
bugs.reach.diff %>% 
  group_by(Size, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>%
  ungroup() %>%
    ggplot(aes(x = Size, y = Diff, color = Treatment, shape = Stream)) + 
      geom_jitter( width = .2, height = .2, size = 4) +
      ggtitle("Differences in Size Classes") +
      scale_color_viridis_d(option = "E")
```

![](index_files/figure-html/Plot Size Differences-1.png)<!-- -->

Two size classes are plotted, small (S) and large (L) based on nothing, just observation... Hopefully I will soon incorporate some information from the Poff database about final larval instar size and have three size classes: small, medium, and large.


### NMS
***
Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570.  I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.


```r
# Spread data to get a taxon matrix, create factor of CollDate and Treatment
bugs.mds <- bugs.agg %>% 
  select(-c("FFG", "Size")) %>%
  group_by(Stream, Treatment, CollDate, Taxon) %>%  
  spread(key = "Taxon", value = "Density") %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>% ungroup() %>%
  mutate(YearTreat = interaction(CollDate, Treatment))
```



```r
# Get grouping variables and taxon matrix
Enviro <- bugs.mds %>%
  select(Stream, Treatment, CollDate, YearTreat) 

Density <- bugs.mds %>% 
  select("Ameletus":"Zapada")

grouping <- select(Enviro, Stream, Treatment, CollDate, YearTreat)
```



```r
#Get MDS for invert density
sol <- metaMDS(Density, distance = "bray", k = 2, trymax = 100)
```

```
## Square root transformation
## Wisconsin double standardization
## Run 0 stress 0.1726121 
## Run 1 stress 0.1726121 
## ... New best solution
## ... Procrustes: rmse 1.088343e-05  max resid 2.412069e-05 
## ... Similar to previous best
## Run 2 stress 0.1726121 
## ... Procrustes: rmse 5.145144e-06  max resid 1.288155e-05 
## ... Similar to previous best
## Run 3 stress 0.1726121 
## ... Procrustes: rmse 2.978185e-05  max resid 7.206061e-05 
## ... Similar to previous best
## Run 4 stress 0.2287457 
## Run 5 stress 0.1726122 
## ... Procrustes: rmse 0.0001131157  max resid 0.0002641682 
## ... Similar to previous best
## Run 6 stress 0.1726121 
## ... Procrustes: rmse 5.036715e-05  max resid 0.0001125872 
## ... Similar to previous best
## Run 7 stress 0.1726121 
## ... New best solution
## ... Procrustes: rmse 2.371582e-06  max resid 4.925835e-06 
## ... Similar to previous best
## Run 8 stress 0.1726122 
## ... Procrustes: rmse 0.0001348066  max resid 0.0003069075 
## ... Similar to previous best
## Run 9 stress 0.1726122 
## ... Procrustes: rmse 0.0001038851  max resid 0.0002312197 
## ... Similar to previous best
## Run 10 stress 0.1726121 
## ... Procrustes: rmse 5.222521e-06  max resid 1.278906e-05 
## ... Similar to previous best
## Run 11 stress 0.1726121 
## ... Procrustes: rmse 3.285539e-05  max resid 8.987912e-05 
## ... Similar to previous best
## Run 12 stress 0.1726121 
## ... Procrustes: rmse 4.742137e-05  max resid 0.0001067743 
## ... Similar to previous best
## Run 13 stress 0.1726121 
## ... Procrustes: rmse 1.159735e-05  max resid 2.82002e-05 
## ... Similar to previous best
## Run 14 stress 0.2437675 
## Run 15 stress 0.1726121 
## ... Procrustes: rmse 6.751397e-05  max resid 0.0001508694 
## ... Similar to previous best
## Run 16 stress 0.1726121 
## ... Procrustes: rmse 1.473293e-05  max resid 3.183357e-05 
## ... Similar to previous best
## Run 17 stress 0.1726121 
## ... Procrustes: rmse 2.726293e-05  max resid 6.344963e-05 
## ... Similar to previous best
## Run 18 stress 0.2488585 
## Run 19 stress 0.1726121 
## ... Procrustes: rmse 1.263578e-05  max resid 2.885207e-05 
## ... Similar to previous best
## Run 20 stress 0.1738211 
## *** Solution reached
```

This is a pretty good stress value, who knows how that will change when I start doing this right...
Can see the transfrmations applied at the top of the output, this was done automatically, I wouldn't know what to do. Used the Bray-Curtis distance metric, max tries is set to 100 with dimensions equal to 2.


```r
#set up NMDS with dimensions of sol and env factors from "Enviro". 
NMDS <- data.frame(x = sol$points[, 1], y = sol$points[ ,2], 
                   Stream = select(grouping, Stream), 
                   Treatment = select(grouping, Treatment), 
                   CollDate = select(grouping, CollDate),
                   YearTreat = select(grouping, YearTreat))
```



```r
#Get mean x,y values 
NMDS.mean <- select(NMDS, x, y) %>% aggregate(grouping, mean)
```



```r
#Load ellipse for NMDS
plot.new()
ord <- ordiellipse(sol, NMDS$YearTreat, display = "sites", kind = "sd", conf = 0.95, label = TRUE)
```

![](index_files/figure-html/ELlipse-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

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


```r
ggplot(data = NMDS, aes(x, y, color = YearTreat)) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2)) +
  annotate("text", x = (NMDS$x), y = (NMDS$y) + .04, label = NMDS$Stream, size = 2) +
  geom_point(aes(shape = Stream)) +
  ggtitle("NMDS of Benthic Invertebrate Community") +
  scale_color_viridis_d()
```

![](index_files/figure-html/Plot Benthic NMS-1.png)<!-- -->

Actually maybe cool result, all of the treatment reaches now plot closer post-gap year. The controls show a similar pattern though maybe a little less pronounced. Everything also shifted left between the two years, although this isn't because of the treatment, just annual changes. Maybe the gap amplified whatever effect the year had, extra, extra light or temperature maybe.


### Summary Info 
***


```r
bugs.agg %>% mutate(CollDate.Treatment = interaction(Treatment, CollDate)) %>%
group_by(Stream, CollDate.Treatment) %>%
  summarise_at(vars(Density), funs(sum)) %>%
  ggplot(aes(x = Stream, y = Density, fill = CollDate.Treatment)) + 
  geom_col(position = "dodge") +
  ggtitle("Total Inverebrate Density (m^2)") +
  scale_fill_viridis_d(option = "E") 
```

![](index_files/figure-html/Total Densities-1.png)<!-- -->

```r
  #datatable(rownames = FALSE, class = "hover", filter = "top", 
            #options = list(pageLength = 10, scrollX=T), ) %>% 
            #formatRound("Density", digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"))
```


```r
group_by(bugs.reach.diff, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% 
  arrange(Treatment) %>% 
  ggplot(aes(x = Stream, y = Diff, fill = Treatment)) + 
  geom_col(position = "dodge") +
  ggtitle("Differences in Density (T-C)") +
  scale_fill_viridis_d(option = "E") 
```

![](index_files/figure-html/Density Diffs-1.png)<!-- -->

```r
  #datatable(rownames = FALSE, class = "hover", filter = "top", 
            #options = list(pageLength = 10, scrollX=T), ) %>% 
            #formatRound("Diff", digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"))
```


```r
bugs.agg %>% mutate(CollDate.Treatment = interaction(Treatment, CollDate)) %>%
  group_by(Stream, CollDate.Treatment, FFG) %>%
  summarise_at(vars(Density), funs(sum)) %>% write_csv("./Data/FFG.Densities.csv")
  
  
  #ggplot(aes(x = FFG, y = Density, fill = CollDate.Treatment)) + 
  #geom_col(position = "dodge") +
  #facet_wrap("Stream") +
  #ggtitle("Differences in Density By FFG (T-C)") +
  #theme_bw() +
     #theme(plot.title = element_text(size = 40, face = "bold")) +
  #scale_fill_viridis_d(option = "E") 
  
  #arrange(CollDate) %>% write_csv("./Data/ffg.diffs.csv")
  
  #datatable(rownames = FALSE, class = "hover", filter = "top", options = list(pageLength = 10, #scrollX=T), ) %>% formatRound("Diff", digits = 2, interval = 3, 
                                         #mark = ",", dec.mark = getOption("OutDec"))
```



## Diets {.tabset .tabset-fade .tabset-pills}
***
<br>


```r
# Fill missing family's with order
Diets$Family[is.na(Diets$Family)] <- Diets$Order[is.na(Diets$Family)]

# Merge diets, sizes and FFG's
Diets <- merge(Diets, sizes, by.x = "Family", by.y = "Taxon") %>% 
  merge(., ffg, by.x = "Family", by.y = "Taxon")

# Aggregate by rep
Diets.by.fish <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size, Rep) %>%
  summarise_at("Count", funs(sum)) %>% ungroup

# Aggregate by reach
Diets.reach <- Diets %>% 
  group_by(Family, Stream, Treatment, Origin, FFG, Size) %>%
  summarise_at("Count", funs(sum)) %>% ungroup
```


```r
bugs.family <- bugs %>% 
  filter(CollDate == "2018") %>%
  select(-Taxon) %>% 
  group_by(Stream, Treatment, Family, CollDate) %>%
  summarise_at(vars(Density), funs(sum)) %>% ungroup

bugndiet <- merge(Diets.reach, bugs.family, all.x = TRUE, all.y = TRUE) %>% 
  filter(Origin == "A") %>%
  mutate_if(is.numeric, replace_na, replace = 0)
```



```r
Diets.reach$Stream.Treat <- interaction(Diets.reach$Treatment, Diets.reach$Stream)

Diets.reach %>%
  group_by(Stream.Treat, Origin) %>%
  summarise_at(vars(Count), funs(sum)) %>% ungroup() %>%

ggplot(aes(x = Stream.Treat, y = Count, fill = Origin)) +
  geom_col() +
  labs(title = "Total Aquatics and Terrestricals by Reach") +
    theme(legend.position = c(.84, .8), legend.key.width = unit(5, "cm")) +
    scale_fill_viridis_d(option = "E", end = .8)
```

![](index_files/figure-html/Terrestrial Vs. Aquatic-1.png)<!-- -->

```r
?scale_fill_viridis_d
```


### By Family
***


```r
# Plot proportion of a taxon in the benthic community versus proporion of a taxon in the diet community

bugndiet %>% 
  group_by(Stream, Treatment, Family) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density)) %>% ungroup() %>%
  
  ggplot(aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    coord_cartesian(ylim = c(0, .8), xlim = c(0, .8)) +
    geom_text(aes(x = frac.benthic, y = frac.diet, label = Family), size = 3, nudge_y = .02) +
    ggtitle("1-1 Prop. of Taxa in Benthic Comm. Vs. Prop. in Diets") +
    facet_wrap("Stream") +
    scale_color_viridis_d(option = "E") +
    theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/Plot Taxa Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual taxon in the benthic community versus the proportional abundance of the aggregate diets for that reach. 

Here we see that Chironomidae typically make up the greatest abundance of both the fish diets and the benthic community, with LOON being the exception (Brachycentridae are equally represented in the diets of control reach and over represented in the treatment). In MCTE, Juga compose a large part of the diets relative to their abundance due to one outlier fish. 

There are no overarching differences in how a taxa maps out given the treatment, but the difference in Brachycentridae in LOON between treatment and control is interesting.

### By FFG
***


```r
# Plot proportion of a FFG in the benthic community versus proporion of a FFG in the diet community
bugndiet %>% 
  group_by(Stream, Treatment, FFG) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density)) %>%
  mutate_if(is.character, replace_na, replace = "T") %>%

  ggplot(aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
    geom_text(aes(x = frac.benthic, y = frac.diet, label = FFG), size = 4, nudge_y = .02) +
    facet_wrap("Stream") +
    ggtitle("1-1 Prop. of FFG's in Benthic Comm. Vs. Prop. in Diets") +
    scale_color_viridis_d(option = "E") +
    theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/Plot FFG Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual FFG in the benthic community versus the proportional abundance of the aggregate diets for that reach.

We see that Collector Gatherers and Shredders are the most abundant both in diets and in the benthic community.  There aren't any over arching trends in how FFG's fall out by treatment.


### By Size Class
***


```r
# Plot proportion of a size class in the benthic community versus proporion of a FFG in the diet community
bugndiet %>% 
  group_by(Stream, Treatment, Size) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(frac.diet = Count / sum(Count)) %>%
  mutate(frac.benthic = Density / sum(Density)) %>%
  mutate_if(is.character, replace_na, replace = "T") %>%

  ggplot(aes(x = frac.benthic, y = frac.diet, color = Treatment)) +
    geom_point() +
    geom_text(aes(x = frac.benthic, y = frac.diet, label = Size), size = 4, nudge_y = .02) +
    geom_abline(slope = 1, intercept = 0) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
    facet_wrap("Stream") +
    ggtitle("1-1 Prop. of Size Classes in Benthic Comm. Vs. Prop. in Diets") +
    scale_color_viridis_d(option = "E") +
    theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/Plot Size Proportions-1.png)<!-- -->

Plot of proportional abundance of an individual size class in the benthic community versus the proportional abundance of the aggregate diets for that reach.


### Costello method plot
***


```r
Diets.fish <- Diets.by.fish %>% 
  group_by(Stream, Treatment, Rep) %>%
  mutate(frac.diet = Count / sum(Count))

Diets.fish <- Diets.fish %>% 
  group_by(Stream, Treatment, Family) %>%
  mutate(frac.fish = length(Family[Count > 0])) %>%
  group_by(Stream, Treatment) %>%
  mutate(frac.fish = frac.fish / max(Rep)) %>%
  group_by(Stream, Treatment, Family)

Diets.fish <- Diets.fish %>%
  summarise_at(vars(frac.fish, frac.diet), funs(mean))
```



```r
ggplot(data = Diets.fish) +
  geom_point(mapping = aes(x = frac.fish, y = frac.diet, color = Treatment)) +
  geom_text(aes(x = frac.fish, y = frac.diet, label = Family), size = 3, nudge_y = .02) +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  facet_wrap("Stream") +
  ggtitle("Costello Plot of Prop. Fish Occurance Vs. Prop. Diet Community") +
  scale_color_viridis_d(option = "E") +
  theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/Plot Costello-1.png)<!-- -->

The Costello method plots % occurance of a taxon in fish versus % of aggregate diet. With the adjusted method the aggregate diet is only of fish that had the taxon present (ignoring zero values).  We see that in MCTE and in W-100 there seem to be just a couple fish that specifically target Juga and ELmidae respectively. In MCTE, the taxa from treatment diets seem to consistently constitute a smaller portion of the diet than would be expected given how many fish they occur in. 



### Diet MDS
***

Who knows what I'm actually doing, all of this ordination stuff is currently pre BOT 570.  I will definitely update all this once I know how to handle singleton taxa, loads of zero's, etc.


```r
# Spread data to get a taxon matrix, create factor of CollDate and Treatment
bugs.mds <- Diets.reach %>% 
  filter(Origin %in% c("A", "T")) %>%
  select(-c("FFG", "Size", "Origin")) %>%
  slice(-65) %>%
  spread(key = "Family", value = "Count") %>%
  mutate_if(is.numeric , replace_na, replace = 0) 
```



```r
# Get grouping variables and taxon matrix
Enviro <- bugs.mds %>%
  select(Stream, Treatment) 

Density <- bugs.mds %>% 
  select("Amphizoidae":"Uenoidae")

grouping <- select(Enviro, Stream, Treatment)
```



```r
#Get MDS for invert density
sol <- metaMDS(Density, distance = "bray", k = 2, trymax = 100)
```

```
## Wisconsin double standardization
## Run 0 stress 0.128467 
## Run 1 stress 0.1204532 
## ... New best solution
## ... Procrustes: rmse 0.2197927  max resid 0.4822916 
## Run 2 stress 0.1204535 
## ... Procrustes: rmse 0.0001837829  max resid 0.0004065083 
## ... Similar to previous best
## Run 3 stress 0.1284668 
## Run 4 stress 0.1284667 
## Run 5 stress 0.1284672 
## Run 6 stress 0.1204532 
## ... New best solution
## ... Procrustes: rmse 0.0001003894  max resid 0.0002061988 
## ... Similar to previous best
## Run 7 stress 0.2129742 
## Run 8 stress 0.2340484 
## Run 9 stress 0.1204533 
## ... Procrustes: rmse 0.0001343163  max resid 0.0002856142 
## ... Similar to previous best
## Run 10 stress 0.2686448 
## Run 11 stress 0.1204531 
## ... New best solution
## ... Procrustes: rmse 5.374669e-05  max resid 0.000113578 
## ... Similar to previous best
## Run 12 stress 0.1204534 
## ... Procrustes: rmse 0.0002344231  max resid 0.0004790352 
## ... Similar to previous best
## Run 13 stress 0.1204533 
## ... Procrustes: rmse 0.000203938  max resid 0.0004227311 
## ... Similar to previous best
## Run 14 stress 0.1204531 
## ... New best solution
## ... Procrustes: rmse 1.499158e-05  max resid 2.825428e-05 
## ... Similar to previous best
## Run 15 stress 0.1284668 
## Run 16 stress 0.1204533 
## ... Procrustes: rmse 0.0002643004  max resid 0.0005826968 
## ... Similar to previous best
## Run 17 stress 0.1204532 
## ... Procrustes: rmse 0.0001492341  max resid 0.0003213405 
## ... Similar to previous best
## Run 18 stress 0.1284671 
## Run 19 stress 0.2397105 
## Run 20 stress 0.1204532 
## ... Procrustes: rmse 0.0001814719  max resid 0.0003873191 
## ... Similar to previous best
## *** Solution reached
```



```r
#set up NMDS with dimensions of sol and env factors from "Enviro". 
NMDS <- data.frame(x = sol$points[, 1], y = sol$points[ ,2], 
                   Stream = select(grouping, Stream), 
                   Treatment = select(grouping, Treatment))
```



```r
#Get mean x,y values 
NMDS.mean <- select(NMDS, x, y) %>% aggregate(grouping, mean)
```



```r
#Load ellipse for NMDS
plot.new()
ord <- ordiellipse(sol, NMDS$Treatment, display = "sites", kind = "sd", conf = 0.95, label = TRUE)
```

![](index_files/figure-html/Diet ELlipse-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
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


```r
ggplot(data = NMDS, aes(x, y, color = Treatment)) +
  geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2)) +
  annotate("text", x = (NMDS$x), y = (NMDS$y) + .04, label = NMDS$Stream, size = 3) +
  geom_point(aes(shape = Stream)) +
  ggtitle("NMDS of Community in Fish Diets") +
  scale_color_viridis_d() +
  theme(legend.position = c(.84, .27), legend.key.width = unit(5, "cm"))
```

![](index_files/figure-html/NMDS Plot6-1.png)<!-- -->

There seems to be total overlap of the diet communities in the control versus treatment reaches.  This seems to be consistent with what we saw in other plots.


### Summary Info. 
***


```r
group_by(bugs.agg, Stream, Treatment, CollDate) %>%
  summarise_at(vars(Density), funs(sum)) %>% 
  arrange(CollDate, Treatment) %>% 
  datatable(rownames = FALSE, class = "hover", filter = "top", options = list(pageLength = 10, scrollX=T)) %>% formatRound("Density", digits = 2, interval = 3, 
                                          mark = ",", dec.mark = getOption("OutDec"))
```

<!--html_preserve--><div id="htmlwidget-89887193163068020890" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-89887193163068020890">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;2017&quot;,&quot;2018&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"274.074074074074\" data-max=\"11506.1728395062\" data-scale=\"13\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["CHUCK","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","W-113"],["N","N","N","N","N","Y","Y","Y","Y","Y","N","N","N","N","N","Y","Y","Y","Y","Y"],["2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018"],[892.592592592593,274.074074074074,2396.57996985032,1440.74074074074,829.62962962963,1466.66666666667,785.185185185185,1307.40740740741,2862.96296296296,766.666666666667,3740.74074074074,3176.36684303351,4908.46560955598,6231.48148148148,6822.22222495111,3377.77777823837,3308.64197530864,11506.1728395062,6644.44444471022,6697.77777804446]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Stream<\/th>\n      <th>Treatment<\/th>\n      <th>CollDate<\/th>\n      <th>Density<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-right","targets":3}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 3, 2, 3, ',', '.');\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


```r
group_by(bugs.reach.diff, Stream, Treatment) %>%
  summarise_at(vars(Diff), funs(sum)) %>% 
  arrange(Treatment) %>% 
  datatable(rownames = FALSE, class = "hover", filter = "top", options = list(pageLength = 10, scrollX=T)) %>% formatRound("Diff", digits = 2, interval = 3, 
                                          mark = ",", dec.mark = getOption("OutDec"))
```

<!--html_preserve--><div id="htmlwidget-0319030f70dc046286d2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0319030f70dc046286d2">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1608.52439754126\" data-max=\"8984.28865386366\" data-scale=\"13\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["CHUCK","LOON","MCTE","W-100","W-113","CHUCK","LOON","MCTE","W-100","W-113"],["N","N","N","N","N","Y","Y","Y","Y","Y"],[3473.01587325772,1608.52439754126,4948.86094778117,3157.71604960482,5377.24867827169,3454.14462133267,2944.57671982739,8984.28865386366,3272.83950761391,6170.37037070016]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Stream<\/th>\n      <th>Treatment<\/th>\n      <th>Diff<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-right","targets":2}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 2, 2, 3, ',', '.');\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


```r
group_by(bugs.reach.diff, Stream, Treatment, FFG) %>%
  summarise_at(vars(Diff), funs(sum)) %>% 
  arrange(Treatment) %>% 
  datatable(rownames = FALSE, class = "hover", filter = "top", options = list(pageLength = 10, scrollX=T)) %>% formatRound("Diff", digits = 2, interval = 3, 
                                          mark = ",", dec.mark = getOption("OutDec"))
```

<!--html_preserve--><div id="htmlwidget-c94bc2b9dbe770a1b9e9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c94bc2b9dbe770a1b9e9">{"x":{"filter":"top","filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"-77.7777777777778\" data-max=\"4403.70370374993\" data-scale=\"15\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["CHUCK","CHUCK","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","LOON","LOON","LOON","LOON","MCTE","MCTE","MCTE","MCTE","MCTE","MCTE","W-100","W-100","W-100","W-100","W-100","W-100","W-113","W-113","W-113","W-113","W-113","W-113","CHUCK","CHUCK","CHUCK","CHUCK","CHUCK","CHUCK","LOON","LOON","LOON","LOON","LOON","LOON","MCTE","MCTE","MCTE","MCTE","MCTE","MCTE","W-100","W-100","W-100","W-100","W-100","W-100","W-113","W-113","W-113","W-113","W-113","W-113"],["N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"],["CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH","CF","CG","P","SCe","SCi","SH"],[623.456790123457,1581.83421529442,320.987654321877,388.536155247438,321.693121750193,236.50793652033,1.23456790123457,879.188712584118,158.43621399177,113.756613778728,225.925925954686,229.982363330724,150.26455027288,3065.85389263093,-7.51322740454613,262.610229339746,1300.92592595437,176.719576987794,1111.11111111111,164.197530865975,567.592592595259,90.1234567901235,900.000000204444,324.691358037902,377.777777937778,2824.69135823269,496.296296563764,427.160493992494,741.446208118199,509.876543426765,-22.2222222152271,1852.20458585735,320.634920760791,141.97530864298,332.098765432099,829.453262854669,406.349206460227,1736.68430346758,150.925925925926,93.8271604962882,466.666666686365,90.1234567910124,-77.7777777777778,2440.74074077807,1269.8853615617,176.543209876543,4403.70370374993,771.193415675193,774.074074123141,2018.51851978942,125.925925944593,276.543209904988,-27.1604937592747,104.938271611036,120.987654325432,3797.53086435664,159.259259277935,570.370370419259,981.481481526815,540.740740794074]],"container":"<table class=\"hover\">\n  <thead>\n    <tr>\n      <th>Stream<\/th>\n      <th>Treatment<\/th>\n      <th>FFG<\/th>\n      <th>Diff<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-right","targets":3}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 3, 2, 3, ',', '.');\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->



##
***

Session Info:


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
## [16] tidyr_0.8.1       tibble_1.4.2      ggplot2_3.1.0    
## [19] tidyverse_1.2.1  
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.3.1       jsonlite_1.5     modelr_0.1.2     shiny_1.2.0     
##  [5] assertthat_0.2.0 cellranger_1.1.0 yaml_2.2.0       pillar_1.2.3    
##  [9] backports_1.1.2  glue_1.2.0       digest_0.6.18    promises_1.0.1  
## [13] rvest_0.3.2      colorspace_1.3-2 htmltools_0.3.6  httpuv_1.4.5    
## [17] Matrix_1.2-14    plyr_1.8.4       psych_1.8.4      pkgconfig_2.0.1 
## [21] broom_0.4.4      haven_1.1.1      xtable_1.8-3     scales_1.0.0    
## [25] later_0.7.5      mgcv_1.8-24      withr_2.1.2      lazyeval_0.2.1  
## [29] cli_1.0.0        mnormt_1.5-5     magrittr_1.5     crayon_1.3.4    
## [33] mime_0.6         evaluate_0.10.1  nlme_3.1-137     MASS_7.3-50     
## [37] xml2_1.2.0       foreign_0.8-70   tools_3.5.1      hms_0.4.2       
## [41] munsell_0.5.0    cluster_2.0.7-1  compiler_3.5.1   rlang_0.3.0.1   
## [45] grid_3.5.1       rstudioapi_0.7   htmlwidgets_1.3  crosstalk_1.0.0 
## [49] labeling_0.3     rmarkdown_1.10   gtable_0.2.0     reshape2_1.4.3  
## [53] R6_2.3.0         gridExtra_2.3    knitr_1.20       bindr_0.1.1     
## [57] rprojroot_1.3-2  stringi_1.2.3    parallel_3.5.1   Rcpp_1.0.0      
## [61] tidyselect_0.2.4
```





