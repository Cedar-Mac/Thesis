library(tidyverse)
density <- t(Density) 
density <- as.tibble(density)
rownames(density) <- colnames(Density)

filter_dens <- density %>% 
  rownames_to_column("rowname") %>%
  filter_if(is.numeric, all_vars(rowMeans(density, na.rm = FALSE) > 20)) %>%
  column_to_rownames(.) %>%
  t(.)
