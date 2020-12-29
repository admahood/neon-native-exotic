library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)
# devtools::install("admahood/neondiversity")
library(neondiversity) 
options(stringsAsFactors = FALSE)

#######################
# using neondiversity #
#######################

x<-readRDS("data/diversity.RDS")

plot_level <- get_diversity_info(neon_div_object = x, scale = "plot")
sp_level_1 <- get_diversity_info(x, "1m")
sp_level_10 <- get_diversity_info(x, "10m")
sp_level_100 <- get_diversity_info(x, "100m")
all_scales <- rbind(plot_level, sp_level_1, sp_level_10, sp_level_100) %>%
  mutate(site_name = lut_sites[site])


