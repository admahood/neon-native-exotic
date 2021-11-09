library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)
# devtools::install_github("admahood/neondiveRsity")
library(neondiveRsity) 
options(stringsAsFactors = FALSE)

x<-readRDS("data/diversity.RDS")

if(!file.exists("data/plot_level.RDS")){
  plot_level <- get_diversity_info(neon_div_object = x, 
                                   scale = "plot", 
                                   betadiversity = TRUE) 
  saveRDS(plot_level, "data/plot_level.RDS")
  }else{
  plot_level <- readRDS("data/plot_level.RDS")
}

if(!file.exists("data/sp_level_1.RDS")){
  sp_level_1 <- get_diversity_info(x, "1m", betadiversity = TRUE)
  saveRDS(sp_level_1, "data/sp_level_1.RDS")
  }else{
  sp_level_1<-readRDS("data/sp_level_1.RDS")
}

if(!file.exists("data/site_level.RDS")){
  site_level <- get_diversity_info(x, "site", betadiversity = TRUE)
  saveRDS(site_level, "data/site_level.RDS")
  }else{
  site_level <- readRDS("data/site_level.RDS")
}

# maybe we're really just interested in comparing 1m and plot scale and site scale

# sp_level_10 <- get_diversity_info(x, "10m")
# sp_level_100 <- get_diversity_info(x, "100m")
# all_scales <- rbind(plot_level, sp_level_1, sp_level_10, sp_level_100) %>%
#   mutate(site_name = lut_sites[site])
# 
# saveRDS(all_scales, "data/all_scales.RDS")

