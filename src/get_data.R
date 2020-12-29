# script to download data
library(tidyverse)
library(sf)
library(neondiversity)
library(neonUtilities)

# neon plots ===================================================================
url <- "https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V8.zip"
file <- str_split(url, "/", simplify = T)[length(str_split(url, "/", simplify = T))]
exdir <-file.path("data", "big",str_split(file, "\\.",simplify=T)[1])
dir.create(exdir,recursive = T)

download.file(url=url, destfile = file.path("data","big",file))
unzip(zipfile = file.path("data","big",file),
      exdir = exdir)

sites <- read_csv(file.path(exdir,
                   str_split(file, "\\.",simplify=T)[1],
                   "nlcdCoverBySite.csv")) %>%
  dplyr::group_by(domainID) %>%
  dplyr::summarise(site = unique(siteID)) %>%
  ungroup()

write_csv(sites, "data/all_terrestrial_sites.csv")

# getting alllll the diversity data ============================================
all_sites <- pull(sites, site)

if(!file.exists("data/diversity.RDS")){
  
  loadByProduct(dpID = "DP1.10058.001", 
                site = all_sites, 
                check.size = F) -> x
  saveRDS(x, "data/diversity.RDS")
  
  }else{
    x<-readRDS("data/diversity.RDS")
    }

# Boo-yah! now we're generating datasets =======================================

# lots -o- RAM, takes a while
plot_level <- get_diversity_info(neon_div_object = x, scale = "plot")
sp_level_1 <- get_diversity_info(x, "1m")
sp_level_10 <- get_diversity_info(x, "10m")
sp_level_100 <- get_diversity_info(x, "100m")

# uggh some kind of glitch I need to fix is messing up column names
# in the meantime...
plot_level <- plot_level%>%
  dplyr::select(site, scale, plotID,subplotID,year, 
                nspp_exotic, nspp_native, nspp_unk, invaded, nspp_total)
sp_level_1<- sp_level_1%>%
  dplyr::select(site, scale, plotID,subplotID,year, 
                nspp_exotic, nspp_native, nspp_unk, invaded, nspp_total)
sp_level_10<- sp_level_10%>%
  dplyr::select(site, scale, plotID,subplotID,year, 
                nspp_exotic, nspp_native, nspp_unk, invaded, nspp_total)
sp_level_100<- sp_level_100%>%
  dplyr::select(site, scale, plotID,subplotID,year, 
                nspp_exotic, nspp_native, nspp_unk, invaded, nspp_total)

all_scales <- rbind(plot_level, sp_level_1, sp_level_10, sp_level_100)
saveRDS(all_scales, "data/all_scales.RDS")
