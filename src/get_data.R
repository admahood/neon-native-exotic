# script to download data
library(tidyverse)
library(sf)

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

