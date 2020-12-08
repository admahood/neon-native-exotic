# getting heights (totally unrelated)
library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)
# devtools::install("admahood/neondiversity")
library(neondiversity) 
options(stringsAsFactors = FALSE)

####################
# downloading data #
####################
sites <- c("SRER", "ONAQ", "MOAB", "JORN")
lut_sites <- c("SRER" = "Santa Rita",
               "ONAQ" = "Onaqui",
               "MOAB" = "Moab",
               "JORN" = "Jornada")
# if statement helps avoid downloading over and over
if(!file.exists("data/diversity.RDS")){
  loadByProduct(dpID = "DP1.10058.001", 
                site = sites, 
                check.size = F) -> x
  saveRDS(x, "data/diversity.RDS")}else{
    x<-readRDS("data/diversity.RDS")}

m2<- x$div_1m2Data %>%
  group_by(scientificName) %>%
  summarise(height = mean(heightPlantSpecies, na.rm=T),
            mad_height = mad(heightPlantSpecies, na.rm=T),
            n = n()) %>%
  ungroup %>%
  na.omit()
