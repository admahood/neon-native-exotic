# extract raster data to neon plots
library(sf)
library(tidyverse)
library(terra)
library(neondiveRsity)

neon_plots <- st_read("data/big/All_NEON_TOS_Plots_V8/All_NEON_TOS_Plots_V8/All_NEON_TOS_Plot_Centroids_V8.shp")

# prism normals ================================================================

clim_files <- list.files("data/prism_normals", 
                         pattern = "bil.bil$",
                         full.names = TRUE)%>%
  terra::rast()

neon_clim<-terra::extract(clim_files,
               as(neon_plots, "SpatVector"),
               list=FALSE) %>%
  as_tibble()%>%
  bind_cols(neon_plots)%>%
  mutate(latitude = st_coordinates(geometry)[,2])

# glimpse(neon_clim)

# aridity index ================================================================

ai_file <- "data/big/ai_et0/ai_et0.tif" %>%
  terra::rast() %>% 
  terra::extract(., as(neon_plots, "SpatVector")) %>%
  as_tibble() %>%
  bind_cols(neon_clim)

# adding in some ancillary data to explore the 1m scale - why is it positve vs negative??
all_scales <- readRDS("data/all_scales.RDS") 

plot_level %>% is.na()%>% any()
plot_level_w_climate<- left_join(plot_level, ai_file %>% 
            filter(!is.na(PRISM_tmean_30yr_normal_4kmM2_annual_bil)) %>%
            dplyr::select(-vertUncert, -horzUncert, -gpsLogs, -plotPdop, -plotHdop)) %>% 
  filter(!is.na(aspect)) %>%
  filter(!is.na(ai_et0)) %>%
  mutate(invaded = as.factor(invaded),
         nlcdClass = as.factor(nlcdClass))

sp1_level_w_climate<- left_join(sp_level_1, ai_file %>% 
                                   filter(!is.na(PRISM_tmean_30yr_normal_4kmM2_annual_bil)) %>%
                                   dplyr::select(-vertUncert, -horzUncert, -gpsLogs, -plotPdop, -plotHdop)) %>% 
  filter(!is.na(aspect)) %>%
  filter(!is.na(ai_et0)) %>%
  filter(invaded != 0) %>%
  mutate(invaded = as.factor(invaded),
         nlcdClass = as.factor(nlcdClass),
         nc= rnorm(n=nrow(.))) %>% # randomly selecting subplots
  group_by(plotID) %>%
  filter(nc == min(nc)) %>%
  ungroup() %>%
  dplyr::select(-nc)

# models ===================
library(randomForest)

rf1<-randomForest(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
      latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
      PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
    data = plot_level_w_climate)

randomForest::varImpPlot(rf1)  
  
rf2<-randomForest(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
                    latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
                    PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
                  data = sp1_level_w_climate)

randomForest::varImpPlot(rf2)  

plot(rf1)
plot(rf2)

# kind of interesting how the two tests aren't super similar

control = party::ctree_control(nresample = 99999,
                               testtype = "MonteCarlo")
ctree1<- party::ctree(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
               latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
               PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
             data = sp1_level_w_climate)

plot(ctree1)

ctree2<- party::ctree(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
                        latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
                        PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
                      data = plot_level_w_climate)
plot(ctree2)

