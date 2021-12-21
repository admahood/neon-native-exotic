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

lut_vars<-c("PRISM_vpdmax_30yr_normal_4kmM2_annual_bil" = "MAVPD",
            "nlcdClass" = "NLCD",
            "PRISM_tmean_30yr_normal_4kmM2_annual_bil" = "MAT",
            "PRISM_ppt_30yr_normal_4kmM2_annual_bil" = "MAP",
            "latitude" = "Latitude",
            "elevation" = "Elevation",
            "ai_et0" = "Aridity",
            "nspp_native" = "Native Richness",
            "nspp_notexotic" = "Native Richness")

ggrf<- function(rf, title){
  require(ggplot2)
  require(dplyr)
  require(ggpubr)
  
  if(rf$type == "classification"){
    imp <- randomForest::importance(rf) %>%
      as_tibble(rownames = "variable") %>%
      arrange((MeanDecreaseGini)) %>%
      mutate(variable = lut_vars[variable],
             variable = fct_inorder(variable))
      
    acc <- 1-mean(rf$confusion[1,3], rf$confusion[2,3])
    acc<- paste("Accuracy is roughly:", round(acc, 2))
    
    ggplot(imp, aes(x=MeanDecreaseGini, y = variable)) +
      geom_point(size=3) +
      xlab("Variable Importance") +
      ggtitle(label = title, subtitle = acc) +
      theme_pubclean()
    
  }else{
    imp <- randomForest::importance(rf) %>%
      as_tibble(rownames = "variable") %>%
      arrange((IncNodePurity)) %>%
      mutate(variable = lut_vars[variable],
             variable = fct_inorder(variable))
    
    acc<- rf$rsq %>% mean()
    acc<- paste("Accuracy is roughly:", round(acc, 2))
    
    ggplot(imp, aes(x=IncNodePurity, y = variable)) +
      geom_point(size=3) +
      xlab("Variable Importance") +
      ggtitle(label = title, subtitle = acc)+
      theme_pubclean()
    
  }
}

rf1<-randomForest(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
      latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
      PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
    data = plot_level_w_climate)

randomForest::varImpPlot(rf1)  
p1<-ggrf(rf1, title = "Plot level, predicting invasion")

  
rf2<-randomForest(invaded ~ nspp_native + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
                    latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
                    PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
                  data = sp1_level_w_climate)

randomForest::varImpPlot(rf2)  
p2<-ggrf(rf2, title = "1m subplot level, predicting invasion")

plot(rf1)
plot(rf2)

rf3<-randomForest(nspp_exotic ~ nspp_notexotic + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
                    latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
                    PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
                  data = plot_level_w_climate)

randomForest::varImpPlot(rf3)  
p3<-ggrf(rf3, title = "Plot level, predicting nspp_exotic")

rf4<-randomForest(nspp_exotic ~ nspp_notexotic + ai_et0 +PRISM_ppt_30yr_normal_4kmM2_annual_bil +
                    latitude + PRISM_tmean_30yr_normal_4kmM2_annual_bil+
                    PRISM_vpdmax_30yr_normal_4kmM2_annual_bil + elevation +nlcdClass,
                  data = sp1_level_w_climate)

randomForest::varImpPlot(rf4)  

p4<-ggrf(rf4, title = "1m supbplot level, predicting nspp_exotic")

plot(rf3)
plot(rf4)

ggarrange(p1,p2,p3,p4, nrow = 2, ncol=2) -> all_rfs

ggsave(all_rfs, filename="draft_figures/all_rfs.png", width=10, height=10)




