# extract raster data to neon plots
library(sf)
library(tidyverse)
library(terra)

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
  bind_cols(neon_plots)

# glimpse(neon_clim)

# aridity index ================================================================

ai_file <- "data/big/ai_et0/ai_et0.tif" %>%
  terra::rast() %>% 
  terra::extract(., as(neon_plots, "SpatVector")) %>%
  as_tibble() %>%
  bind_cols(neon_clim)

# summary(ai_file)

# adding in some ancillary data to explore the 1m scale - why is it positve vs negative??
all_scales <- readRDS("data/all_scales.RDS")

one_meter <- all_scales %>%
  filter(scale == "1m") %>%
  group_nest(site) %>%
  mutate(model = map(data, ~glm(nspp_exotic ~ nspp_native, family="quasipoisson",
                                data=.x)))

res_tib<- data.frame("siteID" = NA, "coef" = NA)
for(i in 1:length(unique(all_scales$site))){
  mod <- pluck(one_meter$model,i) 
  coef<- coefficients(mod)[2]
  site <- pluck(one_meter$site,i)
  
  res_tib[i, 1] <- site
  res_tib[i, 2] <- coef
}

d <- left_join(ai_file, res_tib) %>%
  mutate(lat = st_coordinates(geometry)[,2]) %>%
  group_by(siteID) %>%
  summarise(coef = first(coef),
            aridity_index = mean(ai_et0, na.rm=TRUE),
            MAP = mean(PRISM_ppt_30yr_normal_4kmM2_annual_bil, na.rm=TRUE),
            MAT = mean(PRISM_tmean_30yr_normal_4kmM2_annual_bil, na.rm=TRUE),
            MA_VPD_max = mean(PRISM_vpdmax_30yr_normal_4kmM2_annual_bil, na.rm=TRUE),
            MA_VPD_min = mean(PRISM_vpdmin_30yr_normal_4kmM2_annual_bil, na.rm=TRUE),
            elevation = mean(elevation, na.rm=TRUE),
            latitude = mean(lat),
            slope = mean(slope)) %>%
  ungroup()%>%
  pivot_longer(names_to = "variable", values_to = "value", cols = names(.)[3:ncol(.)])

d1 <- left_join(ai_file, res_tib) %>%
  group_by(siteID) %>%
  summarise(coef = first(coef),
            nlcd = first(nlcdClass),
            soil = first(soilOrder)) %>%
  ungroup()%>%
  pivot_longer(names_to = "variable", values_to = "value", cols = names(.)[3:ncol(.)])

ggplot(d, aes(x=value, y=coef)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey")+
  geom_point(alpha=0.5) +
  # geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free") +
  ylab("Coefficient of glm(nspp_exotic ~ nspp_native) for 1m subplots") +
  ggsave("draft_figures/coefficient_explore.png")

ggplot(d1, aes(y=value, x=coef)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")

ggplot(d, aes(x=PRISM_ppt_30yr_normal_4kmM2_annual_bil, y=coef)) +
  geom_point()
ggplot(d, aes(x=PRISM_ppt_30yr_normal_4kmM2_annual_bil, y=coef)) +
  geom_point()