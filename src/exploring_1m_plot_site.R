library(neondiveRsity)
library(tidyverse)
library(sf)

source("src/diversity_data_prep.R")
neon_plots <- st_read("data/big/All_NEON_TOS_Plots_V8/All_NEON_TOS_Plots_V8/All_NEON_TOS_Plot_Centroids_V8.shp") %>%
  group_by(site = siteID) %>%
  summarise(domain = first(domain)) %>%
  ungroup()

# site, plot, 1m level count comparisons =========================
# note to self, make a collapse_years option
ggplot(site_level %>% left_join(neon_plots), 
       aes(x = nspp_native, y=nspp_exotic))+
  geom_point() +
  geom_smooth(method = "glm", method.args= list(family = "poisson"))

ggplot(plot_level%>% left_join(neon_plots), 
       aes(x = nspp_native, y=nspp_exotic))+
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "glm", method.args= list(family = "quasipoisson"))

ggplot(sp_level_1 %>% left_join(neon_plots), 
       aes(y= nspp_native, x=nspp_exotic))+
  geom_jitter(alpha=0.05) +
  geom_smooth(method = "glm", method.args= list(family = "quasipoisson"))


ggplot(site_level %>% left_join(neon_plots), 
       aes(x =shannon_native, y=shannon_exotic))+
  geom_point() +
  geom_smooth(method = "glm", method.args= list(family = "poisson"))

ggplot(plot_level%>% left_join(neon_plots), 
       aes(x = shannon_native, y=shannon_exotic))+
  geom_jitter(alpha = 0.25) +
  geom_smooth(method = "lm")

ggplot(sp_level_1 %>% left_join(neon_plots), 
       aes(y= shannon_native, x=shannon_exotic))+
  geom_jitter(alpha=0.05) +
  geom_smooth(method = "glm", method.args= list(family = "quasipoisson"))

# data mongering for binomial models===============================================================


uninvaded_sites <- bind_rows(sp_level_1, plot_level, site_level)%>% 
    filter(nspp_exotic == 0) %>% 
  mutate(year0 = as.numeric(year),
         uniqueid = paste0(year0+1,plotID,scale,subplotID, site)) %>%
  dplyr::select(year0, plotID, scale, subplotID, site,uniqueid, 
                nspp_total0 = nspp_total, nspp_native0 = nspp_native,
                nspp_notexotic0 = nspp_notexotic,
                turnover0 = turnover, nestedness0 = nestedness,
                nfamilies0 = nfamilies, 
                shannon_total0 = shannon_total, shannon_native0 = shannon_native,
                cover_native0 = cover_native,rel_cover_native0 = rel_cover_native)

uniqueids <- uninvaded_sites$uniqueid

next_year<-bind_rows(sp_level_1, plot_level, site_level) %>% 
  mutate(year = as.numeric(year),
         uniqueid = paste0(year,plotID,scale,subplotID, site))%>%
  filter(uniqueid %in% uniqueids) %>%
  dplyr::select(uniqueid, 
                next_nspp_total1 = nspp_total, 
                turnover1 = turnover,
                nestedness1 = nestedness,
                nfamilies1= nfamilies,
                rel_cover_exotic1 = rel_cover_exotic,
                nspp_exotic1 = nspp_exotic,
                nspp_native1 = nspp_native) %>%
  mutate(invaded1 = ifelse(nspp_exotic1 > 0, 1, 0))

prev_year_div <- left_join(next_year, uninvaded_sites) %>%
  group_by(site, scale) %>%
  mutate(n = n()) %>%
  ungroup()

ggplot(prev_year_div, aes(x = nspp_native0, invaded1, color = scale)) +
  geom_smooth(method="glm", method.args = list(family = "binomial")) 

ggplot(prev_year_div, aes(x = rel_cover_exotic1, nspp_native1)) +
  geom_point() +
  facet_wrap(~scale, scales = "free")

ggplot(prev_year_div %>% filter(scale == "plot"), aes(x = rel_cover_exotic1, nfamilies1)) +
  geom_point()

# exploratory plots===============
# this is especially strange
ggplot(prev_year_div %>% 
         filter(scale != "site")%>%
         left_join(neon_plots), aes(x = nspp_native0, y= invaded1, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~domain, scales = "free") +
  theme_classic()

ggplot(prev_year_div %>% 
         filter(scale != "site")%>%
         left_join(neon_plots), 
       aes(x = shannon_native0, y= invaded1, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  #facet_wrap(~domain, scales = "free") +
  theme_classic()

ggplot(prev_year_div %>% 
         filter(scale != "site")%>%
         left_join(neon_plots), 
       aes(x = cover_native0, y= invaded1, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  # facet_wrap(~domain, scales = "free") +
  theme_classic()

# good plot
bind_rows(sp_level_1, plot_level, site_level) %>% 
  # filter(scale != "site")%>%
  left_join(neon_plots)%>%
  ggplot(aes(x = nspp_notexotic, y= nspp_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  # facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  xlab("nspp_native") +
  ylab("nspp_exotic")

bind_rows(sp_level_1, plot_level, site_level) %>% 
  # filter(scale != "site")%>%
  left_join(neon_plots)%>%
  ggplot(aes(x = shannon_notexotic, y= nspp_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  # facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  xlab("Shannon Diversity Native") +
  ylab("nspp_exotic")

# good plot
bind_rows(sp_level_1, plot_level, site_level) %>% 
  # filter(scale != "site")%>%
  left_join(neon_plots)%>%
  ggplot(aes(x = rel_cover_notexotic, y= nspp_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  # facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  xlab("relative_cover_native") +
  ylab("nspp_exotic")

bind_rows(sp_level_1, plot_level, site_level) %>% 
  # filter(scale != "site")%>%
  left_join(neon_plots)%>%
  ggplot(aes(x = nspp_notexotic, y= shannon_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  # facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) 

# good plot
bind_rows(sp_level_1, plot_level, site_level) %>% 
  # filter(scale != "site") %>%
  left_join(neon_plots)%>%
  ggplot(aes(x = nspp_notexotic, y= rel_cover_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  # geom_point() +
  # facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) 

bind_rows(sp_level_1, plot_level, site_level) %>% 
  filter(scale != "site") %>%
  left_join(neon_plots)%>%
  ggplot(aes(x = shannon_notexotic, y= shannon_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  # geom_point() +
  facet_wrap(~domain, scales = "free") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) 

ggplot(prev_year_div %>% filter(scale != "site")%>%
         left_join(neon_plots), 
       aes(x =shannon_total0, y= invaded1, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
  facet_wrap(~domain, scales = "free") +
  theme_classic()

# binomial models ==============================================================

# many minutes per mod
ma0<- bind_rows(sp_level_1, plot_level, site_level) %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  lme4::glmer(invaded ~ nspp_native +(nspp_native|scale)+ (1|site), 
              data = ., family = "binomial")

# this is the ggplot model, basically... 
ma1 <- bind_rows(sp_level_1, plot_level, site_level) %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  glm(invaded ~ nspp_native * scale, 
      data = ., family = "binomial")
#not sure what to think about this stuff
vif(ma0)
vif(ma1)
AIC(ma0,ma1)
anova(ma0,ma1)

mc1<- prev_year_div %>%
  filter(scale == "1m") %>%
  lme4::glmer(invaded ~ nspp_native + (nspp_native|site), 
              data = ., family = "binomial")
mc1_i<- prev_year_div %>%
  filter(scale == "1m") %>%
  lme4::glmer(invaded ~ nspp_native + (1|site), 
              data = ., family = "binomial")
AIC(mc1, mc1_i)
anova(mc1, mc1_i)

summary(mc1)
Anova(mc1)

prev_year_div %>%
  filter(scale == "1m") %>%
  ggplot(aes(x=nspp_native, y=invaded, color=site)) +
  geom_line(aes(y=(predict(mc1, type="response")))) +
  theme_classic()

mc10<- prev_year_div %>%
  filter(scale == "10m") %>%
  lme4::glmer(invaded ~ nspp_native + (nspp_native|site), 
              data = ., family = "binomial")
summary(mc10)
Anova(mc10)

mc100<- prev_year_div %>%
  filter(scale == "100m") %>%
  lme4::glmer(invaded ~ nspp_native + (nspp_native|site), 
              data = ., family = "binomial")
summary(mc100)

mcplot<- prev_year_div %>%
  filter(scale == "plot") %>%
  lme4::glmer(invaded ~ nspp_native + (nspp_native|site), 
              data = ., family = "binomial")
summary(mcplot)


mc0<-prev_year_div%>%
  filter(scale == "plot") %>%
  # mutate(nspp_native = scale(nspp_native)) %>%
  glm(invaded ~ nspp_native*site, 
      data = ., family = "binomial")

