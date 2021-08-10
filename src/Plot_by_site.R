# Plot by site for all scales and then focused at the 1m scale
all_scales <- readRDS("data/all_scales.RDS")
# Plot all sites individually
for (ss in 1:length(unique(all_scales$site))){
  print(ss)
  print(ggplot(all_scales[all_scales$site==unique(all_scales$site)[ss],], 
               aes(x = nspp_native, y=nspp_exotic, color = scale))+
          # geom_point(alpha=0.5)+
          geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
          theme_classic()+
          theme(legend.position = "none")+
          scale_color_viridis_d(option = "B") +
          geom_hline(yintercept = 1, lty=2, color = "grey80")+
          scale_y_continuous(breaks = c(0,1,2,4,6,8))+
          xlab("Native Species Richness")+
          ylab("Exotic Species Richness")+
          ggtitle(unique(all_scales$site)[ss]) +
          theme(plot.title = element_text(hjust = 0.5))
  )
}




# plot 1m data by site
ggplot(all_scales[all_scales$scale=="1m",],
       aes(x = nspp_native, y=nspp_exotic, color = site)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))+
  scale_color_viridis_d(option = "B") +
  xlab("Native Species Richness") +
  ylab("Exotic Species Richness") 


# plot with individual subplots 
#all together
plot(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_native[all_scales$scale=="1m"])
summary(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_native[all_scales$scale=="1m"]))
abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_native[all_scales$scale=="1m"]),col=2,lwd=3)

for (ss in 1:length(unique(all_scales$site))){
  abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_native[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
}

par(mfrow=c(3,3))
for (ss in 1:length(unique(all_scales$site))){
  plot(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_native[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]],main=unique(all_scales$site)[ss])
  abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_native[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
  
}

