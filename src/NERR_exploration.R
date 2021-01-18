# Script for running a number of NERR related analyses

# Things to account for:
# Are there uninvaded plots at the site
# Are there any invaders across the entire site (invader supply, though for terrestrial may be less of an issue)
# Evaluate number of Unknowns
site_dat=read.csv(file = "data/NEON_Field_Site_Metadata.csv")
site_dat$main_nlcd=gsub(x = paste(site_dat$field_dominant_nlcd_classes),pattern = "\\|.+",replacement = "")
for (ss in 1:length(unique(prev_year_div$site))){
  print(ss)
print(ggplot(prev_year_div[prev_year_div$site==unique(prev_year_div$site)[ss]&prev_year_div$scale!="plot",], 
       aes(x = nspp_native, y=next_nspp_exotic, color = scale))+
  # geom_point(alpha=0.5)+
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_viridis_d(option = "B") +
  geom_hline(yintercept = 1, lty=2, color = "grey80")+
  scale_y_continuous(breaks = c(0,1,2,4,6,8))+
  xlab("Native Species Richness (Uninvaded Sites, Year 1)")+
  ylab("Exotic Species Richness; Year 2"))
}


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





ggplot(all_scales[all_scales$scale=="1m",],
       aes(x = nspp_native, y=nspp_exotic, color = site)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))+
  scale_color_viridis_d(option = "B") +
  xlab("Native Species Richness") +
  ylab("Exotic Species Richness") 



#1m by site
par(mfrow=c(5,5))
for (ss in 1:length(unique(all_scales$site))){
  print(ss)
  print(ggplot(all_scales[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss],],
       aes(x = nspp_native, y=nspp_exotic, color = site)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))+
  scale_color_viridis_d(option = "B") +
  xlab("Native Species Richness") +
  ylab("Exotic Species Richness") 

)
}

ss=12
print(all_scales[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018,c(3,4,5,6,7,10)],n=Inf)
plot(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018])



plot(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"])
summary(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"]))
abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"]),col=2,lwd=3)

for (ss in 1:length(unique(all_scales$site))){
  abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
}

par(mfrow=c(3,3))
for (ss in 1:length(unique(all_scales$site))){
  plot(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]],main=unique(all_scales$site)[ss])
  abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
  
}


# Calculate local scale NERR for each site to compare between habitat types etc.

site_exotics=aggregate(all_sub_plot_div_data$nspp_exotic,by = list(all_sub_plot_div_data$site),FUN=sum)
site_mean_nat=aggregate(all_sub_plot_div_data$nspp_native,by = list(all_sub_plot_div_data$site),FUN=mean)

aggregate(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$year==2018],by=list(all_scales$site[all_scales$scale=="1m"&all_scales$year==2018]),FUN=sum)

head(all_sub_plot_div_data)
head(all_scales[all_scales$scale=="1m"&all_scales$year==2018&all_scales$plotID=="BART_006",])


all_sub_plot_div_data[which(all_sub_plot_div_data$plotID=="BART_006"),]
all_scales[all_scales$scale=="1m"&all_scales$year==2018&all_scales$plotID=="BART_006",]


use_sites=site_exotics$Site[which(site_exotics$exotic_sum>0)]
inv_site_sub_plot_div_data=all_sub_plot_div_data[which(all_sub_plot_div_data$site%in%use_sites),]
NERR_coefs=data.frame(site=use_sites,tot_coef=NA,tot_tval=NA,tot_pval=NA,nat_coef=NA,nat_tval=NA,nat_pval=NA)
NERR_coefs$mean_nat=site_mean_nat$x[which(site_exotics$exotic_sum>0)]
NERR_coefs$main_nlcd=site_dat$main_nlcd[match(NERR_coefs$site,site_dat$field_site_id)]


par(mfrow=c(3,4))
for (ss in 1:length(unique(inv_site_sub_plot_div_data$site))){
  plot(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]],main=unique(inv_site_sub_plot_div_data$site)[ss],xlab="total richness",ylab="exotic richness")
  abline(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]),col=ss)
  
  tot_mod_res=summary(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]),col=ss)
  
  NERR_coefs[ss,2:4]=tot_mod_res$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
  
  plot(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]],main=unique(inv_site_sub_plot_div_data$site)[ss],xlab="native richness",ylab="exotic richness")
  abline(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]),col=ss)
  
  nat_mod_res=summary(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==unique(inv_site_sub_plot_div_data$site)[ss]]),col=ss)
  
  NERR_coefs[ss,5:7]=nat_mod_res$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
}

plot(NERR_coefs$nat_coef~NERR_coefs$mean_nat,cex=abs(NERR_coefs$nat_tval/2))
abline(lm(NERR_coefs$nat_coef~NERR_coefs$mean_nat))
summary(lm(NERR_coefs$nat_coef~NERR_coefs$mean_nat))

plot(NERR_coefs$nat_coef~NERR_coefs$,cex=abs(NERR_coefs$nat_tval/2))
plot(NERR_coefs$nat_coef~as.factor(NERR_coefs$main_nlcd))
NERR_coefs$main_nlcd=as.factor(NERR_coefs$main_nlcd)
summary(lm(NERR_coefs$nat_coef~NERR_coefs$main_nlcd))
