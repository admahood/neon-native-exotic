# Script for running a number of NERR related analyses
library(car)
library(scales)

# Things to account for:
# Are there uninvaded plots at the site
# Are there any invaders across the entire site (invader supply, though for terrestrial may be less of an issue)
# Evaluate number of Unknowns
site_dat=read.csv(file = "data/NEON_Field_Site_Metadata.csv")
# site_dat$main_nlcd=gsub(x = paste(site_dat$field_dominant_nlcd_classes),pattern = "\\|.+",replacement = "")
# 
# for (ss in 1:length(unique(prev_year_div$site))){
#   print(ss)
# print(ggplot(prev_year_div[prev_year_div$site==unique(prev_year_div$site)[ss]&prev_year_div$scale!="plot",], 
#        aes(x = nspp_native, y=next_nspp_exotic, color = scale))+
#   # geom_point(alpha=0.5)+
#   geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
#   theme_classic()+
#   theme(legend.position = "none")+
#   scale_color_viridis_d(option = "B") +
#   geom_hline(yintercept = 1, lty=2, color = "grey80")+
#   scale_y_continuous(breaks = c(0,1,2,4,6,8))+
#   xlab("Native Species Richness (Uninvaded Sites, Year 1)")+
#   ylab("Exotic Species Richness; Year 2"))
# }
# 
# 
# for (ss in 1:length(unique(all_scales$site))){
#   print(ss)
#   print(ggplot(all_scales[all_scales$site==unique(all_scales$site)[ss],], 
#                aes(x = nspp_native, y=nspp_exotic, color = scale))+
#           # geom_point(alpha=0.5)+
#           geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
#           theme_classic()+
#           theme(legend.position = "none")+
#           scale_color_viridis_d(option = "B") +
#           geom_hline(yintercept = 1, lty=2, color = "grey80")+
#           scale_y_continuous(breaks = c(0,1,2,4,6,8))+
#           xlab("Native Species Richness")+
#           ylab("Exotic Species Richness")+
#           ggtitle(unique(all_scales$site)[ss]) +
#           theme(plot.title = element_text(hjust = 0.5))
#   )
# }
# 
# 
# 
# 
# 
# ggplot(all_scales[all_scales$scale=="1m",],
#        aes(x = nspp_native, y=nspp_exotic, color = site)) +
#   geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
#   theme_classic() +
#   theme(legend.position = c(1,0),
#         legend.justification = c(1,0))+
#   scale_color_viridis_d(option = "B") +
#   xlab("Native Species Richness") +
#   ylab("Exotic Species Richness") 
# 
# 
# 
# #1m by site
# par(mfrow=c(5,5))
# for (ss in 1:length(unique(all_scales$site))){
#   print(ss)
#   print(ggplot(all_scales[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss],],
#        aes(x = nspp_native, y=nspp_exotic, color = site)) +
#   geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
#   theme_classic() +
#   theme(legend.position = c(1,0),
#         legend.justification = c(1,0))+
#   scale_color_viridis_d(option = "B") +
#   xlab("Native Species Richness") +
#   ylab("Exotic Species Richness") 
# 
# )
# }
# 
# ss=12
# print(all_scales[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018,c(3,4,5,6,7,10)],n=Inf)
# plot(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]&all_scales$year==2018])
# 
# 
# 
# plot(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"])
# summary(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"]))
# abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"]~all_scales$nspp_total[all_scales$scale=="1m"]),col=2,lwd=3)
# 
# for (ss in 1:length(unique(all_scales$site))){
#   abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
# }
# 
# par(mfrow=c(3,3))
# for (ss in 1:length(unique(all_scales$site))){
#   plot(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]],main=unique(all_scales$site)[ss])
#   abline(lm(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]~all_scales$nspp_total[all_scales$scale=="1m"&all_scales$site==unique(all_scales$site)[ss]]),col=ss)
#   
# }
# 

# Calculate local scale NERR for each site to compare between habitat types etc.

site_exotics=aggregate(all_sub_plot_div_data$nspp_exotic,by = list(all_sub_plot_div_data$site),FUN=sum)
names(site_exotics)=c("site","exotic_sum")
# site_mean_nat=aggregate(all_sub_plot_div_data$nspp_native,by = list(all_sub_plot_div_data$site),FUN=mean)
# names(site_mean_nat)=c("site","native_mean")

site_mean_nat=aggregate(sp_level_1$nspp_native,by = list(sp_level_1$site),FUN=mean)
names(site_mean_nat)=c("site","native_mean")



site_unk=aggregate(all_sub_plot_div_data$nspp_unk,by = list(all_sub_plot_div_data$site),FUN=sum)
names(site_unk)=c("site","unk_sum")

site_cover=aggregate(all_sub_plot_div_data$tot_cover,by = list(all_sub_plot_div_data$site),FUN=mean,na.rm=TRUE)
names(site_cover)=c("site","cover_mean")



# aggregate(all_scales$nspp_exotic[all_scales$scale=="1m"&all_scales$year==2018],by=list(all_scales$site[all_scales$scale=="1m"&all_scales$year==2018]),FUN=sum)
# 
# head(all_sub_plot_div_data)
# head(all_scales[all_scales$scale=="1m"&all_scales$year==2018&all_scales$plotID=="BART_006",])
# 
# 
# all_sub_plot_div_data[which(all_sub_plot_div_data$plotID=="BART_006"),]
# all_scales[all_scales$scale=="1m"&all_scales$year==2018&all_scales$plotID=="BART_006",]
# 

use_sites=site_exotics$site[which(site_exotics$exotic_sum>0)]
inv_site_sub_plot_div_data=all_sub_plot_div_data[which(all_sub_plot_div_data$site%in%use_sites),]
NERR_coefs=data.frame(site=use_sites,tot_coef=NA,tot_tval=NA,tot_pval=NA,nat_coef=NA,nat_tval=NA,nat_pval=NA)
NERR_coefs$mean_nat=site_mean_nat$native_mean[which(site_exotics$exotic_sum>0)]
NERR_coefs$mean_cover=site_cover$cover_mean[which(site_exotics$exotic_sum>0)]

# NERR_coefs$main_nlcd=site_dat$main_nlcd[match(NERR_coefs$site,site_dat$field_site_id)]
NERR_coefs$latitude=site_dat$field_latitude[match(NERR_coefs$site,site_dat$field_site_id)]
NERR_coefs$precip=site_dat$field_mean_annual_precipitation_mm[match(NERR_coefs$site,site_dat$field_site_id)]
NERR_coefs$tot_rich=site_dat$tot_rich[match(NERR_coefs$site,site_dat$field_site_id)]

NERR_coefs=cbind(NERR_coefs,site_nlcd[match(NERR_coefs$site,site_nlcd$site),-1])


par(mfrow=c(3,4))
for (ss in 1:length(use_sites)){
  # plot(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==use_sites[ss]],main=use_sites[ss],xlab="total richness",ylab="exotic richness")
  # tot_mod_res=summary(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==use_sites[ss]]),col=ss)
  # 
  # if(tot_mod_res$coefficients[2,"Pr(>|t|)"]<0.05){
  # abline(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_total[inv_site_sub_plot_div_data$site==use_sites[ss]]),col=ss)
  # }
  #   
  # NERR_coefs[ss,2:4]=tot_mod_res$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
  # 
  plot(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==use_sites[ss]],main=use_sites[ss],xlab="native richness",ylab="exotic richness")
  
  nat_mod_res=summary(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==use_sites[ss]]),col=ss)
  if(nat_mod_res$coefficients[2,"Pr(>|t|)"]<0.05){
    abline(lm(inv_site_sub_plot_div_data$nspp_exotic[inv_site_sub_plot_div_data$site==use_sites[ss]]~inv_site_sub_plot_div_data$nspp_native[inv_site_sub_plot_div_data$site==use_sites[ss]]),col=ss)
  }
  NERR_coefs[ss,5:7]=nat_mod_res$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
  if(nat_mod_res$coefficients[2,"Pr(>|t|)"]<0.05){}
}

plot(NERR_coefs$nat_coef~NERR_coefs$mean_nat,cex=abs(NERR_coefs$nat_tval/2))
abline(lm(NERR_coefs$nat_coef~NERR_coefs$mean_nat))
summary(lm(NERR_coefs$nat_coef~NERR_coefs$mean_nat))

plot(NERR_coefs$nat_coef~NERR_coefs$mean_nat,cex=abs(NERR_coefs$nat_tval/2))

plot(NERR_coefs$nat_coef~NERR_coefs$main_nlcd)
summary(lm(NERR_coefs$nat_coef~NERR_coefs$main_nlcd))
summary(aov(NERR_coefs$nat_coef~NERR_coefs$main_nlcd))


plot(NERR_coefs$nat_coef~NERR_coefs$prop_grasslandHerbaceous)
abline(lm(NERR_coefs$nat_coef~NERR_coefs$prop_grasslandHerbaceous),col=2)
summary(lm(NERR_coefs$nat_coef~NERR_coefs$prop_grasslandHerbaceous))

all_forest=(NERR_coefs$prop_mixedForest+NERR_coefs$prop_evergreenForest+NERR_coefs$prop_deciduousForest)
all_grass=(NERR_coefs$prop_grasslandHerbaceous+NERR_coefs$prop_pastureHay+NERR_coefs$prop_cultivatedCrops)

plot(NERR_coefs$nat_coef~all_grass)
abline(lm(NERR_coefs$nat_coef~all_grass,col=2))
summary(lm(NERR_coefs$nat_coef~all_grass))


plot(NERR_coefs$nat_coef~all_forest)
abline(lm(NERR_coefs$nat_coef~all_forest,col=2))
summary(lm(NERR_coefs$nat_coef~all_forest))

plot(NERR_coefs$nat_coef~NERR_coefs$tot_rich,type="p")
abline(lm(NERR_coefs$nat_coef~NERR_coefs$tot_rich,type="p"))
summary(lm(NERR_coefs$nat_coef~NERR_coefs$tot_rich,type="p"))
text(NERR_coefs$nat_coef~NERR_coefs$tot_rich,labels=paste(NERR_coefs$site))
dev.off()



# Run analysis with direct NLCD data

uber_dat=cbind(NERR_coefs,nlcd_site_vals[match( NERR_coefs$site,nlcd_site_vals$siteID),])
names(uber_dat)
plot(uber_dat$prop_Mixed_Forest~uber_dat$prop_mixedForest)               

plot(uber_dat$nat_coef~uber_dat$prop_Grasslands_Herbaceous)
abline(lm(uber_dat$nat_coef~uber_dat$prop_Grasslands_Herbaceous),col=2)
summary(lm(uber_dat$nat_coef~uber_dat$prop_Grasslands_Herbaceous))

all_forest=(uber_dat$prop_Mixed_Forest+uber_dat$prop_Evergreen_Forest+uber_dat$prop_Deciduous_Forest)
all_grass=(uber_dat$prop_grasslandHerbaceous+uber_dat$prop_Pasture_Hay+uber_dat$prop_Cultivated_crops)


plot(uber_dat$nat_coef~uber_dat$mean_cover)
abline(lm(uber_dat$nat_coef~uber_dat$mean_cover),col=2)
summary(lm(uber_dat$nat_coef~uber_dat$mean_cover))
mod=lm(uber_dat$nat_coef~uber_dat$mean_cover+uber_dat$tot_rich)
summary(mod)
crPlots(mod)

plot(uber_dat$nat_coef~uber_dat$tot_rich)
abline(lm(uber_dat$nat_coef~uber_dat$tot_rich),col=2)
summary(lm(uber_dat$nat_coef~uber_dat$tot_rich))

pred=(uber_dat$prop_evergreenForest+uber_dat$prop_deciduousForest+uber_dat$prop_Mixed_Forest)
plot(uber_dat$nat_coef~pred)
abline(lm(uber_dat$nat_coef~pred),col=2)
summary(lm(uber_dat$nat_coef~pred))


summary(lm(uber_dat$nat_coef~uber_dat$mean_cover+uber_dat$mean_nat))

uber_dat$siteName[order(uber_dat$nat_coef,decreasing = F)]
sort(uber_dat$nat_coef,decreasing = F)


plot(uber_dat$prop_grasslandHerbaceous~uber_dat$poaceae_cover)
abline(lm(uber_dat$prop_grasslandHerbaceous~uber_dat$poaceae_cover),col=2)
summary(lm(uber_dat$prop_grasslandHerbaceous~uber_dat$poaceae_cover))
