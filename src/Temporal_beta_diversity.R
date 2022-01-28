# Temporal beta diversity analysis
load("~/Dropbox/Documents/Data/R Info/cb_palette_vals.rdata")

# Get data for each year of each subplot

# all_years=sort(unique(plant_dat_1m$year))
# all_sub_plot_species_data_yearly=list()
# all_sub_plot_div_data_yearly=data.frame(site=NA,plotID=NA,subplotID=NA,subplot_uni_ID=rep(all_plots,each=length(all_years)),year=rep(all_years,times=length(all_plots)),nspp_native=NA,nspp_exotic=NA,nspp_unk=NA,nspp_total=NA)
# 
# for(pp in 1:length(all_plots)){
# 
#   print(pp)
#   for(yy in 1:length(all_years)){
#   curr_sub=plant_dat_1m[which(plant_dat_1m$subplot_uni_ID==all_plots[pp]&plant_dat_1m$year==all_years[yy]),]
#   # print(curr_sub)
#   all_sub_plot_div_data_yearly$site[(pp-1)*length(all_years)+yy]=curr_sub$siteID[1]
#   all_sub_plot_div_data_yearly$plotID[(pp-1)*length(all_years)+yy]=curr_sub$plotID[1]
#   all_sub_plot_div_data_yearly$year[(pp-1)*length(all_years)+yy]=all_years[yy]
#   all_sub_plot_div_data_yearly$subplotID[(pp-1)*length(all_years)+yy]=curr_sub$subplotID[1]
#   all_sub_plot_div_data_yearly$nspp_native[(pp-1)*length(all_years)+yy]=sum(curr_sub$nativeStatusCode=="N")
#   all_sub_plot_div_data_yearly$nspp_exotic[(pp-1)*length(all_years)+yy]=sum(curr_sub$nativeStatusCode=="I")
#   all_sub_plot_div_data_yearly$nspp_total[(pp-1)*length(all_years)+yy]=length(unique(curr_sub$taxonID))
#   all_sub_plot_div_data_yearly$nspp_unk[(pp-1)*length(all_years)+yy]=sum(curr_sub$nativeStatusCode=="UNK")
#   all_sub_plot_div_data_yearly$tot_cover[(pp-1)*length(all_years)+yy]=sum(curr_sub$percentCover)
#   all_sub_plot_div_data_yearly$tot_cover[(pp-1)*length(all_years)+yy]=sum(curr_sub$percentCover)
# 
#   all_sub_plot_species_data_yearly[[(pp-1)*length(all_years)+yy]]=plant_dat_1m$taxonID[which(plant_dat_1m$subplot_uni_ID==all_plots[pp]&plant_dat_1m$year==all_years[yy])]
#     
#   }
# }


all_sub_plot_div_data_yearly_sampled_vals=which(!is.na(all_sub_plot_div_data_yearly$site))
all_sub_plot_div_data_yearly_sampled=all_sub_plot_div_data_yearly[which(!is.na(all_sub_plot_div_data_yearly$site)),]


all_sub_plot_species_data_yearly_sampled=list()
for(ii in 1:length(all_sub_plot_div_data_yearly_sampled_vals)){
  all_sub_plot_species_data_yearly_sampled[[ii]]=all_sub_plot_species_data_yearly[[all_sub_plot_div_data_yearly_sampled_vals[ii]]]
  print(ii)
}


all_sub_plot_species_data_yearly_sampled


# Run through each uique plot and calculate beta diversity for each time point
all_plots_mean_gamma=rep(NA,length(all_plots))
all_plots_mean_alpha=rep(NA,length(all_plots))
all_plots_mean_beta=rep(NA,length(all_plots))

for (pp in 1:length(all_plots)){
curr_lines=which(all_sub_plot_div_data_yearly_sampled$subplot_uni_ID==all_plots[pp])
curr_plot=all_sub_plot_div_data_yearly_sampled[curr_lines,]
use_lines=curr_lines[which(!is.na(curr_plot$nspp_native))]
curr_gamma=NA
curr_alpha=NA
curr_beta=NA
if(length(use_lines)>1){
for(yy in 1:(length(use_lines)-1)){
  
  s1=unique(all_sub_plot_species_data_yearly_sampled[[use_lines[yy]]])
  s2=unique(all_sub_plot_species_data_yearly_sampled[[use_lines[yy+1]]])
  shared=length(which(s1%in%s2))
  total_sp=length(unique(c(s1,s2)))
  curr_gamma[yy]=total_sp
  curr_alpha[yy]=mean(c(length(s1),length(s2)))
  curr_beta[yy]=((length(s1)-shared)+(length(s2)-shared))/total_sp
}
all_plots_mean_gamma[pp]=mean(curr_gamma)
all_plots_mean_alpha[pp]=mean(curr_alpha)
all_plots_mean_beta[pp]=mean(curr_beta)
}
print(pp)
}



beta_df=data.frame(site=gsub(x = all_plots,pattern = "(....)_.+",replacement = "\\1"),subplot_uni_ID=all_plots,subplot_mean_gamma=all_plots_mean_gamma,subplot_mean_alpha=all_plots_mean_alpha,subplot_mean_beta=all_plots_mean_beta)


site_mean_div=aggregate(beta_df$subplot_mean_beta,by=list(beta_df$site),FUN=mean,na.rm=TRUE)
names(site_mean_div)=c("site","mean_beta")
site_mean_div$mean_nat=site_mean_nat$native_mean[match(site_mean_nat$site,site_mean_div$site)]

plot(beta_df$subplot_mean_beta~beta_df$subplot_mean_gamma)
abline(lm(beta_df$subplot_mean_beta~beta_df$subplot_mean_gamma),col=2)
summary(lm(beta_df$subplot_mean_beta~beta_df$subplot_mean_gamma))


plot(beta_df$subplot_mean_beta~beta_df$subplot_mean_alpha)
abline(lm(beta_df$subplot_mean_beta~beta_df$subplot_mean_alpha),col=2)
summary(lm(beta_df$subplot_mean_beta~beta_df$subplot_mean_alpha))



plot(site_mean_div$mean_beta~site_mean_div$mean_nat)
abline(lm(site_mean_div$mean_beta~site_mean_div$mean_nat),col=2)
summary(lm(site_mean_div$mean_beta~site_mean_div$mean_nat))



par(mfrow=c(1,2),mar=c(5,5,2,2))
# Subplot scale turnover
pred_var=beta_df$subplot_mean_alpha
resp_var=beta_df$subplot_mean_beta
mod=lm(resp_var~pred_var)  

plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),main = "",plot_type = "point",ylab="Temporal turnover", xlab="Subplot species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35)

# Site level means
pred_var=site_mean_div$mean_nat
resp_var=site_mean_div$mean_beta
mod=lm(resp_var~pred_var)  

plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),main = "",plot_type = "point",ylab="Mean subplot temporal turnover", xlab="Site species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35)




par(mfrow=c(4,4),mar=c(5,5,2,2))


# xx=plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),plot_type = "only_conf",ylab="Temporal turnover", xlab="Subplot species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35,pt_col_set = ss)


plot(beta_df$subplot_mean_beta~beta_df$subplot_mean_alpha,ylab="Temporal turnover", xlab="Subplot species richness")
site_mean_div$turnover_coef=NA


# separate temporal turnover by site
for(ss in 1:nrow(sites)){
  par(new=T)
  pred_var=beta_df$subplot_mean_alpha[which(beta_df$site==sites$site[ss])]
  resp_var=beta_df$subplot_mean_beta[which(beta_df$site==sites$site[ss])]
  mod=lm(resp_var~pred_var)  
  
  xx=plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),plot_type = "only_conf",ylab="", xlab="",conf_col=alpha(cb_palette[ceiling(ss/5)],alpha = .75),cex.lab=1.4,cex.axis=1.35,pt_col_set = ss,yaxt="n",xaxt="n")
  
  # xx=plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),plot_type = "point",ylab="Temporal turnover", xlab="Subplot species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35,pt_col_set = ss)
  site_mean_div$turnover_coef[which(site_mean_div$site==sites$site[ss])]=xx$coefficients[2,1]
  
  if(xx$coefficients[2,4]<0.01){
    mtext(side = 3,text = paste(sites$site[ss], "**"),cex=1.5)
  } else if(xx$coefficients[2,4]<=0.05){
    mtext(side = 3,text = paste(sites$site[ss], "*"),cex=1.5)
  }else {
    mtext(side = 3,text = paste(sites$site[ss]),cex=1.5)
}
}




# Compare site coefficients with diversity-invasibility

plot(NERR_coefs$nat_coef~site_mean_div$turnover_coef[match(NERR_coefs$site,site_mean_div$site)],type="p")

text(NERR_coefs$nat_coef~site_mean_div$turnover_coef[match(NERR_coefs$site,site_mean_div$site)],labels=NERR_coefs$site)

abline(lm(NERR_coefs$nat_coef~site_mean_div$turnover_coef[match(NERR_coefs$site,site_mean_div$site)]),col=2)

summary(lm(NERR_coefs$nat_coef~site_mean_div$turnover_coef[match(NERR_coefs$site,site_mean_div$site)]),col=2)


par(mar=c(5,5,4,2))

resp_var=NERR_coefs$nat_coef
pred_var=site_mean_div$turnover_coef[match(NERR_coefs$site,site_mean_div$site)]
plot_with_conf(pred_var,resp_var,min_range = -.04,max_range = .08,main = "",plot_type = "point",ylab="Invasibility coef ", xlab="Stability coef",conf_col=2,cex.lab=1.4,cex.axis=1.35,cex=site_mean_div$mean_nat[match(NERR_coefs$site,site_mean_div$site)]/4,pch=1)
abline(h=0)
abline(v=0)
