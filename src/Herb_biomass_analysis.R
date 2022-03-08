#Script to read and organize NEON herbaceous clip harvest data


zipsByProduct(dpID="DP1.10023.001", 
              startdate="2017-01", 
              enddate="2020-12",
              package="basic", check.size=T,
              savepath = "data/NEON_data_products/")


stackByTable(filepath = "data/NEON_data_products/filesToStack10023/")

herb_dat=readTableNEON("data/NEON_data_products/NEON_Herb_clip/stackedFiles/hbp_massdata.csv",varFile = "data/NEON_data_products/NEON_Herb_clip/stackedFiles/variables_10023.csv")

# herb_dat$siteID=as.factor(herb_dat$siteID)
# herb_dat$plotID=as.factor(herb_dat$plotID)

# herb_dat=herb_dat_all[herb_dat_all$plotType=="distributed",]
herb_dat$year=format(herb_dat$collectDate,"%Y")
herb_dat$samp_day=format(herb_dat$collectDate,"%Y-%m-%d")
herb_dat$samp_month=format(herb_dat$collectDate,"%Y-%m")
# herb_dat$tot_biomass=
# herb_dat=herb_dat[herb_dat$year==2020,]

  
herb_plots=unique(herb_dat$plotID)  


month_tot_biomass=aggregate(herb_dat$dryMass,by=list(herb_dat$plotID,herb_dat$year,herb_dat$samp_month),FUN=sum,na.rm=TRUE)

names(month_tot_biomass)=c("plotID","year","month","tot_biomass")

year_tot_biomass=aggregate(herb_dat$dryMass,by=list(herb_dat$plotID,herb_dat$year,herb_dat$samp_month),FUN=sum,na.rm=TRUE)

names(month_tot_biomass)=c("plotID","year","month","tot_biomass")


yr_max_biomass=aggregate(month_tot_biomass$tot_biomass,by=list(month_tot_biomass$plotID,month_tot_biomass$year),FUN=max)
names(yr_max_biomass)=c("plotID","year","max_biomass")

plot_mean_biomass=aggregate(yr_max_biomass$max_biomass,by=list(yr_max_biomass$plotID),FUN=mean)
names(plot_mean_biomass)=c("plotID","mean_biomass")

herb_tot_dat=herb_dat[match(herb_plots,herb_dat$plotID),]
herb_tot_dat$mean_biomass=plot_mean_biomass$mean_biomass[match(herb_tot_dat$plotID,plot_mean_biomass$plotID)]
  
head(herb_dat,10)
str(herb_dat)
names(herb_dat)



lm(herb_tot_dat$mean_biomass~plot_level_div$plot_mean_alpha[match(herb_tot_dat$plotID,plot_level_div$plot_ID)])

pred_var=plot_level_div$plot_mean_alpha[match(herb_tot_dat$plotID,plot_level_div$plot_ID)]
resp_var=herb_tot_dat$mean_biomass
# resp_var=herb_tot_dat$CNratio
# mod=lmer(mean_biomass~plot_rich+(1|plotID),data =  herb_tot_dat  )
mod=lm(resp_var~pred_var)

plot_with_conf(pred_var,resp_var,mod = mod,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),main = "",plot_type = "point",ylab="Total biomass", xlab="Distributed plot species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35)


