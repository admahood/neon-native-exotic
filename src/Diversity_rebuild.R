# Rebuilding diversity data
raw_1m_dat=neon_div_object$div_1m2Data
plant_dat_1m=raw_1m_dat[which(raw_1m_dat$divDataType=="plantSpecies"),]
plant_dat_1m$subplot_uni_ID=paste(plant_dat_1m$plotID,plant_dat_1m$subplotID,sep="_")
plant_dat_1m$year=format(plant_dat_1m$endDate, format="%Y")

all_plots=unique(plant_dat_1m$subplot_uni_ID)
all_sub_plot_div_data=data.frame(site=NA,plotID=NA,subplotID=NA,subplot_uni_ID=all_plots,nspp_native=NA,nspp_exotic=NA,nspp_unk=NA,nspp_total=NA)
for(pp in 1:length(all_plots)){
  print(pp)
  curr_sub=plant_dat_1m[which(plant_dat_1m$subplot_uni_ID==all_plots[pp]&plant_dat_1m$year==2018),]
  # print(curr_sub)
  all_sub_plot_div_data$site[pp]=curr_sub$siteID[1]
  all_sub_plot_div_data$plotID[pp]=curr_sub$plotID[1]
  all_sub_plot_div_data$subplotID[pp]=curr_sub$subplotID[1]
  all_sub_plot_div_data$nspp_native[pp]=sum(curr_sub$nativeStatusCode=="N")
  all_sub_plot_div_data$nspp_exotic[pp]=sum(curr_sub$nativeStatusCode=="I")
  all_sub_plot_div_data$nspp_total[pp]=length(unique(curr_sub$taxonID))
  all_sub_plot_div_data$nspp_unk[pp]=sum(curr_sub$nativeStatusCode=="UNK")
  
    
}

all_sub_plot_div_data[which(is.na(all_sub_plot_div_data[,1])),]
all_sub_plot_div_data=all_sub_plot_div_data[!is.na(all_sub_plot_div_data[,1]),]#Remove plots with no sampling in 2018

plot(all_sub_plot_div_data$nspp_exotic~all_sub_plot_div_data$nspp_native)
par(mfrow=c(3,3))
for (ss in 1:length(unique(all_sub_plot_div_data$site))){
  plot(all_sub_plot_div_data$nspp_exotic[all_sub_plot_div_data$site==unique(all_sub_plot_div_data$site)[ss]]~all_sub_plot_div_data$nspp_native[all_sub_plot_div_data$site==unique(all_sub_plot_div_data$site)[ss]],main=unique(all_sub_plot_div_data$site)[ss])
  
  abline(lm(all_sub_plot_div_data$nspp_exotic[all_sub_plot_div_data$site==unique(all_sub_plot_div_data$site)[ss]]~all_sub_plot_div_data$nspp_native[all_sub_plot_div_data$site==unique(all_sub_plot_div_data$site)[ss]]))
}  

aggregate(all_sub_plot_div_data$nspp_exotic,by = list(all_sub_plot_div_data$site),FUN=sum)
plant_dat_1m$nativeStatusCode
