# Rebuilding diversity data
raw_1m_dat=neon_div_object$div_1m2Data
plant_dat_1m=raw_1m_dat[which(raw_1m_dat$divDataType=="plantSpecies"),]
plant_dat_1m$subplot_uni_ID=paste(plant_dat_1m$plotID,plant_dat_1m$subplotID,sep="_")
plant_dat_1m$year=format(plant_dat_1m$endDate, format="%Y")
plant_dat_1m$nlcdClass=as.factor(plant_dat_1m$nlcdClass)

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

# get total species richness for each site
for(ss in 1:length(all_sites)){
  site_nlcd$tot_rich[ss]=length(unique(plant_dat_1m$scientificName[which(plant_dat_1m$siteID==all_sites[ss])]))
  }
  
dd=data.frame(tid=unique(plant_dat_1m$taxonID[which(plant_dat_1m$siteID==all_sites[ss])]),sn=unique(plant_dat_1m$scientificName[which(plant_dat_1m$siteID==all_sites[ss])]))
dd[order(dd[,2]),]

aggregate(all_sub_plot_div_data$nspp_exotic,by = list(all_sub_plot_div_data$site),FUN=sum)
plant_dat_1m$nativeStatusCode

all_classes=names(summary(plant_dat_1m$nlcdClass))

site_nlcd=data.frame(cultivatedCrops=vector(), deciduousForest=vector(), dwarfScrub=vector(), emergentHerbaceousWetlands=vector(), evergreenForest=vector(), grasslandHerbaceous=vector(), mixedForest=vector(), pastureHay=vector(), sedgeHerbaceous=vector(), shrubScrub=vector(), woodyWetlands=vector(), NAs=vector()) 

sublpot_sing=plant_dat_1m[match(all_plots,plant_dat_1m$subplot_uni_ID),] #simplified dataset with a single row for each unique subplot such that metadata can be extrated

all_sites=unique(plant_dat_1m$siteID)
for(ss in 1:length(all_sites)){
  curr_site= all_sites[ss]
  curr_nlcd=sublpot_sing$nlcdClass[sublpot_sing$siteID==curr_site]
  
  curr_nlcd_sum=summary(curr_nlcd)
  
  site_nlcd[ss,match(names(curr_nlcd_sum),all_classes)]=curr_nlcd_sum
  
}

site_nlcd$tot_plots=rowSums(site_nlcd,na.rm = TRUE)
site_nlcd$site=all_sites
site_prop_nlcd=site_nlcd
site_prop_nlcd[,1:12]=site_prop_nlcd[,1:12]/site_prop_nlcd[,13]
names(site_prop_nlcd)[1:12]=paste0("prop_",names(site_prop_nlcd)[1:12])
site_nlcd=cbind(site_nlcd[,14],site_nlcd[,1:13],site_prop_nlcd[,1:12])
names(site_nlcd)[1]="site"
site_nlcd$main_nlcd=NA
for(ss in 1:length(all_sites)){
  site_nlcd$main_nlcd[ss]=all_classes[order(site_prop_nlcd[ss,1:12],na.last = TRUE,decreasing = TRUE)[1]]
}
site_nlcd$main_nlcd=as.factor(site_nlcd$main_nlcd)



