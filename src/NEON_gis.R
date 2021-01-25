# NLCD processing to identify cover classes at each NEON site
# library(FedData)
library(rgdal)

# Load NEON site shapefiles
neon_site_shp=readOGR(dsn="data/Field_Sampling_Boundaries_2020", layer="terrestrialSamplingBoundaries")
nlcd_shp=raster(x = "~/Downloads/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
alaska_domains=c("D18","D19")
hawaii_domains=c("D20")

nlcd_alaska=raster(x = "~/Downloads/NLCD_2016_Land_Cover_AK_20200724/NLCD_2016_Land_Cover_AK_20200724.img")
nlcd_hawaii=raster(x = "~/Downloads/HI_landcover_wimperv_9-30-08_se5/hi_landcover_wimperv_9-30-08_se5.img")

nlcd_classes=read_csv("data/nlcd_classes.csv")

nlcd_site_vals=data.frame(matrix(NA, ncol=99,nrow=length(neon_site_shp)))
names(nlcd_site_vals)=c("siteName","siteID",   "areaKm2",paste(0:95) )


names(neon_site_shp)

for(ss in 1:length(neon_site_shp)){
  print(ss)
  print(neon_site_shp@data[ss,4])
  if(neon_site_shp@data[ss,1]%in%alaska_domains){
    site_vals=extract(nlcd_alaska,neon_site_shp[ss,])
  }else if(neon_site_shp@data[ss,1]%in%hawaii_domains){
    site_vals=extract(nlcd_hawaii,neon_site_shp[ss,])
  }else{
  site_vals=extract(nlcd_shp,neon_site_shp[ss,])
  }
  nlcd_site_vals[ss,match(names(table(site_vals)),names(nlcd_site_vals))]=table(site_vals)
  nlcd_site_vals[ss,1:3]=neon_site_shp@data[ss,c(4,5,7)]
  
}
nlcd_site_vals=nlcd_site_vals[,which(apply(X = !is.na(nlcd_site_vals),MAR=2,FUN = any))]

nlcd_site_vals[is.na(nlcd_site_vals)]=0
nlcd_site_vals$tot_pix=rowSums(nlcd_site_vals[,5:ncol(nlcd_site_vals)])

nlcd_classes$Level_2_class_no_sp=gsub(x = nlcd_classes$Level_2_class,pattern = " ",replacement = "_")
names(nlcd_site_vals)[which(!is.na(match(names(nlcd_site_vals),nlcd_classes$Class_value)))]=nlcd_classes$Level_2_class_no_sp[na.omit(match(names(nlcd_site_vals),nlcd_classes$Class_value))]

prop_nlcd_vals=nlcd_site_vals[,4:(ncol(nlcd_site_vals)-1)]/nlcd_site_vals[,ncol(nlcd_site_vals)]
names(prop_nlcd_vals)=paste0("prop_",names(nlcd_site_vals)[4:(ncol(nlcd_site_vals)-1)])

nlcd_site_vals=cbind(nlcd_site_vals,prop_nlcd_vals)

nlcd_site_vals$tot_pix
nlcd_site_vals$siteID

