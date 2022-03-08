#Script to read and organize NEON biogeochemistry data

library(neonUtilities)
#check why this does not seem to include all sites
# stackByTable("data/NEON_data_products/NEON_soil-periodic.zip")
# zipsByProduct(dpID="DP1.10086.001", 
#               startdate="2017-01", 
#               enddate="2020-12",
#               package="basic", check.size=T,
#               savepath = "data/NEON_data_products/")

bgc_dat=readTableNEON("data/NEON_data_products/NEON_soil-periodic/stackedFiles/sls_soilChemistry.csv",varFile = "data/NEON_data_products/NEON_soil-periodic/stackedFiles/variables_10086.csv")

head(bgc_dat)
str(bgc_dat)

table(bgc_dat$plotID)

bgc_dat[bgc_dat$siteID=="HARV",]

C_dat=bgc_dat[bgc_dat$plotType=="distributed"&!is.na(bgc_dat$organicCPercent),]
C_dat$year=format(C_dat$collectDate,"%Y")


C_dat$plot_rich=plot_level_div$plot_mean_alpha[match(C_dat$plotID,plot_level_div$plot_ID)]
# for(pp in 1:nrow(C_dat)){
#   plot_level[which(plot_level$plotID==C_dat$plotID[pp]&plot_level$year==C_dat$year[pp]),"plotID"]
# }

lm(C_dat$organicCPercent~plot_level_div$plot_mean_alpha[match(C_dat$plotID,plot_level_div$plot_ID)])

# pdf("draft_figures/div_carbon_seq.pdf",height=5,width=5)
par(mfrow=c(1,1),mar=c(5,5,2,2))
pred_var=plot_level_div$plot_mean_alpha[match(C_dat$plotID,plot_level_div$plot_ID)]
resp_var=C_dat$organicCPercent
# resp_var=C_dat$CNratio
# mod=lmer(CNratio~plot_rich+(1|plotID),data =  C_dat  )
# mod=lmer(resp_var~pred_var+(1|C_dat$plotID))

plot_with_conf(pred_var,resp_var,mod = NULL,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),main = "",plot_type = "point",ylab="% Soil Organic Carbon", xlab="Distributed plot species richness",conf_col=2,cex.lab=1.4,cex.axis=1.35)

# dev.off()



# Show separate regressions lines for each site

# separate carbon sequestration by site

# pdf("draft_figures/div_carbon_seq_with_sites.pdf",height=5,width=5)
par(mfrow=c(1,1),mar=c(5,5,2,2))

plot(C_dat$organicCPercent~plot_level_div$plot_mean_alpha[match(C_dat$plotID,plot_level_div$plot_ID)],pch=19,cex=.2,type='p',col="grey60",ylab="% Soil Organic Carbon", xlab="Distributed plot species richness",cex.lab=1.4,cex.axis=1.35)

c_sites=sites$site[which(sites$site%in%C_dat$siteID)]
for(ss in 1:length(c_sites)){
  # par(new=T)
  
  pred_var=plot_level_div$plot_mean_alpha[match(C_dat$plotID[which(C_dat$siteID==c_sites[ss])],plot_level_div$plot_ID)]
  resp_var=C_dat$organicCPercent[which(C_dat$siteID==c_sites[ss])]
  # mod=lm(resp_var~pred_var)  
  
  xx=plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),plot_type = "only_regress",new_plot = FALSE,ylab="", xlab="",conf_col=alpha(cb_palette[ceiling(ss/5)],alpha = .4),cex.lab=1.4,cex.axis=1.35,pt_col_set = ss,yaxt="n",xaxt="n")
  

  #   if(xx$coefficients[2,4]<0.01){
  #     mtext(side = 3,text = paste(c_sites[ss], "**"),cex=1.5)
  #   } else if(xx$coefficients[2,4]<=0.05){
  #     mtext(side = 3,text = paste(c_sites[ss], "*"),cex=1.5)
  #   }else {
  #     mtext(side = 3,text = paste(c_sites[ss]),cex=1.5)
  # }
}

# par(new=T)
pred_var=plot_level_div$plot_mean_alpha[match(C_dat$plotID,plot_level_div$plot_ID)]
resp_var=C_dat$organicCPercent
mod=lm(resp_var~pred_var)  
plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var,na.rm = TRUE)),max_range = ceiling(max(pred_var,na.rm = TRUE)),main = "",plot_type = "only_conf",new_plot = FALSE,conf_col=2)

# dev.off()