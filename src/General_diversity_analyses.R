site_nlcd[,c("site","main_nlcd")]
site_mean_nat

par(mfrow=c(2,1),mar=c(10,4,4,2))
plot(x=as.numeric(site_nlcd[match(site_mean_nat$site,site_nlcd$site),"main_nlcd"]),y=site_mean_nat$native_mean,xaxt="n",xlab="")


axis(side = 1,at = 1:10,labels = paste(levels(site_nlcd$main_nlcd)),las=2)


NERR_coefs$site
NERR_coefs$nat_coef
plot(x=as.numeric(site_nlcd[match(NERR_coefs$site,site_nlcd$site),"main_nlcd"]),y=NERR_coefs$nat_coef,xaxt="n",xlab="")



# Plot with climate parameters
# plot(x=jitter(site_dat$field_mean_annual_temperature_C[match(NERR_coefs$site,site_dat$field_site_id)],factor = 10),y=jitter(site_dat$field_mean_annual_precipitation_mm[match(NERR_coefs$site,site_dat$field_site_id)],factor=20),xlab="",cex=site_mean_nat$native_mean/3,col=cb_palette[as.numeric(site_nlcd$main_nlcd[match(site_mean_nat$site,site_nlcd$site)])],pch=19)

plot(x=jitter(site_dat$field_mean_annual_temperature_C[match(NERR_coefs$site,site_dat$field_site_id)][order(site_mean_nat$native_mean,decreasing = TRUE)],factor = 10),y=jitter(site_dat$field_mean_annual_precipitation_mm[match(NERR_coefs$site,site_dat$field_site_id)][order(site_mean_nat$native_mean,decreasing = TRUE)],factor=20),cex=site_mean_nat$native_mean[order(site_mean_nat$native_mean,decreasing = TRUE)]/3,col=cb_palette[as.numeric(site_nlcd$main_nlcd[match(site_mean_nat$site,site_nlcd$site)])[order(site_mean_nat$native_mean,decreasing = TRUE)]],pch=19,ylab="Site mean annual precipitation (mm)",xlab="Site mean annual temperature (\u00B0C)")

legend(x=16,y=2800,legend = levels(site_nlcd$main_nlcd),col = cb_palette,bty = "n",pch=19, cex=1.2)


# calculate temporal beta diversity for each plot/site
