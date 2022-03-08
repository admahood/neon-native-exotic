# plot data with confidence interval

source("~/Dropbox/Documents/Data/R Info/Scripts/Confidence_interval_plot.R")




plot_with_conf=function(pred_var,resp_var,co_var,co_var_sim,mod=NULL,main="",pt_col_set=1,min_range=0,max_range=1,plot_type="point",new_plot=TRUE,conf_col=2,...){
  # resp_var
  # pred_var
  
  # abline_mod=lm(respvar~pred_var)
  # expect_vals=predict(spline_mod,newdata=data.frame(co_var_sim=co_var))
  
  if(is.null(mod)){
    emp_mod=lm(resp_var~pred_var)
  }
  else{
    print("Mod avail")
    emp_mod=mod
  }
  
  
  plot_out=plot_fit(emp_mod,focal_var = "pred_var",n = 50,min_range = min_range,max_range = max_range)
  
  if(plot_type=="box"){
    boxplot(resp_var~pred_var,at=unique(pred_var),boxwex=.25,outline=FALSE)
  }
  
  if(new_plot){
    if(plot_type=="only_regress"){
      plot(y=resp_var,x=pred_var,type="n",...)
      lines(plot_out[,1],plot_out$Pred,lwd=2,col=conf_col)
    }else{
    if(plot_type=="point"){
      plot(y=resp_var,x=pred_var,main=main,col=pt_col_set,...)
    }
    if(plot_type=="only_conf"){
      plot(y=resp_var,x=pred_var,type="n",...)
    }    
      lines(plot_out[,1],plot_out$Pred,lwd=2,col=conf_col)
      lines(plot_out[,1],plot_out$LC,lwd=2,lty=2,col=conf_col)
      lines(plot_out[,1],plot_out$UC,lwd=2,lty=2,col=conf_col)
    }
    
  }else{
    if(plot_type=="only_regress"){
      lines(plot_out[,1],plot_out$Pred,lwd=2,col=conf_col)
    }else{
      if(plot_type=="point"|plot_type=="only_conf"){
       
    
      lines(plot_out[,1],plot_out$Pred,lwd=2,col=conf_col)
      lines(plot_out[,1],plot_out$LC,lwd=2,lty=2,col=conf_col)
      lines(plot_out[,1],plot_out$UC,lwd=2,lty=2,col=conf_col)
    }
    
    
  }
  }
  
  print(summary(emp_mod))
  return(summary(emp_mod))
}




# 
# 
# 
# 
# 
# 
# 
# mixmod=lmer(resp_var~pred_var+(1|rand_var))
# 
# par(mar=c(5,5,2,2))
# plot_with_conf(pred_var,resp_var,min_range = floor(min(pred_var)),max_range = ceiling(max(pred_var)),mod=mixmod,main = "",pt_col_set=cb_palette[as.numeric(transect_meter_changes$lake)],plot_type = "point",ylab="Temporal turnover", xlab="Subplot species richness",conf_col=1,cex.lab=1.4,cex.axis=1.35)
# 
# 
# mod=lm(resp~pred)
# plot_out=plot_fit(mod,focal_var = "pred",n = 50,min_range = 0,max_range = 90)
# plot(y=resp,x=pred,main=main)#,ylab=resp_var,col=pt_col_set,...)
# 
# lines(plot_out[,1],plot_out$Pred,lwd=2,col=2)
# lines(plot_out[,1],plot_out$LC,lwd=2,lty=2,col=2)
# lines(plot_out[,1],plot_out$UC,lwd=2,lty=2,col=2)
# 
# 
# 
# 
# boxplot(resp_var~pred_var,at=unique(pred_var),boxwex=.25)
