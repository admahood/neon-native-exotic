# Aggregate species diversity data at the plot level

all_dist_plots=unique(plot_level$plotID)

# Run through each uique plot and calculate beta diversity for each time point
plot_level_mean_gamma=rep(NA,length(all_dist_plots))
plot_level_mean_alpha=rep(NA,length(all_dist_plots))
plot_level_mean_beta=rep(NA,length(all_dist_plots))

for (pp in 1:length(all_dist_plots)){
  curr_lines=which(all_sub_plot_div_data_yearly_sampled$plotID==all_dist_plots[pp])
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
    plot_level_mean_gamma[pp]=mean(curr_gamma)
    plot_level_mean_alpha[pp]=mean(curr_alpha)
    plot_level_mean_beta[pp]=mean(curr_beta)
  }
  print(pp)
}

plot_level_div=data.frame(site=gsub(x = all_dist_plots,pattern = "(....)_.+",replacement = "\\1"),plot_ID=all_dist_plots,plot_mean_gamma=plot_level_mean_gamma,plot_mean_alpha=plot_level_mean_alpha,plot_mean_beta=plot_level_mean_beta)
