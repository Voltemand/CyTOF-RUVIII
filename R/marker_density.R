


plot_marker_densities <- function(data){
  
  data <- gather(data, key = "marker", value = "intensity", -cluster, -sample)
  
  plot <- ggplot(data, aes(x = intensity, col = sample)) + 
    geom_density() +
    # Hard code plot limits
    scale_y_continuous(limits= c(0, 1)) + 
    scale_x_continuous(limits= c(0, 7)) + 
    labs(y = "Density", 
         x = "Transformed Intenstiy") + 
    facet_wrap( ~ marker, nrow = 4, scales = "free") + 
    theme_bw()
  plot
}
