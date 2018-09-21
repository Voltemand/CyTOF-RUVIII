

plot_median_exprs <- function(data){

  n_clust <- max(data$cluster)

  plot_data <- data %>%
    group_by(cluster) %>%
    select(-sample) %>%
    summarise_all(median) %>%
    gather(key = cluster) %>%
    bind_cols(cluster = factor(rep(1:n_clust, ncol(data) - 2),
      levels = as.character(1:n_clust))) %>%
    set_names(c("marker", "intensity", "cluster"))

  plot <- ggplot(plot_data, aes(factor(marker), factor(cluster))) +
    geom_tile(aes(fill = intensity)) +
    theme_bw() +
    labs(x = "Cluster", y = "Marker", fill = "Median Intensity") +
    scale_fill_gradientn(colours =
        (colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(50))) +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

  plot
}
