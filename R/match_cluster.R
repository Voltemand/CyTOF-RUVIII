
plot_cluster_match <- function(data1, data2){

  n_clust <- max(data1$cluster)

  mat <- matrix(0, nrow = n_clust, ncol = n_clust)
  for (i in 1:nrow(data1)){
    clust1 <- data1$cluster
    clust2 <- data2$cluster
    mat[clust1[i], clust2[i]] <- mat[clust1[i], clust2[i]] + 1
  }
  plot_data <- tibble(clust_1 = factor(rep(seq(1:n_clust), n_clust), 
                                       levels = 1:n_clust),
                      clust_2 = factor(rep(seq(1:n_clust), each = n_clust), 
                                       levels = rev(1:n_clust)),
                      data = asinh(as.vector(mat)))

  plot <- ggplot(plot_data, aes(clust_1, clust_2)) +
    geom_tile(aes(fill = data)) +
    labs(fill = "Num. cells \n(asinh)") +
    scale_fill_gradientn(colours =
        (colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(50))) +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
    theme_bw()

  plot
}

