# Idea: compute tsne once, pass to all functions...

# Take in data subsamples and converts it into annotated tsne form
compute_tsne <- function(data, N){
  samp_data <- sample_n(data, N, replace = FALSE)
  tsne <- cytof_dimReduction(samp_data[3:ncol(samp_data)], method = "tsne", tsneSeed = 42)
  colnames(tsne) <- c("tSNE1", "tSNE2")
  tsne_data <- data.frame(samp_data[,1:2], tsne)
  tsne_data
}

plot_tsne_sample <- function(tsne_data){
  plot <- ggplot(tsne_data , aes(x = tSNE1, y = tSNE2, col = sample)) +
  geom_point(alpha = 0.3) +
  labs(col = "Sample") +
  theme_bw()
  plot
}

plot_tsne_cluster <- function(tsne_data){
  plot <- ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, col = factor(cluster))) +
  geom_point(alpha = 0.3) +
  labs(col = "Cluster") +
  theme_bw()
  plot
}


plot_tsne_marker <- function(data, tsne_data){
  plots <- list()
  for (i in 3:ncol(data)){
    col_marker <- data[,i]
    marker_name <- colnames(data)[i]
    plot_data <- data.frame(tsne_data, col_marker)
    plot <- ggplot(plot_data, aes(tSNE1, tSNE2, col = col_marker)) +
      geom_point() +
      scale_colour_gradientn(colours =
          colorRampPalette(rev(brewer.pal(n = 11, name = "Spectral")))(50)) +
      labs(x = "tSNE1", y = "tSNE2", color = marker_name) +
      theme_bw()
    plots[[i-2]] <- plot
  }
  plots
}




