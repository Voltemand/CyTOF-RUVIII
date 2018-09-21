
marker_F_stats <- function(data){
  n_clust <- max(data$clusters)

  F_stats <- matrix(0, nrow = n_clust, ncol = length(3:ncol(data)))
  for (j in 1:n_clust){
    cluster_data <- dplyr::filter(data, cluster == j)
    for(i in 3:ncol(data)) {
      marker <- cluster_data[ ,i]
      if (!(length(unique(cluster_data$sample)) == 1)){
        anova_data <- tibble(sample = cluster_data$sample, marker)
        model <- aov(marker ~ sample, anova_data)
        F_stats[j,i-2] <- summary(model)[[1]]["F value"][[1]][[1]]
      } else {
        F_stats[j,i-2] <- NA
      }
    }
  }

  F_stats <- asinh(F_stats/5)
  colnames(F_stats) <- colnames(data)[3:ncol(data)]
  rownames(F_stats) <- as.character(1:max(data$clusters))
  plots <- list(hist = F_hist(F_stats), heatmap = F_heatmap(F_stats))
  plots
}


PCA_F_stats <- function(data, n_PCA = 5){

  n_clust <- max(data$clusters)
  pca_data <- prcomp(data[, 3:ncol(data)], scale. = TRUE)$x[,1:n_PCA]
  pca_data <- as.data.frame(cbind(data$cluster, data$sample, pca_data))
  colnames(pca_data)[1:2] <- c("cluster", "sample")

  F_stats <- matrix(0, nrow = n_clust, ncol = NUM_PCS)
  for (j in 1:n_clust){
    cluster_data <- dplyr::filter(pca_data, cluster == j)
    for(i in 3:ncol(pca_data)) {
      prin_comp <- cluster_data[ ,i]
      anova_data <- tibble(sample = cluster_data$sample, prin_comp)
      model <- aov(prin_comp ~ sample, anova_data)
      F_stats[j,i-2] <- summary(model)[[1]]["F value"][[1]][[1]]
    }
  }

  F_stats <- asinh(F_stats/5)
  colnames(F_stats) <- colnames(data)[3:ncol(data)]
  rownames(F_stats) <- as.character(1:max(data$clusters))
  plots <- list(hist = F_hist(F_stats), heatmap = F_heatmap(F_stats))
  plots
}

tsne_F_stats <- function(tsne_data){

  n_clust <- max(tsne_data$cluster)

  F_stats <- matrix(0, nrow = n_clust, ncol = 2)
  for (j in 1:n_clust){
    cluster_data <- dplyr::filter(tsne_data, cluster == j)
    for(i in 3:ncol(tsne_data)) {
      # Disallow those that don't have enough different samples represneted
      if(length(unique(cluster_data$sample)) >= 2 & length(cluster_data$sample) > 6){
        tsne_dim <- cluster_data[ ,i]
        anova_data <- tibble(sample = cluster_data$sample, tsne_dim)
        model <- aov(tsne_dim ~ sample, anova_data)
        F_stats[j,i-2] <- summary(model)[[1]]["F value"][[1]][[1]]
      } else {
        F_stats[j,i-2] <- NA
      }
    }
  }

  F_stats <- asinh(F_stats/5)
  colnames(F_stats) <- colnames(tsne_data)[3:ncol(tsne_data)]
  rownames(F_stats) <- as.character(1:n_clust)
  plots <- list(hist = F_hist(F_stats), heatmap = F_heatmap(F_stats))
  plots
}


# Helpers
# Takes matrix of F statistics
F_hist <- function(F_stats){
  plot <- ggplot(as.tibble(as.vector(F_stats)), aes(value)) +
    geom_histogram(colour = "black", fill = "skyblue", binwidth = 0.25) +
    geom_vline(xintercept = median(as.vector(F_stats), na.rm = TRUE), col = "red") +
    labs(y = "Count", x = "F-stats") + 
    annotate(geom="text", x=Inf, y=Inf, label=as.character(median(as.vector(F_stats), na.rm = TRUE)),
             vjust = 2, hjust = 2) + 
    theme_bw()
  plot
}

# Takes matrix of F statistics with labelled columns
F_heatmap <- function(F_stats){
  F_stats <- tibble(data = as.vector(F_stats),
                    cluster = factor(rep(rownames(F_stats), ncol(F_stats)),
                        levels = as.character(1:nrow(F_stats))),
                    variable = rep(colnames(F_stats), each = nrow(F_stats)))

  plot <- ggplot(F_stats, aes(factor(variable), factor(cluster))) +
    geom_tile(aes(fill = data), col = "black") +
    theme_bw() +
    labs(x = "Variable",
      y = "Cluster",
      fill = "") +
    scale_fill_gradientn(colours =
        (colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(50))) +
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  plot
}

# Notes: Could try to rewrite the core into a more general function

