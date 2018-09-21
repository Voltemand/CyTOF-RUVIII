

plot_cluster_freq <- function(data){

  count_matrix <- cluster_count_matrix(data)
  freq_mat <- 100 * apply(count_matrix, 2, function(x) x/sum(x))

  NUM_CLUSTERS <- max(data$cluster)
  NUM_SAMPLES <- length(unique(data$sample))
  
  # Produce a marker matrix
  cell_amounts <- table(data$cluster)
  cell_freqs <- round(cell_amounts/sum(cell_amounts) * 100, 2)
  labels_row_percent <- paste0(1:NUM_CLUSTERS, " (", cell_freqs, "%)")
  labels_row <- paste0(1:NUM_CLUSTERS)

  # Reform the matrix into a tibble
  plot_data <- tibble(
    centre = factor(rep(unique(data$sample), each = NUM_CLUSTERS),
      levels = unique(data$sample)),
    # Reverse the plotting layout
    cluster = factor(rep(labels_row_percent, NUM_SAMPLES),
      levels = rev(labels_row_percent)),
    freq = as.vector(freq_mat)
  )

  # Plot the matrix
  heatmap <- ggplot(plot_data, aes(factor(centre), factor(cluster))) +
    geom_tile(aes(fill = freq)) +
    theme_bw() +
    labs(x = "Centre",
      y = "Cluster",
      fill = "Percentage of \ncells in cluster") +
    scale_fill_gradientn(colours =
        (colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(50))) +
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())

  plots <- list(heatmap = heatmap, hist = plot_stat_hist(count_matrix))
  plots
}

# TODO: Could refactor heatmap generation into seperate function

plot_stat_hist <- function(matrix){

  stats <- compute_stats(matrix)
  
  plot <- ggplot(as.tibble(stats), aes(value)) + 
    geom_histogram(colour = "black", fill = "skyblue") +
    geom_vline(xintercept = median(as.vector(stats), na.rm = TRUE), col = "red") +
    labs(y = "Count", x = "Stats") + 
    annotate(geom="text", x=Inf, y=Inf, label=as.character(median(as.vector(stats), na.rm = TRUE)),
             vjust = 2, hjust = 2) + 
    theme_bw()
    
  plot
}

compute_stats <- function(mat){
  row_sums <- rowSums(mat)
  col_sums <- colSums(mat)
  n <- sum(mat)
  stats <- numeric(nrow(mat))
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      E <- (row_sums[i]*col_sums[j])/n
      O <- mat[i,j]
      stats[i] <- stats[i] + (O - E)^2/E
    }
  }
  stats
}

cluster_count_matrix <- function(data){
  num_clusters <- as.numeric(max(data$cluster))
  num_samples <- length(unique(data$sample))

  samp_ind <- rep(1:num_samples, each = 10000)
  mat <- matrix(0, nrow = num_clusters, ncol = num_samples)

  for(i in 1:nrow(data)) {
    mat[data$cluster[i], samp_ind[i]] <- mat[data$cluster[i], samp_ind[i]] + 1
  }
  mat
}

