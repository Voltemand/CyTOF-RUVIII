# Wrapper around the fastRUVIII function which takes care of changing the
# input and output scale and mean of the data
run_RUVIII <- function(data, norm_clusters, k){
  
  M <- make_M(data$cluster, norm_clusters)
  raw_Y <- as.matrix(data[3:ncol(data)])
  
  # Standardise the input and then compensate output
  col_means <- colMeans(raw_Y)
  col_sds <- apply(raw_Y, 2, function(x) sd(x))
  
  for(i in 1:ncol(raw_Y)){
    raw_Y[,i] <- (raw_Y[,i] - col_means[i])/col_sds[i]
  }
  # Run the actual RUVIII
  norm_Y <- fastRUVIII(Y = raw_Y, M, ctl = c(1:ncol(raw_Y)), eta = 1, k = k)$newY
  
  for(i in 1:ncol(norm_Y)){
    norm_Y[,i] <- norm_Y[,i]*col_sds[i] + col_means[i]
  }
  
  norm_Y
}

# Wrapper to allow the conversion of an annotated data object to it's normalised 
# version. Normalises cells and reclusters the data.  
normalise_data <- function(data, norm_clusters, k, num_clusters = 40, seed = 42){
  # Normalise the cells
  norm_cells <- run_RUVIII(data, norm_clusters, k)
  # Recluster the data
  norm_clusters <- cluster_FlowSOM(data = norm_cells, k = num_clusters, seed = seed)
  # Collate the data
  norm_data <- data.frame(cluster = norm_clusters, sample = data$sample, norm_cells)
  norm_data
}