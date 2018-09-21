make_M <- function(clusters, norm_clus){
  M <- matrix(0, nrow = length(clusters), ncol = length(norm_clus))
  # rewrite the for loop
  for(i in 1:length(norm_clus)){
    M[clusters == norm_clus[i], i] = 1
  }
  as.matrix(M)
}


