

BatchEntropy <- function(dataset, batch0, L=100, M=100, k=100) {
#entropy of batch mixing
# L is the number bootstrapping times
# M is the number of randomly picked cells
# k is the number of nearest neighbours of cell (from all batches) to check

require(RANN)
nbatches<-length(unique(batch0))

entropy<-matrix(0,L,1)
set.seed(42)
for (boot in 1:L) {
  bootsamples<-sample(1:nrow(dataset),M)
  W21<-nn2(dataset,query=dataset[bootsamples,],k)

  for (i in 1:length(bootsamples)){

    for (j in 1:nbatches) {
    xi<-max(1,sum(batch0[W21$nn.idx[i,]]==j))
    entropy[boot]<-entropy[boot]+xi*log(xi)
    }
  }
}

return( (-1)*entropy/length(bootsamples) )
}

# Easier to write a wrapper in the calling program at this stage

# Idea:
#batch <- map_dbl(tsne_plot$sample, function(x) as.numeric(substr(x,1,1)))
#test <- my_BatchEntropy(dataset = tsne_plot, batch0 = batch)
#norm_entropy <- test
# entropy_data <- tibble(type = c(rep("Raw Data", 100), rep("RUVIII Nomrmalisation", 100)),
#                        entropy = c(raw_entropy, norm_entropy))
#
# ggplot(entropy_data, aes(x = factor(type), y = entropy)) +
#   geom_boxplot() +
#   labs(x = "", y = "Entropy of mixing") +
#   theme_bw()

