
# Imports Catalyst
finck_norm <- function(raw_frame){
  norm_frame <- normCyToF(raw_frame, k = 500, plot = FALSE)
  norm_frame
}
