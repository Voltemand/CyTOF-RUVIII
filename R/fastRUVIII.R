fastRUVIII = function(Y, M, ctl, k=NULL, eta=NULL, average=FALSE, fullalpha=NULL){
  # Assumes good input
  Y = RUV1(Y,eta,ctl)
  m = nrow(Y)
  Y0 = residop(Y, M)
  fullalpha = diag(rsvd(Y0)$d) %*% t(rsvd(Y0)$v)
  alpha = fullalpha[1:k,,drop=FALSE]
  ac = alpha[,ctl,drop=FALSE]
  W = Y[,ctl] %*% t(ac) %*% solve(ac %*% t(ac))
  newY = Y - W %*% alpha
  return(list(newY = newY, fullalpha=fullalpha))
}

# Should check the results coming out of this


