function (predfun, X, Y, K = 10, B = 20, verbose = TRUE, ...) 
{
  ygrouped = group.samples(Y)
  groupsize = sapply(ygrouped, length)
  nfolds = min(K, max(groupsize))
  if (verbose) 
    cat("Number of folds:", nfolds, "\n")
  allfolds = B * nfolds
  if (verbose) 
    cat("Total number of CV fits:", allfolds, "\n")
  stat.cv = NULL
  i = 1
  for (b in 1:B) {
    if (verbose) 
      cat("\nRound #", b, "of", B, "\n")
    folds = get.folds(ygrouped, K = nfolds)
    for (f in 1:nfolds) {
      if (verbose) 
        cat("CV Fit #", i, "of", allfolds, "\n")
      test.idx = folds[[f]]
      train.x = X[-test.idx, , drop = FALSE]
      train.y = Y[-test.idx]
      test.x = X[test.idx, , drop = FALSE]
      test.y = Y[test.idx]
      stat.new = predfun(train.x, train.y, test.x, test.y, 
                         ...)
      stat.cv = rbind(stat.cv, stat.new)
      rownames(stat.cv)[i] = paste0("B", b, ".F", f)
      i = i + 1
    }
  }
  stat = apply(stat.cv, 2, mean)
  stat.se = apply(stat.cv, 2, sd)/sqrt(allfolds)
  return(list(stat.cv = stat.cv, stat = stat, stat.se = stat.se))
}