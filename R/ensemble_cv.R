h2o.ensemble_cv <- function(model, training_frame = train, K = 3, times = 2, seed = 1000){
  
  dd <- training_frame
  set.seed(seed)
  ix <- caret::createMultiFolds(as.vector(dd[,model$y]), k = K, times = times)
  out <- vector("list", length(ix))
  names(out) <- names(ix)
  
  for (j in 1:length(ix)){
    print(paste0("Begin outer cross-validation : ",names(out)[j]))
    tt <- dd[ ix[[j]],]
    vv <- dd[-ix[[j]],]
    # fit the ensemble
    ff <- h2o.ensemble(x = model$x, y = model$y,
                       training_frame = tt,
                       family   = model$family,
                       learner  = model$learner,
                       metalearner = model$metalearner,
                       cvControl = list(V = model$cvControl$V, shuffle = model$cvControl$shuffle))
    print(paste0("End outer cross-validation : ",names(out)[j]," ",round(as.vector(ff$runtime$total),1)," ","seconds"))
    
    ff$tt_ind <- ix[[j]]
    ff$folds <- K
    ff$repeats <- times
    out[[j]] <- ff
  }
  class(out) <- "h2o.ensemble_cv"
  return(out)
}


### print function for class 'h2o.ensemble_cv'
print.h2o.ensemble_cv <- function(x, ...) {
  cat("\nH2O Ensemble CV fit")
  cat("\n----------------")
  cat("\nfamily: ")
  cat(x[[1]]$family)
  cat("\nlearner: ")
  cat(x[[1]]$learner)
  cat("\nmetalearner: ")
  cat(x[[1]]$metalearner)
  cat("\nRepeated CV: ")
  cat(x[[1]]$folds,'fold CV repeated',x[[1]]$repeats, ifelse(x[[1]]$repeats<2,'time','times'))
  cat("\n\n")
}