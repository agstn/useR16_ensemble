h2o.ensemble_performance_cv <- function(object, training_frame=train, score_base_models=T){
  out <- vector("list", length(object))
  for (i in 1:length(out)){
    out[[i]] <- h2o.ensemble_performance(object[[i]], newdata=train[-object[[i]]$tt_ind,], score_base_models=score_base_models)
  }
  names(out) <- names(object)
  class(out) <- "h2o.ensemble_cv_performance"
  return(out)
}


print.h2o.ensemble_cv_performance <- function(x, metric = c("AUTO", "logloss", "MSE", "AUC", "r2"), ...) {
  
  # We limit metrics to those common among all possible base algos
  metric <- match.arg(metric)
  if (metric == "AUTO") {
    if (class(x[[1]]$ensemble) == "H2OBinomialMetrics") {
      metric <- "AUC"
      family <- "binomial"
    } else {
      metric <- "MSE"
      family <- "gaussian"
    }
  }
  
  # Base learner test set AUC (for comparison)
  if (!is.null(x[[1]]$base)) {
    res <- data.frame(model=NA, learner=NA, metric=NA)
    names(res)[3] <- metric
    
    for (i in 1:length(x)){
      model <- names(x)[i]
      learner <- names(x[[i]]$base)
      L <- length(learner)
      base_perf <- sapply(seq(L), function(l) x[[i]]$base[[l]]@metrics[[metric]])
      res2 <- data.frame(model = model, learner = learner, base_perf)
      names(res2)[3] <- metric
      # Sort order for base learner metrics
      res <- rbind(res, res2)
    }
    if (metric %in% c("AUC", "r2")) {
      # Higher AUC/R2, the better
      decreasing <- FALSE
    } else {
      decreasing <- TRUE
    }
    cat("\nBase learner performance, sorted by specified metric:\n")
    res <- na.omit(res[order(res[, c('model',metric)], decreasing = decreasing), ])
    print(res)
  }
  cat("\n")
  
  # Ensemble test set AUC
  metares <- vector(mode='numeric',length=length(x))
  
  for (i in 1:length(x)){
    metares[i] <- x[[i]]$ensemble@metrics[[metric]]
  }
  ensemble_perf <- mean(metares, na.rm=T)
  
  cat("\nH2O Ensemble CV Performance on <newdata>:")
  cat("\n----------------")
  cat(paste0("\nFamily: ", family))
  cat("\n")
  cat(paste0("\nCross-validation mean ensemble performance (", metric, "): ", ensemble_perf))
  cat("\n\n")
}