# Ideas for imporvments
# 1- Add a parameter to export the results of the cross validation
# 2- Create a function process the resutls of the cross validation
# 3- Add a parameter to processed different metalernears

# Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble)
#, dplyr, tidyr)

# Suppress warnings
options(warn=-1)
h2o.no_progress()

# Start an H2O cluster with nthreads = num cores on your machine
localH2O <-  h2o.init(nthreads = -1)

# Bring data into H2O
train <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/train.csv", destination_frame = "train")
test  <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/test.csv",  destination_frame = "test")


# Setup Model
y      <- "TARGET"
x      <- setdiff(names(train), y)
family <- "binomial"
nfolds <- 5

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])  

# Subset data
train <- train[1:1000,]
test  <-  test[1:1000,]

#' Specify the base learner library & the metalearner
h2o.glm.1 <- function(..., alpha = 0.00, solver='L_BFGS') h2o.glm.wrapper(..., alpha = alpha, solver = solver)
h2o.glm.2 <- function(..., alpha = 0.50)                  h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 0.99)                  h2o.glm.wrapper(..., alpha = alpha)

learner <- c("h2o.glm.1","h2o.glm.2","h2o.glm.3")

# Specify a defalt GLM as the metalearner
h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)

# Train the ensemble using 5-fold CV to generate level-one data
fit <- h2o.ensemble(x = x, y = y,
                    training_frame = train, 
                    family = family, 
                    learner = learner, 
                    metalearner = "h2o.glm_nn",
                    cvControl = list(V = nfolds , shuffle = TRUE))

# Create Cross Validation Function w/ Performance OutperLoop Metric
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

fit_cv  <- h2o.ensemble_cv(model = fit, training_frame = train, K = 3, times = 2, seed = 1000)


### metalearn cv function
h2o.metalearn_cv <- function(object, metalearner = "h2o.glm.wrapper", seed = 1, keep_levelone_data = TRUE){
  out <- vector("list", length(object))
  for (i in 1:length(out)){
    out[[i]] <- h2o.metalearn(object[[i]], metalearner=metalearner, keep_levelone_data=keep_levelone_data)
    out[[i]]$metalearner <- metalearner
  }
  class(out) <- "h2o.ensemble_cv"
  return(out)
}
fit_cv_new <- h2o.metalearn_cv(object=fit_cv)


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


### performance cv function
h2o.ensemble_performance_cv <- function(object, training_frame=train, score_base_models=T){
    out <- vector("list", length(object))
    for (i in 1:length(out)){
      out[[i]] <- h2o.ensemble_performance(object[[i]], newdata=train[-object[[i]]$tt_ind,], score_base_models=score_base_models)
    }
    names(out) <- names(object)
    class(out) <- "h2o.ensemble_cv_performance"
    return(out)
}
perf_cv <- h2o.ensemble_performance_cv(fit_cv, train, score_base_models = T)


### print function for class 'h2o.ensemble_cv_performance'
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
  metares <- data.frame(Repeat=NA, metric=NA)
  row <- 1
  reps <- sapply(stringr::str_split(names(x), "\\."),"[", 2)
  
  for (i in 1:length(x)){
    Repeat <- reps[i]
    perf <- x[[i]]$ensemble@metrics[[metric]]
    metares[row, 'Repeat'] <- Repeat
    metares[row, 'metric'] <- perf
    
    row <- row + 1
  }
  metares <- aggregate(metares$metric, list(metares$Repeat), mean)
  names(metares) <- c('repeat',paste0('mean ',metric))
  
  ensemble_perf <- mean(metares[,2], na.rm=T)
  
  cat("\nH2O Ensemble CV Performance on <newdata>:")
  cat("\n----------------")
  cat(paste0("\nFamily: ", family))
  cat("\n")
  cat("\nK-fold cross validation mean performance:\n")
  print(metares)
  cat(paste0("\nRepeated K-fold cross-validation mean ensemble performance (", metric, "): ", ensemble_perf))
  cat("\n\n")
}




### model codes:
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/ensemble.R
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/metalearn.R
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/performance.R


# All done, shutdown H2O
# h2o.shutdown(prompt=FALSE)

