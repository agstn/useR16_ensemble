# Ideas for imporvments
# 1- Add a parameter to export the results of the cross validation
# 2- Create a function process the resutls of the cross validation
# 3- Add a parameter to processed different metalernears

# Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble, dplyr, tidyr, purrr,lattice)

# Suppress warnings
options(warn=-1)
h2o.no_progress()

# Start an H2O cluster with nthreads = num cores on your machine
localH2O <-  h2o.init(nthreads = -1)

# Bring data into H2O
train <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/train.csv", destination_frame = "train")
test  <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/test.csv",  destination_frame = "test")

# Description of A Dataset
# h2o.describe(train)

# Another way to import the data
# train <- readRDS("~/useR16_ensemble/Data/Derive/train.rds")
# train <- as.h2o(train, destination_frame = 'train')
# 
# test <- readRDS("~/useR16_ensemble/Data/Derive/test.rds")
# test <- as.h2o(test, destination_frame = 'test')

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
    # Predict on validation set
    ff$tt_ind <- ix[[j]]
    ff$folds <- K
    ff$repeats <- times
    out[[j]] <- ff
  }
  names(out) <- paste0(names(out), '__', model$metalearner)
  class(out) <- "h2o.ensemble_cv"
  return(out)
}

fit_cv  <- h2o.ensemble_cv(model = fit, training_frame = train, K = 3, times = 1, seed = 1000)


### metalearn cv function
h2o.metalearn_cv <- function(object, metalearner = "h2o.glm.wrapper", seed = 1, keep_levelone_data = TRUE){
  out <- object %>% map(~h2o.metalearn(., metalearner=metalearner, keep_levelone_data=keep_levelone_data))
  names(out) <- paste0(unlist(lapply(strsplit(names(out), '_', fixed = TRUE), '[', 1)), '__', metalearner)
  for(i in 1:length(out)){
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
h2o.ensemble_performance_cv <- function(object, training_frame=train, score_base_models=F){
    out <- object %>% map(~h2o.ensemble_performance(., newdata=train[-.$tt_ind,], score_base_models=score_base_models)$ensemble)
    class(out) <- "h2o.ensemble_cv_performance"
    return(out)
}
perf_cv <- h2o.ensemble_performance_cv(fit_cv, train)


### print function for class 'h2o.ensemble_cv_performance'
print.h2o.ensemble_cv_performance <- function(x, metric = c("AUTO", "logloss", "MSE", "AUC", "r2"), ...) {
  
  # We limit metrics to those common among all possible base algos
  metric <- match.arg(metric)
  if (metric == "AUTO") {
    if (class(x$ensemble) == "H2OBinomialMetrics") {
      metric <- "AUC"
      family <- "binomial"
    } else {
      metric <- "MSE"
      family <- "gaussian"
    }
  }
  
  # Base learner test set AUC (for comparison)
  if (!is.null(x$base)) {
    learner <- names(x$base)
    L <- length(learner)
    base_perf <- sapply(seq(L), function(l) x$base[[l]]@metrics[[metric]])
    res <- data.frame(learner = learner, base_perf)
    names(res)[2] <- metric
    # Sort order for base learner metrics
    if (metric %in% c("AUC", "r2")) {
      # Higher AUC/R2, the better
      decreasing <- FALSE
    } else {
      decreasing <- TRUE
    }
    cat("\nBase learner performance, sorted by specified metric:\n")
    res <- res[order(res[, metric], decreasing = decreasing), ]
    print(res)
  }
  cat("\n")
  
  # Ensemble test set AUC
  ensemble_perf <- x$ensemble@metrics[[metric]]
  
  cat("\nH2O Ensemble Performance on <newdata>:")
  cat("\n----------------")
  cat(paste0("\nFamily: ", family))
  cat("\n")
  cat(paste0("\nEnsemble performance (", metric, "): ", ensemble_perf))
  cat("\n\n")
}


# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/ensemble.R
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/metalearn.R
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/ensemble/h2oEnsemble-package/R/performance.R




# perf[1][[1]]@metrics$AUC
# perf[1][[1]]@metrics$MSE
# perf[1][[1]]@metrics$r2
# perf[1][[1]]@metrics$thresholds_and_metric_scores
# perf[1][[1]]@metrics$max_criteria_and_metric_scores


# All done, shutdown H2O
# h2o.shutdown(prompt=FALSE)

