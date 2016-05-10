# Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble)

# Suppress warnings
options(warn=-1)

# Start an H2O cluster with nthreads = num cores on your machine
localH2O <-  h2o.init(nthreads = -1)

# Bring data into H2O
train <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/train.csv", destination_frame = "train")
test <- h2o.uploadFile(path = "~/useR16_ensemble/Data/Raw/test.csv", destination_frame = "test")

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
h2o_cv <- function(model, K = 3, times = 2, seed = 1000){
  
  model = fit; K = 2; times = 2; seed = 1000;
  d <- train
  set.seed(seed)
  ind <- caret::createMultiFolds(as.vector(d[,model$y]), k = K, times = times)
  out <- vector("list", length(ind))
  names(out) <- names(ind)
  
  for (j in 1:length(ind)){
    print(paste0("Begin outer cross-validation loop: ", j, " of ", K*times))
    t <- d[ ind[[j]],]
    v <- d[-ind[[j]],]
    # fit the ensemble
    f <- h2o.ensemble(x = model$x, y = model$y,
                      training_frame = t,
                      family   = model$family,
                      learner  = model$learner,
                      metalearner = model$metalearner,
                      cvControl = list(V = model$cvControl$V, shuffle = model$cvControl$shuffle))
    # Predict on validation set
    out[[j]] <- h2o.ensemble_performance(f, newdata = v, score_base_models = FALSE)$ensemble
  }
  return(out)
}

fit_cv  <- h2o_cv(model = fit, K = 2, times = 2, seed = 1000)

names(fit_cv[[1]]@metrics)
fit_cv[[1]]@metrics$AUC
AUC  <- sapply(seq(length(fit_cv)), function(l)  fit_cv[[l]]@metrics$AUC)
AUC
# out[[1]]@metrics$thresholds_and_metric_scores
# out[[1]]@metrics$max_criteria_and_metric_scores

# All done, shutdown H2O
# h2o.shutdown(prompt=FALSE)

