# Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble)

# Start an H2O cluster with nthreads = num cores on your machine
localH2O <-  h2o.init(nthreads = -1)

# Import a sample binary outcome train/test set into R
train <- readRDS("~/useR16_ensemble/Data/Derive/train.rds")
test  <- readRDS("~/useR16_ensemble/Data/Derive/test.rds")

y <- "TARGET"
x <- setdiff(names(train), y)
family <- "binomial"
nfolds <- 3

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])  

# Send data to h2o
train <- as.h2o(train)
test  <- as.h2o(test)


#' Specify the base learner library & the metalearner
h2o.glm.1 <- function(..., alpha = 0.00, solver='L_BFGS') h2o.glm.wrapper(..., alpha = alpha, solver = solver)
h2o.glm.2 <- function(..., alpha = 0.50)                  h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 0.99)                  h2o.glm.wrapper(..., alpha = alpha)

learner <- c("h2o.glm.1","h2o.glm.2","h2o.glm.3")

# Specify a defalt GLM as the metalearner
h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)

# Train the ensemble using 5-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    family = family, 
                    learner = learner, 
                    metalearner = "h2o.glm_nn",
                    cvControl = list(V = nfolds , shuffle = TRUE))

h2o.getFrame(model$basefits[[1]]@parameters$training_frame)
Y <- fit$basefits[[1]]@parameters$y


#' cross validation

h2o_cv <- function(model, K=5, B=1, seed=1000){
 
  results <- data.frame('repeatnum'=NA, 'foldnum'=NA, 'error'=NA)
  n <- 1
  
  d <- h2o.getFrame(model$basefits[[1]]@parameters$training_frame)

  set.seed(seed)
  ind <- caret::createMultiFolds(d[,model$y], k = K, times=B)
  
#  data.h2o <- as.h2o(data)
  
  for (b in 1:B){
    for (k in 1:K) {
      
      
      train <- d[-ind[[b*k]],]
      valid <- d[ind[[b*k]],]
      
      #' Find X & Y
    # y <- Y
      x <- setdiff(names(train), model$y)
      train[,model$y] <- as.factor(train[,model$y])  
      
      #' fit the ensemble
      fit <- h2o.ensemble(x = model$x, y = model$y,
                          training_frame = train,
                          family = model$family,
                          learner = model$learner,
                          metalearner = model$metalearner,
                          cvControl = list(V = model$cvControl$V, shuffle = model$cvControl$shuffle))
      
      #' Predict on validation set
      predicted <- as.vector(predict(fit,valid)$pred$predict)
      compare <- data.frame('predicted'=predicted, 'observed'=as.vector(valid[,y]))
      compare$match <- I(compare$predicted==compare$observed)
      
      results[n, 'repeatnum'] <- b
      results[n, 'foldnum'] <- k
      results[n, 'error'] <- sum(compare$match)/length(compare$match)
      
      n <- n+1
    }
  }
  
  return(results)
}


train0_sub <- train0[1:1000,]  ### make dataset small for development
results  <- h2o_cv(data=train0_sub, Y='TARGET', K=5, B=2, seed=1000)

results  <- h2o_cv(model = fit, K=5, B=2, seed=1000)
  