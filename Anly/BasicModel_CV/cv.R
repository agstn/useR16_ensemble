#' Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble, caret)

#' Start an H2O cluster with nthreads = num cores on your machine
localH2O <-  h2o.init(nthreads = -1)

#' Import a sample binary outcome train/test set into R
train0 <- readRDS("~/useR16_ensemble/Data/Derive/train.rds")
test0  <- readRDS("~/useR16_ensemble/Data/Derive/test.rds")

#' model params
family <- "binomial"
nCV <- 5

#' Specify the base learner library & the metalearner
h2o.glm.1 <- function(..., alpha = 0.00, solver='L_BFGS') h2o.glm.wrapper(..., alpha = alpha, solver = solver)
h2o.glm.2 <- function(..., alpha = 0.50)                  h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 0.99)                  h2o.glm.wrapper(..., alpha = alpha)

learner <- c("h2o.glm.1","h2o.glm.2","h2o.glm.3")

#' Specify a defalt GLM as the metalearner
h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)

metalearner <- "h2o.glm.wrapper" 


Y <- 'TARGET'
train0_sub <- train0[1:500,]  ### make dataset small for development
data <- train0_sub  
K <- 5 
B <- 1

#' cross validation
#' 

h2o_cv <- function(data, Y, K=5, B=1){
  results <- data.frame('repeatnum'=NA, 'foldnum'=NA, 'error'=NA)
  n <- 1
  
  for (b in 1:B){
    
    set.seed(352*B)
    ind <- caret::createFolds(data[,Y], k = K, list = TRUE, returnTrain = FALSE)
    
    data.h2o <- as.h2o(data)
    
    for (k in 1:K) {
      
      train <- data.h2o[-ind[[1]],]
      valid <- data.h2o[ind[[1]],]
      
      #' Find X & Y
      y <- Y
      x <- setdiff(names(train), y)
      train[,y] <- as.factor(train[,y])  
      
      #' fit the ensemble
      fit <- h2o.ensemble(x = x, y = y,
                          training_frame = train,
                          family = family,
                          learner = learner,
                          metalearner = metalearner,
                          cvControl = list(V = nCV, shuffle = TRUE))
      
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

results  <- h2o_cv(data=train0, Y='TARGET', K=5, B=1)
  