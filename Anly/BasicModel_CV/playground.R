# Ideas for imporvments
# 1- Add a parameter to export the results of the cross validation
# 2- Create a function process the resutls of the cross validation
# 3- Add a parameter to processed different metalernears

# Package: Requires version >=0.1.8 of h2oEnsemble 
pacman::p_load(h2oEnsemble)

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

library(purrr)
library(dplyr)
library(tidyr)

dd <-train
K <- 2 
times <- 3
metalearner <- 'h2o.glm_nn'
model <- fit

# Create Cross Validation Function w/ Performance OutperLoop Metric
h2o.ensemble_cv <- function(model, training_frame = train, K = 3, times = 2, seed = 1000, metalearner=c('h2o.glm_nn')){
  
  dd <- training_frame
  set.seed(seed)
  
  ix <- caret::createMultiFolds(as.vector(dd[,model$y]), k = K, times = times)
  
  dr <- data.frame(name=rep(names(ix), length=metalearner), 
                   metalearner=rep(metalearner, length(ix))) %>% 
    group_by(name, metalearner) %>% 
    nest %>% 
    select(-data) %>% 
    mutate(ix = ix,
           ft = map(ix, ~h2o.ensemble(x = model$x, y = model$y,
                                      training_frame = dd[.,],
                                      family   = model$family,
                                      learner  = model$learner,
                                      metalearner = model$metalearner,
                                      cvControl = list(V = model$cvControl$V, shuffle = model$cvControl$shuffle))))
  return(dr)
}
fit_cv  <- h2o.ensemble_cv(model = fit, training_frame = train, K = 3, times = 1, seed = 1000)


### metalearn cv function
h2o.metalearn_cv <- function(model_cv = fit_cv, metalearner = c('h2o.glm_nn'), seed = 1){
  fit_cv %>% mutate(newfit = map(~h2o.metalearn(., metalearner=metalearner)))
}
h2o.metalearn()

### performance cv function
h2o.ensemble_performance_cv <- function(model_cv = fit_cv, training_frame=train){
  p1 <- fit_cv %>% mutate(perf = map2(ft, ix, ~h2o.ensemble_performance(.x, newdata=train[-.y,], score_base_models=F)$ensemble))
  return(p1)
}
p <- h2o.ensemble_performance_cv(fit_cv, train)

p$perf %>%  
  lapply(function(x) x@metrics)

for (i in 1:3){
  print(p$perf[[i]])
}




