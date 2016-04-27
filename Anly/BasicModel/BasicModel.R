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

# Specify the base learner library & the metalearner
# learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
#              "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")

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

# Save ensemble model (a collection of H2OModels and an RData object) to disk
# h2o.save_ensemble(fit, path = "~/useR16_ensemble/Anly/BasicModel", force = FALSE, export_levelone = TRUE)
# fit <- h2o.load_ensemble(path = "~/useR16_ensemble/Anly/BasicModel")

# Compute test set performance:
perf <- h2o.ensemble_performance(fit, newdata = train)
print(perf)

# To access results directly: 

# Ensemble test set AUC
perf$ensemble@metrics$AUC

# Base learner test set AUC (for comparison)
L <- length(learner)
auc <- sapply(seq(L), function(l) perf$base[[l]]@metrics$AUC)
data.frame(learner, auc)

# If desired, you can generate predictions on the test set
# This is useful if you need to calculate custom performance metrics in R (not provided by H2O)
pp <- predict(fit, test)
predictions <- as.data.frame(pp$pred)[,3]  #third column, p1 is P(Y==1)
labels <- as.data.frame(test[,y])[,1]
