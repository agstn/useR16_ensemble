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
test  <- as.h2o(train)

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
perf <- h2o.ensemble_performance(fit, newdata = test)
print(perf)

# New metalerner
fit2 <- h2o.metalearn(fit, metalearner = "h2o.deeplearning.wrapper")

# Compute test set performance:
perf2 <- h2o.ensemble_performance(fit2, newdata = test)
print(perf2)

# Run loop with metalerners
metalearner <- c("h2o.glm_nn",
                 "h2o.glm.wrapper", "h2o.randomForest.wrapper", 
                 "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
L           <- length(metalearner)
fit2      <- vector("list", L)
perf2     <- vector("list", L)

for (i in seq_along(metalearner)) {
  fit2[[i]]  <- h2o.metalearn(fit, metalearner = metalearner[[i]])
  perf2[[i]] <- h2o.ensemble_performance(fit2[[i]], newdata = test)
}

auc <- sapply(seq(L), function(l) perf2[[l]]$ensemble@metrics$AUC)
data.frame(metalearner, auc)
