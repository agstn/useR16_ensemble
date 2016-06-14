#' ---
#' title:   "Ensemble Cross-Validation Example"
#' author:  "useR16_ensemble"
#' date:    "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    html_document:
#'         toc: false
#'         toc_float: false
#'         code_folding: show
#'         fig_align: 'left'
#'         fig_width  : 8
#'         fig_height : 10
#'         number_sections: false
#' ---

library(h2oEnsemble) 
h2o.no_progress()
localH2O <-  h2o.init(nthreads = -1)

# load files
source("/home/acalatroni/useR16_ensemble/R/performance_cv.R")
source("/home/acalatroni/useR16_ensemble/R/metalearn_cv.R")
source("/home/acalatroni/useR16_ensemble/R/ensemble_cv.R")

# Import a sample binary outcome train/test set into R
train <- h2o.importFile("http://www.stat.berkeley.edu/~ledell/data/higgs_10k.csv")
test  <- h2o.importFile("http://www.stat.berkeley.edu/~ledell/data/higgs_test_5k.csv")
y <- "C1"
x <- setdiff(names(train), y)
family <- "binomial"

#For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])  
test[,y]  <- as.factor( test[,y])

# Specify the base learner library & the metalearner
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")

# Specify Non-Negative Least Square as metalearner
h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)

# Train the ensemble using 5-fold CV to generate level-one data
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    family = family, 
                    learner = learner, 
                    metalearner = "h2o.glm_nn",
                    cvControl = list(V = 5, shuffle = TRUE))

fit_cv <- h2o.ensemble_cv(fit, training_frame = train, K = 5, times = 3)

# Compute test set performance:
perf <- h2o.ensemble_performance_cv(fit_cv, training_frame=train)
perf 

# Train with new Metalearner
nfit_cv <- h2o.metalearn_cv(fit_cv, metalearner = "h2o.deeplearning.wrapper")

# test Performance with new Metalerner
nperf <- h2o.ensemble_performance_cv(nfit_cv, training_frame=train)
nperf 
