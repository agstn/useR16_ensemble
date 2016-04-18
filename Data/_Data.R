#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/useR16_ensemble/Data/Raw"
  loc_out  <- "C:/Git/useR16_ensemble/Data/Derive"
} else {
  loc_in   <- "/home/acalatroni/useR16_ensemble/Data/Raw"
  loc_out  <- "/home/acalatroni/useR16_ensemble/Data/Derive"
}

#' libraries
pacman::p_load(pacman)
p_load(dplyr,caret)

#' Import data
test  <- read.csv(paste0(loc_in,"/test.csv"))
train <- read.csv(paste0(loc_in,"/train.csv"))

#' Outcome
y      <- "TARGET"

#' Remove constant columns
train <- train[sapply(train, function(x) length(unique(na.omit(x)))) > 1]

#' Remove duplicate columns
train <- train[!duplicated(lapply(train,summary))]

#' Impute median/mode
train <- randomForest::na.roughfix(train)

#' Find and Remove linear combinations
ldv <- findLinearCombos(train)
train <- train[,-ldv$remove]

#' Export rds
test <- test[,setdiff(names(train), y)]
saveRDS(test,paste0(loc_out,"/test.rds"))
saveRDS(train,paste0(loc_out,"/train.rds"))

