# Tosin's code for prepping, training, and testing
# SKor Data

setwd("~/R/UCL/BENV0091/EDA-Project/EDA-Project/R")

library(tidyverse)
library(rpart) # for regression trees
library(caret) # for splitting into train and test sets

df <- read_csv("../data/sKor_data_tot_v02.csv")
excluded_vars <- c("X1", "id_hs", "id_hh") #removing IDs so I can comfortably use (.)
df <- df %>% dplyr::select(-excluded_vars)
# r said to use `all_of(excluded_vars)` to silence warning but
# it worked sooo

#changing all cat_ variables to factors
df <- df %>% mutate(across(starts_with("cat"), as.factor))


set.seed(123)
train_index <- createDataPartition(df$num_tot_energy_heat, times = 1, 
                                   p = 0.7, list = FALSE)
#meant to create partition on the y values, as it stratifies that data
train <- df %>% slice(train_index)
test <- df %>% slice(-train_index)

#rpart tuning parameters: cp, minsplit, minbucket, maxdepth
#see rpart.control for explanations
#cp - most important - decides no. of splits in a tree
      # and therefore affects the rest of them i guess
#maxdepth - maximum depth of any node of the final tree (meaning?)
#minbucket is automatically minsplit/3

#cross validation to pick the best tuning parameters

train_rpart <- train(num_tot_energy_heat ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     trControl = trainControl(method = "repeatedcv",
                                  number = 10),
                     data = train)

best_cp <- train_rpart$bestTune
#bestTune before = 0.00625 (before change to factors)
#then = 0.008333333[5] RMSE 5026
#currently = 0.0041666 [3] RMSE 4927.586 (after cross validation)

#very rudimentary way of sourcing the row with the best cp but oh well
train_rpart[["results"]][3,]

y_hat <- predict(train_rpart, newdata = train, type = "raw")

#use vector if its a regression tree, and class if its a classification tree
# also: thought i could just extract final model from caret and use for predications
# but apparently not? caret_fit <- train_rpart$finalModel 
# use full trian_rpart


plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

results <- train %>% transmute(
  actual = num_tot_energy_heat,
  predicted = y_hat,
  error = actual-predicted
)





#creating a hypergrid that we can then use to tune rpart for minsplit
#and maxdepth
#honestly not sure if this is worth it, as this happens outside caret and 
#cp is most important 

hyper_grid <- expand.grid(
  minsplit = seq(20, 60, 1),
  maxdepth = seq(30, 60, 1)
)

#setting up a for loop to iterate through each minsplit and maxdepth
#combination, creating a different model for each combo

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = num_tot_energy_heat ~ .,
    data    = train,
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp from each model
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

#seeing best cps and errors alongside the minsplit and maxbucket values
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

#right now, very high errors lol