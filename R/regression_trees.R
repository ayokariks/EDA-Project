# Tosin's code for prepping, training, and testing
# SKor Data

setwd("~/R/UCL/BENV0091/EDA-Project/EDA-Project/R")

library(tidyverse)
library(rpart) # for regression trees
library(caret) # for splitting into train and test sets

df <- read_csv("../data/sKor_data_tot_v01.csv")
excluded_vars <- c("X1", "id_hs", "id_hh") #removing IDs so I can comfortably use (.)
df <- df %>% dplyr::select(-excluded_vars)
# r said to use `all_of(excluded_vars)` to silence warning but
# it worked sooo

set.seed(123)
train_index <- createDataPartition(df$num_tot_energy_heat, times = 1, 
                                   p = 0.7, list = FALSE)
#meant to create partition on the y values, as it stratifies that data
train <- df %>% slice(train_index)
test <- df %>% slice(-train_index)

#rpart tuning parameters: cp, minsplit, minbucket, maxdepth
#see rpart.control for explanations

#cross validation to pick the best tuning parameters

train_rpart <- train(num_tot_energy_heat ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = train)

best_cp <- train_rpart$bestTune
#bestTune currently = 0.00625

#cant seem to be able to cross validate minbucket/minsplit
#look up how to prune with cross validation? will that be after 
#plotting actual tree?

#creating a hypergrid that we can then use to tune rpart 
hyper_grid <- expand.grid(
  minsplit = seq(20, 60, 1),
  maxdepth = seq(8, 15, 1)
)