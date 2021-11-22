# Tosin's code for prepping, training, and testing
# SKor Data

library(tidyverse)
library(rpart) # for regression trees
library(caret) # for splitting into train and test sets

df <- read_csv("sKor_data_tot_v01")

set.seed(123)
train_index <- createDataPartition(df$X1, times = 1, p = 0.7, list = FALSE)
train <- df %>% slice(train_index)
test <- df %>% slice(-train_index)

