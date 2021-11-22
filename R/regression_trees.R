# Tosin's code for prepping, training, and testing
# SKor Data

library(tidyverse)
library(rpart) # for regression trees
library(caret) # for splitting into train and test sets

df <- read_csv("../data/sKor_data_v01.csv")

#set.seed(123)
train_index <- createDataPartition(df$X1, times = 1, p = 0.7, list = FALSE)
train <- df %>% slice(train_index)
test <- df %>% slice(-train_index)


fit <- rpart(num_tot_energy_heat ~ ., data = df)

qplot(num_tot_energy_heat, num_hus_age, data = df)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)
