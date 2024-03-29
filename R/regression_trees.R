# Tosin's code for prepping, training, and testing
# SKor Data

setwd("~/R/UCL/BENV0091/EDA-Project/EDA-Project/R")

library(tidyverse)
library(rpart) # for regression trees
library(rpart.plot)
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
errors <- train_rpart[["results"]][3,]

y_hat <- predict(train_rpart, newdata = train, type = "raw")

#use vector if its a regression tree, and class if its a classification tree
# also: thought i could just extract final model from caret and use for predications
# but apparently not? caret_fit <- train_rpart$finalModel 
# use full trian_rpart

results <- train %>% transmute(
  actual = num_tot_energy_heat,
  predicted = y_hat,
  error = actual-predicted,
  mse = mean(error^2),
  rmse = sqrt(mse),
  mae = mean(abs(actual-predicted)),
  mape = mean(abs(actual-predicted)/actual*100)
)

#right now, very high errors lol
#i've seen that the rmses/values when using rpart(), using train
# method = rpart are different, and also manually calculating rmse
#is different from the one given in the cp table?

#plotting the decision tree

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#using rpart plot gives a much better tree but low quality
rpart.plot(train_rpart$finalModel, tweak = 1.8, # adjusts size from default
           box.palette = "Purples",
           type = 1,
           fallen.leaves = FALSE,
           extra = 0) # takes away % 
# see https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf
# pg 17 for more help with playing with rpart.plot

#plotting errors
results %>% ggplot(aes(actual,predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0, max(results$predicted)) +
  xlim(0, max(results$actual))
#abline isnt going through origin?

## testing the dataset 
y_hat_test <- predict(train_rpart, newdata = test,
                      type = "raw")
test_results <- test %>% transmute(
  actual = num_tot_energy_heat,
  predicted = y_hat_test,
  error = actual-predicted,
  mse = mean(error^2),
  rmse = sqrt(mse),
  mae = mean(abs(actual-predicted)),
  mape = mean(abs(actual-predicted)/actual*100)
)
  
##comparing test and train
trainvtest <- merge(results, test_results, by = 0, all = TRUE)

## notes to self
min(results$actual)
min(results$predicted) #should we remove outliers?

results %>% ggplot(aes(actual,error)) + 
  geom_point()

test_results %>% ggplot(aes(actual,predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylim(0, max(results$predicted)) +
  xlim(0, max(results$actual))

#exporting results
write.csv(results, "../data/reg_tree_train_results.csv", row.names = FALSE)
write.csv(test_results, "../data/reg_tree_test_results.csv", row.names = FALSE)

#importance
varImp_importance <- varImp(train_rpart$finalModel, scale = TRUE)
write.csv(varImp_importance, "../data/reg_tree_varImp.csv", row.names = TRUE)
#HOWEVER the variable importances here do not align with common sense as
#seen in the rpart plot OR the rules below.... 
rules <- rpart.rules(train_rpart$finalModel)
