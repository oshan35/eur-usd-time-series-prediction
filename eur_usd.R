
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

exchangeEUR <- read_excel("ExchangeUSD.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())



#braking the original dataset in to 3 diffrent lags and three diffrent classes 
#and combine them to one data frame
eur_exchange_full = exchangeEUR %>%
  mutate(previous_one_day_set_a = lag(exchangeEUR$usd_eur,1),
         previous_one_day_set_b = lag(exchangeEUR$usd_eur,1),
         previous_two_day_set_b = lag(exchangeEUR$usd_eur,2),
         previous_one_day_set_c = lag(exchangeEUR$usd_eur,1),
         previous_two_day_set_c = lag(exchangeEUR$usd_eur,2),
         previous_three_day_set_c = lag(exchangeEUR$usd_eur,3),
         previous_one_day_set_d = lag(exchangeEUR$usd_eur,1),
         previous_two_day_set_d = lag(exchangeEUR$usd_eur,2),
         five_day_rolling = rollmean(usd_eur,5, fill = NA),
         ten_day_rolling = rollmean(usd_eur,10, fill = NA)) %>%
  
  drop_na()

boxplot(eur_exchange_full$usd_eur)

eur_exchange_full %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Set of Input Variables") +
  theme(legend.position = "none")


eur_exchange_full %>%
  pivot_longer(cols = c(4,5),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Set of Input Variables") +
  theme(legend.position = "none")

eur_exchange_full %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Set of Input Variables") +
  theme(legend.position = "none")

eur_exchange_full %>%
  pivot_longer(cols = 9:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Set of Input Variables") +
  theme(legend.position = "none")

summary(eur_exchange_full)


# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# All the variables are normalized
normalized_eur = eur_exchange_full %>%
  mutate(across(2:12, ~normalize(.x)))
# Look at the data that has been normalized
summary(normalized_eur)

boxplot(normalized_eur$usd_eur)

set.seed(123)
eur_train <- normalized_eur[1:400,]
eur_test <- normalized_eur[401:491,]

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }
# Get the min and max of the original training values
eur_min_train <- min(eur_exchange_full[1:400,2])
eur_max_train <- max(eur_exchange_full[1:400,2])
# Get the min and max of the original testing values
eur_min_test <- min(eur_exchange_full[401:491,2])
eur_max_test <- max(eur_exchange_full[401:491,2])
# Check the range of the min and max of the training dataset
eur_min_test

eur_min_train

eur_max_test
eur_max_train
relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}
View(eur_train)


set.seed(12345)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(usd_eur ~ previous_one_day_set_b+previous_two_day_set_b, data=eur_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  
  #plot(nn_model_true)
  pred <- predict(nn_model_true, eur_test)
  
  validation_df <- data.frame(c(eur_test$date_in_ymd),c(pred),c(eur_test$usd_eur))
  

  ###############################
  p = ggplot() + 
    geom_line(data = validation_df, aes(x = c.eur_test.date_in_ymd., y = c.pred.), color = "blue") +
    geom_line(data = validation_df, aes(x = c.eur_test.date_in_ymd., y = c.eur_test.usd_eur.), color = "red") +
    xlab('Dates') +
    ylab('percent.change')
  print(p)
  ############################
  
  train_results = compute(nn_model_true,eur_test[,2:3])
  truthcol = eur_exchange_full[401:491,2]$usd_eur
  predcol = unnormalize(train_results$net.result,eur_min_train, eur_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "B") %>%
    filter(.metric != "rsq")
}

# creation of different models with varying number of nodes
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m)
      }))})) %>%
  janitor::clean_names()

model_two_hidden_layers(2,3)

# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])


##########################################################################
# three layer model
set.seed(12345)
# function setup that creates 3 layer model
model_three_hidden_layers = function(hidden,sec_hidden,third_hidden) {
  nn_model_true = neuralnet(usd_eur ~ previous_one_day_set_b+previous_two_day_set_b, data=eur_train, hidden=c(hidden,sec_hidden,third_hidden), linear.output=TRUE)
  
  #plot(nn_model_true)
  pred <- predict(nn_model_true, eur_test)
  
  validation_df <- data.frame(c(eur_test$date_in_ymd),c(pred),c(eur_test$usd_eur))
  
  
  ################
  p = ggplot() + 
    geom_line(data = validation_df, aes(x = c.eur_test.date_in_ymd., y = c.pred.), color = "blue") +
    geom_line(data = validation_df, aes(x = c.eur_test.date_in_ymd., y = c.eur_test.usd_eur.), color = "red") +
    xlab('Dates') +
    ylab('percent.change')
  print(p)
  ################
  
  train_results = compute(nn_model_true,eur_test[,2:3])
  truthcol = eur_exchange_full[401:491,2]$usd_eur
  predcol = unnormalize(train_results$net.result,eur_min_train, eur_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Three Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden," and ",third_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}



# creation of different models with varying number of nodes
results_three_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        bind_rows(
          lapply(1:5, function(l) {
            model_three_hidden_layers(n,m,l)
          }))
      }))})) %>%
  janitor::clean_names()

model_three_hidden_layers(7,4,1)

# save the stat indices to a dataframe
set_a_models_three_layers = results_three_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_three_layers[1:10,])



