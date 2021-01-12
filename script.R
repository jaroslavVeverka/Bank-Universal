rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(dlookr)
library(tidyr)
library(reshape2)
library(gridExtra)
library(fastDummies)
library(caret)
library(tidyverse)
library(MLeval)

universalbank_df <-
  read.csv(
    'UniversalBank.csv',
    na = c('', 'NA', '?'),
    header = TRUE,
    encoding = 'UTF-8'
  )
class(universalbank_df)

# basic
dim(universalbank_df)
str(universalbank_df)
head(universalbank_df)
tail(universalbank_df)
colnames(universalbank_df)
summary(universalbank_df)

# mutate categorical variables from int to factor
universalbank_df <- universalbank_df %>%
  mutate(
    Personal.Loan = as.factor(Personal.Loan),
    Securities.Account = as.factor(Securities.Account),
    CD.Account = as.factor(CD.Account),
    Online = as.factor(Online),
    CreditCard = as.factor(CreditCard),
    Family = as.factor(Family),
    Education = as.factor(Education)
  )

str(universalbank_df)
summary(universalbank_df)

# missing values
sum(is.na(universalbank_df))
sapply(universalbank_df, function(x)
  sum(is.na(x)))

# remove unused variables
universalbank_df <- universalbank_df %>%
  select(
    Age,
    Experience,
    Income,
    Family,
    CCAvg,
    Education,
    Mortgage,
    Personal.Loan,
    Securities.Account,
    CD.Account,
    Online,
    CreditCard
  )

# histograms
g1 <-
  ggplot(universalbank_df, aes(x = Age, fill = Personal.Loan)) + geom_histogram(binwidth =
                                                                                  5) + theme(legend.position = "none")
g2 <-
  ggplot(universalbank_df, aes(x = Experience, fill = Personal.Loan)) + geom_histogram(binwidth = 5) + theme(legend.position =
                                                                                                               "none")
g3 <-
  ggplot(universalbank_df, aes(x = Income, fill = Personal.Loan)) + geom_histogram(binwidth = 5) + theme(legend.position =
                                                                                                           "none")
g4 <-
  ggplot(universalbank_df, aes(x = Family, fill = Personal.Loan)) + geom_histogram(stat =
                                                                                     "count") + theme(legend.position = "none")
g5 <-
  ggplot(universalbank_df, aes(x = CCAvg, fill = Personal.Loan)) + geom_histogram(binwidth = 5) + theme(legend.position =
                                                                                                          "none")
g6 <-
  ggplot(universalbank_df, aes(x = Education , fill = Personal.Loan)) + geom_histogram(stat =
                                                                                         "count") + theme(legend.position = "none")
g7 <-
  ggplot(universalbank_df, aes(x = Mortgage, fill = Personal.Loan)) + geom_histogram(binwidth = 5) + theme(legend.position =
                                                                                                             "none")
g8 <-
  ggplot(universalbank_df,
         aes(x = Securities.Account, fill = Personal.Loan)) + geom_histogram(stat =
                                                                               "count") + theme(legend.position = "none")
g9 <-
  ggplot(universalbank_df, aes(x = CD.Account, fill = Personal.Loan)) + geom_histogram(stat =
                                                                                         "count") + theme(legend.position = "none")
g10 <-
  ggplot(universalbank_df, aes(x = Online, fill = Personal.Loan)) + geom_histogram(stat =
                                                                                     "count") + theme(legend.position = "none")
g11 <-
  ggplot(universalbank_df, aes(x = CreditCard, fill = Personal.Loan)) + geom_histogram(stat =
                                                                                         "count") + theme(legend.position = "none")

grid.arrange(
  g1,
  g2,
  g3,
  g4,
  g5,
  g6,
  g7,
  g8,
  g9,
  g10,
  g11,
  nrow = 6,
  ncol = 2,
  widths = c(2, 2)
)

# create dummies from categorical variables
universalbank_df <-
  dummy_cols(
    universalbank_df,
    select_columns = c('Family', 'Education'),
    remove_first_dummy = T,
    remove_selected_columns = T
  )

universalbank_df <- universalbank_df %>%
  mutate(
    Family_2 = as.factor(Family_2),
    Family_3 = as.factor(Family_3),
    Family_4 = as.factor(Family_4),
    Education_2 = as.factor(Education_2),
    Education_3 = as.factor(Education_3)
  )

str(universalbank_df)
summary(universalbank_df)

# distribution of targer variable
ggplot(data = universalbank_df, aes(x = Personal.Loan, fill = Personal.Loan)) + geom_bar()

sum(universalbank_df$Personal.Loan == 1)
sum(universalbank_df$Personal.Loan == 0)

# correlation matrix
data <- as.matrix(select_if(universalbank_df, is.numeric))

res <- cor(data)
round(res, 2)

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(
  x = res,
  col = col,
  symm = TRUE,
  Rowv = NA
)

# drop multicolineared variable
universalbank_df <-
  subset(universalbank_df, select = -c(Experience))

### MODELING ###

set.seed(123)

universalbank_df <- universalbank_df %>%
  mutate(Personal.Loan = dplyr::recode(Personal.Loan, '1' = "Yes", '0' = "No"))

# spit dataset into train and test part in ratio 8:2 with respect to target variable
index_train <-
  createDataPartition(universalbank_df$Personal.Loan,
                      p = .8,
                      list = FALSE)
train <- universalbank_df[index_train,]
test  <- universalbank_df[-index_train,]

sum(train$Personal.Loan == 'Yes')
sum(train$Personal.Loan == 'No')

sum(test$Personal.Loan == 'Yes')
sum(test$Personal.Loan == 'No')

# Logistic Regression
library(stepPlr)

#training
fitControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T,
  savePredictions = T,
  search = 'grid'
)

plr_grid <- expand.grid(cp = c('aic', 'bic'),
                        lambda = c(0.001, 0.01, 0.1, 1, 10, 100))


plr_fit <- train(
  Personal.Loan ~ .,
  data = train,
  method = "plr",
  trControl = fitControl,
  preProc = c("center", "scale"),
  tuneGrid = plr_grid,
  metric = "ROC"
)

plr_fit

ggplot(plr_fit)

# prediction
plr_predicted_class <- predict(plr_fit, newdata = test)
str(plr_predicted_class)

plr_predicted_proba <-
  predict(plr_fit, newdata = test, type = "prob")
head(plr_predicted_proba)

# evaluation
confusionMatrix(data = plr_predicted_class, test$Personal.Loan)

## ROC curve
plr_predicted_proba$obs <- test$Personal.Loan
plr_predicted_proba

test1 <-
  evalm(
    plr_predicted_proba ,
    plots = 'r',
    rlinethick = 0.8,
    fsize = 8,
    bins = 8
  )

## Cumulative gain curve
cumu_chart <- function(Y_test, test_probs, k)
{
  comb_data = data.frame(test_probs, as.factor(Y_test), as.numeric(test_probs > 0.5))
  ordered_data = comb_data[order(-test_probs),]
  names(ordered_data) = c("test_probs", "Y_test", "Y_pred")
  
  #Probability cutoff vector
  cutoff.vector <- seq(0, 1, length = 1000)
  
  #Create empty vector for cumugains
  p_CumGain <- numeric(length(cutoff.vector))
  #Create an empty vector for proportion of data
  p_data <- numeric(length(cutoff.vector))
  
  for (i in 1:length(cutoff.vector))
  {
    p_CumGain[i] <-
      sum(ordered_data$test_probs > cutoff.vector[i] &
            ordered_data$Y_test == 1) / sum(ordered_data$Y_test == 1)
    p_data[i] <-
      (
        sum(
          ordered_data$test_probs > cutoff.vector[i] &
            ordered_data$Y_test == 1
        ) + sum(
          ordered_data$test_probs > cutoff.vector[i] &
            ordered_data$Y_test == 0
        )
      ) / 500
  }
  plot(
    p_data,
    p_CumGain,
    type = "l",
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = "Proportion of Targeted Records",
    ylab = "Proportion of Cumulative Gains",
    col = "blue",
    main = paste0("Cumulative Gains Chart ", k)
  )
  abline(0, 1, lty = 2, col = "red")
  
}

test_cgc <- test %>%
  mutate(Personal.Loan = dplyr::recode(Personal.Loan, 'Yes' = 1, 'No' = 0))

cumu_chart(test_cgc$Personal.Loan, plr_predicted_proba$Yes, "Logistic")

# Random Forest
set.seed(123)
library(randomForest)

#training
fitControl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = T,
  savePredictions = T,
  search = 'random'
)


rf_fit <- train(
  Personal.Loan ~ .,
  data = train,
  method = "rf",
  metric = 'Accuracy',
  trControl = fitControl,
  preProc = c("center", "scale"),
  tuneLength = 6
)

rf_fit

ggplot(rf_fit)

# prediction
rf_predicted_class <- predict(rf_fit, newdata = test)
str(rf_predicted_class)

rf_predicted_proba <-
  predict(rf_fit, newdata = test, type = "prob")
head(rf_predicted_proba)

# evaluation
confusionMatrix(data = rf_predicted_class, test$Personal.Loan)

## ROC curve
rf_predicted_proba$obs <- test$Personal.Loan

roc_curve_rf <-
  evalm(
    rf_predicted_proba ,
    plots = 'r',
    rlinethick = 0.8,
    fsize = 8,
    bins = 8
  )

cumu_chart(test_cgc$Personal.Loan, rf_predicted_proba$Yes, "Random Forest")
