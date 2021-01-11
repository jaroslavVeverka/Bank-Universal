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

universalbank_df <- read.csv('UniversalBank.csv', na = c('', 'NA', '?'), header = TRUE, encoding = 'UTF-8')
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
  mutate(Personal.Loan = as.factor(Personal.Loan),
         Securities.Account = as.factor(Securities.Account),
         CD.Account = as.factor(CD.Account),
         Online = as.factor(Online),
         CreditCard = as.factor(CreditCard))

str(universalbank_df)
summary(universalbank_df)

# missing values
sum(is.na(universalbank_df))
sapply(universalbank_df, function(x) sum(is.na(x)))

# remove unused variables
universalbank_df <- universalbank_df %>%
  select(Age, Experience, Income, Family, CCAvg, Education, Mortgage,
         Personal.Loan, Securities.Account, CD.Account, Online, CreditCard)

# histograms
g1 <- ggplot(universalbank_df, aes(x = Age, fill = Personal.Loan)) + geom_histogram(binwidth=5)+ theme(legend.position="none")
g2 <- ggplot(universalbank_df, aes(x = Experience, fill = Personal.Loan)) + geom_histogram(binwidth = 5)+ theme(legend.position="none")
g3 <- ggplot(universalbank_df, aes(x = Income, fill = Personal.Loan)) + geom_histogram(binwidth = 5)+ theme(legend.position="none")
g4 <- ggplot(universalbank_df, aes(x = Family, fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")
g5 <- ggplot(universalbank_df, aes(x = CCAvg, fill = Personal.Loan)) + geom_histogram(binwidth = 5)+ theme(legend.position="none")
g6 <- ggplot(universalbank_df, aes(x = Education , fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")
g7 <- ggplot(universalbank_df, aes(x = Mortgage, fill = Personal.Loan)) + geom_histogram(binwidth = 5)+ theme(legend.position="none")
g8 <- ggplot(universalbank_df, aes(x = Securities.Account, fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")
g9 <- ggplot(universalbank_df, aes(x = CD.Account, fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")
g10 <- ggplot(universalbank_df, aes(x = Online, fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")
g11 <- ggplot(universalbank_df, aes(x = CreditCard, fill = Personal.Loan)) + geom_histogram(stat="count")+ theme(legend.position="none")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, nrow = 6, ncol=2, widths = c(2,2))

# create dummies from categorical variables
universalbank_df <- dummy_cols(universalbank_df, select_columns = c('Family', 'Education'), 
                               remove_first_dummy = T,
                               remove_selected_columns = T)

universalbank_df <- universalbank_df %>%
  mutate(Family_2 = as.factor(Family_2),
         Family_3 = as.factor(Family_3),
         Family_4 = as.factor(Family_4),
         Education_2 = as.factor(Education_2),
         Education_3 = as.factor(Education_3))

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

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE, Rowv = NA)

# drop multicolineared variable
universalbank_df <- subset(universalbank_df, select = -c(Experience))

### MODELING ###

set.seed(123)

universalbank_df <- universalbank_df %>%
  mutate(Personal.Loan = dplyr::recode(Personal.Loan, '1' = "Yes", '0' = "No"))
  
  # spit dataset into train and test part in ratio 8:2 with respect to target variable
index_train <- createDataPartition(universalbank_df$Personal.Loan, p = .8, list = FALSE)
train <- universalbank_df[ index_train,]
test  <- universalbank_df[-index_train,]

sum(train$Personal.Loan == 1)
sum(train$Personal.Loan == 0)

sum(test$Personal.Loan == 1)
sum(test$Personal.Loan == 0)


library(stepPlr)

fitControl <- trainControl(method = "cv",
                          # repeats = 3,
                           number = 5,
                           summaryFunction = twoClassSummary, 
                           classProbs = T,
                           savePredictions = T)

plr_grid <- expand.grid(
  cp = c("aic", "bic"),
  lambda = c(1e+02, 1e+01, 0e+00, 1e-01, 1e-02, 1e-03)
)

plr_fit <- train(Personal.Loan ~ ., data = train, 
                 method = "plr", 
                 trControl = fitControl,
                 tuneGrid = plr_grid,
                 preProc = c("scale")
                 )
plr_fit

ggplot(plr_fit)

plr_predicted_class <- predict(plr_fit, newdata = test)
str(plr_predicted_class)

plr_predicted_proba <- predict(plr_fit, newdata = test, type = "prob")
head(plr_predicted_proba)

confusionMatrix(data = plr_predicted_class, test$Personal.Loan)

library(MLeval)

plr_predicted_proba$obs <- test$Personal.Loan
plr_predicted_proba

test1 <- evalm(plr_predicted_proba ,plots='r',rlinethick=0.8,fsize=8,bins=8)



