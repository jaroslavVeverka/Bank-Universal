rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(dlookr)
library(tidyr)
library(reshape2)
library(gridExtra)

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

pokus <- 'pokus'





