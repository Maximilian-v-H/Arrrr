#!/usr/bin/env Rscript
source("utils.R")
library(shiny)
library("rpart")

# Load data
df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE)

# Set default parameters for structuring data
params <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "age", "balance", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
relevant <- c("age", "balance", "marital", "loan", "duration", "previous", "poutcome")

# Preprocess the data into categorical and numeric values
for (c in classes) {
    df[, c] <- as.factor(df[, c])
}

for (n in numeric) {
    df[, n] <- as.numeric(df[, n])
}

# Decision Tree with rpart
dt_rpart <- function(
                     data,
                     minsplit = 4,
                     minbucket = round(5 / 3),
                     maxdepth = 3,
                     splitrate = 0.8
                     ) {
    data_train <- create_train_test(data, splitrate, train = TRUE)
    data_test <- create_train_test(data, splitrate, train = FALSE)
    control <- rpart.control(
                             minsplit = minsplit,
                             minbucket = minbucket,
                             maxdepth = maxdepth
    )
    tune_fit <- rpart(y~., data = data_train, method = "class", control = control)
    print(accuracy_tune(tune_fit, data_test))
    # rpart.plot(tune_fit, extra = 106)
    return(list(tune_fit, data_train, data_test))
}

random_forest_adapt <- function(data) {

}

rpart_dt <- dt_rpart(df)
# Shiny App
# runApp("app.R", launch.browser = FALSE)
