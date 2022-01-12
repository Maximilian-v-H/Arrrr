#!/usr/bin/env Rscript
library(methods)
library(data.tree)

df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE)
params <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "age", "balance", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
relevant <- c("age", "balance", "marital", "loan", "duration", "previous", "poutcome")

gen_model <- function(params) {
    return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}

is_pure <- function(data) {
    length(unique(data[, ncol(data)])) == 1
}

entropy <- function(vlss) {
    res <- vlss / sum(vlss) * log2(vlss / sum(vlss))
    res[vlss == 0] <- 0
    - sum(res)
}

information_gain <- function(t) {
    t <- as.data.frame.matrix(t)
    entropy_before <- entropy(colSums(t))
    s <- rowSums(t)
    entropy_after <- sum(s / sum(s) * apply(t, MARGIN = 1, FUN = entropy))
    information_gain <- entropy_before - entropy_after
    return(information_gain)
}

train_id3 <- function(node, data) {
    node$obsCount <- nrow(data)
    if (is_pure(data)) {
        child <- node$AddChild(unique(data[, ncol(data)]))
        node$feature <- tail(names(data), 1)
        child$obsCount <- nrow(data)
        child$feature <- ""
    } else {
        ig <- sapply(colnames(data)[-ncol(data)],
                     function(x) information_gain(table(data[, x], data[, ncol(data)]))
        )
        feature <- names(ig)# [ig == max(ig)][1]
        node$feature <- feature
        child_obs <- split(data[, !(names(data) %in% feature)], data[, feature], drop = TRUE)
        for (i in 1:length(child_obs)) {
            child <- node$AddChild(names(child_obs)[i])
            train_id3(child, child_obs[[i]])
        }
    }
}

predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return(predict(child, features))
}

accuracy_tune <- function(fit) {
    predict_unseen <- predict(fit, data_test, type = "class")
    table_mat <- table(data_test$y, predict_unseen)
    accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_test
}
create_train_test <- function(data, size = 0.8, train = TRUE) {
    n_row <- nrow(data)
    total_row <- size * n_row
    train_sample <- 1:total_row
    if (train == TRUE) {
        return(data[train_sample, ])
    } else {
        return(data[-train_sample, ])
    }
}
for (c in classes) {
    df[, c] <- as.factor(df[, c])
}
for (n in numeric) {
    df[, n] <- as.numeric(df[, n])
}
data_train <- create_train_test(df, 0.8, train = TRUE)
data_test <- create_train_test(df, 0.8, train = FALSE)
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(y~., data = data_train, method = "class", control = control)
accuracy_tune(tune_fit)
control <- rpart.control(
                         minsplit = 6,
                         minbucket = round(5 / 3),
                         maxdepth = 9,
                         cp = 0.01
)
tune_fit <- rpart(y~., data = data_train, method = "class", control = control)
accuracy_tune(tune_fit)
rpart.plot(tune_fit, extra = 106)

for (c in classes) {
    if (c %in% relevant) {
        df[, c] <- as.factor(df[, c])
    }
}
for (n in numeric) {
    if (n %in% relevant) {
        df[, n] <- as.numeric(df[, n])
    }
}
nn <- function() {
    X <- model.matrix(gen_model(relevant),
                      c(df[relevant], df["y"])
    )
    X <- X[, -1]   # entferne den Intercept
    y <- as.factor(df[, "y"])
    model <- neuralnetwork(X, y, hidden.layers = c(4, 3), regression = FALSE,
                           loss.type = "log", learn.rates = 1e-04, n.epochs = 1,
                           verbose = TRUE)
}



library(shiny)

runApp("app.R", launch.browser = FALSE)
