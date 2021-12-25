#!/usr/bin/env Rscript
library(methods)
library(data.tree)

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

train_id3 <- function(node, data, poly = TRUE) {
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
        if (poly) {
            features <- names(ig)
        } else {
            features <- names(ig)[ig == max(ig)][1]
        }
        for (feature in features) {
            node$feature <- feature
            child_obs <- split(data[, !(names(data) %in% feature)], data[, feature], drop = TRUE)
            for (i in 1:length(child_obs)) {
                child <- node$AddChild(names(child_obs)[i])
                train_id3(child, child_obs[[i]], poly)
            }
        }
    }
}

predict_tree <- function(tree, features) {
    if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
    child <- tree$children[[features[[tree$feature]]]]
    return(predict_tree(child, features))
}

accuracy_tune <- function(fit, data_test) {
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

neural_net <- function(
                       data,
                       x_rel,
                       y_rel,
                       hiddenlayers = c(4, 3),
                       regression = FALSE,
                       losstype = "log",
                       learnrates = 1e-04,
                       nepochs = 1,
                       verbose = TRUE
                       ) {
    x <- model.matrix(
                      gen_model(x_rel),
                      c(data[x_rel], data[y_rel])
    )
    x <- x[, -1]   # entferne den Intercept
    y <- as.factor(data[, y_rel])
    model <- neuralnetwork(
                           x,
                           y,
                           hidden.layers = hiddenlayers,
                           regression = regression,
                           loss.type = losstype,
                           learn.rates = learnrates,
                           n.epochs = nepochs,
                           verbose = verbose
    )
    return(model)
}
