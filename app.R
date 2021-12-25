library(shiny)
library(shinyWidgets)
library(plotly)
library(ANN2)
library(gbm)
library(mcclust)
library(rpart)
library(rpart.plot)

gen_model <- function(params) {
    return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}

setClass("tree", slots = list(left = "tree", right = "tree", value = "character")

id3 <- function(examples, attributes, parent_examples) {
    if empty(examples) {
        return(PLURALITY_VAL(parent_examples))
    } else if (same_class(examples)) {
        return(get_class(examples))
    } else if (empty(attributes)) {
        return(PLURALITY_VAL(examples))
    } else {
        A <- argmax(IMPORTANCE(a, examples))
        tree <- a new decision tree with root test A
        for (vk ∈ A) {
            exs <- {e|e ∈ examples ∧ e.A = vk}
            subtree <- DT-LEARNING(exs, attributes − A, examples)
            add a branch to tree with label (A = vk) and subtree subtree
        }
        return tree
    }
}



accuracy_tune <- function(fit) {
    predict_unseen <- predict(fit, data_test, type = "class")
    table_mat <- table(data_test$y, predict_unseen)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test
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
df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE)
params <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "age", "balance", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
relevant <- c("age", "balance", "marital", "loan", "duration", "previous", "poutcome")
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
ui <- fluidPage(
                titlePanel("WHY DOES HE NOT WANT MY LOAN D:"),
                fluidRow(
                         column(1,
                                checkboxInput("customize", "Customize"),
                                conditionalPanel(
                                                 condition = "input.customize == true",
                                                 checkboxGroupInput(
                                                                    "params",
                                                                    "Choose your relevant Parameters",
                                                                    choices = params,
                                                                    selected = params, # selected = relevant,
                                                                    inline = FALSE),
                                                 ),
                                ),
                         column(4,
                                fluidRow(
                                         #   1 - age (numeric)
                                         column(1,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('age')",
                                                                 noUiSliderInput(
                                                                                 inputId = "age",
                                                                                 label = "Age:",
                                                                                 update_on = "end",
                                                                                 value = 35,
                                                                                 min = 0,
                                                                                 orientation = "vertical",
                                                                                 max = 100,
                                                                                 step = 1,
                                                                                 width = "100%",
                                                                                 height = "200px"
                                                                                 ),
                                                                 ),
                                                ),
                                         #   6 - balance: average yearly balance, in euros (numeric)
                                         column(1,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('balance')",
                                                                 noUiSliderInput(
                                                                                 inputId = "balance",
                                                                                 label = "Balance:",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 update_on = "end",
                                                                                 height = "200px",
                                                                                 value = 35,
                                                                                 min = -20000,
                                                                                 max = 150000,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         #  10 - day: last contact day of the month (numeric)
                                         column(2,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('day')",
                                                                 noUiSliderInput(
                                                                                 inputId = "day",
                                                                                 label = "Last day of contact:",
                                                                                 update_on = "end",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 height = "200px",
                                                                                 value = 10,
                                                                                 min = 0,
                                                                                 max = 31,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         #  12 - duration: last contact duration, in seconds (numeric)
                                         column(2,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('duration')",
                                                                 noUiSliderInput(
                                                                                 inputId = "duration",
                                                                                 label = "Last contact duration in seconds:",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 height = "200px",
                                                                                 update_on = "end",
                                                                                 value = 35,
                                                                                 min = 0,
                                                                                 max = 10000,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         #  13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
                                         column(2,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('campaign')",
                                                                 noUiSliderInput(
                                                                                 inputId = "campaign",
                                                                                 label = "Number of contacts performed",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 height = "200px",
                                                                                 update_on = "end",
                                                                                 value = 35,
                                                                                 min = 0,
                                                                                 max = 100,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         #  14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
                                         column(2,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('pdays')",
                                                                 noUiSliderInput(
                                                                                 inputId = "pdays",
                                                                                 label = "Number of days since last contact (-1 for not contacted):",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 update_on = "end",
                                                                                 height = "200px",
                                                                                 value = 35,
                                                                                 min = -1,
                                                                                 max = 1000,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         #  15 - previous: number of contacts performed before this campaign and for this client (numeric)
                                         column(2,
                                                conditionalPanel(
                                                                 condition = "input.params.includes('previous')",
                                                                 noUiSliderInput(
                                                                                 inputId = "previous",
                                                                                 label = "Number of contacts performed:",
                                                                                 orientation = "vertical",
                                                                                 width = "100%",
                                                                                 height = "200px",
                                                                                 update_on = "end",
                                                                                 value = 35,
                                                                                 min = 0,
                                                                                 max = 300,
                                                                                 step = 1
                                                                                 ),
                                                                 ),
                                                ),
                                         ),
                                ),
                         column(4,
                                fluidRow(
                                         column(6,
                                                #   2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                                #                                       "blue-collar","self-employed","retired","technician","services") 
                                                conditionalPanel(
                                                                 condition = "input.params.includes('job')",
                                                                 selectInput("job", label = "What type of job you have:",
                                                                             choices = list(
                                                                                            "Admin" = "admin.",
                                                                                            "Unknown" = "unknown",
                                                                                            "Unemployed" = "unemployed",
                                                                                            "Management" = "management",
                                                                                            "Housemaid" = "housemaid",
                                                                                            "Entrepreneur" = "entrepreneur",
                                                                                            "Student" = "student",
                                                                                            "Blue-collar" = "blue-collar",
                                                                                            "Self-employed" = "self-employed",
                                                                                            "Retired" = "retired",
                                                                                            "Technician" = "technician",
                                                                                            "Services" = "services"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                #   3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
                                                conditionalPanel(
                                                                 condition = "input.params.includes('marital')",
                                                                 selectInput("marital", label = "What is your marital status:",
                                                                             choices = list(
                                                                                            "Married" = "married",
                                                                                            "Divorced" = "divorced",
                                                                                            "Single" = "single"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                #   4 - education (categorical: "unknown","secondary","primary","tertiary")
                                                conditionalPanel(
                                                                 condition = "input.params.includes('education')",
                                                                 selectInput("education", label = "What is your education:",
                                                                             choices = list(
                                                                                            "Unknown" = "unknown",
                                                                                            "Secondary" = "secondary",
                                                                                            "Primary" = "primary",
                                                                                            "Tertiary" = "tertiary"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                ),
                                         column(6,
                                                #   9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
                                                conditionalPanel(
                                                                 condition = "input.params.includes('contact')",
                                                                 selectInput("contact", label = "What was the communication type?",
                                                                             choices = list(
                                                                                            "Unknown" = "unknown",
                                                                                            "Telephone" = "telephone",
                                                                                            "Cellular" = "cellular"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                #  11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
                                                conditionalPanel(
                                                                 condition = "input.params.includes('month')",
                                                                 selectInput("month", label = "During what month was the last contact?",
                                                                             choices = list(
                                                                                            "January" = "jan",
                                                                                            "February" = "feb",
                                                                                            "March" = "mar",
                                                                                            "April" = "apr",
                                                                                            "May" = "may",
                                                                                            "June" = "jun",
                                                                                            "July" = "jul",
                                                                                            "August" = "aug",
                                                                                            "September" = "sep",
                                                                                            "October" = "oct",
                                                                                            "November" = "nov",
                                                                                            "December" = "dec"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                #  16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
                                                conditionalPanel(
                                                                 condition = "input.params.includes('poutcome')",
                                                                 selectInput("poutcome", label = "Outcome of your previous marketing campaign:",
                                                                             choices = list(
                                                                                            "Unknown" = "unknown",
                                                                                            "Other" = "other",
                                                                                            "Failure" = "failure",
                                                                                            "Success" = "success"
                                                                                            ),
                                                                             selected = 1
                                                                             ),
                                                                 ),
                                                ),
                                         ),
                                ),
                         column(3,
                                fluidRow(
                                         #   5 - default: has credit in default? (binary: "yes","no")
                                         conditionalPanel(
                                                          condition = "input.params.includes('default')",
                                                          checkboxInput("default", "Do you have default credit?"),
                                                          ),
                                         #   7 - housing: has housing loan? (binary: "yes","no")
                                         conditionalPanel(
                                                          condition = "input.params.includes('housing')",
                                                          checkboxInput("housing", "Do you have a housing loan?"),
                                                          ),
                                         #   8 - loan: has personal loan? (binary: "yes","no")
                                         conditionalPanel(
                                                          condition = "input.params.includes('loan')",
                                                          checkboxInput("loan", "Do you have a personal loan?"),
                                                          ),
                                         ),
                                ),
                         ),
                fluidRow(
                         plotOutput(outputId = "Verteilung"),
                         textOutput("Prognose"),
                )
)
server <- function(input, output) {
    model <- reactive({})
    prognose <- reactive({
        XPred <- df[, input$params]
        for (el in input$params) {
            if (el %in% classes) {
                if (typeof(reactiveValuesToList(input)[[el]]) == "logical") {
                    XPred[1, el] <- as.factor(ifelse(reactiveValuesToList(input)[[el]] == FALSE, "no", "yes"))
                } else {
                    XPred[1, el] <- as.factor(reactiveValuesToList(input)[[el]])
                }
            }
            if (el %in% numeric) {
                XPred[1, el] <- as.numeric(reactiveValuesToList(input)[[el]])
            }
        }
        X.neu <- model.matrix(gen_model(input$params), XPred)
        X.neu <- X.neu[, -1]   # entferne den Intercept
        prognosevektor <- predict(model, X.neu)$predictions
        prog <- prognosevektor[1]
        prog
    })
    output$Verteilung <- renderPlot({
        plot(model)
        # prog <- prognose()
        # X <- df[, input$params]
        # X <- model.matrix(gen_model(input$params), X)
        # X <- X[, -1]
        # y <- as.factor(df[, "y"])
        # abweichungen <- y - predict(model, X)$predictions
        # hist(prog + abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten")
    })
    output$Prognose <- renderText({
        prog <- prognose()
        Ausgabe <- paste("Your score: ", prog)
    })
}
shinyApp(ui, server)
