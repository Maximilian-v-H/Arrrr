library(shiny)
library(shinyWidgets)
library(plotly)
library(ANN2)
library(gbm)
library(mcclust)
library(rpart)
library(rpart.plot)
library(hash)


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
print(summary(df))

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

gen_model <- function(params) {
  return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}

controlledSliderUI <- function(id){
  ns = NS(id)
  conditionalPanel(
    condition = "input.params.includes('balance')",
    sliderInput(ns("balance"), "Kontostand in $:", min = -20000, max = 150000, value = cParams[["balance"]]),
    numericInput(ns("value"), NULL, min = -20000, max = 150000, value = cParams[["balance"]]),
  )
}

controlledSlider <- function(input, output, session, value){
  reactiveRange <- reactiveValues(value = value[1])
  updateSliderInput(session, "balance", value = value)
  
  ## observe slider
  observeEvent(input$balance,{
    reactiveRange$value <- input$balance[1]
  }, ignoreInit = TRUE)
  
  ## oberserve numericInput
  observeEvent(input$value,{
    reactiveRange$value <- as.numeric(input$value)})
  
  ## observe reactive
  observeEvent({reactiveRange$value},{
    updateSliderInput(
      session, "balance", value = reactiveRange$value)
    updateNumericInput(session, "value", value = reactiveRange$value)
  })
  
  return(reactiveRange)
}

# Vorgefertigte Kunden / Standard Parameter
cParams <-hash()
# default paramter
cParams[["age"]] <- 40
cParams[["marital"]] <- "married"
cParams[["job"]] <- "unknown"
cParams[["education"]] <- "unkown"
cParams[["default"]] <- FALSE
cParams[["housing"]] <- FALSE
cParams[["loan"]] <- FALSE
cParams[["balance"]] <- 1000
cParams[["day"]] <- 1 
cParams[["month"]] <- "jan"
cParams[["pdays"]] <- 100
cParams[["duration"]] <- 200
cParams[["campaign"]] <- 2
cParams[["previous"]] <- 1
cParams[["contact"]] <- "unknown"
cParams[["poutcome"]] <- "unknown"

changeCustomer <- function(input, session){
  ns = NS("balance")
  #observe customer
  observeEvent(input$customer, {
    
    #customer 0 / default customer
    if (input$customer == "customer0"){
      updateNumericInput(session, "age", value = 40)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "unknown")
      updateSelectInput(session, "education", selected = "unknown")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = FALSE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 1000)
      updateNumericInput(session, ns("value"), value = 1000)
      updateSelectInput(session, "day", selected = 1)
      updateSelectInput(session, "month", selected = "jan")
      updateNumericInput(session, "pdays", value = 100)
      updateNumericInput(session, "duration", value = 200)
      updateSliderInput(session, "campaign", value = 2)
      updateSliderInput(session, "previous", value = 1)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "unknown")
    }
    
    #customer 1
    if (input$customer == "customer1"){
    updateNumericInput(session, "age", value = 66)
    updateSelectInput(session, "marital", selected = "single")
    updateSelectInput(session, "job", selected = "services")
    updateSelectInput(session, "education", selected = "primary")
    updateCheckboxInput(session, "default", value = FALSE)
    updateCheckboxInput(session, "housing", value = FALSE)
    updateCheckboxInput(session, "loan", value = FALSE)
    updateSliderInput(session, ns("balance"), value = 10000)
    updateNumericInput(session, ns("value"), value = 10000)
    updateSelectInput(session, "day", selected = 15)
    updateSelectInput(session, "month", selected = "may")
    updateNumericInput(session, "pdays", value = 303)
    updateNumericInput(session, "duration", value = 577)
    updateSliderInput(session, "campaign", value = 2)
    updateSliderInput(session, "previous", value = 4)
    updateSelectInput(session, "contact", selected = "cellular")
    updateSelectInput(session, "poutcome", selected = "success")
    }
    
    #customer 2
    if (input$customer == "customer2"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = -1)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 3
    if (input$customer == "customer3"){
      updateNumericInput(session, "age", value = 39)
      updateSelectInput(session, "marital", selected = "divorced")
      updateSelectInput(session, "job", selected = "management")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = TRUE)
      updateSliderInput(session, ns("balance"), value = 70563)
      updateNumericInput(session, ns("value"), value = 70563)
      updateSelectInput(session, "day", selected = 21)
      updateSelectInput(session, "month", selected = "jun")
      updateNumericInput(session, "pdays", value = 351)
      updateNumericInput(session, "duration", value = 2431)
      updateSliderInput(session, "campaign", value = 3)
      updateSliderInput(session, "previous", value = 11)
      updateSelectInput(session, "contact", selected = "telephone")
      updateSelectInput(session, "poutcome", selected = "success")
    }
    
    #customer 4
    if (input$customer == "customer4"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 5
    if (input$customer == "customer5"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 6
    if (input$customer == "customer6"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 7
    if (input$customer == "customer7"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 8
    if (input$customer == "customer8"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 9
    if (input$customer == "customer9"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 10
    if (input$customer == "customer10"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 11
    if (input$customer == "customer11"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 12
    if (input$customer == "customer12"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 13
    if (input$customer == "customer13"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 14
    if (input$customer == "customer14"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 15
    if (input$customer == "customer15"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 16
    if (input$customer == "customer16"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 17
    if (input$customer == "customer17"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 18
    if (input$customer == "customer18"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 19
    if (input$customer == "customer19"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    
    #customer 20
    if (input$customer == "customer20"){
      updateNumericInput(session, "age", value = 35)
      updateSelectInput(session, "marital", selected = "married")
      updateSelectInput(session, "job", selected = "admin")
      updateSelectInput(session, "education", selected = "tertiary")
      updateCheckboxInput(session, "default", value = FALSE)
      updateCheckboxInput(session, "housing", value = TRUE)
      updateCheckboxInput(session, "loan", value = FALSE)
      updateSliderInput(session, ns("balance"), value = 30000)
      updateNumericInput(session, ns("value"), value = 30000)
      updateSelectInput(session, "day", selected = 13)
      updateSelectInput(session, "month", selected = "apr")
      updateNumericInput(session, "pdays", value = 31)
      updateNumericInput(session, "duration", value = 0)
      updateSliderInput(session, "campaign", value = 0)
      updateSliderInput(session, "previous", value = 0)
      updateSelectInput(session, "contact", selected = "unknown")
      updateSelectInput(session, "poutcome", selected = "other")
    }
    })
}




ui <- fluidPage(
                titlePanel("Verkaufskampagne Darlehen"),
                fluidRow(
                         column(1,
                                h4(strong("Bestandskunden"), align="center"),
                                  selectInput("customer", label = "Bestandskunden:", selectize = FALSE, size = "10", width = "400px",
                                              choices = list(
                                                "Standard" = "customer0",
                                                "Kunde 1" = "customer1",
                                                "Kunde 2" = "customer2",
                                                "Kunde 3" = "customer3",
                                                "Kunde 4" = "customer4",
                                                "Kunde 5" = "customer5",
                                                "Kunde 6" = "customer6",
                                                "Kunde 7" = "customer7",
                                                "Kunde 8" = "customer8",
                                                "Kunde 9" = "customer9",
                                                "Kunde 10" = "customer10",
                                                "Kunde 11" = "customer11",
                                                "Kunde 12" = "customer12",
                                                "Kunde 13" = "customer13",
                                                "Kunde 14" = "customer14",
                                                "Kunde 15" = "customer15",
                                                "Kunde 16" = "customer16",
                                                "Kunde 17" = "customer17",
                                                "Kunde 18" = "customer18",
                                                "Kunde 19" = "customer19",
                                                "Kunde 20" = "customer20"
                                              ),
                                              selected = "customer0"
                                  ),

                         ),
                         column(2, offset = 1,
                                h4(strong("Persoenliche Informationen"), align="center"),
                                         #   1 - age (numeric)
                                                conditionalPanel(
                                                                 condition = "input.params.includes('age')",
                                                                 numericInput(
                                                                   inputId = "age", 
                                                                   label = "Alter:",
                                                                   value = cParams[["age"]],
                                                                   min = 18,
                                                                   max = 100,
                                                                   step = 1
                                                                   ),
                                                         ),
                                #   2 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
                                conditionalPanel(
                                  condition = "input.params.includes('marital')",
                                  selectInput("marital", label = "Ehestatus:",
                                              choices = list(
                                                "Verheiratet" = "married",
                                                "Geschieden" = "divorced",
                                                "Ledig" = "single"
                                              ),
                                              selected = cParams[["marital"]]
                                  ),
                                ),
                                
                                #   3 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                #                                       "blue-collar","self-employed","retired","technician","services")
                                conditionalPanel(
                                  condition = "input.params.includes('job')",
                                  selectInput("job", label = "Beruf:",
                                              choices = list(
                                                "Admin" = "admin",
                                                "Unbekannt" = "unknown",
                                                "Arbeitslos" = "unemployed",
                                                "Management" = "management",
                                                "Hauswirtschaftshelfer" = "housemaid",
                                                "Unternehmer" = "entrepreneur",
                                                "Student" = "student",
                                                "Handwerker" = "blue-collar",
                                                "Selbststaendig" = "self-employed",
                                                "Rentner" = "retired",
                                                "Techniker" = "technician",
                                                "Services" = "services"
                                              ),
                                              selected = cParams[["job"]]
                                  ),
                                ),
                                
                                #   4 - education (categorical: "unknown","secondary","primary","tertiary")
                                conditionalPanel(
                                  condition = "input.params.includes('education')",
                                  selectInput("education", label = "Bildung:",
                                              choices = list(
                                                "Unbekannt" = "unknown",
                                                "Abitur" = "secondary",
                                                "Regelschulabschluss" = "primary",
                                                "Abgeschlossenes Studium" = "tertiary"
                                              ),
                                              selected = cParams[["education"]]
                                  ),
                                ),
                         ),
                          column(2, h4(strong("Finanzielle Informationen"), align="center"),
                                 #   5 - default: has credit in default? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('default')",
                                   checkboxInput("default", "Gibt es ein Verzugskredit?", cParams[["default"]]),
                                 ),
                                 #   7 - housing: has housing loan? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('housing')",
                                   checkboxInput("housing", "Gibt es ein Baudarlehnen?", cParams[["housing"]]),
                                 ),
                                 #   8 - loan: has personal loan? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('loan')",
                                   checkboxInput("loan", "Besteht ein privat Kredit?", cParams[["loan"]]),
                                 ),
                  controlledSliderUI("balance")
                            ),
                  column(2,  h4(strong("Informationen zum letzten Kontakt innerhalb der aktuellen Kampagne"), align="center"), h5(strong("Letzter Kontakt Versuch war am")),
                         #  9 - day: last contact day of the month (numeric) current campaign
                         conditionalPanel(
                           condition = "input.params.includes('day')",
                           selectInput(
                             inputId = "day", 
                             label="Tag des Monats:", 
                             choices = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                             selected = cParams[["day"]]
                           )
                         ),
                         
                         #  10 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec") current campaign
                         conditionalPanel(
                           condition = "input.params.includes('month')",
                           selectInput("month", label = "Monat:",
                                       choices = list(
                                         "Januar" = "jan",
                                         "Februar" = "feb",
                                         "Maerz" = "mar",
                                         "April" = "apr",
                                         "Mai" = "may",
                                         "Juni" = "jun",
                                         "Juli" = "jul",
                                         "August" = "aug",
                                         "September" = "sep",
                                         "Oktober" = "oct",
                                         "November" = "nov",
                                         "Dezember" = "dec"
                                       ),
                                       selected = cParams[["month"]]
                           ),
                         ),
                         
                         #  11 - duration: last contact duration, in seconds (numeric) current campaign
                         conditionalPanel(
                           condition = "input.params.includes('duration')",
                           numericInput(inputId = "duration", label = "Gespraechsdauer des letzten Kontakt in Sekunden):", min = 0, max = 5000, value = cParams[["duration"]]),
                         ),
                         
                         
                         #   12 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
                         conditionalPanel(
                           condition = "input.params.includes('contact')",
                           selectInput("contact", label = "Wie wurde der Kontakt aufgenommen?",
                                       choices = list(
                                         "Unbekannt" = "unknown",
                                         "Festnetz" = "telephone",
                                         "Handy" = "cellular"
                                       ),
                                       selected = cParams[["contact"]]
                           ),
                         ),
                        ),
                  column(2 , h4(strong("Weitere Informationen"), align="center"),
                         
                         #  13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
                         conditionalPanel(
                           condition = "input.params.includes('pdays')",
                           numericInput(inputId = "pdays", label = "Tage seit letztem Kontakt der vorherigen Kampagne (-1 fuer kein Kontakt):", min = -1, max = 1000, value = cParams[["pdays"]])
                         ),
                         
                         #  14 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
                         conditionalPanel(
                           condition = "input.params.includes('campaign')",
                           sliderInput("campaign", "Anzahl der Kontakte in der aktuellen Kampagne", min = 0, max = 100, value = cParams[["campaign"]]),
                         ),
                         
                         #  15 - previous: number of contacts performed before this campaign and for this client (numeric)
                         conditionalPanel(
                           condition = "input.params.includes('previous')",
                           sliderInput("previous", "Anzahl der Kontakte insgesamt", min = 0, max = 100, value = cParams[["previous"]]),
                         ),
                         
                         
                         #  16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
                         conditionalPanel(
                           condition = "input.params.includes('poutcome')",
                           selectInput("poutcome", label = "Ergebnis der letzten Kampagne:",
                                       choices = list(
                                         "Unbekannt" = "unknown",
                                         "Anderes" = "other",
                                         "Nicht erfolgreich" = "failure",
                                         "Erfolgreich" = "success"
                                       ),
                                       selected = cParams[["poutcome"]]
                           ),
                         ),
                  ),
                ),
                fluidRow(
                  titlePanel("Ausgabe"),
                  textOutput("Prognose")
                )
  )
server <- function(input, output, session) {
 #input_test <- "customer5"
 #observeEvent(input$customer,{
 #  input_test$customer <- input$customer
 #  print(input_test)
 #})
 #
  #observeEvent(input$value,{
  #  reactiveRange$value <- as.numeric(input$value)})
  
  
  range <- callModule(controlledSlider, "balance", 1000)
  changeCustomer(input, session)
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
