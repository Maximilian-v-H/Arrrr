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
users <- read.csv("./users.csv", header = TRUE, sep = ";", fill = TRUE)
rownames(users) <- c(users[["name"]])
users <- subset(users, select = -c(name))
params <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "age", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")

for (c in classes) {
    df[, c] <- as.factor(df[, c])
}
for (n in numeric) {
    df[, n] <- as.numeric(df[, n])
}

gen_model <- function(params) {
    return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}


controlledSliderUI <- function(id) {
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
cParams <- hash()
# default paramter
cParams[["age"]] <- 40
cParams[["marital"]] <- "married"
cParams[["job"]] <- "unknown"
cParams[["education"]] <- "unknown"
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
                     if (input$customer == "Kein Kunde"){
                         updateNumericInput(session, "age", value = cParams[["age"]])
                         updateSelectInput(session, "marital", selected = cParams[["marital"]])
                         updateSelectInput(session, "job", selected = cParams[["job"]])
                         updateSelectInput(session, "education", selected = cParams[["education"]])
                         updateCheckboxInput(session, "default", value = cParams[["default"]])
                         updateCheckboxInput(session, "housing", value = cParams[["housing"]])
                         updateCheckboxInput(session, "loan", value = cParams[["loan"]])
                         updateSliderInput(session, ns("balance"), value = cParams[["balance"]])
                         updateNumericInput(session, ns("value"), value = cParams[["value"]])
                         updateSelectInput(session, "day", selected = cParams[["day"]])
                         updateSelectInput(session, "month", selected = cParams[["month"]])
                         updateNumericInput(session, "pdays", value = cParams[["pdays"]])
                         updateNumericInput(session, "duration", value = cParams[["duration"]])
                         updateSliderInput(session, "campaign", value = cParams[["campaign"]])
                         updateSliderInput(session, "previous", value = cParams[["previous"]])
                         updateSelectInput(session, "contact", selected = cParams[["contact"]])
                         updateSelectInput(session, "poutcome", selected = cParams[["poutcome"]])
                     } else {
                         updateNumericInput(session, "age", value = users[input$customer, "age"])
                         updateSelectInput(session, "marital", selected = users[input$customer, "marital"])
                         updateSelectInput(session, "job", selected = users[input$customer, "job"])
                         updateSelectInput(session, "education", selected = users[input$customer, "education"])
                         updateCheckboxInput(session, "default", value = users[input$customer, "default"])
                         updateCheckboxInput(session, "housing", value = users[input$customer, "housing"])
                         updateCheckboxInput(session, "loan", value = users[input$customer, "loan"])
                         updateSliderInput(session, ns("balance"), value = users[input$customer, "balance"])
                         updateNumericInput(session, ns("value"), value = users[input$customer, "value"])
                         updateSelectInput(session, "day", selected = users[input$customer, "day"])
                         updateSelectInput(session, "month", selected = users[input$customer, "month"])
                         updateNumericInput(session, "pdays", value = users[input$customer, "pdays"])
                         updateNumericInput(session, "duration", value = users[input$customer, "duration"])
                         updateSliderInput(session, "campaign", value = users[input$customer, "campaign"])
                         updateSliderInput(session, "previous", value = users[input$customer, "previous"])
                         updateSelectInput(session, "contact", selected = users[input$customer, "contact"])
                         updateSelectInput(session, "poutcome", selected = users[input$customer, "poutcome"])
                     }
                     })
}

ui <- fluidPage(
                titlePanel("Verkaufskampagne Darlehen"),
                fluidRow(
                         column(1,
                                h4(strong("Bestandskunden"), align="center"),
                                selectInput("customer", label = "Bestandskunden:", selectize = FALSE, size = "10", width = "400px",
                                            choices = c(list("Kein Kunde"), rownames(users)),
                                            selected = "Kein Kunde"
                                            ),

                                ),
                         column(2, offset = 1,
                                h4(strong("Persönliche Informationen"), align="center"),
                                #   1 - age (numeric)
                                numericInput(
                                             inputId = "age", 
                                             label = "Alter:",
                                             value = cParams[["age"]],
                                             min = 18,
                                             max = 100,
                                             step = 1
                                             ),
                                #   2 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
                                selectInput("marital", label = "Ehestatus:",
                                            choices = list(
                                                           "Verheiratet" = "married",
                                                           "Geschieden" = "divorced",
                                                           "Ledig" = "single"
                                                           ),
                                            selected = cParams[["marital"]]
                                            ),

                                #   3 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                #                                       "blue-collar","self-employed","retired","technician","services")
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
                                                           "Selbstständig" = "self-employed",
                                                           "Rentner" = "retired",
                                                           "Techniker" = "technician",
                                                           "Services" = "services"
                                                           ),
                                            selected = cParams[["job"]]
                                            ),

                                #   4 - education (categorical: "unknown","secondary","primary","tertiary")
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
                         column(2,
                                h4(strong("Finanzielle Informationen"), align="center"),
                                #   5 - default: has credit in default? (binary: "yes","no")
                                checkboxInput("default", "Gibt es ein Verzugskredit?", cParams[["default"]]),
                                #   7 - housing: has housing loan? (binary: "yes","no")
                                checkboxInput("housing", "Gibt es ein Baudarlehnen?", cParams[["housing"]]),
                                #   8 - loan: has personal loan? (binary: "yes","no")
                                checkboxInput("loan", "Besteht ein privat Kredit?", cParams[["loan"]])
                                ,controlledSliderUI("balance")
                                ),
                         column(2,
                                h4(strong("Informationen zum letzten Kontakt innerhalb der aktuellen Kampagne"), align="center"),
                                h5(strong("Letzter Kontakt Versuch war am")),
                                #  9 - day: last contact day of the month (numeric) current campaign
                                selectInput(
                                            inputId = "day", 
                                            label="Tag des Monats:", 
                                            choices = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                                            selected = cParams[["day"]]
                                            ),

                                #  10 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec") current campaign
                                selectInput("month", label = "Monat:",
                                            choices = list(
                                                           "Januar" = "jan",
                                                           "Februar" = "feb",
                                                           "März" = "mar",
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

                                #  11 - duration: last contact duration, in seconds (numeric) current campaign
                                numericInput(
                                             inputId = "duration", 
                                             label = "Gesprächsdauer des letzten Kontakt in Sekunden):", 
                                             min = 0, 
                                             max = 5000, 
                                             value = cParams[["duration"]]),


                                #   12 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
                                selectInput("contact", 
                                            label = "Wie wurde der Kontakt aufgenommen?",
                                            choices = list(
                                                           "Unbekannt" = "unknown",
                                                           "Festnetz" = "telephone",
                                                           "Handy" = "cellular"
                                                           ),
                                            selected = cParams[["contact"]]
                                )
                                ),
                         column(2 , h4(strong("Weitere Informationen"), align="center"),

                                #  13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
                                numericInput(
                                             inputId = "pdays", 
                                             label = "Tage seit letztem Kontakt der vorherigen Kampagne (-1 für kein Kontakt):", 
                                             min = -1, 
                                             max = 1000, 
                                             value = cParams[["pdays"]]),

                                #  14 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
                                sliderInput("campaign", "Anzahl der Kontakte in der aktuellen Kampagne", min = 0, max = 100, value = cParams[["campaign"]]),

                                #  15 - previous: number of contacts performed before this campaign and for this client (numeric)
                                sliderInput("previous", "Anzahl der Kontakte insgesamt", min = 0, max = 100, value = cParams[["previous"]]),


                                #  16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
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
                fluidRow(
                         titlePanel("Auskunft über den Kunden:"),
                         textOutput("Prognose"),
                         plotlyOutput("Verteilung")
                )
)
server <- function(input, output, session) {
    range <- callModule(controlledSlider, "balance", 1000)
    changeCustomer(input, session)
    model <- reactive({
        X <- model.matrix(gen_model(params),
                          df
        )
        X <- X[, -1]   # entferne den Intercept
        y <- as.factor(df[, "y"])
        model <- neuralnetwork(X, y, hidden.layers = c(4, 3), regression = FALSE,
                               loss.type = "log", learn.rates = 1e-04, n.epochs = 1,
                               verbose = TRUE)
        model
    })
    prognose <- reactive({
        model <- model()
        for (el in params) {
            if (el %in% classes) {
                if (typeof(reactiveValuesToList(input)[[el]]) == "logical") {
                    df[nrow(df) + 1, el] <- as.factor(ifelse(reactiveValuesToList(input)[[el]] == FALSE, "no", "yes"))
                } else {
                    df[nrow(df) + 1, el] <- as.factor(reactiveValuesToList(input)[[el]])
                }
            }
            if (el %in% numeric) {
                if (el == "balance") {
                    df[nrow(df) + 1, el] <- NS(as.numeric(reactiveValuesToList(input)[["balance-balance"]]))
                } else {
                    df[nrow(df) + 1, el] <- as.numeric(reactiveValuesToList(input)[[el]])
                }
            }
        }
        X.neu <- model.matrix(gen_model(params), df[nrow(df) + 1, ])
        X.neu <- X.neu[, -1]   # entferne den Intercept
        prognosevektor <- predict(model, X.neu)$predictions
        prog <- prognosevektor[1]
        prog
    })
    output$Verteilung <- renderPlotly({
        # X <- model.matrix(gen_model(params), df)
        # X <- X[, -1]
        # y <- as.factor(df[, "y"])
        # abweichungen <- y - predict(model, X)$predictions
        # # hist(prog + abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten")
        # # Verteilung <- plot_ly(x = prog, y = abweichungen, type = "histogram")
        # plot(abweichungen)
    })
    output$Prognose <- renderText({
        prog <- prognose()
        Prognose <- paste("Your score: ", prog)
    })
}
shinyApp(ui, server)
