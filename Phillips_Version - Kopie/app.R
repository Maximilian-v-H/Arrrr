library(shiny)
library(shinyWidgets)
library(plotly)
library(ANN2)
library(gbm)
library(mcclust)
library(rpart)
library(rpart.plot)


df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE)
params <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "age", "balance", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
relevant <- c("age", "balance", "marital", "loan", "duration", "previous", "poutcome")

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

gen_model <- function(params) {
  return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}

controlledSliderUI <- function(id){
  ns = NS(id)
  conditionalPanel(
    condition = "input.params.includes('balance')",
    sliderInput(ns("balance"), "Kontostand in $:", min = -20000, max = 150000, value = 1000),
    numericInput(ns("value"), NULL, min = -20000, max = 150000, value = 1000),
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





ui <- fluidPage(
                titlePanel("Verkaufskampagne Darlehen"),
                fluidRow(
                         column(1,
                                h4(strong("Bestandskunden"), align="center"),
                                wellPanel(
                                  selectInput("job", label = "Beruf:",
                                              choices = list(
                                                "Admin" = "admin.",
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
                                              selected = 1
                                  ),
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
                                                                   value = 0,
                                                                   min = 0,
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
                                              selected = 1
                                  ),
                                ),
                                
                                #   3 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                #                                       "blue-collar","self-employed","retired","technician","services")
                                conditionalPanel(
                                  condition = "input.params.includes('job')",
                                  selectInput("job", label = "Beruf:",
                                              choices = list(
                                                "Admin" = "admin.",
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
                                              selected = 1
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
                                              selected = 1
                                  ),
                                ),
                         ),
                          column(2, h4(strong("Finanzielle Informationen"), align="center"),
                                 #   5 - default: has credit in default? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('default')",
                                   checkboxInput("default", "Gibt es ein Verzugskredit?"),
                                 ),
                                 #   7 - housing: has housing loan? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('housing')",
                                   checkboxInput("housing", "Gibt es ein Baudarlehnen?"),
                                 ),
                                 #   8 - loan: has personal loan? (binary: "yes","no")
                                 conditionalPanel(
                                   condition = "input.params.includes('loan')",
                                   checkboxInput("loan", "Besteht ein privat Kredit?"),
                                 ),
                  controlledSliderUI("balance")
                            ),
                  column(2,  h4(strong("Informationen zum letzten Kontakt"), align="center"),
                         #  9 - day: last contact day of the month (numeric)
                         conditionalPanel(
                           condition = "input.params.includes('day')",
                           selectInput(
                             inputId = "day", 
                             label="Letzter Kontakt (Tag des Monats):", 
                             choices = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                             selected = 1
                           )
                         ),
                         
                         #  10 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
                         conditionalPanel(
                           condition = "input.params.includes('month')",
                           selectInput("month", label = "Letzter Kontakt (Monat):",
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
                                       selected = 1
                           ),
                         ),
                         
                         
                         #  11 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
                         conditionalPanel(
                           condition = "input.params.includes('pdays')",
                           numericInput(inputId = "pdays", label = "Tage seit letztem Kontakt (-1 fuer kein Kontakt):", min = -1, max = 1000, value = 100)
                         ),
                         
                         #  12 - duration: last contact duration, in seconds (numeric)
                         conditionalPanel(
                           condition = "input.params.includes('duration')",
                           numericInput(inputId = "duration", label = "Gespraechsdauer des letzten Kontakt in Sekunden):", min = 0, max = 1000, value = 35),
                         ),
                        ),
                  column(2 , h4(strong("Weitere Informationen"), align="center"),
                         #  13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
                         conditionalPanel(
                           condition = "input.params.includes('campaign')",
                           sliderInput("campaign", "Anzahl der Kontakte in der aktuellen Kampagne", min = 0, max = 20, value = 1),
                         ),
                         
                         #  14 - previous: number of contacts performed before this campaign and for this client (numeric)
                         conditionalPanel(
                           condition = "input.params.includes('previous')",
                           sliderInput("previous", "Anzahl der Kontakte insgesamt", min = 0, max = 20, value = 1),
                         ),
                         
                         #   15 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
                         conditionalPanel(
                           condition = "input.params.includes('contact')",
                           selectInput("contact", label = "Wie wurde der Kontakt aufgenommen?",
                                       choices = list(
                                         "Unnbekannt" = "unknown",
                                         "Festnetz" = "telephone",
                                         "Handy" = "cellular"
                                       ),
                                       selected = 1
                           ),
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
                                       selected = 1
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
  range <- callModule(controlledSlider, "balance", 1000)
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
