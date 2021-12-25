
Daten <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE)

Daten[, "bad"] <- as.factor(Daten[, "bad"])
Daten[, "kueche"] <- as.factor(Daten[, "kueche"])
Daten[, "lage"] <- as.factor(Daten[, "lage"])
Daten[, "zh"] <- as.factor(Daten[, "zh"])
library(ANN2)
X <- model.matrix(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, Daten)
X <- X[, -1]   # entferne den Intercept
y <- Daten[, "mieteqm"]
model <- neuralnetwork(X, y, hidden.layers = c(4, 3), regression = TRUE,
                       loss.type = "absolute", learn.rates = 1e-04, n.epochs = 100,
                       verbose = FALSE)
ui <- fluidPage(
                titlePanel("Münchner Mietspiegel"),
                sidebarLayout(
                              sidebarPanel(
                                           h3("Wohnung:",align="left"),
                                           hr(style="height: 1px; background: black"),
                                           sliderInput(inputId = "flaeche",
                                                       label = "Groesse in qm:",
                                                       min = 30,
                                                       max = 100,
                                                       value = 75
                                                       ),
                                           numericInput(inputId="bjahr", 
                                                        label="Baujahr:", 
                                                        value = 1981,
                                                        min=1950,max=2000,step=1
                                                        ),
                                           selectInput("lage",label="Lage:", 
                                                       choices = list("normale Lage" = 1, "gute Lage" = 2,
                                                                      "beste Lage" = 3), selected = 1
                                                       ), 
                                           h5(strong("Ausstattung:"),align="left"),
                                           checkboxInput(inputId="luxuskueche", label="Luxusküche", value = FALSE),
                                           checkboxInput(inputId="luxusbad", label="Luxusbad", value = FALSE),
                                           checkboxInput(inputId="zentralheizung", label="mit Zentralheizung", value = FALSE),

                                           ),
                              mainPanel(
                                        plotOutput(outputId = "Verteilung"),
                                        textOutput("Prognose"),
                              )
                )
)
server <- function(input, output) {
    prognose <- reactive({
        Daten.neu <- Daten
        Daten.neu[1,"flaeche"] <- input$flaeche 
        Daten.neu[1,"bjahr"] <- input$bjahr
        Daten.neu[1,"lage"] <- as.factor(input$lage)
        Daten.neu[1,"kueche"] <- as.factor(ifelse(input$luxuskueche == FALSE, 0, 1))
        Daten.neu[1,"bad"] <- as.factor(ifelse(input$luxusbad == FALSE, 0, 1))
        Daten.neu[1,"zh"] <- as.factor(ifelse(input$zentralheizung == FALSE, 0, 1))
        X.neu <- model.matrix(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, Daten.neu)
        X.neu <- X.neu[,-1]   # entferne den Intercept
        prognosevektor <- predict(model,X.neu)$predictions
        prog <- prognosevektor[1]
        prog <- round(prog,digits=2)
        prog
    })         
    output$Verteilung <- renderPlot({
        prog <- prognose()
        X <- Daten[,c("flaeche","bjahr","bad","kueche","lage","zh")]
        X <- model.matrix(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, Daten)
        X <- X[,-1]   # entferne den Intercept
        y <- Daten[,"mieteqm"]
        abweichungen <- y-predict(model,X)$predictions
        hist(prog+abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten",xlim=c(0,15))
    })
    output$Prognose <- renderText({
        prog <- prognose()
        Ausgabe <- paste("Durchschnittliche Miete: ", prog," Euro")
    })
}
shinyApp(ui = ui, server = server)
