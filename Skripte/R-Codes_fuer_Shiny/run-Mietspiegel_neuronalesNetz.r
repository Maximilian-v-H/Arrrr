
# Setzen des Pfades und Einlesen der Daten

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/2021-2022_Wintersemester/Assistenzsysteme/Shiny")
  Daten <- read.csv("Mietspiegel.csv",header=TRUE,sep=";",fill=TRUE)

  Daten[,"bad"] <- as.factor(Daten[,"bad"])
  Daten[,"kueche"] <- as.factor(Daten[,"kueche"])
  Daten[,"lage"] <- as.factor(Daten[,"lage"])
  Daten[,"zh"] <- as.factor(Daten[,"zh"])

# Berechnung des neuronalen Netzes

  # Laden des R-Pakets  
    library(ANN2)

  # Erstellen eines Datensatzes mit Dummy-Codierung der kategoriellen Variablen  
    X <- model.matrix(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, Daten)
    X <- X[,-1]   # entferne den Intercept
   
    y <- Daten[,"mieteqm"]

  # Trainieren des neuronalen Netzes
  # mit 2 Hidden Layer, wobei der 1. Hidden Layer 4 Hidden Units hat und
  # der 2. Hidden Layer 3 Units hat
  
    model <- neuralnetwork(X, y, hidden.layers=c(4,3), regression = TRUE, 
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                       verbose=FALSE)


# Starten der Shiny-App

  library(shiny)

  runApp("App-Mietspiegel_neuronalesNetz")

