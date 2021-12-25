
#####################################################
# neuronales Netz
#################################################

# Dateneinlesen und Korrektur der Datentypen

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
  Daten <- read.csv("Mietspiegel.csv",header=TRUE,sep=";",fill=TRUE)
  summary(Daten)

  Daten[,"bad"] <- as.factor(Daten[,"bad"])
  Daten[,"kueche"] <- as.factor(Daten[,"kueche"])
  Daten[,"lage"] <- as.factor(Daten[,"lage"])
  Daten[,"zh"] <- as.factor(Daten[,"zh"])

# Berechnung der linearen Regression

  model <- lm( mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, data=Daten)

# Berechnung des neuronalen Netzes

  # Laden des R-Pakets
  
    library(ANN2)

  # Erstellen eines Datensatzes mit Dummy-Codierung der kategoriellen Variablen
  
    X <- model.matrix(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, Daten)
    X <- X[,-1]   # entferne den Intercept
    summary(X)
    
    y <- Daten[,"mieteqm"]

  # Trainieren des neuronalen Netzes
  # mit 2 Hidden Layer, wobei der 1. Hidden Layer 4 Hidden Units hat und
  # der 2. Hidden Layer 3 Units hat
  
    model <- neuralnetwork(X, y, hidden.layers=c(4,3), regression = TRUE, 
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                       verbose=FALSE)
    plot(model)

  # Berechnung der Prognosegüte
  # ACHTUNG: Verwerflicherweise wurde hier nicht in Trainings- und Testdaten getrennt
  
    prognosen <- predict(model,X)$predictions    
    mean(abs(prognosen-y))
    
  # Berechnung der Prognose für eine Wohnung mit 50qm, Baujahr 1981, normalen Bad, luxuriöser Küche,
  # beste Lage und Zentralheizung:
  
    summary(X)
    x.neu <- c(50,1981,0,1,0,1,1)
    X <- rbind(X,x.neu)
        
  # Berechnung der Prognosen
    predict(model,X)$predictions

    
    
    
  
  