###############################################################
# Produktionsqualität: Einstellung von Maschinenparametern
# Übung zu Maschinellem Lernen
###############################################################


# Teil 1
#############

  # Setzen des Pfades und Einlesen der Beispiel-Daten

    setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
    Daten <- read.csv("Maschinendaten.csv",header=TRUE,sep=";",fill=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen und Ausgabe der Summary
    
    summary(Daten)
    
  # Umwandlung und Kontrolle der Datentypen sowie Ausgabe der Summary

    Daten[,"Werkzeug"] <- as.factor(Daten[,"Werkzeug"])
    Daten[,"Ausschuss"] <- as.factor(Daten[,"Ausschuss"])
    Daten[1:10,]
    
    summary(Daten)

  # Laden des R-Pakets 

    library(ANN2)


  # Aufteilen der Daten:
    # Anzahl der Daten
      length(Daten[,1])
    # Da es 500 Datenpunkte sind, werden bei einer (70% zu 30% Aufteilung)
    # 350 Datenpunkte als Trainingsdaten gewählt und die restlichen Datenpunkte als
    # 150 Testdaten
    Daten.train <- Daten[1:350,]
    Daten.test <- Daten[351:500,]

  # Berechnung des Modells auf den Trainingsdaten:
  # Achtung: im Befehl 'model.matrix' steht bei 'data' nun Daten.train !!!

    X <- model.matrix(Qualitaetsparameter ~ Werkzeug + Maschine + Vorschub, Daten.train)
    X <- X[,-1]
    summary(X)
    
    y <- Daten.train[,"Qualitaetsparameter"]
  
    model <- neuralnetwork(X, y, hidden.layers=c(4,3), regression = TRUE, 
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                       verbose=FALSE)
    plot(model)
  
  # Berechnung der Prognosen auf den Testdaten:
  # Achtung: im Befehl 'model.matrix' steht bei 'data' nun Daten.test !!!

    X.test <- model.matrix(Qualitaetsparameter ~ Werkzeug + Maschine + Vorschub, Daten.test)
    X.test <- X.test[,-1]
    prognosen <- predict(model,X.test)$predictions
    y.test <- Daten.test[,"Qualitaetsparameter"]   
    mean(abs(y.test-prognosen))



# Teil 2
#################

  # Berechnung einer Prognose für einen neuen Produktionsauftrag: 
  # Welche Qualität beim produzierten Bauteil ist
  # bei Verwendung von Werkzeug 3, Maschine A, Vorschub von 2.5 zu erwarten?

  # Hinzufügen eines neuen Datenpunkts (Werkzeug 3, Maschine A, Vorschub von 2.5)
  
    summary(X.test)
    x.neu <- c(0,1,0,2.5)
    X <- rbind(X.test,x.neu)
        
  # Berechnung der Prognosen
    predict(model,X)$predictions

