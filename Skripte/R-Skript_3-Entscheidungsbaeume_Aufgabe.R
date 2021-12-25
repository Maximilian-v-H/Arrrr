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

  # Laden des R-Pakets 'tree'

    library(tree)


  # Aufteilen der Daten:
    # Anzahl der Daten
      length(Daten[,1])
    # Da es 500 Datenpunkte sind, werden bei einer (70% zu 30% Aufteilung)
    # 350 Datenpunkte als Trainingsdaten gewählt und die restlichen Datenpunkte als
    # 150 Testdaten
    Daten.train <- Daten[1:350,]
    Daten.test <- Daten[351:500,]

  # Berechnung des Modells auf den Trainingsdaten:
  # Achtung: im Befehl 'cv.tree' steht bei 'data' nun Daten.train !!!

    Baum <- tree(Qualitaetsparameter ~ Werkzeug + Maschine + Vorschub, data=Daten.train)
    tuning <- cv.tree(Baum, K=5)
    t <- which.min(tuning$dev)
    Anzahl.Endknoten <- tuning$size[t]

    model <- prune.tree(Baum,best=Anzahl.Endknoten)
    plot(model)
    text(model)


  # Berechnung der Prognoseergebnisse auf den Testdaten:

    X.test <- Daten.test[,c("Werkzeug","Maschine","Vorschub")]
    prognosen <- predict(model,X.test)

  # Berechnung des mittleren Prognosefehlers (MAE)

    y.test <- Daten.test[,"Qualitaetsparameter"]
    mean(abs(y.test-prognosen))


# Teil 2
#################

  # Berechnung einer Prognose für einen neuen Produktionsauftrag: 
  # Welche Qualität beim produzierten Bauteil ist
  # bei Verwendung von Werkzeug 3, Maschine A, Vorschub von 2.5 zu erwarten?

  # Hinzufügen eines neuen Datenpunkts (Werkzeug 3, Maschine A, Vorschub von 2.5)
    x.neu <- data.frame(3,"A",2.5)
    names(x.neu) <- names(X.test)
    X <- rbind(X.test,x.neu)
        
  # Berechnung der Prognosen
    predict(model,X)


###########################################
# Teil 3: Klassifikation mit Zielvarable "Ausschuss"
#######################

    Baum <- tree(Ausschuss ~ Werkzeug + Maschine + Vorschub, data=Daten.train)
    tuning <- cv.tree(Baum, K=5)
    t <- which.min(tuning$dev)
    Anzahl.Endknoten <- tuning$size[t]

    model <- prune.tree(Baum,best=Anzahl.Endknoten)
    plot(model)
    text(model)


  # Berechnung der Prognoseergebnisse auf den Testdaten und der Prognosegüte:

    X.test <- Daten.test[,c("Werkzeug","Maschine","Vorschub")]
    prognosen <- predict(model,X.test)
    prognosen
    
    prognosen <- round(prognosen[,2])
    
    y <- Daten.test[,"Ausschuss"]

    A <- matrix(0,ncol=2,nrow=2)
    colnames(A) <- c("Real: gut", "Real: schlecht") 
    rownames(A) <- c("Prognose: gut", "Prognose: schlecht") 
    
    A[1,1] <- sum(ifelse(y == 0 & prognosen == 0, 1,0))
    A[1,2] <- sum(ifelse(y == 1 & prognosen == 0, 1,0))
    A[2,1] <- sum(ifelse(y == 0 & prognosen == 1, 1,0))
    A[2,2] <- sum(ifelse(y == 1 & prognosen == 1, 1,0))

    A

  # Hinzufügen eines neuen Datenpunkts (Werkzeug 3, Maschine A, Vorschub von 2.5)
    x.neu <- data.frame(3,"A",2.5)
    names(x.neu) <- names(X.test)
    X <- rbind(X.test,x.neu)
        
  # Berechnung der Prognosen
    predict(model,X)








