###############################################################
# Übung zu Klassifikation (logistische Regression)
###############################################################


# Maschinendaten
#########################################################

  # Setzen des Pfades und Einlesen der Beispiel-Daten

    setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
    Daten <- read.csv("Maschinendaten.csv",header=TRUE,sep=";",fill=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen durch Ausgabe der Summary
    
    summary(Daten)

  # Umwandlung und Kontrolle der Datentypen sowie Ausgabe der Summary

    Daten[,"Werkzeug"] <- as.factor(Daten[,"Werkzeug"])
    Daten[,"Ausschuss"] <- as.factor(Daten[,"Ausschuss"])
    summary(Daten)
    
  # Berechnung der Klassifikation (Logistische Regression)   
                     
    model <- glm(Ausschuss ~ Werkzeug + Maschine + Vorschub, data=Daten,binomial(link = "logit"))
    model

  # Berechnung des Klassifikationsfehler (Misclassification Error Rate)

    y <- Daten[,"Ausschuss"] 
    Prognosen <- round(model$fitted.values) 
    Prognosefehler <- mean( ifelse( y != Prognosen, 1,0 ) )
    Prognosefehler

    
  # Berechnung von Prognosen für einen neuen Produktionsauftrag 
  # Wähle Werkzeug 3, Maschine A, Vorschub von 2.5  
  
    z <- -5.314 + 1.683 + 2.5*1.926  
    exp(z)/(1+exp(z))
  
  # Wähle nun statt Maschine A die Maschine B 
  
    z <- -5.314 + 1.683 - 2.894 + 2.5*1.926  
    exp(z)/(1+exp(z))
  
  # Wähle nun Werkzeug 2 und Maschine A 
  
    z <- -5.314 - 2.575 + 2.5*1.926  
    exp(z)/(1+exp(z))
    
