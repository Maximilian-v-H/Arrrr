###############################################################
# Übung zu Regression
###############################################################


# Maschinendaten
#########################################################

  # Setzen des Pfades und Einlesen der Beispiel-Daten

    setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
    Daten <- read.csv("Maschinendaten.csv",header=TRUE,sep=";",fill=TRUE)
    Daten[1:10,]

  # Kontrolle der Datentypen und Ausgabe der Summary
    
    summary(Daten)

  # Berechnung einer Regression 
  
    model <- lm( Qualitaetsparameter ~ Werkzeug + Maschine + Vorschub, data=Daten)
    model


  # Umwandlung und Kontrolle der Datentypen sowie Ausgabe der Summary

    Daten[,"Werkzeug"] <- as.factor(Daten[,"Werkzeug"])   
    Daten[1:5,]
    
    summary(Daten)

  # Berechnung der Regression 
  
    model <- lm( Qualitaetsparameter ~ Werkzeug + Maschine + Vorschub, data=Daten)
    model
 
    
  # Berechnung des mittleren Prognosefehlers (MAE)

    y <- Daten[,"Qualitaetsparameter"] 
    Prognosen <- model$fitted.values 
    Prognosefehler <- mean( abs( y - Prognosen ) )
    Prognosefehler


  # Berechnung einer Prognose für einen neuen Produktionsauftrag: 
  # Welche Qualität beim produzierten Bauteil ist
  # bei Verwendung von Werkzeug 3, Maschine A, Vorschub von 2.5 zu erwarten?
  
    27.985 - 2.799 - 4.077*2.5 
  


