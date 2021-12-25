

#################################################################
# Lineare Regression 
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################



######################################
# Beispiel für Regression
#############


# Setzen des Pfades und Einlesen der Beispiel-Daten

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")
  Daten <- read.csv2("Koerpergewicht_2.csv",header=TRUE,sep=";",fill=TRUE)
  Daten[1:5,]

# Kontrolle der Datentypen und Ausgabe der Summary
  
  summary(Daten)


# Berechnung der Regression

  model <- lm( Gewicht ~ Geschlecht + Alter + Groesse, data=Daten)
  model

# Berechnung des mittleren Prognosefehlers (MAE)

  y <- Daten[,"Gewicht"] 
  Prognosen <- model$fitted.values 
  Prognosefehler <- mean( abs( y - Prognosen ) )
  Prognosefehler
  
  
#####  
# Beispiel für einfache Regression (nur eine Einflussvariablen)

  model.2 <- lm( Gewicht ~ Groesse, data=Daten)
  model.2 

  # Zeichnen der Punktewolke mit Regressionsfunktion
  
    # Zunächst die Punktewolke
    
      x <- Daten[,"Groesse"]
      y <- Daten[,"Gewicht"]
      plot(x,y,pch=19,main="Zusammenhang Groesse und Gewicht")
    
    # Die Regressionsgerade lässt sich folgendermaßen zeichnen
    # (nur möglich bei nur einer Einflussvariablen)
    
      Parameter <- model.2$coefficients
      Parameter
      
      Intercept <- Parameter[1]
      Steigung <- Parameter[2]
      
      abline(a=Intercept,b=Steigung) 





  
  
  
    
