


#################################################################
#Daten in R einlesen und erste Schritte in R
#################################################################
# von Prof. Dr. Robert Hable, Technische Hochschule Deggendorf
#################################################################


##########################
# Dateneinlesen in R
##########################


# Zunächst müssen Sie in R den Pfad für den Ordner setzen, in dem Ihre Daten
# gespeichert sind.

# Bei mir lautet der Pfad in Windows:
#     C:\Users\rHable\Nextcloud\THD-Robert\Lehre\Daten\Einfuehrung-KI
# Entsprechend muss ich den Pfad in R folgendermaßen setzen:

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")


# Nach der Kontrolle der csv-Datei können die Daten einlesen werden
# Der Befehl zum Einlesen der Daten hängt von der Gestalt der csv-Datei ab:
# Beachten Sie hierzu die Schritt-für-Schritt-Anleitung im Skript!!!

  Daten <- read.csv("Koerpergewicht.csv",header=TRUE,sep=";",fill=TRUE)

# Ausgabe der Daten in R:

  Daten

# Möchte man sich nur den Eintrag in der 5. Zeile und der 3. Spalte
# ausgeben lassen, dann geht das mit

  Daten[5,3]
  
# Möchte man sich nur die gesamte 2. Zeile ausgeben lassen, dann:

  Daten[2,]

# Möchte man sich die ersten 13 Zeilen ausgeben lassen, dann:

  Daten[1:13,]

# Möchte man sich nur die gesamte 2. Spalte ausgeben lassen, dann:

  Daten[,2]

# Besser ist es aber, die Spaltennamen zu verwenden:

  Daten[,"Alter"]

# Nach dem Einlesen der Daten sollte man sich zur Kontrolle immer
# die Zusammenfassung der Daten anschauen, und zwar mit

  summary(Daten)
  
  
####################################
# Datentypen
###################################

# Es gibt zwei wichtige Typen von Daten: 
# nominalskalierte Daten (factor) und
# metrische Daten (numeric)

# Enthält eine Spalte Zahlen, dann werden die Daten dieser Spalte
# als metrisch (numeric) eingelesen. Handelt es sich bei diesen Zahlen
# aber um keine "echten" Zahlen, sondern um Codierungen 
# (z.B. 0 für männlich, 1 für weiblich)
# dann müssen wir den Datentyp ändern, also von numeric auf factor

# Machen wir das am Beispiel der Spalte Alter:

  Daten[,"Alter"] <- as.factor(Daten[,"Alter"])

# Auf dem ersten Blick hat sich an den Daten nichts geändert:

  Daten[1:10,]
  
# Aber R behandelt jetzt die Daten anders, wie man an der summary sieht:

  summary(Daten)
  
# R rechnet nun auch nicht mehr mit den Daten, wie man hier sieht:

  x <- Daten[,"Alter"]
  mean(x)
  
# Weil Alter aber eine metrische Variable ist, war das Umwandeln in factor
# hier nicht sinnvoll. Deshalb machen wir das rückgängig, indem wir die Daten 
# nochmal neu einlesen. 

  Daten <- read.csv("Koerpergewicht.csv",header=TRUE,sep=";",fill=TRUE)
  summary(Daten)
  
# Jetzt rechnet R auch wieder mit den Daten:

  x <- Daten[,"Alter"]
  mean(x)
  



###############################################################
# Beispiel für Daten mit deutscher Dezimalschreibweise
##############

# Der Datensatz Koerpergewicht_2.csv enthält Daten mit deutscher 
# Dezmalschreibweise. Die Daten müssen daher mit dem Befehl read.csv2
# eingelesen werden. Sehen wir, was passiert, wenn wir dies nicht beachten:

  Daten <- read.csv("Koerpergewicht_2.csv",header=TRUE,sep=";",fill=TRUE)
  
# Auf den ersten Blick sieht alles gut aus, die Daten wurden eingelesen:

  Daten

# Aber R hat die Daten falsch interpretiert:

  summary(Daten)

# Und Berechnungen mit den Daten sind unmöglich

  x <- Daten[,"Groesse"]
  mean(x)

# Die Daten müssen neu eingelesen werden.
# Nun korrekt mit read.csv2:

  Daten <- read.csv2("Koerpergewicht_2.csv",header=TRUE,sep=";",fill=TRUE)
  Daten
  
  summary(Daten)

# Jetzt funktionieren auch die Berechnungen:

  x <- Daten[,"Groesse"]
  mean(x)
  

##################################################
# Grundlegende Befehle in R
#####################################

# Grundrechenarten:

  2+4
  5-6
  2/3
  5*7
  
# Funktionen

  3^2
  sqrt(16)
  exp(1)
  log(5)
  sin(3)
  
# Mittelwert, Median etc.

  x <- Daten[,"Groesse"]
  
  mean(x)  # Mittelwert
  median(x)   # Median
  sd(x)       # Standardabweichung
  sum(x)      # Summe
  
# Definition von Vaiablen / Objekten

  a <- 3
  a
  
  b <- 5
  b
  
  a+b
  
  b <- 2*b
  b
  
  zahl <- a+7
  zahl
  
  a <- 6
  zahl
  
# Malen einfacher Punktewolken

  x <- Daten[,"Groesse"]
  y <- Daten[,"Gewicht"]
  
  plot(x,y)
  
# Malen von Punktewolken mit Grafikoptionen

  plot(x,y,main="Meine selbstgewählte Überschrift",xlab="Größe",ylab="Gewicht",col="blue",pch=19)
  
  # main: text der Bildüberschrift
  # xlab und ylab: Achsenbeschriftung
  # col: Farbe der Punkte (englischer Farbname)
  # pch: Ändern des Stils, in dem Punkte gemalt werden (eine Zahl)
  
   
# Malen von Punktewolken mit Achsenskalierung

  plot(x,y,main="Meine selbstgewählte Überschrift",xlim=c(1.5,2),ylim=c(50,100))
  
  # xlim: die Grenzen der x-Achse, hier 1.5 (linke Grenze) und 2 (rechte Grenze)
  # ylim: die Grenzen der y-Achse, hier 50 (untere Grenze) und 100 (obere Grenze)
  






