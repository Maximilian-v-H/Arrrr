
# Setzen des Pfades und Einlesen der Daten

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/2021-2022_Wintersemester/Assistenzsysteme/Shiny")
  Daten <- read.csv("Mietspiegel.csv",header=TRUE,sep=";",fill=TRUE)

  Daten[,"bad"] <- as.factor(Daten[,"bad"])
  Daten[,"kueche"] <- as.factor(Daten[,"kueche"])
  Daten[,"lage"] <- as.factor(Daten[,"lage"])
  Daten[,"zh"] <- as.factor(Daten[,"zh"])

# Berechnung des Entscheidungsbaumes

  library(tree)
  Baum <- tree(mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, data=Daten)
  tuning <- cv.tree(Baum, K=5)
  t <- which.min(tuning$dev)
  Anzahl.Endknoten <- tuning$size[t]
  model <- prune.tree(Baum,best=Anzahl.Endknoten)

# Starten der Shiny-App

  library(shiny)

  runApp("App-Mietspiegel_Entscheidungsbaum")

