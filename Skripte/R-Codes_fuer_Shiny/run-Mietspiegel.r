
# Setzen des Pfades und Einlesen der Daten

  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/2021-2022_Wintersemester/Assistenzsysteme/Shiny")
  Daten <- read.csv("Mietspiegel.csv",header=TRUE,sep=";",fill=TRUE)

  Daten[,"bad"] <- as.factor(Daten[,"bad"])
  Daten[,"kueche"] <- as.factor(Daten[,"kueche"])
  Daten[,"lage"] <- as.factor(Daten[,"lage"])
  Daten[,"zh"] <- as.factor(Daten[,"zh"])

# Berechnung der linearen Regression

  model <- lm( mieteqm ~ flaeche + bjahr + bad + kueche + lage + zh, data=Daten)


# Starten der Shiny-App

  library(shiny)

  runApp("App-Mietspiegel")

