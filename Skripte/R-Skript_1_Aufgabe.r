
  setwd("C:/Users/rHable/Nextcloud/THD-Robert/Lehre/Daten/Einfuehrung-KI")

  Daten <- read.csv("FreierFall.csv",header=TRUE,sep=";",fill=TRUE)
  
  Daten[1:12,]

  summary(Daten)

  x <- Daten[,"Zeit"]
  y <- Daten[,"Position"]
  
  plot(x,y)


  y <- Daten[,"Position"]
  
  mean(y)  # Mittelwert
  sd(y)       # Standardabweichung
