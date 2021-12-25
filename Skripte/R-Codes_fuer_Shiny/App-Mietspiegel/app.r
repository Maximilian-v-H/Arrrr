


################
# Im folgenden Abschnitt wird das User Interface (UI) definiert
ui <- fluidPage(

  # Titel der App
  titlePanel("Münchner Mietspiegel"),

  # Layout für die Eingaben in die App und die Ausgaben
  sidebarLayout(

    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(
    
      # Eine Überschrift mit Linie darunter
      h3("Wohnung:",align="left"),
      hr(style="height: 1px; background: black"),

      # Ein Slider für die Fläche der Wohnung
      # der Slider geht hier von 30 (min) bis 100 (max), 
      # die Voreinstellung ist 75 (value)
      sliderInput(inputId = "flaeche",
                  label = "Groesse in qm:",
                  min = 30,
                  max = 100,
                  value = 75
      ),
      
      # Das Baujahr als numerische Eingabe
      # die Werte gehen von 1950 (min) bis 2000 (max) in Einerschritten (step)
      # die Voreinstellung ist 1981 (value)
      numericInput(inputId="bjahr", 
                    label="Baujahr:", 
                        value = 1981,
                        min=1950,max=2000,step=1
      ),
      
      # Die Lage als Auswahlliste
      # die Auswahlmöglichkeiten sind "normale Lage", "gute Lage" und "beste Lage",
      # die entsprechende Zuordnung mit Zahlen 1, 2 und 3 sind wie im Datensatz,
      # die Voreinstellung ist 1 (selected) - also eine "normale Lage" 
      selectInput("lage",label="Lage:", 
                       choices = list("normale Lage" = 1, "gute Lage" = 2,
                                      "beste Lage" = 3), selected = 1
      ), 
      
      # eine Überschrift für die weiteren Ausstattungsmerkmale
      h5(strong("Ausstattung:"),align="left"),
      
      # die weiteren drei Ausstattungsmerkmale (kueche, bad, zh) mit
      # Boxen zum Anklicken
      # die Voreinstellung ist jeweils FALSE (value), das heißt, es ist als
      # Voreinstellung keine Box angeklickt
      checkboxInput(inputId="luxuskueche", label="Luxusküche", value = FALSE),
      checkboxInput(inputId="luxusbad", label="Luxusbad", value = FALSE),
      checkboxInput(inputId="zentralheizung", label="mit Zentralheizung", value = FALSE),
     
    ),

    # der Hauptbereich der Nutzeroberfläche für die Ausgabe der Ergebnisse
    mainPanel(

      # Ausgabe des Histogramms
      plotOutput(outputId = "Verteilung"),
      
      # Ausgabe der Prognose
      textOutput("Prognose"),

    )
  )
)

############


server <- function(input, output) {

  # Innerhalb dieser Funktion werden die Bilder für die Ausgabe
  # erzeugt und die Ergebnisse berechnet
  
  # Folgende Funktion berechnet die Prognose für die eingegeben Werte  
  prognose <- reactive({

    # Speichere die Daten der Einflussvariablen in ein Objekt X
    X <- Daten[,c("flaeche","bjahr","bad","kueche","lage","zh")]
    
    # Ersetze die erste Zeile in X nun mit den neuen, eingegebenen Werten
      
    # zunächst die Werte für flaeche und bjahr 
    X[1,"flaeche"] <- input$flaeche 
    X[1,"bjahr"] <- input$bjahr
    # der angegebene Wert für lage muss zusätzlich noch in factor umgewandelt werden
    X[1,"lage"] <- as.factor(input$lage)
    
    # die Eingaben TRUE/FALSE für die Ausstattungsmerkmale kueche, bad und zh
    # werden jeweils in 0/1-Variablen umgewandelt (mit ifelse) und in
    # den Datentyp factor umgewandelt (mit as.factor);
    # die Werte werden in die erste Zeile von X eingetragen
    X[1,"kueche"] <- as.factor(ifelse(input$luxuskueche == FALSE, 0, 1))
    X[1,"bad"] <- as.factor(ifelse(input$luxusbad == FALSE, 0, 1))
    X[1,"zh"] <- as.factor(ifelse(input$zentralheizung == FALSE, 0, 1))
    
    # Berechne die Prognosen für X
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    prognosevektor <- predict(model,X)
    prog <- prognosevektor[1]
    
    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)
    
    # der errechnete Wert soll als Ergebnis der Funktion zurückgegeben werden
    prog
  })         
    
  # diese Funktion erzeugt das Histogramm und speichert es als Ausgabebild 
  # mit dem Namen output$Verteilung
  output$Verteilung <- renderPlot({

    # die errechnete Prognose aus der oben geschriebenen Funktion prognose()
    prog <- prognose()
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    # und die Daten der Zielvariable in y.
    # Berechne dann die Abweichungen zwischen den Prognosen und den realen Werten
    X <- Daten[,c("flaeche","bjahr","bad","kueche","lage","zh")]
    y <- Daten[,"mieteqm"]
    abweichungen <- y-predict(model,X)
    
    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert die bandbreite der Mieten für diese Wohnung 
    hist(prog+abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten",xlim=c(0,15))

  })
  
  # Definition einer Textausgabe mit dem namen output$Prognose 
  # In dieser Textausgabe soll der in der Funktion prognose() 
  # errechnete Prognosewert ausgegeben werden
  output$Prognose <- renderText({
  
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognose()
  
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog 
    Ausgabe <- paste("Durchschnittliche Miete: ", prog," Euro")
  })

}



# Aufruf der App-Funktionen
###############

shinyApp(ui = ui, server = server)

###############






