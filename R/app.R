library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Charger les données (remplace "data.csv" par ton fichier réel)
#data <- read.csv("data.csv", sep=";", stringsAsFactors = FALSE)

# Convertir les colonnes en types appropriés
data$Sexe <- as.factor(data$Sexe)
data$Annee.de.debut.de.pratique.du.football <- as.numeric(data$Annee.de.debut.de.pratique.du.football)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des réponses des joueurs de football"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sexe", "Choisir le sexe :", 
                  choices = unique(data$Sexe), 
                  selected = unique(data$Sexe)[1]),
      
      sliderInput("annee_debut", "Filtrer par année de début :", 
                  min = min(data$Annee.de.debut.de.pratique.du.football, na.rm = TRUE), 
                  max = max(data$Annee.de.debut.de.pratique.du.football, na.rm = TRUE), 
                  value = c(min(data$Annee.de.debut.de.pratique.du.football, na.rm = TRUE),
                            max(data$Annee.de.debut.de.pratique.du.football, na.rm = TRUE)),
                  step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Données", DTOutput("table")),
        tabPanel("Visualisation", plotOutput("plot_anxiete")),
        tabPanel("Statistiques", verbatimTextOutput("summary"))
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  # Filtrer les données
  filtered_data <- reactive({
    data %>%
      filter(Sexe == input$sexe, 
             Annee.de.debut.de.pratique.du.football >= input$annee_debut[1],
             Annee.de.debut.de.pratique.du.football <= input$annee_debut[2])
  })
  
  # Afficher les données filtrées
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  # Créer un graphique du niveau d’anxiété avant un match
  output$plot_anxiete <- renderPlot({
    ggplot(filtered_data(), aes(x = as.numeric(Sur.une.echelle.de.0..pas.anxieuse.a.10.tres.anxieuse.je.me.sens.anxieuse.avant.un.match.a.))) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Distribution du niveau d'anxiété avant un match",
           x = "Niveau d'anxiété (0 à 10)", y = "Nombre de joueurs") +
      theme_minimal()
  })
  
  # Résumé statistique des affirmations
  output$summary <- renderPrint({
    summary(filtered_data()[, c("Affirmation.1", "Affirmation.12", "Affirmation.13", "Affirmation.14")])
  })
}

# Lancer l’application
shinyApp(ui = ui, server = server)
