# Installer les packages si nécessaire
if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(httr)) {
  install.packages("httr")
}
if (!require(jsonlite)) {
  install.packages("jsonlite")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(plotly)) {
  install.packages("plotly")
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
}

# Charger les données depuis l'API JCDecaux
base <- httr::GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=8d2b71ec0cc951c380e6bb5da02b76a32f6f8559")
data <- jsonlite::fromJSON(httr::content(base, "text"), flatten = TRUE)

# Calcul du Taux de Déséquilibre entre Supports et Vélos
taux_desequilibre <- ((sum(data$bike_stands) - sum(data$available_bikes)) / sum(data$bike_stands)) * 100

# Calcul de l'Indice de Disponibilité des Stations de Vélos (IDSV)
IDSV <- ((sum(data$available_bike_stands) + sum(data$available_bikes)) / (sum(data$bike_stands) + sum(data$available_bikes))) * 100

# Création de la boîte à moustaches
boxplot_data <- ((data$available_bike_stands / data$bike_stands) * 100)

library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(shinydashboard)
library(plotly)
library(leaflet.extras)

ui <- dashboardPage(
  dashboardHeader(title = "Velov's Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte Leaflet", tabName = "map", icon = icon("map")),
      menuItem("Tableau de bord avec Statistiques", tabName = "dashboard_stats", icon = icon("dashboard")),
      menuItem("Tableau de données brutes", tabName = "raw_data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBox(
                  value = sum(data$available_bikes),  # Nombre de Vélos Disponibles
                  subtitle = "Nombre de Vélos Disponibles",
                  icon = icon("bicycle"),
                  color = "blue",
                  width = 4
                ),
                valueBox(
                  value = sum(data$bike_stands),  # Nombre Total de Vélos
                  subtitle = "Nombre Total de Vélos",
                  icon = icon("bicycle"),
                  color = "purple",
                  width = 4
                ),
                valueBox(
                  value = length(unique(data$name)),  # Nombre Total de Stations
                  subtitle = "Nombre Total de Stations",
                  icon = icon("map-marker"),
                  color = "purple",
                  width = 4
                )
              ),
              fluidRow(
                # Sélecteur pour choisir le critère de tri
                selectInput("sort_criteria", "Trier par critère :",
                            choices = c("Nombre de vélos" = "available_bikes", "Nombre de supports à vélo" = "bike_stands", "Vélos disponibles" = "available_bike_stands"),
                            selected = "available_bikes"
                )
              ),
              fluidRow(
                # Graphique en barres pour le top 10 des stations
                plotlyOutput("top_stations_chart", width = "100%")
              )
      ),
      tabItem("raw_data", 
              fluidRow(
                column(12, tableOutput("raw_data_table"))
              )
      ),
      tabItem("map", 
              fluidRow(
                column(12, leafletOutput("map"))
              )
      ),
      tabItem("dashboard_stats",
              fluidRow(
                valueBox(
                  value = {
                    rate <- (sum(data$available_bikes) / sum(data$bike_stands)) * 100
                    paste0(round(rate, 2), "%")
                  },
                  subtitle = "Taux d'Utilisation des Vélos",
                  icon = icon("bicycle"),
                  color = "red",
                  width = 4
                ),
                valueBox(
                  value = paste0(round(IDSV, 2), "%"),
                  subtitle = "Indice de Disponibilité des Stations de Vélos",
                  icon = icon("bicycle"),
                  color = "blue",
                  width = 4
                ),
                valueBox(
                  value = paste0(round(taux_desequilibre, 2), "%"),
                  subtitle = "Taux de Déséquilibre entre Supports et Vélos",
                  icon = icon("exclamation-triangle"),
                  color = "orange",
                  width = 4
                )
              ),
              fluidRow(
                plotlyOutput("boxplot", width = "100%")  # Ajout de la sortie pour le graphique en boîte à moustaches
              ),
              fluidRow(
                # Graphique pour montrer la distribution du nombre total de supports à vélo en fonction du nombre de supports à vélo disponibles
                plotlyOutput("plot_supports_vs_disponibles", width = "100%")  # Rétrécir à 50%
              )
      )
    )
  )
)

server <- function(input, output) {
  # Afficher les données brutes dans un tableau
  output$raw_data_table <- renderTable({
    data
  })
  
  # Créez une carte Leaflet automatique
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addFullscreenControl(position = "topright", pseudoFullscreen = TRUE) %>%
      addCircleMarkers(
        lng = ~position.lng, lat = ~position.lat,
        popup = ~paste("<strong>Nom de la station:</strong> ", "<strong>", name, "</strong>", "<br>",
                       "Nombre de place de vélo disponible: ", available_bike_stands, "<br>",
                       "Nombre de vélo : ", available_bikes),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Fonction pour filtrer les données en fonction du critère de tri
  filtered_data <- reactive({
    criteria <- input$sort_criteria
    sorted_data <- data[order(-data[[criteria]]), ]
    top_10_data <- sorted_data[1:10, ]
    return(top_10_data)
  })
  
  # Graphique pour le top 10 des stations
  output$top_stations_chart <- renderPlotly({
    top_10_data <- filtered_data()
    p <- plot_ly(data = top_10_data, x = ~name, y = ~as.integer(get(input$sort_criteria)), type = "bar") %>%
      layout(title = "Top 10 des Stations", xaxis = list(title = "Stations"), yaxis = list(title = "Valeurs"))
    p
  })
  
  # Graphique en boîte à moustaches
  output$boxplot <- renderPlotly({
    p <- plot_ly(y = boxplot_data, type = "box")
    p
  })
  
  output$plot_supports_vs_disponibles <- renderPlotly({
    p <- plot_ly(data, x = ~bike_stands, y = ~available_bike_stands, type = "scatter", mode = "markers") %>%
      layout(
        title = "Relation : Nombre Total de Supports vs. Supports Disponibles",
        xaxis = list(title = "Nombre Total de Supports à Vélo"),
        yaxis = list(title = "Supports à Vélo Disponibles")
      )
    p
  })
}

shinyApp(ui = ui, server = server)