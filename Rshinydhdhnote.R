# Installer les packages si nécessaire
if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
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

# Charger les bibliothèques
library(httr)
library(jsonlite)
library(shiny)
library(leaflet)
library(shinydashboard)

# Charger les données depuis l'API JCDecaux
base <- GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=8d2b71ec0cc951c380e6bb5da02b76a32f6f8559")
data <- fromJSON(rawToChar(base$content), flatten = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Velov's Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Données brutes", tabName = "raw_data", icon = icon("table")),
      menuItem("Carte Leaflet", tabName = "map", icon = icon("map")),
      menuItem("Graphique à Barres", tabName = "bar_chart", icon = icon("bar-chart")),
      menuItem("Nuage de Points", tabName = "scatter_plot", icon = icon("scatter-plot"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("raw_data", tableOutput("raw_data_table")),
      tabItem("map", leafletOutput("map")),
      tabItem("bar_chart", plotOutput("bar_chart")),
      tabItem("scatter_plot",
              sliderInput("start_date", "Date de début", min(data$last_update), max(data$last_update), value = min(data$last_update)),
              sliderInput("end_date", "Date de fin", min(data$last_update), max(data$last_update), value = max(data$last_update)),
              plotOutput("scatter_plot"),
              textOutput("connected_stations_rate"),
              textOutput("average_bikes_per_connected_station"),
              textOutput("utilization_rate"),
              textOutput("average_bikes_per_station")
      )
    )
  )
)

server <- function(input, output) {
  # Fonction pour rafraîchir les données
  refreshData <- function() {
    base <- GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=8d2b71ec0cc951c380e6bb5da02b76a32f6f8559")
    data <<- fromJSON(rawToChar(base$content), flatten = TRUE)
  }
  
  # Réagir au bouton de rafraîchissement
  observeEvent(input$refresh_data, {
    refreshData()  # Appelez la fonction pour rafraîchir les données
  })
  # Afficher les données brutes dans un tableau
  output$raw_data_table <- renderTable({
    refreshData()  # Rafraîchissez les données
    data  # Affichez les données mises à jour
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
  # Créer un nuage de points interactif pour le taux d'occupation vs le taux d'utilisation
  output$scatter_plot <- renderPlot({
    filtered_data <- data[data$last_update >= input$start_date & data$last_update <= input$end_date, ]
    plot_data <- data.frame(
      Occupation_Rate = (filtered_data$available_bike_stands / filtered_data$bike_stands) * 100,
      Utilization_Rate = (filtered_data$available_bikes / filtered_data$bike_stands) * 100
    )
    plot(plot_data$Occupation_Rate, plot_data$Utilization_Rate,
         xlab = "Taux d'Occupation des Stations",
         ylab = "Taux d'Utilisation des Vélos",
         main = "Taux d'Occupation vs. Taux d'Utilisation")
  })
  # Taux de Stations Connectées
  output$connected_stations_rate <- renderText({
    rate <- (sum(data$connected == TRUE) / length(data$connected)) * 100
    paste("Taux de Stations Connectées : ", round(rate, 2), "%")
  })
  
  # Vélos Disponibles Moyens par Station Connectée
  output$average_bikes_per_connected_station <- renderText({
    avg <- mean(data$available_bikes[data$connected == TRUE])
    paste("Vélos Disponibles Moyens par Station Connectée : ", round(avg, 2))
  })
  # Taux d'Utilisation des Vélos
  output$utilization_rate <- renderText({
    rate <- (sum(data$available_bikes) / sum(data$bike_stands)) * 100
    paste("Taux d'Utilisation des Vélos : ", round(rate, 2), "%")
  })
  
  # Nombre Moyen de Vélos par Station
  output$average_bikes_per_station <- renderText({
    avg <- mean(data$available_bikes)
    paste("Nombre Moyen de Vélos par Station : ", round(avg, 2))
  })
}
shinyApp(ui = ui, server = server)