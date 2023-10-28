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
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")
}
if (!require(webshot)) {
  install.packages("webshot")
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
library(ggplot2)
library(tidygeocoder)
library(webshot)

# Filtre codes postaux
donnees_geocode <- data[, c("position.lat", "position.lng")]
codes_postaux <- character(0)

# Effectuez le géocodage inverse pour obtenir le code postal
resultat_geocode <- reverse_geocode(donnees_geocode, lat = position.lat, long = position.lng, method = 'osm', address = NULL, full_results = TRUE)
code_postal <- resultat_geocode$postcode

data$code_postal <- code_postal

ui <- dashboardPage(
  dashboardHeader(title = "Velov's Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte Leaflet", tabName = "map", icon = icon("map")),
      menuItem("Tableau de bord avec Statistiques", tabName = "dashboard_stats", icon = icon("dashboard")),
      menuItem("Tableau de données brutes", tabName = "raw_data", icon = icon("table")),
      # Ajouter un nouvel onglet d'exportation PNG
      menuItem("Exporter PNG", tabName = "export_png", icon = icon("file-image"))
    )
  ),
  dashboardBody(
    # Section CSS personnalisé
    tags$head(
      tags$style(HTML("
        /* Votre CSS personnalisé ici */
        body {
          background-color: #f5f5f5;
        }
        .value-box {
          border: 2px solid #0073e6;
        }
      "))
    ),
    # Fin de la section CSS personnalisé
    tabItems(
      tabItem("dashboard",
              fluidRow(
                selectInput("filter_postcode_dashboard", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal)))
                )
              ),
              fluidRow(
                valueBoxOutput("value_box_available_bikes"),
                valueBoxOutput("value_box_total_bikes"),
                valueBoxOutput("value_box_total_stations")
              ),
              fluidRow(
                selectInput("sort_criteria", "Trier par critère :",
                            choices = c("Nombre de vélos" = "available_bikes", "Nombre de supports à vélo" = "bike_stands", "Vélos disponibles" = "available_bike_stands"),
                            selected = "available_bikes"
                )
              ),
              fluidRow(
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
              ),
              fluidRow(
                column(12, selectInput("filter_postcode_map", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal))))
                )
              )
      ),
      tabItem("dashboard_stats",
              fluidRow(
                selectInput("filter_postcode_stats", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal)))
                ),
                valueBoxOutput("value_box_taux_utilisation"),
                valueBoxOutput("value_box_indice_disponibilite"),
                valueBoxOutput("value_box_taux_desequilibre")
              ),
              fluidRow(
                plotlyOutput("pie_chart", width = "100%")
              )
      ),
      # Nouvel onglet d'exportation PNG
      tabItem("export_png",
              fluidRow(
                downloadButton("export_button", "Exporter en PNG", type = "image/png")  # Spécifiez le type comme "image/png"
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
    if (input$filter_postcode_map == "Tous") {
      # Si "Tous" est sélectionné, n'appliquez aucun filtre, utilisez l'ensemble des données.
      filtered_map_data <- data
    } else {
      # Sinon, filtrez les données en fonction de l'option sélectionnée.
      filtered_map_data <- data[data$code_postal == input$filter_postcode_map, ]
    }
    
    leaflet(filtered_map_data) %>%
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
  filtered_data_dashboard <- reactive({
    criteria <- input$sort_criteria
    sorted_data <- data[order(-data[[criteria]]), ]
    top_10_data <- sorted_data[1:10, ]
    return(top_10_data)
  })
  
  # Graphique pour le top 10 des stations dans l'onglet "Tableau de bord"
  output$top_stations_chart <- renderPlotly({
    top_10_data <- filtered_data_dashboard()
    p <- plot_ly(data = top_10_data, x = ~reorder(name, -as.integer(get(input$sort_criteria)), FUN = sum), y = ~as.integer(get(input$sort_criteria)), type = "bar") %>%
      layout(title = "Top 10 des Stations", xaxis = list(title = "Stations"), yaxis = list(title = "Valeurs"))
    p
  })
  
  # Graphique à secteurs des taux de disponibilité dans l'onglet "Tableau de bord avec statistiques"
  output$pie_chart <- renderPlotly({
    filtered_data_stats <- data
    if (input$filter_postcode_stats != "Tous") {
      filtered_data_stats <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    availability_data <- data.frame(
      Category = c("Vélos disponibles", "Nombre de supports vides"),
      Value = c(sum(filtered_data_stats$available_bikes), sum(filtered_data_stats$bike_stands - filtered_data_stats$available_bikes))
    )
    p <- plot_ly(availability_data, labels = ~Category, values = ~Value, type = "pie") %>%
      layout(title = "Taux de Disponibilité par Catégorie", showlegend = TRUE) %>%
      layout(pie = list(textinfo = "label+percent")) %>%
      add_trace(textfont = list(color = "white"))
    p
  })
  
  # Rendu des valueBox sous l'onglet "dashboard"
  output$value_box_available_bikes <- renderValueBox({
    if (input$filter_postcode_dashboard == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    value <- sum(filtered_data$available_bikes)
    valueBox(
      value = value,
      subtitle = "Nombre de Vélos Disponibles",
      icon = icon("bicycle"),
      color = "blue",
      width = 4
    )
  })
  
  output$value_box_total_bikes <- renderValueBox({
    if (input$filter_postcode_dashboard == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    value <- sum(filtered_data$bike_stands)
    valueBox(
      value = value,
      subtitle = "Nombre Total de Vélos",
      icon = icon("bicycle"),
      color = "purple",
      width = 4
    )
  })
  
  output$value_box_total_stations <- renderValueBox({
    if (input$filter_postcode_dashboard == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    value <- length(unique(filtered_data$name))
    valueBox(
      value = value,
      subtitle = "Nombre Total de Stations",
      icon = icon("map-marker"),
      color = "purple",
      width = 4
    )
  })
  
  # Rendu des valueBox sous l'onglet "dashboard_stats"
  output$value_box_taux_utilisation <- renderValueBox({
    if (input$filter_postcode_stats == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    rate <- (sum(filtered_data$available_bikes) / sum(filtered_data$bike_stands)) * 100
    valueBox(
      value = paste0(round(rate, 2), "%"),
      subtitle = "Taux d'Utilisation des Vélos",
      icon = icon("bicycle"),
      color = "red",
      width = 4
    )
  })
  
  output$value_box_indice_disponibilite <- renderValueBox({
    if (input$filter_postcode_stats == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    value <- paste0(round(IDSV, 2), "%")
    valueBox(
      value = value,
      subtitle = "Indice de Disponibilité des Stations de Vélos",
      icon = icon("bicycle"),
      color = "blue",
      width = 4
    )
  })
  
  output$value_box_taux_desequilibre <- renderValueBox({
    if (input$filter_postcode_stats == "Tous") {
      filtered_data <- data
    } else {
      filtered_data <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    value <- paste0(round(taux_desequilibre, 2), "%")
    valueBox(
      value = value,
      subtitle = "Taux de Déséquilibre entre Supports et Vélos",
      icon = icon("exclamation-triangle"),
      color = "orange",
      width = 4
    )
  })
  
  # Nouvelle fonction pour générer un graphique pour l'exportation PNG
  output$export_plot <- renderPlot({
    p <- ggplot(data = data, aes(x = code_postal, fill = code_postal)) +
      geom_bar() +
      labs(title = "Distribution des codes postaux des stations", x = "Code postal", y = "Nombre de stations")
    print(p)  # Utilisez print(p) au lieu de simplement p
  })
  
  # Gestionnaire de téléchargement pour l'exportation PNG
  output$export_button <- downloadHandler(
    filename = function() {
      "exported_plot.png"  # Nom du fichier de sortie en PNG
    },
    content = function(file) {
      p <- output$export_plot()
      
      # Utiliser ggsave pour sauvegarder le graphique en tant qu'image PNG
      ggsave(file, p, device = "png", width = 800, height = 600, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)