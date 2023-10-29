# Nous vérifions si les packages nécessaires pour le scripts sont utilisés car Ces packages sont souvent utilisés pour des tâches spécialisées qui ne sont pas couvertes par les fonctions de base de R.
# Nous avions utilisons `require()` qui teste si un package est présent, et si ce n'est pas le cas, `install.packages()` l'installe automatiquement.
# Cela permet de nous assurés que toutes les fonctionnalités nécessaires, qui ne sont pas incluses dans R de base, sont disponibles.
if (!require(shiny)) {
  install.packages("shiny") # on utilise le packages Shiny pour créer des applications web interactives.
}
if (!require(leaflet)) {
  install.packages("leaflet") # on utilise le packages Leaflet pour les cartes interactives.
}
if (!require(httr)) {
  install.packages("httr") # on utilise le packages httr pour faire des requêtes HTTP.
}
if (!require(jsonlite)) {
  install.packages("jsonlite")  #on utilise le packages jsonlite pour travailler avec des données JSON.
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard") # on utilise le packages shinydashboard pour créer des tableaux de bord.
}
if (!require(plotly)) {
  install.packages("plotly")   # on utilise le packages plotly pour créer des graphiques interactifs.
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras") # on utilise le packages leaflet.extras pour des fonctionnalités supplémentaires de leaflet.
}
if (!require(ggplot2)) {
  install.packages("ggplot2")    #on utilise le packages ggplot2 pour la création de graphiques avancés.
}
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")  # on utilise le packages tidygeocoder pour le géocodage d'adresses.
}
if (!require(webshot)) {
  install.packages("webshot")  ## on utilise le packages webshot pour prendre des captures d'écran de web pages ou de visualisations Shiny.
}
# Nous interagissons avec l'API JCDecaux pour récupérer des informations actualisées sur les stations de vélo de Lyon.
# En utilisant `GET` du package `httr`, nous envoyons une requête et recevons en retour les données au format JSON.
# Puis, avec `fromJSON` de `jsonlite`, nous convertissons le JSON en un dataframe R, tout en aplanissant la structure pour une manipulation aisée.
# Charger les donnÃ©es depuis l'API JCDecaux
base <- httr::GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=8d2b71ec0cc951c380e6bb5da02b76a32f6f8559")
data <- jsonlite::fromJSON(httr::content(base, "text"), flatten = TRUE)

# Ici, nous analysons l'équilibre entre les supports de vélos et les vélos disponibles dans les stations.
# Le 'taux_desequilibre' mesure le pourcentage de supports inoccupés, ce qui nous indique s'il y a suffisamment de vélos pour les utilisateurs.
# Un nombre plus élevé signifierait que de nombreuses stations pourraient nécessiter un réapprovisionnement en vélos.
# Calcul du Taux de DÃ©sÃ©quilibre entre Supports et VÃ©los
taux_desequilibre <- ((sum(data$bike_stands) - sum(data$available_bikes)) / sum(data$bike_stands)) * 100

# Maintenant, nous calculons l'IDSV, un indice qui reflète la disponibilité globale des vélos et des places dans les stations.
# C'est un bon indicateur de la facilité avec laquelle les utilisateurs peuvent trouver un vélo ou un emplacement libre.
# Un IDSV élevé indique que les utilisateurs ont de bonnes chances de trouver ce qu'ils cherchent dans les stations.
# Calcul de l'Indice de DisponibilitÃ© des Stations de VÃ©los (IDSV)
IDSV <- ((sum(data$available_bike_stands) + sum(data$available_bikes)) / (sum(data$bike_stands) + sum(data$available_bikes))) * 100

# la fonction `library()` est essentielle pour activer les capacités des packages que nous avons précédément installés. 
# Cela équivaut à préparer l'environnement R avec une suite d'outils spécifiques et prêts à l'emploi.
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


# Nous initialisation le processus de géocodage inverse. 
# Nous fesons  l'extraction des coordonnées spatiales, spécifiquement latitude et longitude, pour la conversion subséquente en vecteurs de codes postaux.
# Ce procédé est destiné à contextualiser spatialement les données. 
donnees_geocode <- data[, c("position.lat", "position.lng")]
codes_postaux <- character(0)

# Nous avons exécuter la procédure de géocodage inverse via la fonction `reverse_geocode`.
# Exploitation des ressources cartographiques d'OpenStreetMap pour l'attribution exacte de codes postaux correspondant aux coordonnées géospatiales fournies.
# L'intégrité et la précision des données de sortie sont assurées par la qualité des services cartographiques utilisés.
resultat_geocode <- reverse_geocode(donnees_geocode, lat = position.lat, long = position.lng, method = 'osm', address = NULL, full_results = TRUE)
code_postal <- resultat_geocode$postcode

# Agrégation des données postales à la base principale de données.
# Cette incorporation métamorphose le jeu de données en ajoutant une dimension géographique informative,
# facilitant ainsi une exploration analytique géospatiale et la génération de connaissances locales spécialisées.
data$code_postal <- code_postal

# Ajoutez une variable rÃ©active pour stocker le titre du graphique en fonction du critÃ¨re sÃ©lectionnÃ©
# Création d'une variable réactive pour le titre du graphique
# Cette variable réactive va changer dynamiquement en fonction du critère de tri sélectionné par l'utilisateur dans l'interface.
# La fonction `switch()` est utilisée pour sélectionner le titre approprié en fonction de la valeur de `input$sort_criteria`.
# - Si l'utilisateur choisit 'available_bikes', le titre sera "Top 10 des stations en fonction du nombre de vélos".
# - Si le critère est 'bike_stands', le titre changera pour "Top 10 des stations en fonction du nombre de supports à vélo".
# - Et si 'available_bike_stands' est sélectionné, le titre indiquera "Top 10 des stations en fonction des vélos disponibles".
# Ainsi, le titre reflète précisément le critère de tri actuel,
selected_criteria_text <- reactive({
  switch(input$sort_criteria,
         "available_bikes" = "Top 10 des stations en fonction du nombre de vÃ©los",
         "bike_stands" = "Top 10 des stations en fonction du nombre de supports Ã  vÃ©lo",
         "available_bike_stands" = "Top 10 des stations en fonction des vÃ©los disponibles"
  )
})
# Création de l'interface utilisateur (UI) pour le tableau de bord Velov.
ui <- dashboardPage(
  # En-tête du tableau de bord avec un titre.
  dashboardHeader(title = "Velov's Dashboard"),
  # Barre latérale du tableau de bord contenant des éléments de menu.
  dashboardSidebar(
    sidebarMenu(
      # Onglet "Tableau de bord" avec une icône.
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      # Onglet "Carte Leaflet" avec une icône.
      menuItem("Carte Leaflet", tabName = "map", icon = icon("map")),
      # Onglet "Tableau de bord avec Statistiques" avec une icône.
      menuItem("Tableau de bord avec Statistiques", tabName = "dashboard_stats", icon = icon("dashboard")),
      # Onglet "Tableau de données brutes" avec une icône.
      menuItem("Tableau de donnÃ©es brutes", tabName = "raw_data", icon = icon("table")),
      # Onglet "Exporter PNG" avec une icône pour exporter en PNG.
      menuItem("Exporter PNG", tabName = "export_png", icon = icon("file-image"))
    )
  ),
  # Corps du tableau de bord avec des onglets.
  dashboardBody(
    tabItems(
      # Onglet "Tableau de bord".
      tabItem("dashboard",
              fluidRow(
                # Sélecteur pour filtrer les données par code postal.
                selectInput("filter_postcode_dashboard", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal)))
                )
              ),
              fluidRow(
                # Boîtes de valeur pour afficher des statistiques.
                valueBoxOutput("value_box_available_bikes"),
                valueBoxOutput("value_box_total_bikes"),
                valueBoxOutput("value_box_total_stations")
              ),
              fluidRow(
                # Sélecteur pour trier les données par critère.
                selectInput("sort_criteria", "Trier par critère :",
                            choices = c("Nombre de vélos disponible" = "available_bikes", "Nombre de supports à vélo" = "bike_stands", "Vélos disponibles" = "available_bike_stands"),
                            selected = "available_bikes"
                )
              ),
              fluidRow(
                # Graphique interactif (utilisation de Plotly).
                plotlyOutput("top_stations_chart", width = "100%")
              )
      ),
      
      # Onglet "Tableau de données brutes".
      tabItem("raw_data",
              fluidRow(
                # Tableau pour afficher les données brutes.
                column(12, tableOutput("raw_data_table"))
              )
      ),
      
      # Onglet "Carte Leaflet".
      tabItem("map",
              fluidRow(
                # Carte Leaflet pour afficher des données géospatiales.
                column(12, leafletOutput("map"))
              ),
              fluidRow(
                # Sélecteur pour filtrer les données sur la carte par code postal.
                column(12, selectInput("filter_postcode_map", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal))))
                )
              )
      ),
      
      # Onglet "Tableau de bord avec Statistiques".
      tabItem("dashboard_stats",
              fluidRow(
                # Sélecteur pour filtrer les données par code postal.
                selectInput("filter_postcode_stats", "Filtrer par code postal:", c("Tous", sort(unique(data$code_postal)))
                ),
                # Boîtes de valeur pour afficher des statistiques.
                valueBoxOutput("value_box_taux_utilisation"),
                valueBoxOutput("value_box_indice_disponibilite"),
                valueBoxOutput("value_box_taux_desequilibre")
              ),
              fluidRow(
                # Graphique en secteurs (utilisation de Plotly).
                plotlyOutput("pie_chart", width = "100%")
              )
      ),
      
      # Onglet "Exporter PNG".
      tabItem("export_png",
              fluidRow(
                # Bouton de téléchargement pour exporter le graphique en PNG.
                downloadButton("export_pie_chart_button", "Exporter en PNG", type = "image/png"),
                # Bouton d'action pour rafraîchir les données.
                actionButton("refresh_button", "Rafraîchir les données")
              )
      )
    )
  )
)
# Fonction du serveur pour l'application Shiny
server <- function(input, output) {
  
  # Fonction pour mettre à jour les données lorsque le bouton "Rafraîchir les données" est cliqué
  observeEvent(input$refresh_button, {
    refresh_data()  # Appelle la fonction refresh_data() pour actualiser les données.
  })
  
  # Afficher les données brutes dans un tableau
  output$raw_data_table <- renderTable({
    data  # Affiche les données brutes stockées dans "data".
  })
  # Créez une carte Leaflet automatique
  output$map <- renderLeaflet({
    
    # Vérifiez si l'option "Tous" est sélectionnée dans le filtre du code postal
    if (input$filter_postcode_map == "Tous") {
      # Si "Tous" est sélectionné, n'appliquez aucun filtre, utilisez l'ensemble des données.
      filtered_map_data <- data
    } else {
      # Sinon, filtrez les données en fonction de l'option sélectionnée.
      filtered_map_data <- data[data$code_postal == input$filter_postcode_map, ]
    }
    
    # Créez un objet Leaflet basé sur les données filtrées
    leaflet(filtered_map_data) %>%
      
      # Ajoutez une couche de tuiles de fond (carte de base)
      addTiles() %>%
      
      # Ajoutez un contrôle pour activer le mode plein écran en haut à droite de la carte
      addFullscreenControl(position = "topright", pseudoFullscreen = TRUE) %>%
      
      # Ajoutez des marqueurs circulaires sur la carte
      addCircleMarkers(
        lng = ~position.lng, lat = ~position.lat,  # Utilisez les coordonnées longitudinales et latitudinales des données
        popup = ~paste(
          "<strong>Nom de la station:</strong> ", "<strong>", name, "</strong>", "<br>",
          "Nombre de places de vélo disponibles: ", available_bike_stands, "<br>",
          "Nombre de vélos : ", available_bikes
        ),
        clusterOptions = markerClusterOptions()  # Permet de regrouper les marqueurs en clusters
      )
  })
  # Fonction pour filtrer les données en fonction du critère de tri
  filtered_data_dashboard <- reactive({
    criteria <- input$sort_criteria  # Récupération du critère de tri depuis l'entrée de l'utilisateur
    sorted_data <- data[order(-data[[criteria]]), ]  # Triez les données en fonction du critère sélectionné
    top_10_data <- sorted_data[1:10, ]  # Sélectionnez les 10 premières lignes des données triées (top 10)
    return(top_10_data)  # Retournez les données filtrées
  })
  
  # Graphique pour le top 10 des stations dans l'onglet "Tableau de bord"
  output$top_stations_chart <- renderPlotly({
    top_10_data <- filtered_data_dashboard()  # Obtenir les données filtrées à partir de la fonction réactive
    p <- plot_ly(data = top_10_data, x = ~reorder(name, -as.integer(get(input$sort_criteria)), FUN = sum), y = ~as.integer(get(input$sort_criteria)), type = "bar") %>%
      layout(title = paste("Top 10 des Stations en fonction de", input$sort_criteria), xaxis = list(title = "Stations"), yaxis = list(title = input$sort_criteria))
    p  # Retournez le graphique Plotly généré
  })
  # Graphique à secteurs des taux de disponibilité dans l'onglet "Tableau de bord avec statistiques"
  output$pie_chart <- renderPlotly({
    # Copiez les données dans une nouvelle variable filtrée (initialisation)
    filtered_data_stats <- data
    # Vérifiez si l'utilisateur a sélectionné une option de filtrage spécifique
    if (input$filter_postcode_stats != "Tous") {
      # Si oui, filtrez les données en fonction de l'option sélectionnée
      filtered_data_stats <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    # Créez un dataframe contenant les catégories et les valeurs pour le graphique à secteurs
    availability_data <- data.frame(
      Category = c("Vélos disponibles", "Nombre de supports vides"),
      Value = c(sum(filtered_data_stats$available_bikes), sum(filtered_data_stats$bike_stands - filtered_data_stats$available_bikes))
    )
    # Créez le graphique à secteurs avec Plotly
    p <- plot_ly(availability_data, labels = ~Category, values = ~Value, type = "pie") %>%
      layout(title = "Taux de Disponibilité par Catégorie", showlegend = TRUE) %>%
      layout(pie = list(textinfo = "label+percent")) %>%
      add_trace(textfont = list(color = "white"))
    # Renvoyez le graphique créé pour affichage
    p
  })
  # Rendu des valueBox sous l'onglet "dashboard"
  output$value_box_available_bikes <- renderValueBox({
    # Vérifie si l'option "Tous" est sélectionnée dans le filtre du code postal
    if (input$filter_postcode_dashboard == "Tous") {
      # Si "Tous" est sélectionné, utilisez l'ensemble des données
      filtered_data <- data
    } else {
      # Sinon, filtrez les données en fonction de l'option sélectionnée
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    # Calculez la somme des vélos disponibles dans les données filtrées
    value <- sum(filtered_data$available_bikes)
    # Créez une boîte de valeur pour afficher le résultat
    valueBox(
      value = value,  # Le paramètre 'value' définit la valeur numérique à afficher (nombre de vélos disponibles)
      subtitle = "Nombre de Vélos Disponibles",  # Le paramètre 'subtitle' spécifie un sous-titre ou une description (nombre de vélos disponibles)
      icon = icon("bicycle"),  # Le paramètre 'icon' définit l'icône (une icône de bicyclette)
      color = "blue",  # Le paramètre 'color' définit la couleur de la boîte (bleu)
      width = 4  # Le paramètre 'width' détermine la largeur de la boîte
    )
  })
  
  # Rendu des valueBox sous l'onglet "dashboard"
  output$value_box_total_bikes <- renderValueBox({
    # Vérifie si l'option "Tous" est sélectionnée dans le filtre du code postal
    if (input$filter_postcode_dashboard == "Tous") {
      # Si "Tous" est sélectionné, utilisez l'ensemble des données
      filtered_data <- data
    } else {
      # Sinon, filtrez les données en fonction de l'option sélectionnée
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    # Calculez la somme du nombre total de supports à vélo dans les données filtrées
    value <- sum(filtered_data$bike_stands)
    # Créez une boîte de valeur pour afficher le résultat
    valueBox(
      value = value,  # Le paramètre value définit la valeur numérique à afficher (nombre total de supports à vélo)
      subtitle = "Nombre Total de Vélos",  # Le paramètre subtitle spécifie un sous-titre ou une description
      icon = icon("bicycle"),  # Le paramètre icon définit l'icône (une icône de vélo)
      color = "purple",  # Le paramètre color définit la couleur de la boîte (violet)
      width = 4  # Le paramètre width détermine la largeur de la boîte
    )
  })
  # Rendu des valueBox sous l'onglet "dashboard"
  output$value_box_total_stations <- renderValueBox({
    # Vérifie si l'option "Tous" est sélectionnée dans le filtre du code postal
    if (input$filter_postcode_dashboard == "Tous") {
      # Si "Tous" est sélectionné, utilisez l'ensemble des données
      filtered_data <- data
    } else {
      # Sinon, filtrez les données en fonction de l'option sélectionnée
      filtered_data <- data[data$code_postal == input$filter_postcode_dashboard, ]
    }
    
    # Calculez le nombre total de stations uniques dans les données filtrées
    value <- length(unique(filtered_data$name))
    
    # Créez une boîte de valeur pour afficher le résultat
    valueBox(
      value = value,  # Le paramètre value définit la valeur numérique à afficher (nombre total de stations)
      subtitle = "Nombre Total de Stations",  # Le paramètre subtitle spécifie un sous-titre ou une description
      icon = icon("bicycle"),  # Le paramètre icon définit l'icône (une icône de vélo)
      color = "green",  # Le paramètre color définit la couleur de la boîte (vert)
      width = 4  # Le paramètre width détermine la largeur de la boîte
    )
  })
  
  # Rendu des valueBox sous l'onglet "dashboard_stats"
  
  # Crée un élément "valueBox" appelé "value_box_taux_utilisation"
  # dans l'onglet "dashboard_stats"
  output$value_box_taux_utilisation <- renderValueBox({
    
    # Copie le jeu de données original "data" dans une nouvelle variable
    filtered_data_stats <- data
    
    # Vérifie si l'entrée "filter_postcode_stats" n'est pas égale à "Tous"
    if (input$filter_postcode_stats != "Tous") {
      
      # Si la condition est vraie, filtre les données en ne conservant que les lignes où la colonne "code_postal" correspond à la valeur sélectionnée dans "filter_postcode_stats". En d'autres termes, filtre les donnée  en fonction du code postal sélectionné par l'utilisateur.
      filtered_data_stats <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    
    # Le résultat final est la valeur de "filtered_data_stats", qui peut être
    # utilisée pour générer le contenu du "valueBox".
  })
  # Rendu des valueBox sous l'onglet "dashboard_stats"
  output$value_box_taux_utilisation <- renderValueBox({
    filtered_data_stats <- data
    if (input$filter_postcode_stats != "Tous") {
      filtered_data_stats <- data[data$code_postal == input$filter_postcode_stats, ]
    }
    taux_utilisation <- (sum(filtered_data_stats$available_bikes) / sum(filtered_data_stats$bike_stands)) * 100
    valueBox(
      value = round(taux_utilisation, 2),
      subtitle = "Taux d'Utilisation",
      icon = icon("bicycle"),
      color = "orange",
      width = 4
    )
  })
  
  output$value_box_indice_disponibilite <- renderValueBox({
    valueBox(
      value = round(IDSV, 2),
      subtitle = "Indice de DisponibilitÃ©",
      icon = icon("check-circle"),
      color = "blue",
      width = 4
    )
  })
  
  output$value_box_taux_desequilibre <- renderValueBox({
    valueBox(
      value = round(taux_desequilibre, 2),
      subtitle = "Taux de DÃ©sÃ©quilibre",
      icon = icon("warning"),
      color = "red",
      width = 4
    )
  })
  
  # Nouvelle fonction pour exporter le graphique ÃÂ  secteurs en tant qu'image PNG
  # CrÃ©e une sortie "export_pie_chart_button" dans une application Shiny
  output$export_pie_chart_button <- downloadHandler(
    filename = function() {
      "exported_pie_chart.png"  # Nom du fichier de sortie en PNG
    },
    content = function(file) {
      # CrÃ©ez le graphique Ã  secteurs avec plotly
      # CrÃ©ez un graphique Ã  secteurs (camembert) en utilisant Plotly.
      # Les donnÃ©es sont spÃ©cifiÃ©es Ã  partir d'un dataframe avec deux catÃ©gories : "VÃ©los disponibles" et "Nombre de supports vides".
      # Les valeurs correspondent aux sommes des vÃ©los disponibles et des supports vides dans le dataframe 'data'.
      p <- plot_ly(data = data.frame(Category = c("VÃ©los disponibles", "Nombre de supports vides"),
                                     Value = c(sum(data$available_bikes), sum(data$bike_stands - data$available_bikes))),
                   labels = ~Category, values = ~Value, type = "pie") %>%
        layout(title = "Taux de DisponibilitÃ© par CatÃ©gorie", showlegend = TRUE) %>%
        layout(pie = list(textinfo = "label+percent"))
      
      # Sauvegardez le graphique en tant qu'image PNG
      png(file, width = 1200, height = 600)  # Ajustez la largeur et la hauteur selon vos besoins
      p <- plot_ly(data = data.frame(Category = c("VÃ©los disponibles", "Nombre de supports vides"),
                                     Value = c(sum(data$available_bikes), sum(data$bike_stands - data$available_bikes))),
                   labels = ~Category, values = ~Value, type = "pie") %>%
        # la mise en page du graphique.
        layout(title = "Taux de DisponibilitÃ© par CatÃ©gorie", showlegend = TRUE) %>%
        #  la mise en page du graphique Ã  secteurs (camembert) pour afficher des informations spÃ©cifiques.
        layout(pie = list(textinfo = "label+percent"))
      # Affichez le graphique.
      print(p)
      # Terminez la sauvegarde de l'image en PNG.
      dev.off()
    }
  )
}

shinyApp(ui = ui , server = server )