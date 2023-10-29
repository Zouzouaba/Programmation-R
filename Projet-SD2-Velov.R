# Liste des packages à installer
packages_to_install <- c("plotly", "ggmap", "maps", "mapproj", "shiny", "DT", "leaflet", "readr", "jsonlite", "RMySQL", "httr", "shinydashboard", "tidyverse","kableExtra","orca","webshot","rsconnect")

# Installer les packages qui ne sont pas déjà installés
for (package in packages_to_install) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Charger les librairies nécessaires
library(maps)
library(mapproj)
library(shiny)
library(leaflet)
library(jsonlite)
library(RMySQL)
library(httr)
library(readr)
library(DT)
library(data.table)
library(httr)
library(plotly)
library(shinydashboard)
library(dplyr)
library(tidyverse)

#packages nécessaires pour les sorties
library(knitr)
library(kableExtra)
library(dplyr)
library(ggmap)
library(orca)
library(webshot)
library(rsconnect)

taille <- 12  # Taille de la police des df en sortie

# Charger les données "Code_Postaux.csv" et les stocker dans un dataframe
code_postaux_df <- read.csv("Code_Postaux.csv")

base <- GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=14f40934761da90ce82ff1d7ceae62202dba4788")
velov1 <- fromJSON(rawToChar(base$content), flatten = TRUE)

# Créer une fonction pour obtenir les nouvelles données velov depuis l'API (à adapter selon votre API)
getVelovData <- function() {
  base <- GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=14f40934761da90ce82ff1d7ceae62202dba4788")
  velov1 <- fromJSON(rawToChar(base$content), flatten = TRUE)
  
  return(velov1)
}

# Effectuer la jointure gauche entre velov et code_postaux_df
velov <- left_join(velov1, code_postaux_df, by = c("number" = "number"))
velov <- velov %>%
  select(1:3, CODE_POSTAL, 4:14)  # Remplacez ncol(velov) par le nombre total de colonnes dans votre dataframe

# Créer un dataframe pour les communes
nom_commune <- unique(velov$contract_name)
id_commune <- 1:length(velov$number)
communes <- data.frame(id_commune, nom_commune, stringsAsFactors = FALSE)

names(velov)[3] <- "NOM_STATION"
names(velov)[4] <- "CODE_POSTAL"
names(velov)[8] <- "CAPACITE"
names(velov)[9] <- "STANDS_DISPO"
names(velov)[10] <- "VELOS_DISPO"
names(velov)[13] <- "LATITUDE"
names(velov)[14] <- "LONGITUDE"
velov$CODE_POSTAL <- as.character(velov$CODE_POSTAL)

# Définir l'interface utilisateur (UI)
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Tableau de Bord Velov"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stations Proches", tabName = "stations_proches", icon = icon("bicycle")),
      menuItem("Statistiques Graphiques", tabName = "stats_graphiques", icon = icon("bar-chart")), 
      menuItem("Base de Données", tabName = "base_de_donnees", icon = icon("database"))
    )
  ),
  dashboardBody(
    includeCSS("style.css"),
    
    tabItems(
      tabItem(
        tabName = "stations_proches",
        fluidRow(
          sidebarPanel(
            selectInput("var", label = "Choix de la commune", choices = communes$nom_commune, selected = "Percent White"),
            selectInput("code_postal", label = "Code Postal", choices = c("", sort(unique(velov$CODE_POSTAL))), selected = NULL),
            verbatimTextOutput("value"),
            sliderInput(inputId = "id_slider", label = "Nombre de lignes", min = 0, max = 10, value = 5, step = 1),
            actionButton(inputId = "refreshData", label = "Rafraîchir !", icon = icon("refresh"))
          ),
          mainPanel(
            # Sortie pour afficher la carte entière
            leafletOutput("map_entiere"),
            DTOutput("filteredTableProches")
          )
        )
      ),
      tabItem(tabName = "stats_graphiques",
              fluidRow(
                column(4,
                       checkboxInput("useFilteredData", "Avec filtre code postal", value = FALSE),
                       valueBoxOutput("totalVelosBox"),
                       valueBoxOutput("stationsBox"),
                       downloadButton("downloadGraph", "Exporter les graphiques en PNG"),
                       
                ),
                column(8,
                       plotlyOutput("graphique"),
                       plotlyOutput("pieChart")
                )
              )
      )
      
      ,
      tabItem(
        tabName = "base_de_donnees",
        fluidRow(
          actionButton(inputId = "export_filter", label = "Exporter en csv data filtrée"),
          actionButton(inputId = "export_all", label = "Exporter en csv data complète"),
          dataTableOutput("databaseTable")  # Créez une sortie de tableau pour l'onglet "Base_de_données"
          
        )
      )
    )
  )
)


server <- function(input, output, session) {
  initial_lat = 45.757814
  initial_lng = 4.832011
  initial_zoom = 10
  
  updatedVelovData <- eventReactive(input$refreshData, {
    # Récupère les données Velov mises à jour depuis l'API
    velov_data <- getVelovData()
    return(velov_data)
  })
  
  observeEvent(input$refreshData, {
    # Met à jour le dataframe velov avec les données mises à jour
    velov <- head(updatedVelovData())
    
    # Réinitialise la valeur du sélecteur de code postal à NULL
    updateSelectInput(session, "code_postal", selected = " ")
    
    # Vous pouvez ajouter ici d'autres opérations à effectuer après la mise à jour des données.
  })
  
  observeEvent(input$export_filter, {
    # Obtient les données filtrées pour l'export
    filtered_data_to_export <- filtered_data()
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
    csv_file_path <- paste("filtered_velov_data_", timestamp, ".csv", sep = "")
    
    # Exporte les données filtrées vers un fichier CSV
    write.csv(filtered_data_to_export, file = csv_file_path, row.names = FALSE)
    
    showModal(
      modalDialog(
        title = "Export CSV",
        "Les données filtrées ont été exportées avec succès.",
        footer = modalButton("OK")
      )
    )
  })
  
  observeEvent(input$export_all, {
    # Obtient l'ensemble des données Velov pour l'export
    all_velov_data_to_export <- velov
    timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
    csv_file_path <- paste("all_velov_data_", timestamp, ".csv", sep = "")
    
    # Exporte toutes les données Velov vers un fichier CSV
    write.csv(all_velov_data_to_export, file = csv_file_path, row.names = FALSE)
    
    showModal(
      modalDialog(
        title = "Export CSV",
        "Toutes les données Velov ont été exportées avec succès.",
        footer = modalButton("OK")
      )
    )
  })
  
  output$downloadGraph <- downloadHandler(
    filename = function() {
      "graphique.png"  # Nom du fichier de téléchargement
    },
    content = function(file) {
      selector_css <- "graphique"  # Sélecteur CSS pour la région du graphique à capturer
      webshot::webshot("https://bistouflex.shinyapps.io/Projet-SD2-Velov/", file, selector = selector_css, cliprect = "viewport")
    }
  )
  
  filtered_data <- reactive({
    code_postal <- input$code_postal
    if (is.null(code_postal) || code_postal == "") {
      # Si input$code_postal est nul ou vide, affiche toutes les données Velov
      filtered_data <- velov %>%
        select(CODE_POSTAL, NOM_STATION, CAPACITE, STANDS_DISPO, VELOS_DISPO, LATITUDE, LONGITUDE)
    } else {
      # Sinon, filtre les données en fonction du code postal sélectionné
      filtered_data <- velov %>%
        filter(CODE_POSTAL == code_postal) %>%
        select(CODE_POSTAL, NOM_STATION, CAPACITE, STANDS_DISPO, VELOS_DISPO, LATITUDE, LONGITUDE)
    }
    return(filtered_data)
  })
  
  # Crée une réactive pour mettre à jour le nombre de lignes du slider
  observe({
    filtered_data_rows <- nrow(filtered_data())
    updateSliderInput(session, "id_slider", label = "Nombre de lignes", min = 0, max = filtered_data_rows, value = filtered_data_rows, step = 1)
  })
  
  output$filteredTableProches <- renderDT({
    # Affiche les données filtrées dans la DataTable
    filtered_data()
  })
  
  output$databaseTable <- renderDataTable({
    # Affiche la base de données dans l'onglet "Base_de_données"
    velov
  })
  
  output$map_entiere <- renderLeaflet({
    # Génère la carte entière
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = filtered_data()$LATITUDE, lng = filtered_data()$LONGITUDE,
                 popup = paste(filtered_data()$NOM_STATION, "<br>",
                               filtered_data()$ADRESSE, "<br>",
                               "Vélos disponibles :", filtered_data()$VELOS_DISPO),
                 clusterOptions = filtered_data()$CODE_POSTAL,
                 label = filtered_data()$NOM_STATION) %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  
  output$graphique <- renderPlotly({
    if(input$useFilteredData){
      # Résumé des données filtrées pour le graphique
      code_postal_summary <- filtered_data() %>%
        group_by(CODE_POSTAL) %>%
        summarize(Total_Velos = sum(VELOS_DISPO),
                  Nombre_Stations = n(),
                  Velos_Moyen_par_Station = Total_Velos / Nombre_Stations)
      
      plot_ly(data = code_postal_summary, x = ~CODE_POSTAL, y = ~Total_Velos, type = 'bar', color = I('darkred')) %>%
        layout(title = "Nombre total de Velo'v par Code Postal",
               xaxis = list(title = "Code Postal", tickangle = -45),
               yaxis = list(title = "Nombre Total de Velo'v Disponibles"))
    }
    else{
      # Résumé des données Velov pour le graphique
      code_postal_summary <- velov %>%
        group_by(CODE_POSTAL) %>%
        summarize(Total_Velos = sum(VELOS_DISPO),
                  Nombre_Stations = n(),
                  Velos_Moyen_par_Station = Total_Velos / Nombre_Stations)
      
      plot_ly(data = code_postal_summary, x = ~CODE_POSTAL, y = ~Total_Velos, type = 'bar', color = I('darkred')) %>%
        layout(title = "Nombre total de Velo'v par Code Postal",
               xaxis = list(title = "Code Postal", tickangle = -45),
               yaxis = list(title = "Nombre Total de Velo'v Disponibles"))
    }
  })
  
  output$pieChart <- renderPlotly({
    if(input$useFilteredData){
      # Calcule les valeurs pour le diagramme circulaire des données filtrées
      filtered_data_summary <- summarise(filtered_data(),
                                         STANDS_DISPO = sum(STANDS_DISPO),
                                         VELOS_DISPO = sum(VELOS_DISPO),
                                         CAPACITE = sum(CAPACITE))
      
      # Crée un dataframe pour le diagramme circulaire
      pie_data <- data.frame(
        label = c("STANDS_DISPO", "VELOS_DISPO", "CAPACITE RESTANTE"),
        value = c(filtered_data_summary$STANDS_DISPO, filtered_data_summary$VELOS_DISPO,
                  filtered_data_summary$CAPACITE - (filtered_data_summary$STANDS_DISPO + filtered_data_summary$VELOS_DISPO)),
        color = c("blue", "green", "red")
      )
      
      # Génère le diagramme circulaire
      pie_chart <- plot_ly(pie_data, labels = ~label, values = ~value, type = "pie", 
                           marker = list(colors = ~color))
      
      pie_chart %>% 
        layout(title = "Répartition des Vélos Disponibles",
               showlegend = TRUE)
    }
    else{
      # Calcule les valeurs pour le diagramme circulaire des données Velov complètes
      filtered_data_summary <- summarise(velov,
                                         STANDS_DISPO = sum(STANDS_DISPO),
                                         VELOS_DISPO = sum(VELOS_DISPO),
                                         CAPACITE = sum(CAPACITE))
      
      # Crée un dataframe pour le diagramme circulaire
      pie_data <- data.frame(
        label = c("STANDS_DISPO", "VELOS_DISPO", "CAPACITE RESTANTE"),
        value = c(filtered_data_summary$STANDS_DISPO, filtered_data_summary$VELOS_DISPO,
                  filtered_data_summary$CAPACITE - (filtered_data_summary$STANDS_DISPO + filtered_data_summary$VELOS_DISPO)),
        color = c("blue", "green", "red")
      )
      
      # Génère le diagramme circulaire
      pie_chart <- plot_ly(pie_data, labels = ~label, values = ~value, type = "pie", 
                           marker = list(colors = ~color))
      
      pie_chart %>% 
        layout(title = "Répartition des Vélos Disponibles",
               showlegend = TRUE)
    }
  })
  
  output$totalVelosBox <- renderValueBox({
    if(input$useFilteredData){
      # Utilise les données filtrées si nécessaire
      velov_data <- filtered_data()
      totalVelos <- sum(velov_data$VELOS_DISPO)
      valueBox(
        totalVelos,
        "Total Vélos",
        icon = icon("bicycle"),
        color = "blue"
      )
    } else {
      velov_data <- velov
      totalVelos <- sum(velov_data$VELOS_DISPO)
      valueBox(
        totalVelos,
        "Total Vélos",
        icon = icon("bicycle"),
        color = "blue"
      )
    }
  })
  
  output$stationsBox <- renderValueBox({
    if(input$useFilteredData){
      # Utilise les données filtrées si nécessaire
      velov_data <- filtered_data()
      totalStations <- nrow(velov_data)
      valueBox(
        totalStations,
        "Total Stations",
        icon = icon("map-marker"),
        color = "green"
      )
    } else {
      velov_data <- velov
      totalStations <- nrow(velov_data)
      valueBox(
        totalStations,
        "Total Stations",
        icon = icon("map-marker"),
        color = "green"
      )
    }
  })
}

# Exécute l'application Shiny
shinyApp(ui = ui, server = server)
