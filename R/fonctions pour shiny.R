
# fonction qui assigne des noms à un vecteur
vec_nomme <- function(valeur, nom){
  names(valeur) = nom
  valeur
}

# Fonction pour convertir les degrés en radians
deg_to_rad <- function(deg) {
  return (deg * pi / 180)
}

# Fonction pour calculer la distance haversine entre 2 points du globe
# c'est la distance à vol d'oiseau
distance_2d <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Rayon de la Terre en kilomètres
  
  # Conversion des coordonnées en radians
  lat1 <- deg_to_rad(lat1)
  lon1 <- deg_to_rad(lon1)
  lat2 <- deg_to_rad(lat2)
  lon2 <- deg_to_rad(lon2)
  
  # Calcul des différences de coordonnées
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  
  # Application de la formule haversine
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Calcul de la distance
  distance <- R * c
  
  return (distance)
}

#calcul de la distance en tenant compte de l'altitude
distance_3d <- function(lat1, lon1, alt1, lat2, lon2, alt2) {
  # Distance en 2D sur la surface de la Terre
  d_2d <- distance_2d(lat1, lon1, lat2, lon2)
  
  # Différence d'altitude en kilomètres
  d_alt <- (alt2 - alt1) / 1000
  
  # Distance 3D en utilisant le théorème de Pythagore
  distance <- sqrt(d_2d^2 + d_alt^2)
  return(distance)
}

# fonction qui calcule la moyenne en omettant les données manquantes
my_mean <- function(var) mean(var, na.rm = TRUE)

#fonction qui détermine les périodes de valeurs manquantes
periode_na <- function(variable){
  manquant <- is.na(variable)
  dernier <- length(manquant)
  if(length(manquant) == 1){
    debut <- manquant
    fin <- manquant
  }else if(length(manquant) > 1){
    debut <- !manquant[-c(dernier - 1, dernier)] & manquant[-c(1, dernier)]
    debut <- c(manquant[1], debut, !manquant[dernier - 1] & manquant[dernier])
    fin <- manquant[-dernier] & !manquant[-1]
    fin <- c(fin, manquant[dernier])
  } else{
    return(NULL)
  }
  list(debut = which(debut), fin = which(fin))
}

#fonction qui identifie les période de discontinuité d'une variable de date
periode_rupture_date <- function(date){
  rupture <- diff(date) != 1
  list(debut = date[-length(date)][rupture] + 1, fin = date[-1][rupture] - 1)
}

prepare_station_evolution <- function(data){
  interval <- year(range(c(data$premiere_activite, data$derniere_activite)))
  jour = seq(interval[1], interval[2], by = 1)
  jour <- data.frame(jour = jour, jour2 = jour)
  
  ouverture = table(data$premiere_activite |> year()) |> as.data.frame()
  fermeture = table(data$derniere_activite |> year()) |> as.data.frame()
  
  names(ouverture) = c("jour", "nbre_ouverture")
  names(fermeture) = c("jour", "nbre_fermeture")
  
  result <- merge(jour, ouverture, by = "jour", all.x = TRUE)
  result <- merge(result, fermeture, by = "jour", all.x = TRUE)
  
  result$nbre_ouverture[is.na(result$nbre_ouverture)] = 0
  result$nbre_fermeture[is.na(result$nbre_fermeture)] = 0
  
  result <- subset(result, select = -jour2)
  
  result <- result |> mutate(
    nbre_station_act = cumsum(nbre_ouverture) - cumsum(nbre_fermeture),
    nbre_station_ouv = cumsum(nbre_ouverture)
  )
  rownames(result) = as.Date(paste0(result$jour,"-01-01"))

  result <- result[-1]
  result[1,1] <- result[1,1] - sum(data$premiere_activite == "1950-01-01")
  
  result <- as.xts(result, order.by = as.Date(rownames(result)))
  result <- result[-rev(1:nrow(result))[c(1)],]
  result
  
}


evolution_actif_station <- function(base){
  #Traitement de la base
  data <- prepare_station_evolution(base)
  
  #Génération du graphique
  # Construction du graphique
  
  hc <- highchart(type = "stock") |>
    hc_add_series(
      name = "Stations actives",
      data = data$nbre_station_act,
      color = "green",
      lineWidth = 1
    ) |>
    hc_add_series(
      name = "Stations inactives",
      data = data$nbre_station_ouv - data$nbre_station_act,
      color = "darkred", #cyan
      lineWidth = 1
    )
  
  hc <- hc |>
#    hc_title(text = "Nombre de stations actives et inactives") |>
    hc_xAxis(
      title = list(text = "Date"),
      type = "datetime",
      dateTimeLabelFormats = list(year = "%Y")#,
#      labels = list(format = "{value:%Y}")
#      formatter = JS("function() { return Highcharts.dateFormat('%Y', this.value); }")
    ) |>
    hc_yAxis(title = list(text = "Effectif", floor = 0)) |>
    hc_tooltip(shared = TRUE, xDateFormat = "%Y") |> 
    hc_legend(
      enabled = TRUE,
      align = "top",
      verticalAlign = "top",
      layout = "vertical",
      floating = TRUE,
      y = 40
      ) |>
    hc_rangeSelector(enabled = FALSE)
  
  hc
}


evolution_new_old_station <- function(base){
  
  #traitement de la base
  data <- prepare_station_evolution(base)
  
  #Génération du graphique  
  hc <- highchart(type = "stock") |>
    hc_add_series(
      name = "Nouvelles stations",
      data = data$nbre_ouverture,
      color = "royalblue",
      lineWidth = 1
    ) |>
    hc_add_series(
      name = "Nouvelles fermetures", 
      data = data$nbre_fermeture,
      color = "red",
      lineWidth = 1
    )
  
  hc <- hc |>
#    hc_title(text = "Ouverture et fermeture des stations") |>
    hc_xAxis(title = list(text = "Date")) |>
    hc_yAxis(title = list(text = "Effectif")) |>
    hc_tooltip(shared = TRUE) |>
    hc_legend(enabled = TRUE, align = "top", verticalAlign = "top", layout = "vertical", floating = TRUE, y = 40) |>
    hc_rangeSelector(enabled = FALSE)
  
  hc
}


graph_na_temperature <- function(data, an = "Toute"){
  
  data_na_TN <- data |> 
    filter(is.na(TN) & year(date) %in% an) |> 
    select(departement) |> 
    reframe(dep = as.character(unique(departement)), TN = as.numeric(table(departement))) |>
    arrange(desc(TN))
  
  data_na_TX <- data |> 
    filter(is.na(TX) & year(date) %in% an) |> 
    select(departement) |> 
    reframe(dep = as.character(unique(departement)), TX = as.numeric(table(departement))) |>
    arrange(desc(TX))
  
  data_na = merge(data_na_TN, data_na_TX, by = "dep", all = TRUE)
  data_na$TN[is.na(data_na$TN)] <- 0
  
  #graphique du nombre de NA par département pour les variables TN et TX
  
  amBarplot(
    x = "dep", 
    y = c("TX","TN"), 
    data_na, 
    legend = TRUE, 
    xlab = "Departement",
    ylab = "Nombre de NA",
    main = "Nombre de NA des variables de température par département"
  ) |> amOptions(zoom = TRUE)
  
}

high_graph_temp_jour <- function(data){
#  valeur <- data |> 
#    select(date, TN, TX) |>
#    group_by(date) |> 
#    reframe(
#      "TN" = sum(is.na(TN)),
#      "TX" = sum(is.na(TX))
#    ) |> as.xts()
  
  valeur <- data[, .SD, .sdcols = c("date", "TN", "TX")]

  valeur <- valeur[, .(
    TN = sum(is.na(TN)),
    TX = sum(is.na(TX))
  )] |> 
    as.xts()  
  

  
  highchart(type = "stock") |>
    hc_add_series(name = "TX", data = valeur$TX, color = "#ff5e7f", lineWidth = 1) |> 
    hc_add_series(name = "TN", data = valeur$TN, color = "gold", lineWidth = 1) |>
    hc_title(text = "Nombre de départements ayant des NA selon les jours") |>
    hc_xAxis(title = list(text = "Date")) |>
    hc_yAxis(title = list(text = "Nombre de NA")) |>
    hc_tooltip(shared = TRUE) |>
    hc_legend(enabled = TRUE, align = "left", verticalAlign = "top", layout = "vertical")
  
}

#Graphique des périodes d'activités des stations
periode_activ_station <- function(code_dep = 75, data){ #,  attribut = c("TN", "TX")
  nom_dep <- data |> filter(departement == code_dep) |> select(nom) |> first()
  set.seed(0)
  
#  variables <- c(
#    "RR" = "Pluviométrie",
#    "TN" = "Température Minimale",
#    "TX" = "Température Maximale"
#  ) [attribut] |> paste(collapse = ", ")
#  variables <- paste0("Donnée manquante (", variables,")")
  
#  data$Etat[data$Etat == "Donnée manquante"] <- variables
  color_legend <- c("blue", "red") #, "black"
  names(color_legend) = c("Donnée disponible", "Donnée manquante") #, variables
  
  p <- data |>
    mutate(NUM_POSTE = factor(NUM_POSTE)) |>
    filter(departement == code_dep & variable %in% c("rien", "Abscence", "TX", "TN")) |> #attribut
    ggplot(aes(x = premiere_activite, y = NUM_POSTE, group = Id)) +
    geom_segment(
      aes(
        xend = derniere_activite,
        yend = NUM_POSTE, #Id
        colour = Etat,
        text = glue("Station: {NOM_USUEL}<br>Période: {premiere_activite} - {derniere_activite}<br>État: {Etat}")
        ),
      linewidth = 1
    ) +
#    theme_minimal(base_size = 15) + 
    scale_colour_manual(values = color_legend) +
    scale_x_date(limits = range(data_periode_activ$premiere_activite)) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      legend.position = "right",
      legend.title.position = "top",
      legend.text = element_text(size = 12),
      legend.title = element_text(face = "bold", hjust = 0.5, size = 15), # Titre centré et en gras
      legend.key = element_rect(fill = "white", color = NA),   # Fond blanc autour des clés de légende
      legend.box.background = element_rect(color = "black", fill = "gray90", linewidth = 1.2), # Encadré
      legend.spacing.y = unit(0.2, "cm")

#      axis.title.x = element_blank(),
#      axis.title.y = element_blank(),
#      panel.grid.major.y = element_blank(),
#      panel.grid.minor.y = element_blank()

    ) +
    labs(
      title = paste0("Les stations de ", nom_dep, " (", code_dep, ") entre 1950 et 2023"),
      subtitle = "Période d'activité des stations météorologiques",
      x = "Date", 
      y = "Station",
      color = "Légende"
    ) +
    guides(color = guide_legend(title.hjust = 0.5))
  
  
  
  p
  
#  p_plotly <- ggplotly(p, tooltip = "text")  
  
#  p_plotly <- p_plotly %>%
#    layout(
#      legend = list(
#        orientation = "h",
#        x = 0.5,  # Position horizontale centrale
#        xanchor = "center",
#        y = -0.1,  # Position légèrement au-dessus du graphique
#        yanchor = "bottom",
#        title = list(text = "", font = list(size = 14)),
#        font = list(size = 12)
#      ),
#      
#      title = list(
#        text = paste0("Les stations de ",nom_dep, " (",code_dep, ") entre 1950 et 2024", 
#                      '<br><sup>Période d\'activité des stations météorologiques</sup>'),
#        x = 0.5,
#        xanchor = "center",
#        y = 1.1,
#        yanchor = "top",
#        font = list(size = 19)
#      )
#    )
#  
#  p_plotly
}
#periode_activ_station(code_dep = 75, data = data_periode_activ) #,  attribut = c("TN", "TX")
  

#carte interactive des stations dans leurs départements
carte_dep_station <- function(data){
  
  a = gc()
  
  add_station <-  function(map, data, group, color){
    map |>
      addCircleMarkers(
        group = group,
        lng = data$LON, lat = data$LAT,
        radius = 2,
        color = color,
        fillColor = color,
        fillOpacity = 1,
        popup = ~paste(
          "<div style='text-align: center; color: navy; font-weight: bold;'>Détails de la station</div>", 
          
#          "Détails de la station",
          "<b>Id:</b> ", data$NUM_POSTE, 
          "<b><br>Nom:</b> ", data$NOM_USUEL,
          "<b><br>Altitude:</b> ", data$ALTI, "m",
          "<b><br>Code postal:</b> ", data$code_postal,
          "<b><br>Ouverture</b> : ", year(data$premiere_activite),
          "<b><br>Recence</b> : ", year(data$derniere_activite),
          "<b><br>Nombre de ruptures</b> : ", data$nbre_rupture,
          "<b><br>Taux_na_temp</b> : ", paste0(round(data$taux_na_union_temp * 100,2), "%"),
          "<b><br>Statut</b> : ", data$statut
        ),
        label = ~paste0("Station : ",data$NOM_USUEL) 
      )
  }
  
  data_acti <- data |> filter(statut == "Active")
  data_inacti <- data |> filter(statut == "Inactive")
  
  pal <- colorFactor(topo.colors(100), france_dep$nom)
  pal_nbre_stat <- colorNumeric(palette = "YlOrRd", france_dep$nbre_stat)
  pal_stat <- colorFactor(
    palette = c("Active" = "green", "Inactive" = "red"),
    domain = data$statut #data$active
  )
  
  map <- leaflet(france_dep) |>
    #ajout des couches de background
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "GrayCanvas") |>
    addProviderTiles(providers$Esri.OceanBasemap, group = "OceanBasemap") |>
    addProviderTiles(providers$Esri.WorldStreetMap, group = "World Street") |>
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>

    #ajout des polygones des départements
    addPolygons(
      group = "Departements",
      fillColor = "skyblue",
      fillOpacity = 0,
      color = "black",
      weight = 2,
      popup = ~paste("<b>Département</b> : ", nom_departement, "<br> <b>Stations</b> : ", nbre_stat, "<br><b>Stations actives</b> : ", nbre_stat_act),
      label = ~paste0("Dep ",code, " : ", nom_departement),  # Afficher le nom du département au survol
      labelOptions = labelOptions(
        style = list("font-weight" = "bold"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    setView(lng = 2.013749, lat = 46.227638, zoom = 5)  
  
  map <- map |>
    #Ajout des points pour les stations
    add_station(data = data_acti, group = "Station Active", color = "green") |> 
    add_station(data = data_inacti, group = "Station Inactive", color = "red") |> 
    #    addControl(
    #      html = "<h1>Carte des Stations Météorologiques</h1>",
    #      position = "bottomleft", 
    #      className = "map-title"
    #    ) |>
    leaflet::addLegend(
      position = "topright", 
      pal = pal_stat, 
      values = data$statut, 
      title = "Statut des Stations",
      opacity = 0.7
    ) |>
    addLayersControl(
      baseGroups = c("World Street", "Satellite","GrayCanvas", "OceanBasemap", "OpenStreetMap"),
      overlayGroups = c("Departements", "Station Active", "Station Inactive"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    addResetMapButton() |>
    addMouseCoordinates() |>
    addFullscreenControl() |>
    
    #masquer des couches
    hideGroup("Station Inactive")
  
  map  
}


tableau_station_graph <- function(data){
  #data = data_stations : base de données des informations sur les stations
  
  a = gc()
  
  data <- data |> #derniere_rupture
    select(NUM_POSTE, NOM_USUEL, nbre_rupture, taux_na_union_temp, 
           premiere_activite, derniere_activite, statut) |> #, TN_na_an, TX_na_an
    mutate(
      taux_na_union_temp = round(taux_na_union_temp * 100, 1) 
    ) |>
    rename(
      "ID" = "NUM_POSTE",
      "NOM" = "NOM_USUEL",
#      "Département" = "departement",
      "Rupture" = "nbre_rupture",
      "Statut" = "statut",
      "Temp_NA (%)" = "taux_na_union_temp",
      "Création" = "premiere_activite",
      "Récence" = "derniere_activite"#,
#      "TN_NA" = "TN_na_an",
#      "TX_NA" = "TX_na_an"
      #     "Nbre_na_TN" = "TN",
      #      "Nbre_na_TX" = "TX"
    )
  
    reactable(
      data = data,
      defaultColDef = colDef(searchable = TRUE, filterable = TRUE),
      highlight = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE, 
      pageSizeOptions = c(5, 10, 15),
      borderless = TRUE,
      striped = TRUE,
      #  height = 500,
      columns = list(
#        TN_NA = colDef(cell = function(values, index){
#          sparkline(
#            data$TN_NA[[index]],
#            chartRangeMin = 0,
#            chartRangeMax = 1,
#            axis = list(x = TRUE, Y = TRUE)
#            )
#        }),
#        TX_NA = colDef(cell = function(values, index){
#          sparkline(
#            data$TX_NA[[index]],
#            chartRangeMin = 0,
#            chartRangeMax = 1,
#            axis = list(x = TRUE, Y = TRUE)
#          )
#        }),
        Statut = colDef(style = function(value){
          color <- ifelse(value == "Active", "green", "red")
          list(color = color)
        })
      )
    )
}



### fonction application

#fonction qui calcule le taux de croissance à 50 ans d'un point de vu annuel ou pas
croissance_50_ans <- function(ma_strate, base, nom_strate, annuel = FALSE){
  
  a = gc()
  
  # Filtrer les données selon la strate
  data_filtre <- base[get(nom_strate) == ma_strate]
  
  #mensualisation des données
  data_filtre[, date := as.Date(format(date, "%Y-%m-15"))]

  data_filtre[, ":="(
    TN = mean(TN),
    tmoy = mean(tmoy),
    TX = mean(TX)
  ), by = date]
  
  
  # Si l'argument annuel est TRUE, regrouper les données par année et renommer en "date"
  if (annuel) {
    data_filtre[, annee := year(date)]
    data_filtre <- data_filtre[, .(
      TN = min(TN, na.rm = TRUE),         # Température minimale annuelle
      tmoy = mean(tmoy, na.rm = TRUE),    # Température moyenne annuelle
      TX = max(TX, na.rm = TRUE)),          # Température maximale annuelle
      by = annee 
    ] |>
      rename(date = annee) # Renommer "annee" en "date" pour éviter des erreurs dans le modèle linéaire
  }
  
  # Préparation du dataframe pour le calcul des taux de croissance
  tab <- melt(
    data_filtre, 
    measure.vars = c("TN", "tmoy", "TX"), 
    variable.name = "Variable", 
    value.name = "Temperature"
  )
  
  tab[, Variable := factor(
    Variable, 
    levels = c("TN", "tmoy", "TX"),
    labels = c("Température Minimale", "Température Moyenne", "Température Maximale")
  )]
  
  tab <- tab[, .(
    taux_croissance = lm(Temperature ~ date)$coefficients[2],
    sd = summary(lm(Temperature ~ date))$coefficients[2, 2]
  ),
  by = Variable][,
                 taux_croissance_50_ans := taux_croissance * 50 * ifelse(annuel, 1, 365.25),  # Ajuster pour 50 ans, avec ou sans agrégation annuelle
  ]
  
  tab[, ":="(
    IC_bas = taux_croissance_50_ans - sd * 50 * ifelse(annuel, 1, 365.25) * 1.96,
    IC_haut = taux_croissance_50_ans + sd * 50 * ifelse(annuel, 1, 365.25) * 1.96,
    taux_croissance_50_ans = round(taux_croissance_50_ans, 3)
  )] 
  
  tab[, ":="(
    SD = paste0("[", round(IC_bas, 2), " ; ", round(IC_haut, 2), "]")
  )] 
  
  tab <- tab |>
    select(Variable, taux_croissance_50_ans, SD) |>
    t() |>
    unname()
  
  # Ajouter les noms de colonnes et retirer la première ligne utilisée pour cela
  colnames(tab) = tab[1,]
  tab <- tab[-1,]
  
  # Affichage du tableau
  reactable(
    tab,
    defaultColDef = colDef(
      align = "center"  # Aligne le texte au centre pour toutes les colonnes
    ),
    theme = reactableTheme(
      headerStyle = list(
        backgroundColor = "navy",  # Couleur de fond bleu brillant : deepskyblue : darkgreen
        color = "white",                  # Texte blanc
        fontSize = "18px",                # Augmente la taille de la police
        fontWeight = "bold",              # Met le texte en gras
        textAlign = "center"              # Centre les titres
      )
    )
  )
  
  
}
#  croissance_50_ans(ma_strate = "Bretagne", base = data_climat$Region, nom_strate = "nom_region")


#fonction qui calcule le taux de croissance à 50 ans par saison 
croissance_50_ans_saison <- function(ma_strate, base, nom_strate){
  
  a = gc()
  
  # Préparation du dataframe pour le calcul des taux de croissance
  annuel = TRUE
  
  tab <- base[get(nom_strate) == ma_strate]
  
  tab[, date := as.Date(format(date, "%Y-%m-15"))]
  
  tab <- tab[, .(
    tmoy = mean(tmoy)
  ), by = date]
  
  tab[, ":="(
    Saison = sapply(date, get_season)
  )]
  
  tab[, date := as.numeric(format(date, "%Y"))]
  
  tab <- tab[, .(
    tmoy = mean(tmoy)
  ), by = list(date, Saison)]
  
  
  tab[, moy_10_ans := frollmean(tmoy, n = 10, align = "right"), by = Saison]
  tab <- na.omit(tab)
  
  
  tab <- tab[, .(
    taux_croissance = lm(moy_10_ans ~ date)$coefficients[2],
    sd = summary(lm(moy_10_ans ~ date))$coefficients[2, 2]
  ),
  by = Saison][,
                 taux_croissance_50_ans := taux_croissance * 50 * ifelse(annuel, 1, 365.25),  # Ajuster pour 50 ans, avec ou sans agrégation annuelle
  ]
  
  tab[, ":="(
    IC_bas = taux_croissance_50_ans - sd * 50 * ifelse(annuel, 1, 365.25) * 1.96,
    IC_haut = taux_croissance_50_ans + sd * 50 * ifelse(annuel, 1, 365.25) * 1.96,
    taux_croissance_50_ans = round(taux_croissance_50_ans, 3)
  )] 
  
  tab[, ":="(
    SD = paste0("[", round(IC_bas, 2), " ; ", round(IC_haut, 2), "]")
  )] 
  
  tab <- tab |>
    select(Saison, taux_croissance_50_ans, SD) |>
    t() |>
    unname()
  
  # Ajouter les noms de colonnes et retirer la première ligne utilisée pour cela
  colnames(tab) = tab[1,]
  tab <- tab[-1,]
  

  reactable(
    tab,
    defaultColDef = colDef(
      align = "center"  # Aligne le texte au centre pour toutes les colonnes
    ),
    theme = reactableTheme(
      headerStyle = list(
        backgroundColor = "navy",  # Couleur de fond bleu brillant : deepskyblue, darkgreen
        color = "white",                  # Texte blanc
        fontSize = "18px",                # Augmente la taille de la police
        fontWeight = "bold",              # Met le texte en gras
        textAlign = "center"              # Centre les titres
      )
    )
  )
  
}

#  croissance_50_ans_saison(ma_strate = "Bretagne", base = data_climat$Region, nom_strate = "nom_region")


#fonction qui associe à un niveau le strate le nom correspondant dans la base de données de la carte de france
nom_strate <- function(strate, modify_stat = TRUE){
  # strate prend les valeurs :"France", "Region", "Departement" , "Arrondissement" ou "Strate"
  
  nom_stat <- ifelse(modify_stat, "nom_arrondissement", "NUM_POSTE")
  
  nom <- c(
    "France" = "France",
    "Region" = "nom_region",
    "Departement" = "nom_departement",
    "Arrondissement" = "nom_arrondissement", 
    "Station" = nom_stat 
  )[strate] |> unname()
  nom
}

#fonction qui adapte la carte sélectionée en fonction de la strate
prepare_carte_filtre <- function(liste, strate, nom_group){
  # strate prend les valeurs : "Region", "Departement" , "Arrondissement" ou "Strate"
  
  nom <- nom_strate(strate)
  #  if(nom == "NUM_POSTE") nom <- "nom_arrondissement"
  
  liste[[strate]] |>
    mutate("nom" = as.data.frame(liste[[strate]])[,nom])
}

#prepare_carte_filtre(france_filter, strate = "Region", "nom_group" = "nom")

#Fonction pour tracer l'évolution annuelle des températures minimale,
#moyenne et maximale dans un ensemble de stations

evolution_annuelle_temperature <- function(ma_strate, strate, base, nom_strate){ #stations
  #ma_strate : valeur de la strate à représenter
  #strate : niveau de strate ou se situe les données
  #base : jeu de données contenant les données dans la totalité
  #nom_strate : nom de la variable du jeu de données servant  à faire les filtres
  
  a = gc()
  
  data_treat <- base[get(nom_strate) == ma_strate][,
                                     Annee := year(date)
                                     ][,
                                       .(
                                         TN = min(TN),
                                         tmoy = mean(tmoy),
                                         TX = max(TX)
                                       ), by = Annee] 
  
  data_treat <- melt(
    data_treat,
    measure.vars = c("TN", "tmoy", "TX"), 
    variable.name = "Variable", 
    value.name = "Temperature"
    )
  
  data_treat[, ":="(
    Variable = factor(
      Variable, 
      levels = c("TN", "tmoy", "TX"),
      labels = c("Température Minimale", "Température Moyenne", "Température Maximale")
    )#,
#    periode = if_else(Annee >= 1980, "post-1980", "pre-1980")
  )]
  
  data_treat[, moy_10_ans := frollmean(Temperature, n = 10, align = "right"), by = Variable]
  
    #Tracé du graphique
  data_treat |>
    na.omit() |>
    ggplot(aes(x = Annee, colour = Variable, y = moy_10_ans)) + #, group = Variable
    facet_wrap(vars(Variable), scales = "free_y", ncol = 3) + #fixed
    geom_point(aes(y = Temperature), size = 0.5) + 
    geom_line() +
#    geom_point(aes(y = Temperature), size = 0.5) +
#    geom_smooth(aes(y = Temperature), linewidth = 0.5, method = "gam", formula = y ~ s(x, bs = "cs")) +
    geom_smooth(
      method = "lm",
      formula = y ~x,
      linetype = 2,
#      aes(linetype = periode),
      linewidth = 0.7,
      se = FALSE
    ) +
    scale_color_manual(
      values = c(
        "Température Minimale" = "navy",
        "Température Moyenne" = "darkgreen",
        "Température Maximale" = "darkred")
    ) +
#    scale_linetype_manual(values = c("pre-1980" = "dashed", "post-1980" = "dashed")) +
    theme_bw() +
    labs(
      title = "Température en moyenne annuelle (points) et en moyenne décénale (ligne)",
      subtitle = paste0(strate, ma_strate),
      x = "Année",
      y = "Température (°C)",
      color = "Température"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      strip.text = element_text(size = 13, face = "bold", color = "white"),
      strip.background = element_rect(color = "navy", fill = "navy")
    )
}
#evolution_annuelle_temperature(ma_strate = "Bretagne", strate = "Region : ", base = data_climat$Region, nom_strate = "nom_region")

evolution_journaliere_temperature <- function(ma_strate, strate, base, nom_strate){ #stations, strate, nom
  
  a = gc()
  
  
  data_treat <- base[get(nom_strate) == ma_strate]
  
  #mensualisation des données
  data_treat[, date := as.Date(format(date, "%Y-%m-15"))]

  data_treat[,
    .(
      TN = min(TN),
      tmoy = mean(tmoy),
      TX = max(TX)
    ), by = date] 
  
  data_treat <- melt(
    data_treat,
    measure.vars = c("TN", "tmoy", "TX"), 
    variable.name = "Variable", 
    value.name = "Temperature"
  )
  
  data_treat[, ":="(
    Variable = factor(
      Variable, 
      levels = c("TN", "tmoy", "TX"),
      labels = c("Température Minimale", "Température Moyenne", "Température Maximale")
    )#,
#    periode = if_else(year(date) >= 1980, "post-1980", "pre-1980")
  )]
  
#  data_treat[, moy_30_jours := frollmean(Temperature, n = 350, align = "right"), by = Variable]
  
  
  data_treat |>
    #Tracé du graphique
    ggplot(aes(x = date, colour = Variable, y = Temperature)) + #, group = Variable
    facet_wrap(vars(Variable), scales = "free_y", ncol = 3) + #fixed
    geom_smooth(linewidth = 0.5, method = "gam", formula = y ~ s(x, bs = "cs")) + #"loess", formula = 'y ~ x'
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      linetype = 2,
#      aes(linetype = periode),
      linewidth = 0.7,
      se = FALSE
    ) +
    scale_color_manual(
      values = c(
        "Température Minimale" = "navy",
        "Température Moyenne" = "darkgreen",
        "Température Maximale" = "darkred")
    ) +
#    scale_linetype_manual(values = c("pre-1980" = "dashed", "post-1980" = "dashed")) +
    theme_bw() +
    labs(
      title = "Evolution des températures quotidiennes",
      subtitle = paste0(strate, ma_strate),
      x = "Date",
      y = "Température (°C)",
      color = "Température"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      strip.text = element_text(size = 13, face = "bold", color = "white"),
      strip.background = element_rect(color = "navy", fill = "navy")
    )
}

#evolution_journaliere_temperature(ma_strate = "Bretagne", strate = "Region : ", base = data_climat$Region, nom_strate = "nom_region")

#fonction pour connaitre la saison à une date données
get_season <- function(date) {
  # Convertir la date en format "Date" si ce n'est pas déjà fait
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  
  # Année de référence (peut être n'importe laquelle, car seule
  #l'importance relative des dates compte)
  year <- format(date, "%Y")
  
  # Dates des changements de saison
  spring_start <- as.Date(paste0(year, "-03-20"))
  summer_start <- as.Date(paste0(year, "-06-21"))
  fall_start <- as.Date(paste0(year, "-09-22"))
  winter_start <- as.Date(paste0(year, "-12-21"))
  
  # Déterminer la saison en utilisant la date
  if (date >= winter_start || date < spring_start) {
    return("Hiver")
  } else if (date >= spring_start && date < summer_start) {
    return("Printemps")
  } else if (date >= summer_start && date < fall_start) {
    return("Été")
  } else {
    return("Automne")
  }
}

#Fonction pour tracer les tendances par saison de températures 
#moyenne dans un ensemble de stations (lisée)
evol_temp_par_saison <- function(ma_strate, strate, base, nom_strate){ #stations, strate, nom
  
  a = gc()
  
  data_treat <- base[get(nom_strate) == ma_strate]
  
  #mensualisation des données
  data_treat[, date := as.Date(format(date, "%Y-%m-15"))]
  
  data_treat <- data_treat[, .(
    tmoy = mean(tmoy)
  ), by = date]
  
  data_treat[, ":="(
    Saison = sapply(date, get_season)
  )]
  
  data_treat[, date := as.numeric(format(date, "%Y"))]
  
  data_treat <- data_treat[, .(
    tmoy = mean(tmoy)
    ), by = list(date, Saison)]
  
  
  data_treat[, moy_10_ans := frollmean(tmoy, n = 10, align = "right"), by = Saison]
  
  
  data_treat |>
    na.omit() |>
    ggplot(aes(x = date, y = moy_10_ans, colour = Saison)) +
    geom_line() +
    geom_point(size = 0.7) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      linetype = 3,
      linewidth = 0.7,
      se = FALSE,
      show.legend = FALSE
    ) +
    theme_bw() + 
    scale_color_manual(
      values = c(
        "Hiver" = "navy", 
        "Été" = "darkred", 
        "Automne" = "darkgreen",
        "Printemps" = "#FF2B78")
    ) +
#    scale_linetype_manual(values = c("pre-1980" = "dashed", "post-1980" = "dashed")) +
    labs(
      title = "Température en moyenne annuelle par saison",
      subtitle = paste0(strate, ma_strate),
      colour = "Saison",
      x = "Date",
      y = "Température moyenne (°C)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
#      legend.position = "none",
      strip.text = element_text(size = 13, face = "bold", color = "white"),
      strip.background = element_rect(color = "navy", fill = "navy")
    )
}

#evol_temp_par_saison(ma_strate = "Bretagne", strate = "Region : ", base = data_climat$Region, nom_strate = "nom_region")

#fonction qui génère la carte de france avec température à une date données
carte_temperature_date <- function(date, france_carte, data_climat, data_stations){
  date0 = date
  if(nchar(as.character(date0)) > 4) {
    df_temp <-  data_climat |>
      filter(date == date0) |>
      #      mutate(tmoy = sqrt(tmoy - min(tmoy) + 1)) |> 
      merge(data_stations |> 
              dplyr::select(NUM_POSTE, LON, LAT), 
            by = "NUM_POSTE", 
            all.x = TRUE) |>
      na.omit()
    
    titre <- paste0("Températures en France à la date : ", date0)
    breaks = c(min(data_climat$tmoy), -10, 0, 10, 18, 25,  max(data_climat$tmoy))
    
  } else {
    df_temp <-  data_climat |>
      filter(annee == date0) |>
      #      summarise(tmoy = mean(tmoy), .by = NUM_POSTE) |>
      #      mutate(tmoy = sqrt(tmoy - min(tmoy) + 1)) |> 
      merge(data_stations |> 
              dplyr::select(NUM_POSTE, LON, LAT), 
            by = "NUM_POSTE", 
            all.x = TRUE) |>
      na.omit() ## modification récente
    
    titre <- paste0("Températures en France en : ", date0)
    breaks = c(min(data_climat$tmoy), 0, 5, 10, 14, 18,  max(data_climat$tmoy))
    
  }
  
  # Jointure avec data_stations
  
  ## imputation des températures
  france_impute <- france_carte |>
    mutate(tmoy = NA) |> 
    as.data.table() |>
    dplyr::select(LON, LAT, tmoy) |> 
    rbind(df_temp |>
            dplyr::select(LON, LAT, tmoy)
    )
  
  france_impute <- VIM::kNN(
    france_impute, 
    variable = "tmoy", 
    dist_var = c("LON", "LAT"), 
    k = 5,
    weightDist = TRUE, 
    numFun = weighted.mean
  )
  
  france_carte <- france_carte |> 
    cbind(
      france_impute |>
        dplyr::slice(1:nrow(france_carte)) |>
        dplyr::select(tmoy)
    )
  
  df_temp <- df_temp |>
    st_as_sf(coords = c("LON", "LAT"), crs = 4326, remove = FALSE) 
  
  #breaks <- seq(min(data_climat$tmoy), max(data_climat$tmoy), length = 6)
  #  breaks = c(min(data_climat$tmoy), -10, 0, 10, 18, 25,  max(data_climat$tmoy))
  
  #représentation du graphique
  ggplot(data = france_carte) +
    
    # Afficher les frontières de la France
    geom_sf(aes(color = tmoy, fill = tmoy)) +
    scale_fill_gradientn(
      colors = c("black", "navy", "cyan", "skyblue",  "orange", "red"),
      values = scales::rescale(breaks),  # Réscale les breaks entre 0 et 1
      breaks = breaks,
      limits = range(data_climat$tmoy),
      name = "Température (°C)",
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_gradientn(
      colors = c("black", "navy", "cyan", "skyblue", "orange", "red"), #, "lightblue"
      values = scales::rescale(breaks),  # Réscale les breaks entre 0 et 1
      breaks = breaks,
      limits = range(data_climat$tmoy),
      name = "Température (°C)",
      labels = scales::label_number(accuracy = 0.1)
    ) +
    geom_sf(data = df_temp, color = "blue", fill = "blue", size = 0.01) +  
    
    # Ajouter les stations
    #    scale_color_gradient2(
    #      low = "cyan", mid = "white", high = "red", name = "Température (°C)", midpoint = 0,
    #      breaks = sort(c(range(france_carte$tmoy), range(data_climat$tmoy))), #france_carte
    #      limits = range(data_climat$tmoy), #france_carte
    #      labels = scales::label_number(accuracy = 0.1, width = 4)
    #      #      aes(labels = formatC(range(tmoy), format = "f", digits = 1, flag = "0", width = 4))
    #    ) +
    #    scale_fill_gradient2(
    #      low = "cyan", mid = "white", high = "red", name = "Température (°C)", midpoint = 0,
    #      breaks = sort(c(range(france_carte$tmoy), range(data_climat$tmoy))), #france_carte
    #      limits = range(data_climat$tmoy), #france_carte
    #      labels = scales::label_number(accuracy = 0.1, width = 4)
    #      #      aes(labels = formatC(range(tmoy), format = "f", digits = 1, flag = "0", width = 4))
    #    ) +
    labs(
      title = titre,
      caption = "Source : Données Météo France") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(hjust = 0.5),
      legend.key.size = unit(0.8, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      panel.background = element_rect(fill = "white", color = "white"),  # Couleur de fond et bordure du graphique
      plot.background = element_rect(fill = "white", color = "white")
    ) 
}

#carte_temperature_date(
#  date = date, 
#  france_carte = france_canton,
#  data_climat = data_climat$Station,
#  data_stations = data_stations
#  )

#carte_temperature_date(
#  date = 2005, 
#  france_carte = france_canton,
#  data_climat = data_climat_annuel,
#  data_stations = data_stations
#)


# fonction qui fait la mise à jour des boutons dans le shiny
maj_select_input <- function(session, inputId, df, nom_col, val_col, selected = NULL) {
  updateSelectInput(
    session,
    inputId,
    choices = vec_nomme(
      nom = df |> arrange(.data[[nom_col]]) |> pull(.data[[nom_col]]),
      valeur = df |> arrange(.data[[val_col]]) |> pull(.data[[val_col]])
    ) |> as.list(),
    selected = selected
  )
}

#fonction qui fait la mise à jour de la carte de filtre
maj_carte_strate <- function(df, strate_input, strate_nom) {
  palette <- colorNumeric(palette = "RdYlBu", domain = df$croiss, na.color = "transparent", reverse = TRUE)
  
  leafletProxy("map_filtre") %>%
    clearShapes() %>%
    addPolygons(
      data = df,
      group = "nom",
      fillColor = ~ifelse(nom == strate_input, "gold", palette(croiss)),
      color = ~ifelse(nom == strate_input, "white", "black"),
      weight = ~ifelse(nom == strate_input, 5, 1),
      layerId = ~nom,
      smoothFactor = 0.2,
      fillOpacity = 1,
      highlightOptions = highlightOptions(
        color = "blue",
        weight = 2,
        bringToFront = TRUE,
        fillOpacity = 0.9
      ),
      label = ~paste0(strate_nom, " ", code, " : ", nom, 
                      "\nStations : ", nbre_stat, " (", nbre_stat_act, " actives en 2024)",
                      "\nCroissance en 50 ans : ", croiss, " °C"),
      labelOptions = labelOptions(style = list("white-space" = "pre"))
    )
}