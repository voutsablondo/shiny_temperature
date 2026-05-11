 # server.R - Logique serveur de l'application Shiny
# Gère la réactivité, les filtres, les graphiques et les cartes.
# Les données et fonctions sont chargées en amont dans global.R.

server <- function(input, output, session) {
  
  
  # UTILITAIRES SERVEUR ----
  
  ## Gestion de l'ouverture des menus sidebar ----
  # Ouvre un menu parent et active un sous-menu via JavaScript.
  
  openMenuItem <- function(menuName, submenuName) {
    delay(100, {
      js <- sprintf("
      $('.treeview').removeClass('active menu-open');
      $('.treeview-menu').hide();
      var menuItem = $('.treeview:contains(\"%s\")');
      menuItem.addClass('active menu-open');
      menuItem.find('.treeview-menu').show();
      $('.treeview-menu li').removeClass('active');
      $('a[data-value=\"%s\"]').parent().addClass('active');
      var sidebar = $('.sidebar');
      var selectedItem = $('a[data-value=\"%s\"]');
      if (selectedItem.length) {
        sidebar.animate({
          scrollTop: selectedItem.position().top + sidebar.scrollTop() - 150
        }, 300);
      }
    ", menuName, submenuName, submenuName)
      runjs(js)
    })
  }
  
  
  # NAVIGATION — BOUTONS SIDEBAR ----
  
  observeEvent(input$bouton_station, {
    updateTabItems(session, "sidebar", selected = "station_stat")
    openMenuItem("Les stations météo", "station_stat")
  })
  
  observeEvent(input$bouton_temperature, {
    updateTabItems(session, "sidebar", selected = "climat_graphique")
    openMenuItem("Explore climat", "climat_graphique")
  })
  
  
  # DONNÉES RÉACTIVES GLOBALES ----
  
  ## Filtre département global ----
  # Renvoie les NUM_POSTE des stations du ou des départements sélectionnés.
  
  stations <- reactive({
    valeur <- input$departement_global
    if (is.null(valeur)) valeur <- "Ensemble"
    if ("Ensemble" %in% valeur) {
      valeur <- min(data_stations$departement):max(data_stations$departement)
    }
    data_stations |>
      filter(departement %in% valeur) |>
      select(NUM_POSTE) |>
      unlist()
  })
  
  ## Données stations filtrées ----
  
  filtered_data_stations <- reactive({
    data_stations |> filter(NUM_POSTE %in% stations())
  })
  
  filtered_data_periode_activ <- reactive({
    data_periode_activ |> filter(NUM_POSTE %in% stations())
  })
  
  ## Données stations filtrées pour le tableau des NA ----
  
  filtered_tab_stations <- reactive({
    valeur <- input$dep_tab_na
    if (is.null(valeur)) valeur <- "Ensemble"
    if ("Ensemble" %in% valeur) {
      valeur <- min(data_stations$departement):max(data_stations$departement)
    }
    data_stations |> filter(departement %in% valeur)
  })
  
  
  # ONGLET 1 — LES STATIONS MÉTÉO ----
  
  ## Page : statistiques générales ----
  
  output$station_total <- renderValueBox({
    valueBox(
      value    = nrow(filtered_data_stations()),
      subtitle = "Stations historiques",
      icon     = icon("code-compare"),
      color    = "olive"
    )
  })
  
  output$station_de_temperature <- renderValueBox({
    valueBox(
      value    = nrow(filtered_data_stations() |> filter(temperature == "Oui")),
      subtitle = "Stations de température",
      icon     = icon("sun"),
      color    = "aqua"
    )
  })
  
  output$station_en_service <- renderValueBox({
    valueBox(
      value    = nrow(filtered_data_stations() |> filter(statut == "Active" & temperature == "Oui")),
      subtitle = "Stations en service en 2024",
      icon     = icon("clover"),
      color    = "navy"
    )
  })
  
  output$station_par_dep <- renderValueBox({
    valueBox(
      value    = filtered_data_stations() |>
        summarise(result = round(sum(statut == "Active") / n_distinct(departement), 1)),
      subtitle = "Stations par département (2024)",
      icon     = icon("globe"),
      color    = "green"
    )
  })
  
  output$duree_moyenne_activite <- renderValueBox({
    valueBox(
      value    = paste0(
        filtered_data_stations() |>
          mutate(duree = duree_activite / 365.25) |>
          summarise(duree = round(mean(duree), 1)) |>
          as.numeric(),
        " ans"
      ),
      subtitle = "Durée moyenne en service",
      icon     = icon("clock"),
      color    = "purple"
    )
  })
  
  output$taux_manquant_moyen <- renderValueBox({
    valueBox(
      value    = filtered_data_stations() |>
        summarise(result = paste0(mean(100 * taux_na_union_temp) |> as.numeric() |> round(2), " %")),
      subtitle = "Taux moyen de NA d'une station",
      icon     = icon("smile"),
      color    = "teal"
    )
  })
  
  output$nbre_rupture_moyen <- renderValueBox({
    valueBox(
      value    = filtered_data_stations() |>
        summarise(result = paste0(mean(nbre_rupture) |> as.numeric() |> round(2))),
      subtitle = "Nombre moyen de ruptures",
      icon     = icon("plus-circle"),
      color    = "black"
    )
  })
  
  output$taux_rupture <- renderValueBox({
    valueBox(
      value    = filtered_data_stations() |>
        summarise(result = paste0(round(100 * mean(nbre_rupture > 0), 2), " %")),
      subtitle = "Part des stations ayant eu des ruptures",
      icon     = icon("fish"),
      color    = "fuchsia"
    )
  })
  
  output$duree_moyen_avant_rupture <- renderValueBox({
    valueBox(
      value    = paste0(
        filtered_data_stations() |>
          filter(nbre_rupture > 0) |>
          mutate(duree = duree_avant_rupture / 365.25) |>
          reframe(duree = round(mean(duree), 1)) |>
          as.numeric(),
        " ans"
      ),
      subtitle = "Durée moyenne avant la première rupture",
      icon     = icon("plus-circle"),
      color    = "aqua"
    )
  })
  
  output$duree_moyen_sans_rupture <- renderValueBox({
    valueBox(
      value    = paste0(
        filtered_data_stations() |>
          filter(nbre_rupture > 0, statut == "Active") |>
          mutate(duree = duree_derniere_rupture / 365.25) |>
          reframe(duree = round(mean(duree), 1)) |>
          as.numeric(),
        " ans"
      ),
      subtitle = "Durée moyenne depuis la dernière rupture (2024)",
      icon     = icon("plus-circle"),
      color    = "orange"
    )
  })
  
  
  ## Page : tendances stations — GIF et évolution ----
  
  output$gif_evolution_station <- renderImage({
    list(
      src         = "www/Evolution_stations.gif",
      contentType = "image/gif",
      width       = "100%",
      style       = "width: 70%; height: auto; object-fit: contain;"
    )
  }, deleteFile = FALSE)
  
  output$logo_linkpact <- renderImage({
    list(src = "www/LinkPact-logo-bleu.jpg", width = 200, height = 70)
  }, deleteFile = FALSE)
  
  output$evolution_nbre_station <- renderHighchart({
    evolution_actif_station(filtered_data_stations())
  })
  
  output$new_old_station <- renderHighchart({
    evolution_new_old_station(filtered_data_stations())
  })
  
  
  ## Page : tendances stations — périodes d'activité ----
  
  dep_activite <- reactive({ input$dep_periode_activ })
  
  output$periode_activite_station <- renderPlot({
    dep <- dep_activite()
    if (!is.null(dep)) {
      suppressWarnings(
        periode_activ_station(data = data_periode_activ, code_dep = dep)
      )
    }
  })
  
  # Carte de sélection du département (filtre stations)
  output$map_filter_dep <- renderLeaflet({
    don_france <- prepare_carte_filtre(
      liste     = france_filter,
      strate    = strate_climat(),
      nom_group = "nom"
    )
    palette <- colorNumeric(
      palette  = "RdYlBu",
      domain   = don_france$croiss,
      na.color = "transparent",
      reverse  = TRUE
    )
    leaflet(don_france) |>
      addPolygons(
        group            = "nom",
        fillColor        = ~palette(croiss),
        color            = "black",
        weight           = 0.5,
        layerId          = ~nom,
        smoothFactor     = 0.2,
        fillOpacity      = 1,
        highlightOptions = highlightOptions(
          color       = "navy",
          weight      = 2,
          bringToFront = TRUE,
          fillOpacity = 0.9
        ),
        label        = ~paste0(
          strate_climat(), " ", code, " : ", nom,
          "\nStations : ", nbre_stat, " (", nbre_stat_act, " actives en 2024)",
          "\nCroissance en 50 ans : ", croiss, " °C"
        ),
        labelOptions = labelOptions(style = list("white-space" = "pre"))
      )
  })
  
  
  ## Page : tableau des NA par station ----
  
  output$tableau_station_graph <- renderReactable({
    gc()
    tableau_station_graph(filtered_tab_stations())
  })
  
  
  ## Page : carte météo des stations ----
  
  output$carte_station_dep <- renderLeaflet({
    gc()
    carte_dep_station(filtered_data_stations())
  })
  
  
  # ONGLET 2 — EXPLORE CLIMAT ----
  
  ## Valeurs réactives de sélection de strate ----
  
  selected_strate <- reactiveVal(NULL)
  strate_climat   <- reactive({ input$strate_climat })
  
  ma_france  <- reactive({ input$strate_climat })
  ma_region  <- reactive({ input$region_climat })
  ma_dep     <- reactive({ input$dep_climat })
  ma_arrond  <- reactive({ input$arrondissement_climat })
  ma_station <- reactive({ input$station_climat })
  
  ma_strate <- list(
    "France"         = ma_france,
    "Region"         = ma_region,
    "Departement"    = ma_dep,
    "Arrondissement" = ma_arrond,
    "Station"        = ma_station
  )
  
  
  ## Page : carte des strates géographiques ----
  
  output$map_filtre <- renderLeaflet({
    gc()
    req(ma_france())
    req(ma_france() %in% c("Region", "Departement", "Arrondissement", "Station"))
    
    don_france <- prepare_carte_filtre(
      liste     = france_filter,
      strate    = strate_climat(),
      nom_group = "nom"
    )
    palette <- colorNumeric(
      palette  = "RdYlBu",
      domain   = don_france$croiss,
      na.color = "transparent",
      reverse  = TRUE
    )
    
    leaflet(don_france) |>
      addPolygons(
        group            = "nom",
        fillColor        = ~palette(croiss),
        color            = "black",
        weight           = 0.5,
        layerId          = ~nom,
        smoothFactor     = 0.2,
        fillOpacity      = 1,
        highlightOptions = highlightOptions(
          color        = "navy",
          weight       = 2,
          bringToFront = TRUE,
          fillOpacity  = 0.9
        ),
        label        = ~paste0(
          strate_climat(), " ", code, " : ", nom,
          "\nStations : ", nbre_stat, " (", nbre_stat_act, " actives en 2024)",
          "\nCroissance en 50 ans : ", croiss, " °C"
        ),
        labelOptions = labelOptions(style = list("white-space" = "pre"))
      )
  })
  
  
  ## Réactivité carte — clic sur une strate ----
  
  observeEvent(input$map_filtre_shape_click, {
    
    click <- input$map_filtre_shape_click
    selected_strate(click$id)
    
    don_france <- prepare_carte_filtre(
      liste     = france_filter,
      strate    = strate_climat(),
      nom_group = "nom"
    )
    
    # Mise à jour du sélecteur de région
    data_stations_filtre <- data_stations |>
      distinct(nom_region, .keep_all = TRUE) |>
      arrange(nom_region) |>
      mutate(result = paste0(nom_region, " (", INSEE_REG, ")"))
    
    maj_select_input(
      session  = session,
      inputId  = "region_climat",
      df       = data_stations_filtre,
      nom_col  = "result",
      val_col  = "nom_region",
      selected = don_france |>
        filter(don_france[[nom_strate(strate_climat())]] == selected_strate()) |>
        select(nom_region) |>
        first()
    )
    
    # Mise à jour du sélecteur de département si nécessaire
    if (!strate_climat() %in% c("France", "Region")) {
      
      nom_reg_selectionne <- don_france |>
        as.data.frame() |>
        filter(get(nom_strate(strate_climat())) == selected_strate()) |>
        first() |>
        pull(nom_region)
      
      data_stations_filtre <- data_stations |>
        filter(nom_region == nom_reg_selectionne) |>
        distinct(nom_departement, .keep_all = TRUE) |>
        mutate(result = paste0(nom_departement, " (", departement, ")"))
      
      maj_select_input(
        session  = session,
        inputId  = "dep_climat",
        df       = data_stations_filtre,
        nom_col  = "result",
        val_col  = "nom_departement",
        selected = don_france |>
          filter(don_france[[nom_strate(strate_climat())]] == selected_strate()) |>
          select(nom_departement) |>
          first()
      )
    }
  })
  
  
  ## Réactivité — changement de strate ----
  
  observeEvent(input$strate_climat, {
    if (strate_climat() == "Station") {
      updateSelectizeInput(
        session,
        "station_climat",
        choices = c(
          "Sélectionner une station météo :" = "",
          vec_nomme(
            nom = data_stations |>
              filter(NUM_POSTE %in% unique(data_climat$Station$NUM_POSTE)) |>
              arrange(as.character(NUM_POSTE)) |>
              mutate(nom = paste0(NUM_POSTE, " (", NOM_USUEL, ")")) |>
              pull(nom),
            valeur = data_stations |>
              filter(NUM_POSTE %in% unique(data_climat$Station$NUM_POSTE)) |>
              arrange(as.character(NUM_POSTE)) |>
              pull(NUM_POSTE)
          )
        ),
        options = list(
          allowEmptyOption = TRUE,
          placeholder      = "Sélectionner une station météo :",
          maxItems         = 1
        ),
        server = TRUE
      )
    }
  })
  
  
  ## Réactivité — changement de région ----
  
  observeEvent(input$region_climat, {
    
    if (strate_climat() == "Region") {
      don_france <- prepare_carte_filtre(france_filter, strate_climat(), "nom")
      maj_carte_strate(
        df           = don_france,
        strate_input = input$region_climat,
        strate_nom   = strate_climat()
      )
    }
    
    data_stations_filtre <- data_stations |>
      filter(nom_region == input$region_climat) |>
      mutate(result = paste0(nom_departement, " (", departement, ")")) |>
      select(result, nom_departement) |>
      unique()
    
    maj_select_input(
      session  = session,
      inputId  = "dep_climat",
      df       = data_stations_filtre,
      nom_col  = "result",
      val_col  = "nom_departement",
      selected = ma_dep()
    )
  })
  
  
  ## Réactivité — changement de département ----
  
  observeEvent(input$dep_climat, {
    
    data_stations_filtre <- data_stations |>
      filter(nom_departement == input$dep_climat)
    
    if (strate_climat() == "Departement") {
      don_france <- prepare_carte_filtre(france_filter, strate_climat(), "nom")
      maj_carte_strate(
        df           = don_france,
        strate_input = input$dep_climat,
        strate_nom   = strate_climat()
      )
    }
    
    maj_select_input(
      session  = session,
      inputId  = "region_climat",
      df       = data_stations |>
        mutate(return = paste0(nom_region, " (", INSEE_REG, ")")) |>
        select(nom_region, return) |>
        unique(),
      nom_col  = "return",
      val_col  = "nom_region",
      selected = data_stations_filtre |> pull(nom_region) |> first()
    )
  })
  
  
  ## Page : indicateurs de température ----
  
  filtered_data <- reactive({
    data_climat[[ma_france()]]
  })
  
  data_stat_temp <- reactive({
    data_climat[[ma_france()]] |>
      filter(get(nom_strate(ma_france(), modify_stat = FALSE)) == ma_strate[[ma_france()]]())
  })
  
  ### Infoboxes de statistiques ----
  
  output$temperature_moyenne <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Température <br> <sup>moyenne générale</sup>"),
      value = data_stat_temp() |> pull(tmoy) |> mean() |> round(1) |> paste0(" °C"),
      icon  = icon("thermometer-half"),
      color = "blue"
    )
  })
  
  output$temperature_minimale <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Température <br> <sup>minimale</sup>"),
      value = data_stat_temp() |> pull(tmoy) |> min() |> round(1) |> paste0(" °C"),
      icon  = icon("thermometer-empty"),
      color = "blue"
    )
  })
  
  output$temperature_maximale <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Température <br> <sup>maximale</sup>"),
      value = data_stat_temp() |> pull(tmoy) |> max() |> round(1) |> paste0(" °C"),
      icon  = icon("thermometer-full"),
      color = "red"
    )
  })
  
  output$changement_temperature <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Tendance 50 ans <br> <sup>(Toute période)</sup>"),
      value = data_stat_temp() |>
        summarise(croiss = 50 * 365.25 * coef(lm(tmoy ~ date))[["date"]]) |>
        round(1) |> paste0(" °C"),
      icon  = icon("cloud-sun"),
      color = "aqua"
    )
  })
  
  output$changement_temperature_1980 <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Tendance 50 ans <br> <sup>(1980+)</sup>"),
      value = data_stat_temp() |>
        filter(year(date) >= 1980) |>
        summarise(croiss = ifelse(
          n() > 0,
          round(50 * 365.25 * coef(lm(tmoy ~ date))[["date"]], 1) |> paste0(" °C"),
          "Non disponible"
        )),
      icon  = icon("fire"),
      color = "navy"
    )
  })
  
  output$date_temp_min <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Date <br> <sup>Température minimale</sup>"),
      value = data_stat_temp() |> filter(tmoy == min(tmoy)) |> pull(date) |> last(),
      icon  = icon("calendar-alt"),
      color = "light-blue"
    )
  })
  
  output$date_temp_max <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Date <br> <sup>Température maximale</sup>"),
      value = data_stat_temp() |> filter(tmoy == max(tmoy)) |> pull(date) |> last(),
      icon  = icon("calendar-alt"),
      color = "orange"
    )
  })
  
  output$temp_max_10_ans <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Température Haute <br> <sup>observée chaque 10 ans</sup>"),
      value = data_stat_temp() |>
        group_by(format(date, "%Y")) |>
        summarise(tmoy = max(tmoy)) |>
        pull(tmoy) |> quantile(0.9) |> round(1) |> paste0(" °C"),
      icon  = icon("chart-line"),
      color = "maroon"
    )
  })
  
  output$temp_min_10_ans <- renderInfoBox({
    req(ma_strate[[ma_france()]]())
    infoBox(
      title = HTML("Température basse <br> <sup>observée chaque 10 ans</sup>"),
      value = data_stat_temp() |>
        group_by(format(date, "%Y")) |>
        summarise(tmoy = min(tmoy)) |>
        pull(tmoy) |> quantile(0.1) |> round(1) |> paste0(" °C"),
      icon  = icon("chart-line"),
      color = "purple"
    )
  })
  
  
  ### Graphique températures min/max (highcharter) ----
  
  output$temp_max_min <- renderHighchart({
    gc()
    req(ma_strate[[ma_france()]]())
    
    stat_choisie <- ma_strate[[ma_france()]]()
    strate       <- ma_france()
    
    valeur <- filtered_data() |>
      filter(.data[[nom_strate(strate_climat(), modify_stat = FALSE)]] == !!stat_choisie) |>
      select(date, TN, TX) |>
      mutate(TN = round(TN, 1), TX = round(TX, 1)) |>
      as.xts()
    
    # Construction du titre selon la strate
    if (strate == "France") {
      titre <- "Les températures minimales et maximales moyennes en France"
    } else if (strate == "Station") {
      info <- data_stations |>
        filter(NUM_POSTE == stat_choisie) |>
        summarise(nom = NOM_USUEL, num = NUM_POSTE)
      prefixe <- "de la station"
      titre   <- paste0("Les températures ", prefixe, " : ", info$num, " (", info$nom, ")")
    } else {
      don_france <- prepare_carte_filtre(france_filter, strate_climat(), "nom")
      info <- don_france |>
        filter(.data[[nom_strate(strate_climat())]] == stat_choisie) |>
        summarise(nom = first(.data[[nom_strate(strate_climat())]]), num = first(code))
      prefixe <- switch(strate,
                        "Departement"    = "du département",
                        "Arrondissement" = "de l'arrondissement",
                        "de la région"
      )
      titre <- paste0("Les températures ", prefixe, " : ", info$num, " (", info$nom, ")")
    }
    
    highchart(type = "stock") |>
      hc_add_series(name = "TX", data = valeur$TX, color = "#ff5e7f",
                    lineWidth = 1, dataGrouping = list(approximation = "high")) |>
      hc_add_series(name = "TN", data = valeur$TN, color = "royalblue",
                    lineWidth = 1, dataGrouping = list(approximation = "low")) |>
      hc_title(text = titre) |>
      hc_xAxis(title = list(text = "Date")) |>
      hc_yAxis(
        title = list(text = "°C"),
        plotBands = list(
          list(from = -40, to = 0,  color = "rgba(50, 50, 255, 0.15)",
               label = list(text = "Glacial", style = list(color = "blue"))),
          list(from = 0,  to = 10, color = "rgba(68, 170, 213, 0.1)",
               label = list(text = "Froid",   style = list(color = "black"))),
          list(from = 25, to = 70, color = "rgba(255, 0, 0, 0.1)",
               label = list(text = "Chaud",   style = list(color = "red")))
        )
      ) |>
      hc_tooltip(shared = TRUE) |>
      hc_rangeSelector(
        selected = 3,
        buttons  = list(
          list(type = "month", count = 1,  text = "1m"),
          list(type = "month", count = 6,  text = "6m"),
          list(type = "year",  count = 1,  text = "1y"),
          list(type = "year",  count = 2,  text = "2y"),
          list(type = "year",  count = 5,  text = "5y"),
          list(type = "year",  count = 10, text = "10y"),
          list(type = "all",               text = "All")
        )
      ) |>
      hc_legend(enabled = TRUE, align = "left", verticalAlign = "top", layout = "vertical")
  })
  
  
  ### UI dynamique — onglets de graphiques ----
  
  output$dynamic_ui <- renderUI({
    req(ma_france())
    req(ma_strate[[ma_france()]]())
    
    critere <- switch(ma_france(),
                      "France"         = "strate_climat",
                      "Region"         = "region_climat",
                      "Departement"    = "dep_climat",
                      "Arrondissement" = "arrondissement_climat",
                      "Station"        = "station_climat"
    )
    
    conditionalPanel(
      condition = paste0("input.", critere, " != null & input.", critere, " != ''"),
      box(width = 12,
          tabBox(width = 12, selected = "Statistiques",
                 tabPanel("Statistiques",
                          fluidRow(align = "center",
                                   infoBoxOutput("temperature_minimale"),
                                   bsTooltip("temperature_minimale", "Minimum de la température moyenne quotidienne", "top"),
                                   infoBoxOutput("date_temp_min"),
                                   bsTooltip("date_temp_min", "Date de la température la plus basse", "top"),
                                   infoBoxOutput("temp_min_10_ans"),
                                   bsTooltip("temp_min_10_ans", "Température faible de retour 10 ans", "top"),
                                   infoBoxOutput("temperature_maximale"),
                                   bsTooltip("temperature_maximale", "Maximum de la température moyenne quotidienne", "top"),
                                   infoBoxOutput("date_temp_max"),
                                   bsTooltip("date_temp_max", "Date de la température la plus haute", "top"),
                                   infoBoxOutput("temp_max_10_ans"),
                                   bsTooltip("temp_max_10_ans", "Température haute de retour 10 ans", "top"),
                                   infoBoxOutput("temperature_moyenne"),
                                   bsTooltip("temperature_moyenne", "Température moyenne quotidienne", "top"),
                                   infoBoxOutput("changement_temperature"),
                                   bsTooltip("changement_temperature", "Evolution moyenne en 50 ans de la température", "top"),
                                   infoBoxOutput("changement_temperature_1980"),
                                   bsTooltip("changement_temperature_1980", "Evolution moyenne en 50 ans depuis 1980", "top")
                          )
                 ),
                 tabPanel("Evolution température",
                          fluidRow(align = "center",
                                   box(width = 12,
                                       withSpinner(highchartOutput("temp_max_min", width = "100%"),
                                                   type = 6, color = "#007bff", caption = "Chargement ...")
                                   )
                          )
                 ),
                 tabPanel("Tendance journalière",
                          fluidRow(align = "center",
                                   withSpinner(plotOutput("temp_journaliere", width = "100%"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   withSpinner(reactableOutput("tab_evol_temp_variable_journalier"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   box(width = 12, status = "info", "Croissance de la température en 50 ans (en °C)")
                          )
                 ),
                 tabPanel("Tendance annuelle",
                          fluidRow(align = "center",
                                   withSpinner(plotOutput("temp_annuelle", width = "100%"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   withSpinner(reactableOutput("tab_evol_temp_variable_annuel"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   box(width = 12, status = "info", "Croissance de la température en 50 ans (en °C)")
                          )
                 ),
                 tabPanel("Tendance par saison",
                          fluidRow(align = "center",
                                   withSpinner(plotOutput("temp_saisonniere", width = "100%"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   withSpinner(reactableOutput("tab_evol_temp_saison"),
                                               type = 6, color = "#007bff", caption = "Chargement ..."),
                                   box(width = 12, status = "info", "Croissance de la température en 50 ans (en °C)")
                          )
                 )
          )
      )
    )
  })
  
  
  ### Graphiques d'évolution ----
  
  output$temp_annuelle <- renderPlot({
    req(ma_strate[[ma_france()]]())
    strate <- ifelse(ma_france() == "France", "", paste0(ma_france(), " : "))
    evolution_annuelle_temperature(
      ma_strate  = ma_strate[[ma_france()]](),
      strate     = strate,
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE),
      base       = data_climat[[ma_france()]]
    )
  })
  
  output$temp_journaliere <- renderPlot({
    req(ma_strate[[ma_france()]]())
    strate <- ifelse(ma_france() == "France", "", paste0(ma_france(), " : "))
    evolution_journaliere_temperature(
      ma_strate  = ma_strate[[ma_france()]](),
      strate     = strate,
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE),
      base       = data_climat[[ma_france()]]
    )
  })
  
  output$temp_saisonniere <- renderPlot({
    req(ma_strate[[ma_france()]]())
    strate <- ifelse(ma_france() == "France", "", paste0(ma_france(), " : "))
    evol_temp_par_saison(
      ma_strate  = ma_strate[[ma_france()]](),
      strate     = strate,
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE),
      base       = data_climat[[ma_france()]]
    )
  })
  
  
  ### Tableaux de taux de croissance ----
  
  output$tab_evol_temp_saison <- renderReactable({
    req(ma_strate[[ma_france()]]())
    croissance_50_ans_saison(
      ma_strate  = ma_strate[[ma_france()]](),
      base       = data_climat[[ma_france()]],
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE)
    )
  })
  
  output$tab_evol_temp_variable_journalier <- renderReactable({
    req(ma_strate[[ma_france()]]())
    croissance_50_ans(
      ma_strate  = ma_strate[[ma_france()]](),
      base       = data_climat[[ma_france()]],
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE)
    )
  })
  
  output$tab_evol_temp_variable_annuel <- renderReactable({
    req(ma_strate[[ma_france()]]())
    croissance_50_ans(
      ma_strate  = ma_strate[[ma_france()]](),
      base       = data_climat[[ma_france()]],
      nom_strate = nom_strate(ma_france(), modify_stat = FALSE),
      annuel     = TRUE
    )
  })
  
  
  ## Page : carte de température animée ----
  
  output$gif_carte_temperature <- renderImage({
    list(src = "www/Evolution_carte_temperature.gif", width = "100%", height = 500)
  }, deleteFile = FALSE)
  
  output$premiere_carte_temp <- renderImage({
    req(input$premiere_date)
    list(
      src   = paste0("www/carte_temperature/carte_temperature", input$premiere_date, ".png"),
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$deuxieme_carte_temp <- renderImage({
    req(input$deuxieme_date)
    list(
      src   = paste0("www/carte_temperature/carte_temperature", input$deuxieme_date, ".png"),
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  
} # fin server