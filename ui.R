# ui.R - Interface utilisateur de l'application Shiny
# Définit la structure visuelle : header, sidebar, et body.
# Les données et fonctions sont disponibles via global.R.


# THÈME GLOBAL ----

skin <- "blue"


# HEADER ----

header <- dashboardHeader(
  title = tags$div("Shiny Température"),
  disable = FALSE
)


# SIDEBAR ----

sidebar <- dashboardSidebar(
  width = 230,
  
  # Logo Linkpact en bas de la sidebar
  tags$div(
    style = "position: absolute; bottom: 10px; width: 100%; text-align: center;",
    tags$img(src = "LinkPact-logo-blanc réajusté.png", height = "80px")
  ),
  
  sidebarMenu(
    expandOnHover = TRUE,
    fixed         = TRUE,
    collapse      = TRUE,
    id            = "sidebar",
    
    ## Menu : Accueil ----
    menuItem("Accueil", tabName = "home", icon = icon("home")),
    
    ## Menu : Les stations météo ----
    menuItem(
      "Les stations météo",
      tabName = "station",
      icon    = icon("dashboard"),
      
      # Filtre département global (partagé entre tous les sous-onglets stations)
      selectInput(
        inputId  = "departement_global",
        label    = "Filtrer par département",
        choices  = vec_nomme(
          nom    = c("Ensemble",
                     data_stations |>
                       distinct(nom_departement, .keep_all = TRUE) |>
                       arrange(nom_departement) |>
                       mutate(result = paste0(nom_departement, " (", departement, ")")) |>
                       pull(result)),
          valeur = c("Ensemble",
                     data_stations |>
                       distinct(nom_departement, .keep_all = TRUE) |>
                       arrange(nom_departement) |>
                       pull(departement))
        ) |> as.list(),
        selected = "Ensemble"
      ),
      
      menuSubItem("Statistiques",        tabName = "station_stat",      icon = icon("chart-pie")),
      menuSubItem("Evolution des stations", tabName = "station_evolution", icon = icon("chart-line")),
      menuSubItem("Disponibilité",       tabName = "station_activite",  icon = icon("clock")),
      menuSubItem("Liste des stations",  tabName = "station_tab_stat",  icon = icon("envelope")),
      menuSubItem("Carte stations Météo", tabName = "station_carte",    icon = icon("tower-broadcast"))
    ),
    
    ## Menu : Explore Climat ----
    menuItem(
      "Explore climat",
      tabName = "climat_graphique",
      icon    = icon("chart-line"),
      
      menuSubItem("Evolution température", tabName = "climat_graphique", icon = icon("sun")),
      menuSubItem("Carte température",     tabName = "climat_carte",     icon = icon("water"))
    )
  )
)


# CSS PERSONNALISÉ ----

css_custom <- "
.menu-button { transition: transform 0.3s ease; }
.menu-button:hover { transform: scale(1.05); }

.dashboard-button {
  margin: 10px; min-width: 200px; min-height: 100px;
  border-radius: 10px; border: none;
  background-color: #2c3e50; color: white;
  transition: all 0.3s ease; font-weight: bold;
}
.dashboard-button:hover {
  background-color: #34495e;
  box-shadow: 0 5px 15px rgba(0,0,0,0.3);
}
.section_home { width: 300px; height: 250px; }

.small-box { transition: transform 0.3s ease; }
.small-box:hover { transform: scale(1.02); }

.tooltip-inner {
  background-color: #34495e !important; font-size: 14px !important;
  max-width: 300px !important; padding: 10px !important;
}
.tooltip.top .tooltip-arrow    { border-top-color:    #34495e !important; }
.tooltip.bottom .tooltip-arrow { border-bottom-color: #34495e !important; }

/* Entête animée avec flocons de neige */
.snow-header {
  background: linear-gradient(to bottom, #3498db, #2980b9);
  padding: 20px; margin-bottom: 20px; border-radius: 10px;
  position: relative; overflow: hidden;
  color: white; text-align: center;
  box-shadow: 0 4px 6px rgba(0,0,0,0.1); width: 80%;
}
.snow-header h2 { margin: 0; font-size: 2em; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); }

@keyframes snowfall {
  0%   { transform: translateY(-10px) translateX(-10px); opacity: 0; }
  50%  { opacity: 1; }
  100% { transform: translateY(100px) translateX(10px);  opacity: 0; }
}
.snowflake { position: absolute; background: white; border-radius: 50%; width: 5px; height: 5px; opacity: 0; }
.snowflake:nth-child(1)  { left: 10%; animation: snowfall 3.0s infinite 0.0s; }
.snowflake:nth-child(2)  { left: 20%; animation: snowfall 3.5s infinite 0.3s; }
.snowflake:nth-child(3)  { left: 30%; animation: snowfall 3.8s infinite 0.7s; }
.snowflake:nth-child(4)  { left: 40%; animation: snowfall 3.2s infinite 1.2s; }
.snowflake:nth-child(5)  { left: 50%; animation: snowfall 3.7s infinite 0.4s; }
.snowflake:nth-child(6)  { left: 60%; animation: snowfall 3.4s infinite 1.6s; }
.snowflake:nth-child(7)  { left: 70%; animation: snowfall 3.9s infinite 0.2s; }
.snowflake:nth-child(8)  { left: 80%; animation: snowfall 3.1s infinite 1.0s; }
.snowflake:nth-child(9)  { left: 90%; animation: snowfall 3.6s infinite 0.5s; }
.snowflake:nth-child(10) { left: 95%; animation: snowfall 3.3s infinite 0.8s; }

/* Footer animé avec alternance développeur / entreprise */
.contact-footer {
  background: linear-gradient(to right, #2c3e50, #3498db);
  padding: 15px; margin-top: 30px; border-radius: 10px;
  color: white; text-align: center;
  box-shadow: 0 -4px 6px rgba(0,0,0,0.1);
  height: 100px; display: flex; align-items: center;
  justify-content: center; width: 80%;
}
.slide-container  { position: relative; width: 100%; height: 60px; overflow: hidden; }
.slide-content    { position: absolute; width: 100%; animation: slideUpDown 10s infinite;
                    display: flex; flex-direction: column; align-items: center; justify-content: center; }
@keyframes slideUpDown {
  0%, 45%  { transform: translateY(0);    }
  50%, 95% { transform: translateY(-50%); }
  100%     { transform: translateY(0);    }
}
.developer-info, .company-info {
  height: 60px; display: flex; align-items: center; justify-content: center; width: 100%;
}
.company-logo   { max-height: 40px; margin-right: 10px; }
.contact-text   { font-size: 1.2em; font-weight: 300; }
.highlight      { color: #f1c40f; font-weight: bold; }
.dev-link       { color: #f1c40f; text-decoration: none; font-weight: bold; transition: color 0.3s ease; }
.dev-link:hover { color: #ffffff; text-decoration: none; }
"


# BODY ----

body <- dashboardBody(
  
  useShinyjs(),
  
  tags$head(tags$style(css_custom)),
  tags$style(HTML("
    .box-title {
      text-align: center; background-color: lightgreen;
      padding: 10px; font-weight: bold; border-radius: 5px;
    }
    .sidebar-mini.sidebar-collapse .sidebar-menu li a .fa { display: inline-block; }
    .sidebar-mini.sidebar-collapse .sidebar-menu li a span { display: none; }
  ")),
  
  tabItems(
    
    
    # ONGLET : ACCUEIL ----
    
    tabItem(
      tabName = "home",
      fluidRow(
        align = "center",
        
        # Entête animée
        div(class = "snow-header",
            lapply(1:10, function(i) div(class = "snowflake")),
            h2("Tableau de Bord Météorologique"),
            p("Tendances thermométriques de la France")
        ),
        
        # Boutons de navigation vers les deux sections principales
        div(
          align = "center",
          style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; padding: 20px;",
          
          ## Bouton Les stations météo ----
          column(width = 5, align = "center",
                 actionButton(
                   class   = "dashboard-button menu-button",
                   inputId = "bouton_station",
                   icon    = icon("dashboard"),
                   label   = tagList("Les stations météo")
                 ),
                 div(
                   class = "section_home",
                   style = "background-color: rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; text-align: justify;",
                   p(HTML("Les températures en France sont mesurées par un vaste réseau de stations
                météorologiques gérées par Météo France. Ces stations assurent la fiabilité de
                l'information en temps réel grâce à leur précision et leur couverture étendue
                du territoire.<br><br>
                La section <strong>'Les stations Météo'</strong> vous offre un aperçu des
                stations météorologiques à l'état actuel !"))
                 )
          ),
          
          ## Bouton Explore Climat ----
          column(width = 5, align = "center",
                 actionButton(
                   class   = "dashboard-button menu-button",
                   inputId = "bouton_temperature",
                   label   = tagList(icon("chart-line"), "Explore climat")
                 ),
                 div(
                   class = "section_home",
                   style = "background-color: rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; text-align: justify;",
                   p(HTML("Face aux changements environnementaux mondiaux, la température reste un
                indicateur clé de l'évolution du climat. Avec des hausses continues, l'impact
                sur les conditions de vie interroge.<br><br>
                La section <strong>'Explore climat'</strong> met en lumière les tendances de
                température en France, offrant une vue d'ensemble."))
                 )
          )
        ),
        
        # Footer animé développeur / entreprise
        div(class = "contact-footer",
            div(class = "slide-container",
                div(class = "slide-content",
                    div(class = "developer-info",
                        icon("user-tie"),
                        span(class = "contact-text",
                             "Réalisé par ",
                             tags$a(href = "https://www.linkedin.com/in/blondo-voutsa-588299223",
                                    class = "dev-link", target = "_blank", "Blondo VOUTSA"),
                             " | ", icon("envelope"), " voutsablondo@gmail.com")
                    ),
                    div(class = "company-info",
                        tags$img(src = "LinkPact-logo-blanc réajusté.png",
                                 class = "company-logo", height = "40px", width = "auto",
                                 style = "display: block !important;", alt = ""),
                        span(class = "contact-text",
                             "Linkpact | ", icon("globe"),
                             tags$a(href = "http://www.linkpact.fr",
                                    class = "dev-link", target = "_blank", "www.linkpact.fr"))
                    )
                )
            )
        )
      )
    ),
    
    
    # ONGLET : STATISTIQUES STATIONS ----
    
    tabItem(
      tabName = "station_stat",
      fluidRow(
        tags$div(
          style = "border: 2px solid black; padding: 10px 30px; text-align: center;
                   margin: 0 50px 20px 50px; background-color: #000080; font-weight: bold;
                   color: #F0F8FF; text-transform: uppercase;
                   box-shadow: 0px 4px 10px rgba(0,0,0,0.5);",
          tags$h1("Chiffres clés des stations")
        ),
        box(width = 12, status = "primary",
            valueBoxOutput("station_total"),
            bsTooltip("station_total", "Total des stations ayant mesuré des températures depuis 1950", "top"),
            valueBoxOutput("station_en_service"),
            bsTooltip("station_en_service", "Nombre de stations fonctionnelles au 31 décembre 2023", "top"),
            valueBoxOutput("station_par_dep"),
            bsTooltip("station_par_dep", "Nombre moyen de stations actives par département en 2024", "top"),
            valueBoxOutput("nbre_rupture_moyen"),
            bsTooltip("nbre_rupture_moyen", "Nombre moyen de périodes de dysfonctionnement des stations", "top"),
            valueBoxOutput("duree_moyenne_activite"),
            bsTooltip("duree_moyenne_activite", "Durée moyenne entre première et dernière dates de fonctionnement", "top"),
            valueBoxOutput("taux_manquant_moyen"),
            bsTooltip("taux_manquant_moyen", "Taux moyen de valeurs manquantes des stations", "top"),
            valueBoxOutput("duree_moyen_sans_rupture"),
            bsTooltip("duree_moyen_sans_rupture", "Durée moyenne depuis la dernière rupture observée", "top"),
            valueBoxOutput("taux_rupture"),
            bsTooltip("taux_rupture", "Proportion des stations ayant eu des ruptures", "top"),
            valueBoxOutput("duree_moyen_avant_rupture"),
            bsTooltip("duree_moyen_avant_rupture", "Durée moyenne avant la première rupture", "top")
        )
      )
    ),
    
    
    # ONGLET : ÉVOLUTION DES STATIONS ----
    
    tabItem(
      tabName = "station_evolution",
      fluidRow(
        fluidRow(
          column(width = 8, align = "center", offset = 2,
                 div(style = "display: flex; justify-content: center;",
                     box(width = 12, status = "info",
                         "Evolution des stations météorologiques en France")
                 )
          ),
          
          # GIF animé de l'évolution des stations
          fluidRow(
            column(width = 12, align = "center", offset = 2,
                   box(width = 8,
                       div(style = "max-width: 100%; height: auto;",
                           imageOutput("gif_evolution_station", height = "auto"))
                   )
            )
          ),
          
          # Graphiques highcharter d'évolution
          fluidRow(
            column(width = 12, align = "center",
                   box(width = 6, title = "Nombre de stations actives et inactives", status = "success",
                       highchartOutput("evolution_nbre_station", height = "auto", width = "auto")
                   ),
                   box(width = 6, title = "Ouverture et fermeture des stations", status = "success",
                       highchartOutput("new_old_station", height = "auto", width = "auto")
                   )
            )
          )
        )
      )
    ),
    
    
    # ONGLET : DISPONIBILITÉ DES STATIONS ----
    
    tabItem(
      tabName = "station_activite",
      fluidRow(
        box(width = 12, status = "info",
            "Disponibilité des données des stations météorologiques par département"),
        
        column(width = 7,
               selectizeInput(
                 inputId  = "dep_periode_activ",
                 label    = "Département :",
                 choices  = vec_nomme(
                   nom    = unique(paste0(data_stations$nom_departement, " (", data_stations$departement, ")")),
                   valeur = unique(data_stations$departement)
                 ) |> as.list(),
                 selected = 75,
                 width    = "80%",
                 multiple = TRUE,
                 options  = list(allowEmptyOption = TRUE,
                                 placeholder      = "Sélectionner un département :",
                                 maxItems         = 1)
               )
        ),
        
        conditionalPanel(
          condition = "input.dep_periode_activ != null & input.dep_periode_activ != ''",
          fluidRow(
            column(width = 12,
                   div(style = "display: flex; justify-content: center;",
                       box(width = 12, br(),
                           plotOutput("periode_activite_station", width = "90%")
                       )
                   )
            )
          )
        )
      )
    ),
    
    
    # ONGLET : LISTE DES STATIONS ----
    
    tabItem(
      tabName = "station_tab_stat",
      fluidRow(
        box(width = 12, status = "info",
            "Liste des stations météorologiques par département et taux annuel de valeurs manquantes"),
        
        column(width = 7,
               selectizeInput(
                 inputId  = "dep_tab_na",
                 label    = "Département :",
                 choices  = vec_nomme(
                   nom    = unique(paste0(data_stations$nom_departement, " (", data_stations$departement, ")")),
                   valeur = unique(data_stations$departement)
                 ) |> as.list(),
                 selected = "75",
                 width    = "80%",
                 multiple = TRUE,
                 options  = list(allowEmptyOption = TRUE,
                                 placeholder      = "Sélectionner un département :",
                                 maxItems         = 1)
               )
        ),
        
        conditionalPanel(
          condition = "input.dep_tab_na != null & input.dep_tab_na != ''",
          box(width = 12, reactableOutput("tableau_station_graph"))
        )
      )
    ),
    
    
    # ONGLET : CARTE DES STATIONS ----
    
    tabItem(
      tabName = "station_carte",
      fluidRow(
        box(width = 12, status = "info",
            "Répartition des stations météorologiques en France. Cliquer sur une station pour voir son détail."),
        box(width = 12,
            leafletOutput("carte_station_dep", height = "600"))
      )
    ),
    
    
    # ONGLET : ÉVOLUTION DE LA TEMPÉRATURE ----
    
    tabItem(
      tabName = "climat_graphique",
      fluidRow(
        box(width = 12, status = "info",
            "Spécifier la zone géographique où visualiser les températures"),
        
        box(width = 12,
            
            ## Sélecteurs de strate géographique ----
            column(width = 6,
                   
                   selectizeInput(
                     inputId  = "strate_climat",
                     label    = "Echelle de visualisation",
                     choices  = vec_nomme(
                       nom    = c("France", "Region", "Departement"),
                       valeur = c("France", "Region", "Departement")
                     ) |> as.list(),
                     selected = "",
                     width    = "80%",
                     multiple = TRUE,
                     options  = list(allowEmptyOption = TRUE,
                                     placeholder      = "Sélectionner une strate :",
                                     maxItems         = 1)
                   ),
                   
                   conditionalPanel(
                     condition = "input.strate_climat != null & input.strate_climat != '' & input.strate_climat != 'France'",
                     selectizeInput(
                       inputId  = "region_climat",
                       label    = "Région",
                       choices  = vec_nomme(
                         nom    = data_stations |>
                           distinct(nom_region, .keep_all = TRUE) |>
                           arrange(nom_region) |>
                           mutate(result = paste0(nom_region, " (", INSEE_REG, ")")) |>
                           pull(result),
                         valeur = data_stations |>
                           distinct(nom_region, .keep_all = TRUE) |>
                           arrange(nom_region) |>
                           pull(nom_region)
                       ) |> as.list(),
                       selected = "",
                       width    = "80%",
                       multiple = TRUE,
                       options  = list(allowEmptyOption = TRUE,
                                       placeholder      = "Sélectionner une région :",
                                       maxItems         = 1)
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.strate_climat != null & input.strate_climat != '' &
                           input.strate_climat != 'France' & input.strate_climat != 'Region'",
                     selectizeInput(
                       inputId  = "dep_climat",
                       label    = "Département",
                       choices  = vec_nomme(
                         nom    = data_stations |>
                           distinct(nom_departement, departement) |>
                           arrange(departement) |>
                           mutate(label = paste0(nom_departement, " (", departement, ")")) |>
                           pull(label),
                         valeur = data_stations |>
                           distinct(nom_departement, departement) |>
                           arrange(departement) |>
                           pull(nom_departement)
                       ) |> as.list(),
                       selected = "",
                       width    = "80%",
                       multiple = TRUE,
                       options  = list(allowEmptyOption = TRUE,
                                       placeholder      = "Sélectionner un département :",
                                       maxItems         = 1)
                     )
                   )
            ),
            
            ## Carte interactive des strates ----
            conditionalPanel(
              condition = "input.strate_climat != null & input.strate_climat != 'France'",
              leafletOutput("map_filtre", height = 600, width = "45%")
            )
        ),
        
        # UI dynamique : graphiques et statistiques de la strate sélectionnée
        uiOutput("dynamic_ui")
      )
    ),
    
    
    # ONGLET : CARTE DE TEMPÉRATURE ----
    
    tabItem(
      tabName = "climat_carte",
      fluidRow(
        box(width = 12, status = "info",
            "Répartition de la température en France à une date précise"),
        
        tabBox(width = 12,
               tabPanel("Carte de température",
                        fluidRow(
                          
                          ## Première carte ----
                          column(width = 6,
                                 selectInput(
                                   inputId  = "premiere_date",
                                   label    = "Choisir une année à observer :",
                                   choices  = setdiff(full_seq(range(year(data_climat$France$date)), 1), 2023),
                                   selected = min(year(data_climat$France$date))
                                 ),
                                 withSpinner(imageOutput("premiere_carte_temp"),
                                             type = 6, color = "#007bff", caption = "Chargement ...")
                          ),
                          
                          ## Deuxième carte (comparaison) ----
                          column(width = 6,
                                 selectInput(
                                   inputId  = "deuxieme_date",
                                   label    = "Choisir une seconde année :",
                                   choices  = setdiff(full_seq(range(year(data_climat$France$date)), 1), 2023),
                                   selected = max(setdiff(year(data_climat$France$date), 2023))
                                 ),
                                 withSpinner(imageOutput("deuxieme_carte_temp"),
                                             type = 6, color = "#007bff", caption = "Chargement ...")
                          )
                        )
               )
        )
      )
    )
    
  ) # fin tabItems
) # fin dashboardBody


# DÉCLARATION UI ----

ui <- dashboardPage(
  header  = header,
  sidebar = sidebar,
  body    = body,
  skin    = skin
)