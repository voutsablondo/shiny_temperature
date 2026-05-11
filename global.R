# global.R - Initialisation de l'application Shiny
# Exécuté automatiquement par Shiny avant ui.R et server.R, à chaque démarrage.
#
# Liens utiles :
#   Cartographie R     : https://juliescholler.gitlab.io/files/M2-etu/Journoux-Thuard-Cartographie_avec_R.html#fond-de-carte
#   Données manquantes : https://thinkr.fr/donnees-manquantes-causes-identification-et-imputation/
#   Régression robuste : https://pmarchand1.github.io/ECL8202/notes_cours/04-Regression_robuste.pdf
#   Shiny tutoriel     : https://lrouviere.github.io/TUTO_VISU/correction/shiny.html
#   Codes postaux      : https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
#   Cartes vectorielles : https://sites.google.com/site/rgraphiques/5--applications/5--realiser-des-cartes-avec-le-logiciel-r/02-aller-plus-loin-fond-de-carte-vectoriel-et-automatisation
#   Shiny avancé       : https://mastering-shiny.org/action-dynamic.html#freezing-reactive-inputs


# Chargement packages ----

library(tidyverse)        # manipulation des données
library(data.table)       # manipulation des données volumineuses
library(sf)               # manipulation des données géographiques
library(tmap)             # création des cartes interactives
library(xts)              # structuration des dates pour les courbes interactives
library(highcharter)      # création de graphiques interactifs
library(plotly)           # graphique des périodes d'activité des stations
library(ggthemes)         # thèmes supplémentaires pour ggplot
library(glue)             # personnalisation des infobulles dans plotly
library(sparkline)        # mini-graphiques dans les tableaux
library(reactable)        # tableaux interactifs
library(geodata)          # téléchargement des données géographiques
library(rAmCharts)        # graphiques complémentaires
library(shiny)            # framework de l'application
library(shinydashboard)   # mise en page dashboard
library(shinyWidgets)     # widgets supplémentaires
library(leaflet)          # cartes interactives
library(leafem)           # extensions leaflet
library(leaflet.extras)   # extensions leaflet supplémentaires
library(shinycssloaders)  # indicateurs de chargement
library(shinyBS)          # composants Bootstrap pour Shiny
library(shinyjs)          # manipulation JavaScript dans Shiny


# Chargement fonctions ----
# Chargement des fonctions utilitaires de l'application.
# Ce fichier est également chargé automatiquement par Shiny via le dossier R/,
# mais le source() explicite ici garantit l'ordre d'exécution.

source("R/fonctions pour shiny.R")


# Chargement des données ----
# Fichiers produits par le pipeline (scripts 7 et 8).
# À regénérer via le pipeline si les données sources ont changé.

load("data/donnees_temperature_par_strate.Rdata")   # -> data_climat (liste 5 strates)
load("data/data_a_charger.Rdata")                   # -> data_stations, france_canton,
#    france_arr, france_dep,
#    data_periode_activ
load("data/donnees_carte_temperature_strate.Rdata") # -> france_filter (taux de croissance)