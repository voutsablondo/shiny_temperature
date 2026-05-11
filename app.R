# app.R - Point d'entrée de l'application Shiny
#
# Prérequis : ouvrir le projet via "App shiny température.Rproj" avant de lancer
#             l'application, afin que le répertoire de travail soit correctement défini.
#
# Architecture :
#   global.R               <- packages, fonctions, données (exécuté auto par Shiny)
#   R/fonctions pour shiny.R <- fonctions utilitaires     (exécuté auto par Shiny)
#   ui.R                   <- interface utilisateur
#   server.R               <- logique serveur
#
# Déploiement :
#   rsconnect::deployApp(appDir = "App shiny température", account = "voutsablondo")

source("ui.R")
source("server.R")
shinyApp(ui, server)