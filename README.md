# Shiny Température - Analyse des températures en France

Application R Shiny d'analyse des tendances thermiques en France
depuis 1950, développée en prélude aux travaux du mémoire actuariel sur la "Modélisation de l'impact direct de la température sur la mortalité" (EURIA, 2025).

## Application en ligne

[Accéder à l'application](https://voutsablondo.shinyapps.io/Temperature_Meteo_France/)

## Lien avec le pipeline de données

Les données de cette application sont produites par le repo
[pipeline_meteo_data](https://github.com/voutsablondo/pipeline_meteo_data).

**En clonant ce repo**, les données sont déjà incluses dans `data/`
et `www/` — l'application est directement fonctionnelle sans avoir
besoin d'exécuter le pipeline.

**Pour mettre les données à jour**, il faudrait exécuter le pipeline
puis copier les fichiers produits dans `data/` et `www/`.

## Structure

```
App shiny température/
├── global.R          # Packages + chargement des données
├── app.R             # Point d'entrée
├── ui.R              # Interface utilisateur
├── server.R          # Logique serveur
├── R/
│   └── fonctions pour shiny.R
├── data/             # Données produites par le pipeline
└── www/              # GIFs et images statiques
```

## Fonctionnalités

- Carte interactive des stations météorologiques
- Statistiques de disponibilité des données par station
- Évolution temporelle des températures (France, région, département,
  arrondissement, station)
- Calcul des tendances thermiques sur 50 ans
- Animations GIF de l'évolution des stations et des températures

## Auteur

Blondo VOUTSA - [LinkedIn](https://www.linkedin.com/in/blondovoutsa) || 
Développé chez Linkpact
