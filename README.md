# NENP_BiodiversityImpactApp
A Shiny app that allows users to explore how local-scale nature actions (e.g., 
planting flowers, creating ponds, or adding log piles) may affect biodiversity, 
based on evidence synthesised from published ecological studies.  

## Repository structure

```text

NENP_BiodiversityImpactApp/
│
├── app.R                           # Main Shiny application
│
├── README.md                       # Repository documentation
│
├── LICENSE                         # CC0-1.0 licence
│
├── NENP_BiodiversityImpactApp.Rproj
│
├── .gitignore
│
├── data/
│   └── biodiversity_data.csv       # Processed biodiversity evidence database
│
├── R/
│   ├── data_preparation.R          # Data cleaning and preparation script
│   └── func.R                      # Custom functions used in data prep script
│
└── www/
│   ├── NENP_logo.svg               # App logo
│   ├── NENP_favicon.ico            # Browser favicon
│   ├── cricket-grass.png           # Image assets used in the app
    └── [all other image files]....
│
├── rsconnect/                      # Shiny deployment configuration
│
├── .git/                           # Git version-control files
│
└── .Rproj.user/                    # RStudio project settings
``` 

## Purpose

The app enables users to explore the estimated effects of different nature  
actions on biodiversity across a range of taxonomic and functional groups and
in urban or rural contexts. Results are derived from meta-analytic models using 
data from a large evidence base of ecological studies. Results are presented 
through an interactive user interface.  

## Running the app Open `app.R` in RStudio and run:  

```r
shiny::runApp()
```