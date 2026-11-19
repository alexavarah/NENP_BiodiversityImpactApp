# NENP_BiodiversityImpactApp

A Shiny app that allows users to explore how local-scale nature actions (e.g., 
planting flowers, creating ponds, or adding log piles) may affect biodiversity, 
based on evidence synthesised from published ecological studies. This is a 
development version: at the moment it only includes data on flower-planting 
actions. Copy development is also necessary to make the app suitable for its
target audience (school children and teachers), and functionality 
improvements will be implemented for the final version.      

## Purpose

The app enables users to explore the estimated effects of different nature 
actions on biodiversity across a range of taxonomic and functional groups and
in urban or rural contexts. Results are derived from meta-analytic models using 
data from a large evidence base of ecological studies. Results are presented 
through an interactive user interface. The intended aim is to aid decision-making
by school children and teachers when planning which nature action to implement
in their school grounds.  

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
│   └── [all other image files]....
│
├── rsconnect/                      # Shiny deployment configuration
│
├── .git/                           # Git version-control files
│
└── .Rproj.user/                    # RStudio project settings
``` 

## Running the app Open `app.R` in RStudio and run:  

```r
shiny::runApp()
```

## Methodology

The systematic review and meta-analysis methods are available in 
Varah, A., Purvis, A., de Palma, A., 2025. Biodiversity response to local-scale 
conservation interventions: A systematic review and meta-analysis protocol. 
Ecological Solutions and Evidence 6, e70070. https://doi.org/10.1002/2688-8319.70070

## Funding

The app was developed as part of the [National Education Nature Park](https://www.educationnaturepark.org.uk/), 
funded by the UK Government's Department for Education.
  