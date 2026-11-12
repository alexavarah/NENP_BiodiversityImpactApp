library(shiny)
library(bslib)
library(shinydisconnect)
library(base64enc)
library(dplyr)
library(metafor)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)

# *************************************************
# Load and prepare data ----
# *************************************************

source("R/prepare_data.R")

print(nrow(dat))

# Load images as shiny isn't finding them otherwise
methods_flow_diagram <- base64enc::dataURI(
  file = "www/methods_flow_diagram.png",
  mime = "image/png"
)

cricket_grass <- base64enc::dataURI(
  file = "www/cricket-grass.png",
  mime = "image/png"
  )

fork_daffodil <- base64enc::dataURI(
  file = "www/fork-daffodil.png",
  mime = "image/png"
  )

shovel_daffodil <- base64enc::dataURI(
  file = "www/shovel-daffodil.png",
  mime = "image/png"
  )

butterfly_cornflower <- base64enc::dataURI(
  file = "www/butterfly-cornflower.png",
  mime = "image/png"
  )

butterfly_poppy <- base64enc::dataURI(
  file = "www/butterfly-poppy.png",
  mime = "image/png"
)

butterfly_grass <- base64enc::dataURI(
  file = "www/butterfly-grass.png",
  mime = "image/png"
)

flower2_stone <- base64enc::dataURI(
  file = "www/flower-2-stone.png",
  mime = "image/png"
)

grass_daffodil <- base64enc::dataURI(
  file = "www/grass-daffodil.png",
  mime = "image/png"
)

microscope_ivy <- base64enc::dataURI(
  file = "www/microscope-ivy.png",
  mime = "image/png"
  )

butterflies_over_flower <- base64enc::dataURI(
  file = "www/butterflies_over_flower.png",
  mime = "image/png"
)

butterflies_over_wheat <- base64enc::dataURI(
  file = "www/butterflies_over_wheat.png",
  mime = "image/png"
)

photo_stack <- base64enc::dataURI(
  file = "www/kids_stack.JPG",
  mime = "image/jpeg"
)

photo_pond <- base64enc::dataURI(
  file = "www/kids_pond.JPG",
  mime = "image/jpeg"
)

# *************************************************
# Set NENP Theme ----
# *************************************************

nenp_theme <- bs_add_variables(
  bs_theme(
    version = 5,
    bg = "#F9F6F1",
    fg = "#132F1E",
    primary = "#657627",
    secondary = "#AABCBC",
    success = "#E7AE3F",
    info = "#657627",
    warning = "#BD4C19",
    base_font = font_link(
      family = "Work Sans",
      href = "https://fonts.googleapis.com/css2?family=Work+Sans:wght@400;600&display=swap"
    )
  )
)

# Glossary helper function ----
glossary_term <- function(term, definition) {
  tags$span(
    term,
    title = definition,
    style = "font-weight: bold; text-decoration: underline dotted; cursor: help;"
  )
}

# *************************************************
# UI ----
# *************************************************

ui <- navbarPage(
  
  # Logo and header
  title = tagList(
    
    tags$img(
      src = "https://www.educationnaturepark.org.uk/themes/custom/natureparks_theme/logo.svg",
      height = "80px",
      style = "vertical-align: middle; margin-right: 10px;"
    ),
    
    span(
      "Nature Action Explorer",
      style = "
      vertical-align: middle;
      font-size: 24px;
      font-weight: 500;
      "
    )
    
  ),
  
  # Colour palette & font
  theme = nenp_theme,
  
  header = tagList(
    
    # Bottom-right image
    tags$div(
      style = "
      position: fixed;
      bottom: 20px;
      right: 20px;
      z-index: -1;
      ",
      tags$img(
        src = cricket_grass,
        style = "
        width: 10vw;
        max-width: 100px;
        min-width: 40px;
        height: auto;
        "
      )
    ),
    
    # Error message
    shinydisconnect::disconnectMessage(
      text = "An error has occurred. Please reload the page and try again.",
      refresh = "",
      width = "full",
      size = 30,
      background = "#132F1E",
      colour = "#F9F6F1",
      overlayColour = "#F9F6F1",
      overlayOpacity = 0.7
    )
  ),
  
  # *****************************************************
  # INTRODUCTION TAB ----
  # *****************************************************
  
  tabPanel(
    
    "Introduction",
    
    fluidPage(
      
      br(),
      
      h2("Welcome"),
      
      p(
        "This tool allows you to explore how different conservation
        actions affect nature. These actions might be things like planting 
        flowers, adding a log pile, or installing a wildlife pond."
      ),
      
      p(
        "The tool works by combining data from many scientific studies carried 
        out all over the world, then analysing all these data together. Go to the 
        Scientific info page to read about the methodology and how the data were 
        collected. Go to the Explore tab to select a conservation action and see 
        its effect on nature."
      ),
      
      br(),
      
      tags$h3(
        "Where were these data collected?",
        style = "font-size: 1.3rem; font-weight: 400;"
      ),
      
      p(
        "Pan and zoom in to find out! The map shows the locations of all the datasets 
        used in the analyses."
      ),
      
      # leafletOutput(
      #   "dataset_map",
      #   height = "600px"
      # )
      fluidRow(
        
        column(
          width = 10,
          offset = 1,
          
          leafletOutput(
            "dataset_map",
            height = "600px"
          )
          
        )
        
      )
      
    )
  ),
  
  # *****************************************************
  # EXPLORE TAB ----
  # *****************************************************
  
  tabPanel(
    
    "Explore",
    
    fluidPage(
      
      # Styling for dropdown group headings ----
      tags$style(HTML("
      select optgroup {
        font-weight: 900;
        font-size: 1.1em;
        color: #132F1E;
        background-color: #F2EEE7;
        font-style: normal;
        }
                      ")),
      
      br(),
      
      h2("Explore options"),
      
      p(
      "Choose a nature action and the biodiversity you want to explore,
      then click Run."
      ),
      
      tags$p(
      "⚠️ A word of warning: if you choose all taxa, all metrics and all
      environments, the model may take over a minute to run.",
      style = "
      color: #BD4C19;
      font-size: 0.9em;
      "
      ),
      
      fluidRow(
        
        # *Left column* -------------------------------------------------
        
        column(
          
          width = 5,
          
          # Nature action selector ----
          selectInput(
            inputId = "action",
            label = tagList(
              "Nature action",
              tags$img(
                src = fork_daffodil,
                height = "20px",
                style = "margin-right: 6px;"
              ),
              tags$img(
                src = shovel_daffodil,
                height = "20px",
                style = "margin-right: 6px;"
              )
            ),
            choices = c(
              "plant flowers" = "flower_interventions",
              "create more species-rich grassland" = "grass_interventions",
              "create some dead wood habitat (e.g., log pile)" = "dead_wood",
              "install a green roof or wall" = "green_roofs_walls",
              "install a wildlife pond" = "ponds"
            )
          ),
          
          selectInput(
            inputId = "group",
            label = tagList(
              "Biodiversity group",
              tags$img(
                src = butterfly_cornflower,
                height = "20px",
                style = "margin-right: 6px;"
              )
            ),
            choices = list(

              "Functional groups" = c(
                "pollinators",
                "natural enemies",
                "pests",
                "decomposers"
              ),

              "Taxonomic groups" = c(
                "plants",
                "birds",
                "invertebrates",
                "insects",
                "mammals",
                "amphibians and reptiles",
                "fungi and bacteria",
                "all taxa"
              )

            )
          ),
          
          uiOutput("group_help"),
          
          # Biodiversity metric selector ----
          selectInput(
            inputId = "metric",
            label = tagList(
              "Biodiversity metric",
              tags$img(
                src = microscope_ivy,
                height = "25px",
                style = "margin-right: 12px;"
              )
            ),
            choices = c(
              "abundance"   = "abundance",
              "richness"    = "richness",
              "diversity"   = "diversity",
              "all metrics" = "all"
            ),
            selected = "all"
          ),
          
          # Environment selector ----
          selectInput(
            inputId = "environment",
            label = "Environment",
            choices = c(
              "urban and suburban" = "urban",
              "rural"              = "rural",
              "all environments"   = "all"
            ),
            selected = "all"
          ),
          
          # Run button ----
          actionButton(
            inputId = "run_analysis",
            label = "Run",
            class = "btn-primary"
          ),
          
          br(),
          br(),
          
          h4("Results"),
          
          htmlOutput("result_text")
          
        ),
        
        # *Right column* ------------------------------------------------
        
        column(
          
          width = 7,
          
          h4("Estimated impact"),
          
          plotOutput(
            outputId = "effect_plot",
            height = "400px"
          )
          
        )
        
      )
      
    )
    
  ),
  
  # *****************************************************
  # SCIENTIFIC INFO TAB ----
  # *****************************************************
  
  tabPanel(
    
    "Scientific info",
    
    fluidPage(
      
      br(),
      br(),
      
      h2("The science behind it all"),
      
      br(),
      
      tags$h3(
        "Short summary",
        style = "font-size: 1.4rem; font-weight: 500;"
      ),
      
      p(
        "The Nature Park research team searched for scientific studies that 
        measured how habitat enhancements (i.e., conservation actions like
        planting flowers) affect nature. They then took the results from these
        studies and analysed them all together."
      ),
      
      tags$img(
        src = methods_flow_diagram,
        style = "
        width: 100%;
        max-width: 900px;
        height: auto;
        display: block;
        margin-left: auto;
        margin-right: auto;
        "
      ),
      
      br(),
      br(),
      
      tags$h3(
        "More details",
        style = "font-size: 1.4rem; font-weight: 500;"
      ),
      
      p(
        "We first searched for relevant scientific studies and extracted their results.
        Each study might have measured several different things, and so we 
        can normally get more than one piece of evidence from a study. 
        For example, if a study counted butterflies and bees in wildflower 
        strips and cereal fields, we would have two comparisons:"
      ),
      
      tags$ol(
        style = "margin-left: 20px;",
        tags$li(
          "Butterfly abundance in wildflower strips versus butterfly abundance in cereal fields"
        ),
        tags$li(
          "Bee abundance in wildflower strips versus bee abundance in cereal fields"
        )
      ),
      
      tags$div(
        style = "
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 8px;
    flex-wrap: nowrap;
    margin: 20px 0;
  ",
        
        tags$img(
          src = butterflies_over_flower,
          style = "width: 10%; max-width: 80px; height: auto;"
        ),
        
        tags$span(
          "versus",
          style = "
            font-weight: 600;
            font-size: 1.2rem;
            white-space: nowrap;
            padding: 0 10px;
          "
        ),
        
        tags$img(
          src = butterflies_over_wheat,
          style = "width: 10%; max-width: 80px; height: auto;"
        ),
        
      ),
      
      p(
        "If that study ALSO recorded the number of different species of bee and
        butterfly in the wildflower strips and cereal fields, we would have a 
        further two comparisons:"
      ),
      
      tags$ol(
        style = "margin-left: 20px;",
        start = 3,
        tags$li(
          "Butterfly species richness in wildflower strips versus butterfly 
          species richness in cereal fields"
        ),
        tags$li(
          "Bee species richness in wildflower strips versus bee species richness 
          in cereal fields"
        )
      ),
      
      p(
        "Each of these comparisons is called an ",
        glossary_term(
          "effect size",
          "A measure of how large the difference is between an intervention and a control. (Go to the glossary for a fuller explanation)."
        ),
        ". Effect sizes tell us how big the difference is."
      ),
      
      p(
        "For example, for bee abundance, the impact 
        of planting wildflower stips might have been that there were twice as 
        many bees in the flower strips than in the cereal fields. So, for this 
        comparison, the 'effect size' of planting wildflower strips would be a 
        two-fold (or a 100%) increase in the number of bees."
      ),
      
      p(
        "You can read more about how the team pulls out results for the meta-analysis in this ",
        tags$a(
          "blog",
          href = "https://www.educationnaturepark.org.uk/will-planting-wildflowers-attract-pollinators-lets-explore-evidence",
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        "."
      ),
      
      br(),
      
      tags$h3(
        "What do we do with all these effect sizes?",
        style = "font-size: 1.4rem; font-weight: 500;"
      ),
      
      p(
        "Once we've found relevant studies and extracted all the effect sizes 
        from them, we build statistical models that can analyse all the data
        together and come up with one overall average effect. This is the 
        'meta-analysis' part. (The 'meta' bit just means we're doing an 
        'analysis of analyses', rather than collecting new observations 
        ourselves)."
      ),
      
      p(
        "The models can account for the fact that effect sizes 
        from each study are not 'independent'. For example, in the study above 
        that counted butterflies and bees (let's call it study A), the 
        scientists might have done things slightly differently to another team 
        of scientists that also studied butterflies and bees in wildflower  
        strips and cereal fields (let's call this one study B).
        We need to be able to tell the model 'hey, the effect sizes from study
        A will be more similar to each other than they will be to the effect 
        sizes from study B'. This helps us get a more accurate overall estimated 
        effect."
      ),
      
      p(
        tags$strong(
          "So, to use more scientific language, meta-analysis is a "
        ),
        glossary_term(
          "systematic",
          "An organised and repeatable way of doing things."
        ),
        tags$strong(
          " way of drawing out the results from many scientific papers and summarising them to create a "
        ),
        glossary_term(
          "consensus",
          "Agreement."
        ),
        tags$strong(
          "."
        )
      ),
      
      p(
        "See ",
        tags$a(
          "this paper",
          href = "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1002/2688-8319.70070",
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        " to read more about the methodology."
      ),
      
      br(),
      
      tags$h3(
        "How to interpret the results",
        style = "font-size: 1.4rem; font-weight: 500;"
      ),
      #h3("How to interpret the results"),
      
      p(
        "Results are shown as an average effect estimated across studies. 
        The confidence intervals indicate the uncertainty around this estimate.
        They show the plausible range of values in which the true effect could lie."
      ),
      
      p(
        "In reality, we can never know the 'true' effect, and in fact there won't 
        ever be one 'true' effect because nature is so variable. Any effect will be 
        different in different contexts. We've given you the option to pick whether
        your school is in a rural or urban/suburban context, but there will also
        be other things that affect how well a conservation action works. For 
        example, the types of flowers planted or the region of the world or 
        whether there is a nature reserve nearby."
      )
      
    ),
    
    br(),
    br(),
    
    tags$div(
      style = "
    display: flex;
    justify-content: center;
    align-items: flex-start;
    gap: 20px;
    margin-top: 20px;
    margin-bottom: 20px;
  ",
      
      tags$figure(
        style = "max-width: 450px;",
        
        tags$img(
          src = photo_stack,
          style = "
        width: 100%;
        height: auto;
        border-radius: 6px;
      "
        ),
        
        tags$figcaption(
          "School children building a wildlife stack.",
          style = "
        text-align: center;
        margin-top: 8px;
        font-style: italic;
      "
        )
      ),
      
      tags$figure(
        style = "max-width: 450px;",
        
        tags$img(
          src = photo_pond,
          style = "
        width: 100%;
        height: auto;
        border-radius: 6px;
      "
        ),
        
        tags$figcaption(
          "School children creating a wildlife pond.",
          style = "
        text-align: center;
        margin-top: 8px;
        font-style: italic;
      "
        )
      )
      
    )
    
  ),
  
  # *****************************************************
  # GLOSSARY TAB ----
  # *****************************************************
  
  tabPanel(
    
    "Glossary",
    
    fluidPage(
      
      br(),
      br(),
      
      h2("Glossary"),
      
      br(),
      br(),
      
      #h3("Ecological terms"),
      tags$h3(
        "Ecological terms",
        style = "font-size: 1.3rem; font-weight: 500;"
      ),
      
      tags$dl(
        tags$dt(
          tags$strong("Biodiversity")
        ),
        tags$dd(
          "The number and type of all living things present."
        )
      ),
      
      br(),
      br(),
      
      #h3("Other terms"),
      tags$h3(
        "Other terms",
        style = "font-size: 1.3rem; font-weight: 500;"
      ),
      
      tags$dl(
        tags$dt(
          tags$strong("Systematic")
        ),
        tags$dd(
          "An organised and repeatable way of doing things."
        ),
        
        tags$dt(
          tags$strong("Consensus")
        ),
        tags$dd(
          "Agreement."
        ),
        
        tags$dt(
          tags$strong("Effect size")
        ),
        tags$dd(
          "The measured impact from a single comparison. For example, the difference 
          between the numbers of bees counted in a wildflower strip compared to 
          in a cereal field. Or, to put it another way, it's a measure of how 
          large the difference is between an intervention and a control."
        )
      )
      
    )
  )
  
# end of UI
)

# *****************************************************
# Server ----
# *****************************************************

# Server code superseded ----
# server <- function(input, output, session) { 
# Server code new ----
server <- function(input, output, session) {
  
  # *****************************************************
  # Map ----
  # *****************************************************
  
  # Create a map to show locations of the datasets
  output$dataset_map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2,
        maxBounds = list(
          c(-87, -180),  # southwest corner
          c(85, 210)     # northeast corner
        ),
        maxBoundsViscosity = 1.0
      )
    ) |>
      
      addTiles(
        urlTemplate =
          paste0(
            "https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=cb1_570s_1_f23e1c4397c3b906cc91b269",
            Sys.getenv("CARTO_API_KEY")
          ),
        options = tileOptions(
          noWrap = TRUE
        ),
        attribution = "© CARTO © OpenStreetMap contributors"
      )  |>
      
      fitBounds(
        lng1 = -180,
        lat1 = -60,
        lng2 = 180,
        lat2 = 85
      ) |>
      
      addCircleMarkers(
        data = dataset_points,
        lng = ~longitude,
        lat = ~latitude,
        
        radius = 6,
        stroke = TRUE,
        weight = 1,
        
        color = "white",
        fillColor = "#132F1E",
        fillOpacity = 0.85,
        
        popup = ~paste0(
          "<b>Dataset ID:</b> ", dataset_ID,
          "<br><b>Country:</b> ", country
        ),
        
        clusterOptions = markerClusterOptions()
      )
    
  })
  
  # Biodiversity group definitions ----
  output$group_help <- renderUI({
    
    help_text <- switch(
      
      input$group,
      
      "pollinators" =
        "Beneficial insects that pollinate flowers, such as bumblebees.",
      
      "natural enemies" =
        "These are beneficial insects and other animals that control plant pests. 
        They are predators or parasites of these pests. For example, ladybirds 
        are natural enemies because they eat aphids.",
      
      "pests" =
        "Animals that can damage plants, crops or gardens, such as aphids.",
      
      "decomposers" =
        "Fungi, bacteria that break down dead material such as leaves and wood into soil nutrients.",
      
      "plants" =
        "Plants such as grasses, wildflowers, shrubs and trees.",
      
      "birds" =
        "Feathered animals such as robins, blackbirds and sparrows.",
      
      "invertebrates" =
        "Animals without a backbone. They include insects, spiders, snails, woodlice and worms.",
      
      "insects" =
        "Invertebrates with six legs, such as bees, butterflies, beetles and flies.",
      
      "mammals" =
        "Warm-blooded animals with fur or hair that feed milk to their young, such as hedgehogs and bats.",
      
      "amphibians and reptiles" =
        "Animals such as frogs, toads, newts, snakes and lizards.",
      
      "fungi and bacteria" =
        "Micro-organisms that help recycle nutrients and keep ecosystems working.",
      
      "all taxa" =
        "Includes all biodiversity groups combined."
      
    )
    
    tags$p(
      help_text,
      style = "
      font-size: 0.9em;
      color: #335945;
      font-style: italic;
      margin-top: 5px;
      margin-bottom: 15px;
    "
    )
    
  })
  
  # ************************************************
  # Analysis run button ----
  # ************************************************
  
  observeEvent(input$run_analysis, {
    
    tryCatch({
      
      # ************************************************
      ## FILTERING ----
      # ************************************************
      
      # Filter by nature action ----
      dat_subset <- dat |>
        dplyr::filter(
          target_intervention == input$action
        )
      
      # Filter by biodiversity metric ----
      if (input$metric != "all") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            bd_metric_type == input$metric
          )
        
      }
      
      # Filter by functional or taxonomic group ----
      if (input$group %in% c(
        "pollinators",
        "natural enemies",
        "pests",
        "decomposers"
      )) {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            grepl(input$group, functional_group)
          )
        
      } else if (input$group == "mammals") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com == "mammals"
          )
        
      } else if (input$group == "plants") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com == "plants"
          )
        
      } else if (input$group == "birds") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com == "birds"
          )
        
      } else if (input$group == "fungi and bacteria") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com %in% c("fungi", "bacteria")
          )
        
      } else if (input$group == "amphibians and reptiles") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com %in% c("amphibians", "reptiles")
          )
        
      } else if (input$group == "invertebrates") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com %in% c(
              "ants",
              "aphids",
              "arthropods",
              "bees",
              "beetles",
              "bugs",
              "butterflies",
              "flies - hoverflies",
              "flies - other flies",
              "insects",
              "invertebrates",
              "mites",
              "moths",
              "nematodes",
              "orthoptera",
              "spiders",
              "wasps",
              "worms"
            )
          )
        
      } else if (input$group == "insects") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            tax_com %in% c(
              "bees",
              "flies - hoverflies",
              "flies - other flies",
              "wasps",
              "bugs",
              "insects",
              "aphids",
              "orthoptera",
              "ants",
              "butterflies",
              "moths"
            )
          )
        
      }
      
      # Filter by urban / rural ----
      if (input$environment == "urban") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            land_use_context_ur %in% c("urban", "suburban")
          )
        
      } else if (input$environment == "rural") {
        
        dat_subset <- dat_subset |>
          dplyr::filter(
            land_use_context_ur == "rural"
          )
        
      }
      
      # Count number of observations ----
      n_comparisons <- nrow(dat_subset)
      
      if (n_comparisons == 0) {
        stop("No data available for this combination of action, biodiversity 
             group, metric and environment.")
      }
      
      # Count number of studies ----
      n_studies <- dplyr::n_distinct(dat_subset$study_ID)
      
      # Require a minimum number of studies
      if (n_studies < 5) {
        
        msg <- if (n_studies == 1) {
          "There is only 1 study available for this combination of nature action, biodiversity group, metric and environment."
        } else {
          paste0(
            "There are only ",
            n_studies,
            " studies available for this combination of nature action, biodiversity group, metric and environment."
          )
        }
        
        stop(
          paste(
            msg,
            "At least 5 studies are required before an analysis can be run."
          )
        )
        
      }
      
      # ************************************************
      ## MODEL ----
      # ************************************************
      
      # Fit model
      model <- rma.mv(
        yi = lnrr,
        V = v_lnrr,
        random = ~ 1 | dataset_ID/es_ID,
        data = dat_subset
      )
      
      # Extract results ----

      # Response ratio
      beta_rr <- predict(model, transf = exp)[[1]]
      #ci_u_rr <- predict(model, transf = exp)[[3]]
      #ci_l_rr <- predict(model, transf = exp)[[4]]
      
      # Percentage change
      pct_change <- 100 * (exp(model$b[1]) - 1)
      lower_pct  <- 100 * (exp(model$ci.lb[1]) - 1)
      upper_pct  <- 100 * (exp(model$ci.ub[1]) - 1)
      
      # Results graph ----
      
      # specify colour for plotting
      col <- dplyr::case_when(
        pct_change > 0 ~ "#657627",  # increase
        pct_change < 0 ~ "#BD4C19",  # decrease
        TRUE           ~ "#AABCBC"   # no change
      )
      
      # specify plot
      output$effect_plot <- renderPlot({
        
        plot(
          pct_change,
          1,
          xlim = c(
            min(lower_pct, 0) - 10,
            max(upper_pct, 0) + 10
          ),
          ylim = c(0.5, 1.5),
          pch = 19,
          cex = 2,
          col = "#657627",
          yaxt = "n",
          ylab = "",
          xlab = "Estimated percentage change (%)",
          main = "Estimated biodiversity response"
        )
        
        segments(
          lower_pct,
          1,
          upper_pct,
          1,
          lwd = 4,
          col = "#657627"
        )
        
        abline(
          v = 0,
          lty = 2,
          col = "grey50"
        )
        
      })
      
      # ************************************************
      ## CREATE RESULTS OUTPUT ----
      # ************************************************
      
      # Action label ----
      action_label <- dplyr::case_when(
        input$action == "flower_interventions" ~ "plant flowers",
        input$action == "grass_interventions" ~ "create more species-rich grassland",
        input$action == "dead_wood" ~ "create some dead wood habitat (e.g., log pile)",
        input$action == "green_roofs_walls" ~ "install a green roof or wall",
        input$action == "ponds" ~ "install a wildlife pond",
        TRUE ~ input$action
      )
      
      # Metric label ----
      metric_label <- dplyr::case_when(
        input$metric == "abundance" ~ "abundance",
        input$metric == "richness" ~ "species richness",
        input$metric == "diversity" ~ "diversity",
        input$metric == "all" ~ "biodiversity",
        TRUE ~ input$metric
      )
      
      # Group label ----
      group_label <- dplyr::case_when(
        input$group == "pollinators" ~ "pollinator",
        input$group == "natural enemies" ~ "natural enemy",
        input$group == "pests" ~ "pest",
        input$group == "decomposers" ~ "decomposer",
        input$group == "plants" ~ "plant",
        input$group == "birds" ~ "bird",
        input$group == "mammals" ~ "mammal",
        input$group == "invertebrates" ~ "invertebrate",
        TRUE ~ input$group
      )
      
      # Environment label ----
      environment_label <- dplyr::case_when(
        input$environment == "urban" ~ " in an urban or suburban environment",
        input$environment == "rural" ~ " in a rural environment",
        input$environment == "all" ~ "",
        TRUE ~ ""
      )
      
      # Effect text ----
      effect_text <- if (beta_rr > 1.001) {
        
        paste0(
          group_label, " ",
          metric_label,
          " would be ",
          round(beta_rr, 2),
          " times the control. This is an estimated ",
          round(pct_change, 0),
          "% increase."
        )
        
      } else if (beta_rr < 0.999) {
        
        paste0(
          group_label, " ",
          metric_label,
          " would be ",
          round(beta_rr, 1),
          " times the control. This is an estimated ",
          round(abs(pct_change), 0),
          "% decrease."
        )
        
      } else {
        
        paste0(
          group_label, " ",
          metric_label,
          " would be the same as the control. This indicates no change."
        )
        
      }
      
      highlight_daffodil <- "#E7AE3F"
      highlight_cornflower <- "#337796"
      highlight_poppy <- "#BD4C19"
      highlight_ivy <- "#335945"
      highlight_butterfly <- "#C87D2C"
      highlight_grass <- "#849931"
      
      # Output text ----
      output$result_text <- renderUI({
        
        HTML(
          paste0(
            
            "If you <span style='font-weight:bold; color:",
            highlight_daffodil,
            ";'>",
            action_label,
            "</span> ",
            
            "<span style='font-weight:bold; color:",
            highlight_poppy,
            ";'>",
            environment_label,
            "</span> ",
            
            "<span style='font-weight:bold; color:",
            highlight_cornflower,
            ";'>",
            effect_text,
            "</span>",
            
            "<br><br>",
            
            "However, because the evidence is not perfectly certain, the true 
            average effect could plausibly be between ",
            
            "<span style='font-weight:bold; color:",
            highlight_cornflower,
            ";'>",
            round(lower_pct, 0),
            "% and ",
            round(upper_pct, 0),
            "%",
            "</span>.",
            
            "<br><br>",
            
            "This analysis used ",
            
            "<span style='font-weight:bold; color:",
            highlight_grass,
            ";'>",
            n_comparisons,
            "</span>",
            
            " comparisons of intervention versus control from ",
            
            "<span style='font-weight:bold; color:",
            highlight_grass,
            ";'>",
            n_studies,
            "</span>",
            
            " studies."
            
          )
        )
        
      })
      
    }, error = function(e) {
      
      output$result_text <- renderUI({
        
        HTML(
          paste0(
            "<strong>The analysis could not be completed.</strong><br><br>",
            "Error: ",
            e$message
          )
        )
        
      })
      
      output$effect_plot <- renderPlot({
        plot.new()
      })
      
    })
    
  })
  
}


# Run ----
shinyApp(ui, server)
#runApp(launch.browser = TRUE)

#rsconnect::deployApp() #NB only run this code in the console! Never have it in the app.
