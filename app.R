library(shiny)
library(bslib)
library(shinydisconnect)
library(base64enc)

cricket_img <- base64enc::dataURI(
  file = "www/cricket-grass.png",
  mime = "image/png"
)


# =====================================================
# NENP Theme
# =====================================================

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

# =====================================================
# UI
# =====================================================

ui <- navbarPage(
  
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
  
  theme = nenp_theme,
  
  header = tagList(
    
    # Bottom-right image
    tags$div(
      style = "
      position: fixed;
      bottom: 20px;
      right: 20px;
      z-index: 1000;
      ",
      tags$img(
        src = cricket_img,
        style = "
        width: 10vw;
        max-width: 100px;
        min-width: 40px;
        height: auto;
        "
      )
    ),
    
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
  
  # =====================================================
  # Introduction Tab
  # =====================================================
  
  tabPanel(
    
    "Introduction",
    
    fluidPage(
      
      br(),
      
      h2("Welcome"),
      
      p(
        "This tool allows you to explore how different conservation
        actions (like planting flowers, adding a log pile, or installing a 
        wildlife pond) may affect nature."
      ),
      
      p(
        "Go to the Explore tab to select a conservation action and see its
        impact on nature."
      )
      
    )
  ),
  
  # =====================================================
  # Explore Tab
  # =====================================================
  
  tabPanel(
    
    "Explore",
    
    fluidPage(
      
      br(),
      
      h2("Explore options"),
      
      p(
        "Choose a nature action and biodiversity group, then click Run."
      ),
      
      selectInput(
        inputId = "action",
        label = "Nature action",
        choices = c(
          "plant flowers",
          "add or improve grassland",
          "add a log pile",
          "install a green roof or wall"
        )
      ),
      
      # Future version:
      # choices = sort(unique(dat$Action))
      
      selectInput(
        inputId = "group",
        label = "Biodiversity group",
        choices = c(
          "pollinators",
          "natural enemies",
          "pests",
          "decomposers",
          "plants",
          "birds",
          "invertebrates",
          "mammals",
          "amphibians and reptiles",
          "fungi and bacteria",
          "all taxa"
        )
      ),
      
      # Future version:
      # choices = sort(unique(dat$Group))
      
      actionButton(
        inputId = "run_analysis",
        label = "Run",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      h4("Results"),
      
      textOutput("result_text")
      
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$run_analysis, {
    
    # Placeholder result
    output$result_text <- renderText({
      
      paste0(
        "If you ",
        input$action,
        ", then ",
        input$group,
        " will increase by 15% ",
        "(but this could range from 5% to 25%)."
      )
      
    })
    
  })
  
}

shinyApp(ui, server)
#runApp(launch.browser = TRUE)
