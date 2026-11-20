#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(mapgl)
library(shinyFiles)
library(bslib)
library(shinyjs)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  
  bg = "#F5F4EE",
  fg = "#303B32",
  
  primary = "#355E45",
  secondary = "#87947D",
  success = "#66845C",
  info = "#799B9B",
  warning = "#C69B53",
  danger = "#A65D4F",
  
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Barlow Semi Condensed")
)

source("functions.R")
# Define UI for application that draws a histogram
tags$head(
  tags$style(HTML("
    body {
      background-color: #F5F4EE;
    }

    .well {
      background-color: #FFFFFF;
      border: 1px solid #DADDD3;
      border-radius: 10px;
      box-shadow: 0 2px 8px rgba(48, 59, 50, 0.05);
    }

    .btn-primary {
      font-weight: 600;
      border-radius: 6px;
    }

    h2, h3, h4 {
      color: #355E45;
    }

    .navbar {
      background-color: #355E45;
    }
  "))
)

ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  
  titlePanel(
    div(
      h2("Wavetable from Terrain")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Terrain Selection"),
      checkboxInput(
        "force_square",
        "Force square AOI",
        TRUE
      ),
      
      hr(),
      
      actionButton(
        "generate",
        "Generate Wavetable",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(),
      br(),
      
      textInput("filename",
                "Filename:",
                "wavetable_from_terrain"),
      hr(),
      uiOutput("download_ui")
    ),
    
    mainPanel(
      width = 9,
      mapgl::maplibreOutput(
        "terrain_map",
        height = "750px"
      )
    )
  )
)


server <- function(input, output, session) {
  
  wav_file <- reactiveVal(NULL)
  
  output$terrain_map <- renderMaplibre({
    
    mapgl::maplibre(style = maptiler_style("outdoor")) |>
      mapgl::fit_bounds(
        bbox = us,
        padding = 10
      ) |>
      mapgl::add_draw_control(
        position = "top-left",
        provider = "terra-draw",
        modes = c("rectangle"),
        download_button = FALSE
      )
    
  })
  
  observeEvent(input$generate, {
    wav_file(NULL)
    drawn_features <- mapgl::get_drawn_features(
      mapgl::maplibre_proxy("terrain_map")
    )
    
    req(drawn_features)
    
    print(drawn_features)
    print(st_crs(drawn_features))
    print(st_bbox(drawn_features))
    # Convert drawn rectangle to a square AOI
    processing_aoe <- if (input$force_square) {
      make_square(drawn_features)
    } else {
      drawn_features
    }
    
    # Display the square on the map
    mapgl::maplibre_proxy("terrain_map") |>
      mapgl::add_source(
        id = "square_aoi",
        data = processing_aoe
      ) |>
      mapgl::add_fill_layer(
        id = "square_aoi_fill",
        source = "square_aoi",
        fill_color = "#3388ff",
        fill_opacity = 0.15
      ) |>
      mapgl::add_line_layer(
        id = "square_aoi_outline",
        source = "square_aoi",
        line_color = "#0066ff",
        line_width = 3
      )
    
    showNotification("Getting terrain from USGS.")
    
    terrain <- get_terrain(
      processing_aoe,
      width = 256,
      height = 256
    )
    
    showNotification("Got terrain.")
    
    output_path <- tempfile(fileext = ".wav")
    
    showNotification("Generating wavetable.")
    
    generate_wavetable(
      terrain_rast = terrain,
      aoe = processing_aoe,
      path = output_path
    )
    
    wav_file(output_path)
    
    showNotification("Wavetable generated.")
    
  })
  
  output$download_wav <- downloadHandler(
    
    filename = function() {
      paste0(input$filename, ".wav")
    },
    
    content = function(file) {
      
      req(wav_file())
      
      file.copy(
        wav_file(),
        file,
        overwrite = TRUE
      )
      
    },
    
    contentType = "audio/wav"
    
  )
  
  output$download_ui <- renderUI({
    req(wav_file())
    downloadButton(
      "download_wav",
      "Download WAV",
      class = "btn-secondary",
      width = "100%"
    )
  })
}

shinyApp(ui, server)