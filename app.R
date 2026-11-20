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

source("functions.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Philmont Terrain Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        "filename",
        "Wavetable filename:",
        value = "philmont_terrain"
      ),
      
      actionButton("generate", "Generate Wavetable"),
      
      downloadButton(
        "download_wav",
        "Download WAV"
      )
    ),
    
    mainPanel(
      mapgl::maplibreOutput("terrain_map", height = "700px"),
      verbatimTextOutput("feature_output")
    )
  )
)


server <- function(input, output, session) {
  
  wav_file <- reactiveVal(NULL)
  
  output$terrain_map <- renderMaplibre({
    
    mapgl::maplibre() |>
      mapgl::add_view(
        data = philmont_display_data,
        layer_id = "terrain",
        legend = FALSE
      ) |>
      mapgl::fit_bounds(
        bbox = raster_ext,
        padding = 30
      ) |>
      mapgl::add_draw_control(
        position = "top-left",
        rectangle = TRUE,
        freehand = TRUE,
        download_button = TRUE
      )
    
  })
  
  observeEvent(input$generate, {
    
    req(input$filename)
    
    drawn_features <- mapgl::get_drawn_features(
      mapgl::maplibre_proxy("terrain_map")
    )
    
    req(!is.null(drawn_features))
    req(nrow(drawn_features) > 0)
    
    # Generate temporary WAV
    output_path <- tempfile(fileext = ".wav")
    
    generate_wavetable(
      philmont_clean,
      drawn_features,
      output_path
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
  
}

shinyApp(ui, server)