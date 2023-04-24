library(shiny)
library(leaflet)
library(dplyr)
library(raster)


# Define UI for application that has a tab for a leaflet map and a documentation tab 
ui <- fillPage(
  navbarPage("Historical Mapping", 
             id="nav",
    #This is the interactive map tab on page.         
    tabPanel(title = "Interactive Map",
             div(class="outer",
                
             tags$head(
               includeCSS("styles.css")
             ),
             
             
             leafletOutput("mymap", 
                           width = "100%", 
                           height = "100%"),
             
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           h2("Raster Selector"),
                           
                           selectInput("rasters", "Display Raster", choices = c("Original", 
                                                                     "Stretched", 
                                                                     "Segmented", 
                                                                     "Classified"))),
        
               )),
    #This is the documentation tab page.
    tabPanel(title = "Documentation",
             titlePanel("Classified Maps"),
             
             sidebarLayout(
               sidebarPanel("",
                            selectInput("rasters", "Select Raster", choices = c("Original", 
                                                                                 "Stretched", 
                                                                                 "Segmented", 
                                                                                 "Classified"))),
               mainPanel(h2("Original Raster"),
                         p("Here is more text"))
               )))
    
    
    
    )
             
             
# Define server logic
server <- function(input, output) {

  Buchanan_Original <- raster("img/png_prj/Postal_Buchanan_Route.png")
  Buchanan_Stretched <- raster("img/png_prj/Buchanan_Stretch.png")
  Buchanan_Segmented <- raster("img/png_prj/Buchanan_Segmented.png")
  Buchanan_Segmented_Classified <- raster("img/png_prj/Buchanan_Segmented_Classified.png")
  
  # Define leafletImage as selected.
  leafletImage <- reactive({
    if (input$rasters == "Original") {
    leafletImage <- Buchanan_Original
    }
    else if (input$rasters =="Stretched") {
      leafletImage <- Buchanan_Stretched
    }
    else if (input$rasters == "Segmented") {
      leafletImage <- Buchanan_Segmented
    }
    else if (input$rasters == "Classified") {
      leafletImage <- Buchanan_Segmented_Classified
    }
  })
  
     output$mymap <- renderLeaflet({
       leaflet() %>%
         addProviderTiles(provider = providers$OpenStreetMap) %>%
         setView(lng = -92.0485599, lat = 42.6008780, zoom = 11)
     })
     
     observe({
       leafletProxy("mymap") %>%
         addRasterImage(leafletImage())
     })

     
}


# Run the application 
shinyApp(ui = ui, server = server)
