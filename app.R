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
             class="docPage",
             
             sidebarLayout(
               sidebarPanel("",
                            selectInput("documentSelect", "Select Raster", choices = c("Original", 
                                                                                 "Stretched", 
                                                                                 "Segmented", 
                                                                                 "Classified"))),
               
               mainPanel(htmlOutput("htmlGoesHere"))
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
  
  # Define documentText as selected.
  documentText <- reactive({
    if (input$documentSelect == "Original") {
      documentText <- "html_files/Original_Raster.html"
    }
    else if (input$documentSelect == "Stretched") {
      documentText <- "html_files/Stretch_Raster.html"
    }
    else if (input$documentSelect == "Segmented") {
      documentText <- "html_files/Segmented_Raster.html"
    }
    else if (input$documentSelect =="Classified") {
      documentText <- "html_files/Classified_Raster.html"
    }
  })
  
     output$mymap <- renderLeaflet({
       leaflet() %>%
         addProviderTiles(provider = providers$OpenStreetMap) %>%
         setView(lng = -92.0485599, lat = 42.6008780, zoom = 11)
     })
     
     
     output$htmlGoesHere <- renderUI(
       includeHTML(documentText())
     )
     
     observe({
       leafletProxy("mymap") %>%
         addRasterImage(leafletImage())
       })

     
}


# Run the application 
shinyApp(ui = ui, server = server)
