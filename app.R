library(shiny)
library(leaflet)
library(dplyr)

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
                           
                           selectInput("color", "Display Raster", choices = c("Original", 
                                                                     "Stretched", 
                                                                     "Segmented", 
                                                                     "Classified"))),
        
               )),
    #This is the documentation tab page.
    tabPanel(title = "Documentation")
    ))
             
             

# Define server logic
server <- function(input, output) {

     output$mymap <- renderLeaflet({
       leaflet() %>%
         addProviderTiles(provider = providers$OpenStreetMap) %>%
         setView(lng = -92.358665, lat = 42.499504, zoom = 12)
     })
}


# Run the application 
shinyApp(ui = ui, server = server)
