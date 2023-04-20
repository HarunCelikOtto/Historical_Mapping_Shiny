#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fillPage(
  navbarPage("Historical Mapping", 
             id="nav",
             
    tabPanel(title = "Interactive Map",
             div(class="outer",
                
             tags$head(
               includeCSS("styles.css")
             ),
             
             
             leafletOutput("mymap", 
                           width = "100%", 
                           height = "100%")
               ))))
             
             

# Define server logic required to draw a histogram
server <- function(input, output) {

     output$mymap <- renderLeaflet({
       leaflet() %>%
         addProviderTiles(provider = providers$OpenStreetMap) %>%
         setView(lng = -92.4303707, lat = 42.4000755, zoom = 12)
     })
}


# Run the application 
shinyApp(ui = ui, server = server)
