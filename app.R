library(shiny)
library(leaflet)
library(RColorBrewer)
library(RCurl)
library(htmltools)

m <- getURL('https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv')

class(m)
m <- read.csv(textConnection(m), header=T)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$h4(
  tags$div(class="text-center", "Earthquakes for the last 24 hours updated every 1 minute (Casey Jenkins)")),
  tags$h6(
  tags$div(class="text-center", "Data Source: https://earthquake.usgs.gov")),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  outline <- m[chull(m$longitude, m$latitude),]
  
  getColor <- function(quakes) {
    sapply(quakes$mag, function(mag) {
      if(mag <= 4) {
        "green"
      } else if(mag <= 5) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-pulse',
    iconColor = "black",
    library = 'ion',
    markerColor = getColor(m)
  )
  
  output$map <- renderLeaflet({
    leaflet(m) %>% addTiles() %>% 
      addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(mag)) %>%
      addPolygons(data = outline, lng = ~longitude, lat = ~latitude,
                  fill = F, weight = 2, color = "white", group = "Outline") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addLayersControl(
        overlayGroups = c("Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)
