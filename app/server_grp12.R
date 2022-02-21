#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("global.R")

library(shiny)


shinyServer(function(input, output) {
  ####################### Tab 2 Map ##################
  map_base <-
    leaflet(options = leafletOptions(dragging = T, minZoom = 10, maxZoom = 16)) %>%
    setView(lng = -73.92,lat = 40.72, zoom = 11) %>% 
    addTiles() %>%
    addProviderTiles("CartoDB.Positron")
  
  # join zipcode geo with covid data from nyc_recent_4w_data
  nyc_zipcode_geo = nyc_zipcode_geo %>%
    left_join(nyc_covid_data, by = c("ZIPCODE"="MODIFIED_ZCTA"))
    #left_join(nyc_shooting_data, by = c("ZIPCODE"="Zip"))
  
  pal <- colorFactor(palette = c('yellow', 'green', 'orange', 'red', 'blue'),domain = nyc_covid_data$BOROUGH_GROUP)
  
  observe({
    output$nyc_map_covid = renderLeaflet({
      
      nyc_map_output = map_base %>% 
        addPolygons( 
          data = nyc_zipcode_geo,
          weight = 0.5, color = "#41516C", fillOpacity = 0,
          popup = ~(paste0( 
            "<b>Zip Code: ",ZIPCODE ,
            "</b><br/>Borough: ",BOROUGH_GROUP,
            "<br/>Confirmed Cases: ", COVID_CASE_COUNT
          )),
          highlight = highlightOptions(
            weight = 2, color = "red", bringToFront = F) ) %>%
        addCircleMarkers(
          data = nyc_zipcode_geo,
          lng = ~LNG_repre, lat = ~LAT_repre,
          color = ~pal(BOROUGH_GROUP), fillOpacity = 0.7,
          radius = ~(COVID_CASE_COUNT)/1000, 
          popup = ~(paste0(
            "<b>Zip Code: ", ZIPCODE,
            "</b><br/>Confirmed Cases: ", COVID_CASE_COUNT
          )),
          group = "Covid Cases"
        ) %>%
        addCircles(
          data = nyc_shooting_data,
          radius = 0.7,
          color = 'black',
          opacity = 1,
          lng = ~Longitude, lat = ~Latitude, popup = "Shooting Case")
    }) # end of observe
    
    leafletProxy("nyc_map_covid")
    
  })
})