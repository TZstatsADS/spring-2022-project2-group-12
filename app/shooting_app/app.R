#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################

if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("readr")) {
    install.packages("readr")
    library(readr)
}
if (!require("tidyverse")) {
    install.packages("tidyverse")
    library(tidyverse)
}
if (!require("lubridate")) {
    install.packages("lubridate")
    library(lubridate)
}
if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(lubridate)
}
if (!require("plotly")) {
    install.packages("plotly")
    library(plotly)
}
if (!require("viridis")) {
    install.packages("viridis")
    library(viridis)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("geosphere")) {
  install.packages("geosphere")
  library(geosphere)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}

shooting_map_sp <- readRDS("shooting_map_sp.RDS")
shooting_map_sp <- shooting_map_sp %>% arrange(OCCUR_YM)

#===============================================Shiny UI=========================================================

ui <- navbarPage(
    "Crimes",
    #################### tab 1 ####################
    tabPanel(
        "Shooting",
        sidebarLayout(
            sidebarPanel(
                
                selectInput(inputId = "period",
                            label = "Choose a time period", 
                            choices = c("Overall Period","Covid Period")),
                selectInput(inputId = "date",
                            label = "Select a date (month ending in):",
                            choices = levels(as.factor(unique(shooting_map_sp$OCCUR_YM))))
            ),
            
            mainPanel(
                plotlyOutput(outputId = "shooting"),
                leafletOutput("shooting_map_interactive")
                )
        )
    )
    )


#===============================================Shiny SERVER=====================================================
# LOAD AND PREPARE DATA ####################################

shinyServer <- function(input, output) {

    shooting_recent <- read_csv("NYPD_Shooting_Incident_Data_recent.csv")
    shooting_historic <- read_csv("NYPD_Shooting_Incident_Data__Historic.csv")
    shooting_recent <- shooting_recent %>% rename(`Lon_Lat` = `New Georeferenced Column`)
    shooting_all <- rbind(shooting_recent, shooting_historic) # combine two datasets using rbind()
    
    shooting_all_overall <- shooting_all %>% 
                    mutate(OCCUR_DATE = as.Date(OCCUR_DATE, "%m/%d/%Y"),
                           OCCUR_YM =  floor_date(OCCUR_DATE, unit = "month"),
                           BORO = as.factor(BORO)) %>%
                    group_by(OCCUR_YM, BORO) %>%
                    summarise(count = n())

    shooting_all_covid <- shooting_all %>% 
                        mutate(OCCUR_DATE = as.Date(OCCUR_DATE, format = "%m/%d/%Y"),
                               OCCUR_YM = floor_date(OCCUR_DATE, unit = "month"),
                               BORO = as.factor(BORO)) %>%
                        filter(OCCUR_YM >= "2020-03-01") %>% 
                        group_by(OCCUR_YM, BORO) %>%
                        summarise(count = n())
    

    
    output$shooting <- renderPlotly({
        if("Overall Period" %in% input$period){
            ggplot(shooting_all_overall, aes(x = OCCUR_YM, y = count, group = BORO, color = BORO)) +
                geom_line()+
                scale_color_viridis(discrete = TRUE, option = "G") +
                scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
                labs(y = "Number of Shooting Occurred", x = "Date", title = "NYC Shooting Occurrence Since 2006") +
                guides(color=guide_legend(title="Location")) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45),
                  axis.text.y = element_text(angle = 45),
                  plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
            ggplotly(width = 1000)
        }
        else if("Covid Period" %in% input$period){
            ggplot(shooting_all_covid, aes(x = OCCUR_YM, y = count, group = BORO, color = BORO)) +
                geom_line()+
                scale_color_viridis(discrete = TRUE, option = "G") +
                scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
                labs(y = "Number of Shooting Occurred", x = "Date", title = "NYC Shooting Occurrence During Covid") +
                guides(color=guide_legend(title="Location")) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45),
                      axis.text.y = element_text(angle = 45),
                      plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
            
            ggplotly(width = 1000)
        }
        
        })
    
    monthly_shooting <- reactive({
      m <- shooting_map_sp %>% filter(OCCUR_YM == input$date)
      return(m)
    })
    
    output$shooting_map_interactive <- renderLeaflet({
      labels <- sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>%g shooting cases", monthly_shooting()$BORO, monthly_shooting()$MODZCTA, 
                        monthly_shooting()$OCCUR_YM,monthly_shooting()$count) %>%
        lapply(htmltools::HTML)
      
      pal <- colorBin(palette = "OrRd", 9, domain= shooting_map_sp$count, bins = c(1,2,4,6,8,10,15,20,31)) # caution: the bins are not equally sized 
      
      monthly_shooting() %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(-73.9, 40.7, zoom = 10) %>%     # for initial view of NYC
        addPolygons(label = labels,
                    stroke = FALSE,
                    smoothFactor = .5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal(monthly_shooting()$count),
                    highlightOptions = highlightOptions(weight =5,
                                                        fillOpacity = 1,
                                                        opacity = 1,
                                                        bringToFront = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~ count,
                  title = "Shooting Cases",
        )
    })
    
  
  
} 



# Run the application 
shinyApp(ui = ui, server = shinyServer)
