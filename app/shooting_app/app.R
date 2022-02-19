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
                            choices = c("Overall Period","Covid Period"))
            ),
            
            mainPanel(
                plotlyOutput(outputId = "shooting")            
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
    
  
  
} 



# Run the application 
shinyApp(ui = ui, server = shinyServer)
