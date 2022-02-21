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

getwd()
setwd('Downloads')
df <- read.csv("Arrest_2019_2021.csv")
#df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
#df$year = format(df$CMPLNT_FR_DT,'%Y')
ui <- navbarPage(theme = shinytheme("darkly"), id="nav", windowTitle = "Hate crime",
  tabPanel("Arrests",
    sidebarLayout(
      sidebarPanel(
        
        
        
        
        
        
        
        
        selectInput(inputId = "year",
                    label = "Choose an year", 
                    choices = c("2019","2020","2021")),
    
        
        
      ),
      
      mainPanel(
        plotlyOutput("Plot1"),
        plotlyOutput('Plot2')
        position = "right"
      )
    )
  )
)


shinyServer <- function(input, output) {
  
  df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
  df$year = format(df$ARREST_DATE,'%Y')
  df1_2019 = df[df$year=='2019',]
  df1_2019$ARREST_DATE = as.Date(df1_2019$ARREST_DATE,format = "%Y-%m-%d")
  df1_2019$month = format(df1_2019$ARREST_DATE,'%m')
  df1_2019_overall <- df1_2019%>% group_by(ARREST_BORO,month)%>%
    summarise(count = n())
  
  df1_2019_covid <- df1_2019%>% group_by(ARREST_BORO,month)%>%
    summarise(count = n())
  
  output$Plot1 <- renderPlotly({
    if(input$year=='2019'){
        df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
        df$year = format(df$ARREST_DATE,'%Y')
        df1_2019 = df[df$year=='2019',]
        df1_2019$ARREST_DATE = as.Date(df1_2019$ARREST_DATE,format = "%Y-%m-%d")
        df1_2019$month = format(df1_2019$ARREST_DATE,'%m')
        df1_2019_overall <- df1_2019%>% group_by(ARREST_BORO,month)%>%
          summarise(count = n())
        ggplot(df1_2019_overall, aes(x = month, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "month", title = "Overall NYC Complaints in 2019") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      
    }
    else if(input$year=='2020'){
     
        df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
        df$year = format(df$ARREST_DATE,'%Y')
        df1_2020 = df[df$year=='2020',]
        df1_2020$ARREST_DATE = as.Date(df1_2020$ARREST_DATE,format = "%Y-%m-%d")
        df1_2020$month = format(df1_2020$ARREST_DATE,'%m')
        df1_2020_overall <- df1_2020%>% group_by(ARREST_BORO,month)%>%
          summarise(count = n())
        ggplot(df1_2020_overall, aes(x = month, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "month", title = " Overall NYC Complaints in 2020d") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      
    }
    
    else if(input$year=='2021'){
      
        df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
        df$year = format(df$ARREST_DATE,'%Y')
        df1_2021 = df[df$year=='2021',]
        df1_2021$ARREST_DATE = as.Date(df1_2021$ARREST_DATE,format = "%Y-%m-%d")
        df1_2021$month = format(df1_2021$ARREST_DATE,'%m')
        df1_2021_overall <- df1_2021%>% group_by(ARREST_BORO,month)%>%
          summarise(count = n())
        
        
        
        ggplot(df1_2021_overall, aes(x = month, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "month", title = "Overall NYC Complaints in 2021") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      
    }
    
  })
  
  
  
    
  
} 
shinyApp(ui = ui, server = shinyServer)