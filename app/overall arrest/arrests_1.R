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
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("shinydashboard")) {
  install.packages('shinydashboard')
  library(shinydashboard)
}
getwd()
#setwd('/Users/jiuruwang/Documents/GitHub/Stat 5243 Projects/spring-2022-project2-group-12/app/overall arrest')
df <- read.csv("../../data/Arrest_2019_2021.csv")
df <- df[!(is.na(df$LAW_CAT_CD) | df$LAW_CAT_CD==""), ]
unique(df$LAW_CAT_CD)
df$ARREST_BORO[df$ARREST_BORO == "B"] <- 'Bronx'
df$ARREST_BORO[df$ARREST_BORO == "K"] <- 'Brooklyn'
df$ARREST_BORO[df$ARREST_BORO == "Q"] <- 'Queens'
df$ARREST_BORO[df$ARREST_BORO == "S"] <- 'Staten Island'
df$ARREST_BORO[df$ARREST_BORO == "M"] <- 'Manhattan'

df$LAW_CAT_CD[df$LAW_CAT_CD == "F"] <- 'Felony'
df$LAW_CAT_CD[df$LAW_CAT_CD == "M"] <- 'Misdemeanor'
df$LAW_CAT_CD[df$LAW_CAT_CD == "V"] <- 'Violation'
df$LAW_CAT_CD[df$LAW_CAT_CD == "I"] <- 'Infractions'

#df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
#df$year = format(df$CMPLNT_FR_DT,'%Y')
ui <- navbarPage(windowTitle = "Hate crime",
                 tabPanel("Arrests",
                          fluidRow(
                            style = "padding: 5%; background-color: white",
                            tags$h1("Arrests", style="color:black;font-weight:bold"),
                            tags$p("While the COVID-19 pandemic caused a nationwide increase in crimes, it also brought a significant increase in arrests, with New York City reportedly experiencing a maximum number of in arrests in the month of May,2020 where the pandemic was at its peak.", style="color:black"),
                            tags$p("This analysis helps individuals to learn more about the rise of arrests in NYC. It shows the rate of arrests with respect to each of the five boroughs in NYC", style="color:black"),
                            #tags$p("The following pages provide a map view of NYC crime complaints, their correspondence to COVID hospitalizations, and reported hate crimes during the pandemic.", style="color:black")
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              selectInput(inputId = "period",
                                         label = "Choose an year", 
                                         choices = c("after","before"),
                                         
                              )
                              
                              
                              
                              
                          
                            ),
                            mainPanel(
                              plotlyOutput("Plot1"),
                              plotlyOutput('Plot2'),
                              
                            )
                            
                          )
                 )
)


shinyServer <- function(input, output) {
  output$Plot1 <- renderPlotly({
    if(input$period=='before'){
      df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
      df_before <- df%>%filter(ARREST_DATE <= "2020-03-01")%>% group_by(ARREST_BORO,LAW_CAT_CD)%>%
        summarise(count = n())
      
      ggplot(df_before, aes(fill=ARREST_BORO, y=count, x=LAW_CAT_CD)) + 
        geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette = "Pastel2")+
        
        ggtitle("NYC Arrests in 2019") +
        xlab("Level of offense") +
        ylab("Percent arrests w.r.t location") +
        theme_bw(base_size = 12)
      ggplotly(width = 1000)
      
    }
    else if(input$period=='after'){
      
      df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
      df_after <- df%>%filter(ARREST_DATE > "2020-03-01")%>% group_by(ARREST_BORO,LAW_CAT_CD)%>%
        summarise(count = n())
      
      
      ggplot(df_after, aes(fill=ARREST_BORO, y=count, x=LAW_CAT_CD)) + 
        geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette = "Pastel2")+
        
        ggtitle("NYC Arrests in 2020") +
        xlab("Level of offense") +
        ylab("Percent arrests w.r.t location") +
        theme_bw(base_size = 12)
      ggplotly(width = 1000)
      
      
      
    }
    
    
    
  })
  
  
  
  output$Plot2 <- renderPlotly({
    df1 <- df %>% 
      mutate(ARREST_DATE = as.Date(ARREST_DATE, "%Y-%m-%d"),
             OCCUR_YM =  floor_date(ARREST_DATE, unit = "month"),
             ARREST_BORO = as.factor(ARREST_BORO)) %>%
      group_by(OCCUR_YM, ARREST_BORO) %>%
      summarise(count = n())

        ggplot(df1, aes(x = OCCUR_YM, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "C") +
          scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of Arrests", x = "Period", title = "NYC Arrests Occurrence Since 2019") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      
      
  
   
    
  })
  
  
  
  
} 
shinyApp(ui = ui, server = shinyServer)