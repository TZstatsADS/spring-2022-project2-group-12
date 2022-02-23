#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#===============================================Shiny SERVER=====================================================
# LOAD AND PREPARE DATA ####################################

shooting_map_sp <- readRDS("./data/shooting_map_sp.RDS")
shooting_map_sp <- shooting_map_sp %>% arrange(OCCUR_YM)

shinyServer <-function(input, output, session) {
  
  # source("global.R")
  
  
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
  
  
    pal <- colorFactor(palette =c('#015B89', '#AB0400', '#CB6A0E', '#D9B30B', '#067D61'), domain = nyc_covid_data$BOROUGH_GROUP)
  
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
          )
      }) # end of observe
  
      leafletProxy("nyc_map_covid")
  }) # end of tab
  
    ####################### Tab 3 Arrest ##################
    
    target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
    year_chosen <- reactive({
      if ( "2019" %in% input$year){
        df1_2019 = df[df$year=='2019',]
        data <- df1_2019
        return( data ) 
      }
      if ( "2020" %in% input$year){
        df1_2020 = df[df$year=='2020',]
        data <- df1_2020
        return( data ) 
      }
      if ( "2021" %in% input$year){
        df1_2021 = df[df$year=='2021',]
        data <- df1_2021
        return( data ) 
      }
      if ( "Before Covid" %in% input$year){
        df1_B = df%>%filter(ARREST_DATE <= "2020-03-01")
        data <- df1_B
        return( data ) 
      }
      if ( "After Covid" %in% input$year){
        df1_A = df%>%filter(ARREST_DATE > "2020-03-01" )%>%filter(ARREST_DATE <= "2021-05-01" )
        data <- df1_A
        return( data ) 
      }
      
      
    })
    
    
    output$Plotall1 <- renderPlotly(
      if ( "Year" %in% input$by){
        data <- df %>% group_by(ARREST_BORO, year) %>%
          summarise(count = n())
        ggplot(data, aes(x = year, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "C") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of Arrests", x = "Time(Year)", title = "NYC Arrests Occurrence Trend of Each Location") +
          guides(color=guide_legend(title="Location")) +
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
        
        ggplotly(width = 750)
        
      } else if ("Month" %in% input$by){
        data <- df %>% group_by(ARREST_BORO, YY_MM) %>%
          summarise(count = n())
        ggplot(data, aes(x = YY_MM, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "C") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of Arrests", x = "Time(Month)", title = "NYC Arrests Occurrence Trend of Each Loacation" ) +
          guides(color=guide_legend(title="Location")) +
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
        
        ggplotly(width = 750)
      }
    )
    
    output$Plotall2 <- renderPlotly(
      if ( "Year" %in% input$by){
        data <- df %>% filter(LAW_CAT_CD %in% target) %>%
          group_by(LAW_CAT_CD, year) %>%
          summarise(count = n())
        ggplot(data, aes(x = year, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "C") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of Arrests", x = "Time(Year)", title = "NYC Arrests Occurrence Trend of Each Level of Offense ") +
          guides(color=guide_legend(title="Level of Offense")) +
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
        
        ggplotly(width = 750)
      
      } else if ( "Month" %in% input$by){
        data <- df %>% filter(LAW_CAT_CD %in% target) %>%
          group_by(LAW_CAT_CD, YY_MM) %>%
          summarise(count = n())
        ggplot(data, aes(x = YY_MM, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "C") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of Arrests", x = "Time(Month)", title = "NYC Arrests Occurrence Trend of Each Level of Offense ") +
          guides(color=guide_legend(title="Level of Offense")) +
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
        
        ggplotly(width = 750)
      }
    )
    
    
    
    output$ARPlot1 <- renderPlotly({
      target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
      data <- year_chosen() %>% filter(LAW_CAT_CD %in% target) %>%
        group_by(ARREST_BORO, LAW_CAT_CD) %>%
        summarise(count = n())
      
      ggplot(data, aes(fill=ARREST_BORO, y=count, x=LAW_CAT_CD)) + 
        geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette = "Pastel2")+
        labs(
          title = paste("1. NYC Arrests in", input$year),
          x = "Level of offense",
          y = "Percent arrests w.r.t location"
        ) +
        theme_bw(base_size = 12)
      
      ggplotly(width = 750)
    })
    
    
    
    
    output$ARPlot2 <- renderPlotly({
      target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
      data <- year_chosen() %>% 
        filter(LAW_CAT_CD %in% target) %>%
        group_by(LAW_CAT_CD,ARREST_BORO) %>%
        summarise(count = n())
      
      ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = ARREST_BORO)) +
        geom_bar(position="stack", stat="identity") +scale_fill_brewer(palette = "Pastel2")+
        #scale_color_viridis(discrete = TRUE, option = "G") +
        labs(y = "Number of crimes Occurred",
             x = "Location",
             title = paste("2. Level of Offense in Each Borough in", input$year)) +
        guides(fill=guide_legend(title="Level of Offense")) +
        theme_bw(base_size = 12)
      
      
      ggplotly(width = 750)
    })
    
    
    output$ARPlot3 <- renderPlotly({
      target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
      data <- year_chosen() %>%
        filter(LAW_CAT_CD %in% target) %>%
        filter(PERP_RACE != 'AMERICAN INDIAN/ALASKAN NATIVE') %>%
        group_by(LAW_CAT_CD,PERP_RACE) %>%
        summarise(count = n())
      
      ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = PERP_RACE)) +
        geom_bar(position="stack", stat="identity") +scale_fill_brewer(palette = "Pastel2")+
        #scale_color_viridis(discrete = TRUE, option = "G") +
        labs(y = "Number of crimes Occurred",
             x = "Perpetrator’s Race",
             title = paste("3. Level of Offense in Each Race in", input$year)) +
        guides(fill=guide_legend(title="Level of Offense")) +
        theme_bw(base_size = 12) +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90))
      
      ggplotly(width = 750, height = 550)
    })
    
    # end of tab
    

    ####################### Tab 4 Shooting  ##################
  
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
 # end of tab
  
  
    ####################### Tab 5 Hate Crime ##################
  output$Plot1 <- renderPlotly({
    if(input$option1 == 1) {
      ggplot(data=anti_data_y)+
        geom_line(mapping = aes(x=Complaint.Year.Number,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(x="Year", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=14),axis.text.y = element_text(face="bold", size=11))
      
      ggplotly()}
    else if(input$option1 == 2){
      ggplot(data=anti_data_m)+
        geom_line(mapping = aes(x=YY_MM,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(xlabel="YY/MM", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
      
      ggplotly()
      }
    })
  
  output$Plot2 <- renderPlot({
    if(input$option2 == 1){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI ASIAN",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
          ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN", 
                                      "Covid New Cases"), 
                           values = c("ANTI-ASIAN"="black", 
                                      "Covid New Cases"="light blue"))
      
      }
    else if(input$option2 == 2){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI BLACK",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-BLACK Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
          ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN", 
                                      "Covid New Cases"), 
                           values = c("ANTI-BLACK"="black", 
                                      "Covid New Cases"="light blue"))
      
      }
    else if(input$option2 == 3){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI JEWISH",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-JEWISH Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
          ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-JEWISH", 
                                      "Covid New Cases"), 
                           values = c("ANTI-JEWISH"="black", 
                                      "Covid New Cases"="light blue"))
      
      }
    else if(input$option2 == 4){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI MALE HOMOSEXUAL (GAY)",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ANTI MALE HOMOSEXUAL (GAY) Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
          ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ANTI MALE HOMOSEXUAL (GAY)", 
                                      "Covid New Cases"), 
                           values = c("ANTI-ANTI MALE HOMOSEXUAL (GAY)"="black", 
                                      "Covid New Cases"="light blue"))
      
      }
    else if(input$option2 == 5){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="OTHER ANTI TYPE",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of OTHER ANTI TYPE Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
          ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("OTHER ANTI TYPE", 
                                      "Covid New Cases"), 
                           values = c("OTHER ANTI TYPE"="black", 
                                      "Covid New Cases"="light blue"))
      
      }
    })
  
  output$Plot3 <- renderPlot({
    if(input$option3 == 1){
      
      data_m_with_loc <- data_m_loc[data_m_loc$County=="MANHATTAN",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000, group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Manhattan",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Manhattan")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        annotate("text", x=31, y=24, label= "Anti-Asian Crimes",size = 5, face = "bold")+
        annotate("text", x=34, y=90, label= "Covid Cases",size = 5,color = "light blue",face="bold")
      
      }
  
    else if(input$option3 == 2){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="BRONX",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Bronx",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Bronx")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        annotate("text", x=31, y=10, label= "Anti-Asian Crimes",size = 5, face = "bold")+
        annotate("text", x=34, y=90, label= "Covid Cases",size = 5,color = "light blue",face="bold")
      
      }
    
    else if(input$option3 == 3){
      
      data_m_with_loc <- data_m_loc[data_m_loc$County=="KINGS",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Kings",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Kings")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        annotate("text", x=31, y=10, label= "Anti-Asian Crimes",size = 5, face = "bold")+
        annotate("text", x=34, y=90, label= "Covid Cases",size = 5,color = "light blue",face="bold")
      
      }
    
    else if(input$option3 == 4){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="QUEENS",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Queens",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Queens")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        annotate("text", x=31, y=10, label= "Anti-Asian Crimes",size = 5, face = "bold")+
        annotate("text", x=34, y=90, label= "Covid Cases",size = 5,color = "light blue",face="bold")
      
      }

    else if(input$option3 == 5){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="STATEN ISLAND",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Staten Island",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Staten Island")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        annotate("text", x=31, y=10, label= "Anti-Asian Crimes",size = 5, face = "bold")+
        annotate("text", x=34, y=90, label= "Covid Cases",size = 5,color = "light blue",face="bold")
      
      }
  }) 
  
  # end of tab
  

}
