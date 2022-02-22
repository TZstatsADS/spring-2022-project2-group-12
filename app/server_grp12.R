#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("global_grp12.R")

library(shiny)


shinyServer <-function(input, output, session) {
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
}) # end of tab
   
    ####################### Tab 4 Hate Crime ##################
  output$Plot1 <- renderPlot({
    if(input$option1 == 1) {
      ggplot(data=anti_data_y)+
        geom_line(mapping = aes(x=Complaint.Year.Number,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(x="Year", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=14),axis.text.y = element_text(face="bold", size=11))}
    else if(input$option1 == 2){
      ggplot(data=anti_data_m)+
        geom_line(mapping = aes(x=YY_MM,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(xlabel="YY/MM", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
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
                                      "Covid New Cases"="light blue"))}
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
                                      "Covid New Cases"="light blue"))}
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
                                      "Covid New Cases"="light blue"))}
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
                                      "Covid New Cases"="light blue"))}
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
                                      "Covid New Cases"="light blue"))}
    })
  
  output$Plot3 <- renderPlot({
    if(input$option3 == 1){
      
      data_m_with_loc <- data_m_loc[data_m_loc$County=="MANHATTAN",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Manhattan",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Manhattan")
          ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Manhattan", 
                                      "Covid New Cases in Manhattan"), 
                           values = c("ANTI-ASIAN Crimes in Manhattan"="black", 
                                      "Covid New Cases in Manhattan"="light blue"))}
  
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
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Bronx", 
                                      "Covid New Cases in Bronx"), 
                           values = c("ANTI-ASIAN Crimes in Bronx"="black", 
                                      "Covid New Cases in Bronx"="light blue"))}
    
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
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Kings", 
                                      "Covid New Cases in Kings"), 
                           values = c("ANTI-ASIAN Crimes in Kings"="black", 
                                      "Covid New Cases in Kings"="light blue"))}
    
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
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Queens", 
                                      "Covid New Cases in Queens"), 
                           values = c("ANTI-ASIAN Crimes in Queens"="black", 
                                      "Covid New Cases in Queens"="light blue"))}

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
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Staten Island", 
                                      "Covid New Cases in Staten Island"), 
                           values = c("ANTI-ASIAN Crimes in Staten Island"="black", 
                                      "Covid New Cases in Staten Island"="light blue"))}
  }) 

}

shiny::shinyApp(ui, shinyServer)
