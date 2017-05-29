library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(plotly)
library(ggplot2)
library(viridis)

histoTemp <- readRDS("histoTemps.rds")
attractions <- fread("attractions.csv", encoding = "UTF-8")

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  values <- reactiveValues(
    selectedID = c()
  )
  
  observeEvent(input$map_marker_click, { # update the map markers and view on map clicks
    p <- input$map_marker_click
    proxy <- leafletProxy("map")
    idP <- attractions$id[attractions$longitude == p$lng & attractions$latitude == p$lat]
    if(idP %in% values$selectedID){
      values$selectedID <- values$selectedID[- which(values$selectedID == idP)]
      proxy %>% removeMarker(layerId=as.character(idP))
    } else{
      values$selectedID <- c(values$selectedID, idP)
      proxy %>% addCircleMarkers(p$lng, p$lat, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId= as.character(idP))
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    df2 <-  histoTemp %>% tbl_df() %>%
      group_by(id) %>% 
      summarise(meanValue = mean(as.numeric(attente))) %>%
      mutate(meanValue2 = 7 + 15/(max(meanValue) - min(meanValue))*(meanValue - min(meanValue)) )
      
      attractions <- attractions %>% tbl_df()
      
      attractions <- left_join(attractions, df2, by = "id") %>% data.table()
    
    palet <- viridis_pal()(100)
    
    pal <- colorNumeric(
      palette = palet,
      domain = attractions$meanValue2)
    
    pal2 <- colorNumeric(
      palette = palet,
      domain = attractions$meanValue)
    
    leaflet(attractions) %>% addTiles() %>%
      fitBounds(~min(longitude)+0.005, ~min(latitude), ~max(longitude)+0.005, ~max(latitude)) %>%
      addCircleMarkers(~longitude, ~latitude, popup = ~as.character(name), radius = 6, color = ~pal(meanValue2), fillOpacity = 0.9) %>%
      addLegend(position = "bottomleft", pal = pal2, values = ~meanValue, title = "Temps d'attente moyen (m)")
  })
  
  output$graphe <- renderPlotly({
    validate(
    need(length(values$selectedID) > 0, "Sélectionnez une ou plusieurs attractions en cliquant dessus. \n L'application présente les temps d'attentes moyens sur l'ensemble des attractions choisies, ainsi que le minimum et le maximum constatés. \n Il est possible de choisir les jours de la semaine à retenir pour l'agrégation, ainsi que l'intervalle de dates d'observation à conserver."),
    
    need(length(input$jour) > 0, "Cochez au moins une case de jour de la semaine")
    )
    df <-  histoTemp %>% tbl_df() %>%
      filter(id %in% values$selectedID, as.Date(temps) <= input$dateRange[2], as.Date(temps) >= input$dateRange[1]) %>%
      mutate(day = wday(as.POSIXct(strptime(temps, "%Y-%m-%d %H:%M:%S")))) %>%
      filter(day %in% input$jour) %>%
      group_by(temps) %>% 
      summarise(inter2 = mean(as.numeric(attente))) %>%
      mutate(hour_of_day = hour(as.POSIXct(strptime(temps, "%Y-%m-%d %H:%M:%S")))) %>%
      mutate(day = wday(as.POSIXct(strptime(temps, "%Y-%m-%d %H:%M:%S")))) %>%
      group_by(day, hour_of_day) %>%
      summarise(inter = mean(as.numeric(inter2)), min2 = min(as.numeric(inter2)), max2 = max(as.numeric(inter2))) %>%
      group_by(hour_of_day) %>%
      summarise(meanValue = mean(inter), min = min(as.numeric(min2)), max = max(as.numeric(max2))) %>%
      data.table()
    
    t <- ggplot(df, aes(x = hour_of_day, y = meanValue, fill = meanValue)) + 
      geom_bar(stat = 'identity') + 
      geom_errorbar(aes(ymin = min, ymax = max)) + 
      scale_x_continuous() +
      scale_fill_viridis() +
      scale_y_continuous() +
      xlab("") +
      ylab("") +
      theme_minimal() + 
      theme(legend.position="none", strip.background = element_blank(), strip.placement = "outside", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    ggplotly(t)
    
  })
  
}