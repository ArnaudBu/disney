library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(plotly)
library(ggplot2)
library(viridis)

jours <- c("lundi" = 2,
           "mardi" = 3,
           "mercredi" = 4,
           "jeudi" = 5,
           "vendredi" = 6,
           "samedi" = 7,
           "dimanche" = 1)

histoTemp <- readRDS("histoTemps.rds")
attractions <- fread("attractions.csv", encoding = "UTF-8")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 10, left = "auto", right = 20,
                width = "300px", height = "auto",
                dateRangeInput('dateRange',
                               label = 'Intervalle de date:',
                               min = min(as.Date(histoTemp$temps)), 
                               max = max(as.Date(histoTemp$temps)),
                               start = min(as.Date(histoTemp$temps)),
                               end = max(as.Date(histoTemp$temps))
                ),
                checkboxGroupInput("jour", "Jours", jours, inline = TRUE, selected = jours)
                
  ),
  absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, bottom = 10, left = "auto", right = 20,
                width = "330px", height = "auto",
                h5("Temps d'attentes moyens, maximum, et minimum sur l'ensemble des attractions sélectionnées"),
                plotlyOutput("graphe")
  )
)