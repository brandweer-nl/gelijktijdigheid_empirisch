# server.R

# workspace
setwd("~/R/risicoprofiel/dashboard")

# dependencies
library(shinydashboard)
library(lubridate) # for gelijktijdigheid function
library(rgdal)
library(leaflet)
library(DT)
source("libs/func_gelijktijdigheidshiny.R")

# load all incident data
incidenten <- readRDS("data/incidenten_bron.rds")

# projection settings, needed for transformations
# Amersfoort RD Nez
src.proj <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
# WGS84
dst.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# main function
function(input, output) {
  # dynamically create input boxes for ui.R
  output$items_vanprio <- renderUI(
    checkboxGroupInput("vanprio", 
                       label = h5("prioriteit"), 
                       sort(unique(incidenten$dim_prioriteit_prio))
                       )
  )
  output$items_vangebied <- renderUI(
    selectInput(
      "vangebied",
      label = h5("verzorgingsgebied"),
      choices = sort(unique(incidenten$kazerne_groep)),
      multiple = TRUE
    )
  )
  output$items_vantype <- renderUI(
    selectInput(
      "vantype",
      label = h5("voertuigtype"),
      choices = sort(unique(unlist(incidenten$voertuig_groep_uniek)))
    )
  )
  output$items_naarprio <- renderUI(
    checkboxGroupInput("naarprio",
                       label = h5("prioriteit"),
                       sort(unique(incidenten$dim_prioriteit_prio))
                       )
  )
  output$items_naargebied <- renderUI(
    selectInput(
      "naargebied",
      label = h5("verzorgingsgebied"),
      choices = sort(unique(incidenten$kazerne_groep)),
      multiple = TRUE
    )
  )
  output$items_naartype <- renderUI(
    selectInput(
      "naartype",
      label = h5("voertuigtype"),
      choices = sort(unique(unlist(incidenten$voertuig_groep_uniek)))
    )
  )
  
  # datatabledata
  datatabledata <- eventReactive(input$bereken, {
    GelijktijdigheidShiny(incidenten, input$vanprio, input$vangebied, input$vantype, input$naarprio, input$naargebied, input$naartype)
  })
  
  # data table data
  #output$gelijkdata <- DT::renderDataTable(datatabledata(), options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Dutch.json'), lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All')), pageLength = 5, scrollX = TRUE, selection = 'single', rownames = FALSE))
  output$gelijkdata <- DT::renderDataTable(datatabledata(), options = list(scrollX = TRUE), selection = 'single', rownames = FALSE)
  
  # sub
  output$gelijkdatasub <- DT::renderDataTable(incidenten[ incidenten$hub_incident_id %in% unlist(datatabledata()$gelijk_overlap[ input$gelijkdata_rows_selected ]), ], options = list(scrollX = TRUE), selection = 'none', rownames = FALSE)

  # kaart
  output$kaart <- renderLeaflet({
    sourcemarker <- datatabledata()[ input$gelijkdata_rows_selected, ][ , c(43,44,68) ]
    sourcemarker[ , 3 ] <- "origineel incident"
    markers <- incidenten[ incidenten$hub_incident_id %in% unlist(datatabledata()$gelijk_overlap[ input$gelijkdata_rows_selected ]), ][ , c(43,44,68) ]
    markers <- rbind(sourcemarker, markers)
    names(markers) <- c("lng", "lat", "label")
    coordinates(markers) <- c("lng", "lat")
    proj4string(markers) <- src.proj
    markers <- spTransform(markers, CRSobj = dst.proj)
    leaflet(data = markers) %>% 
      addProviderTiles("Hydda.Full", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(label = ~as.character(label))
  })
}
