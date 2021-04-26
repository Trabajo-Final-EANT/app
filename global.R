library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)
library(cowplot)
library(plotly)
library(highcharter)
library(hrbrthemes)
library(gganimate)
library(leaflet)
library(ggmap)
library(patchwork)
library(spatialEco)
library(rgeos)
library(rgdal)
library(sf)
library(viridis)
library(viridisLite)
library(gifski)
library(htmlwidgets)
library(reactlog)
library(dplyr)
#library(rsconnect)

rm(list = ls())

options(scipen=10)


#Archivos
Pobreza <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Pobreza.csv")
NBI <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/NBI.csv")
Pobreza_anual <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Pobreza_anual.csv")
Poblacion_Edad <- read_csv("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Poblacion%20por%20Edad.csv")
Piramide <- read_csv("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/PiramidePoblacion.csv")
Piramide$grupo_edad <- cut(x = Piramide$grupo_edad, breaks = seq(0, 100, 5))
Esc_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/EscCom.csv")
Muestra_escuelas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/MuestraEsc.geojson")
EdadEsc_x_Esc<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/EdEscxEscuelas.geojson")
EscPal<-colorNumeric("OrRd", domain= EdadEsc_x_Esc$Prop, n=4)
Hosp_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospCom.csv")
Hospitales_reducido <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospitalesR.geojson")
Comunas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Comunas.geojson")
hacinamiento <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/hacinamiento.csv")
Viviendas <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Viviendas.csv")
Viv_Com_Geo <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/VIVCOM.geojson")
Regimen<-fread("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen.csv",encoding = "UTF-8")

Regimen18<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen18sBOM.geojson")
#Regimen18<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen18cBOM.geojson")

VivPal<-colorNumeric(palette = "PuRd", domain = Regimen18$porcentaje)
Precm2<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Precm2xC.geojson")
palm2<-colorNumeric(palette="Greens", domain=Precm2$US_x_m2)

#Mapa cultura
Mapa_Cul<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/MapaCul.geojson")
Cul_x_C<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Espacios_x_Comuna.geojson")
pal<- colorFactor(c("#c23c3c","#e08d07", "#c7fa39", "#02d606", "#00dfe3", "#752957"), 
                  domain = c("Bibliotecas", "Centro Cultural", "Comercios",
                             "Esp. Publicos","Esp. de Formacion", "Exhibicion"))
coroPal<-colorNumeric(palette = "PuRd", domain= Cul_x_C$relativo)

##Transporte
Red_Bondis<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/colectivos.geojson")
Red_SbtPrem<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Subte_Prem.geojson")
PaleSubt<-c("#18cccc","#eb0909","#233aa8","#02db2e","#c618cc","#f6ff00","#ffdd00")
SubtePal<-colorFactor(PaleSubt,Red_SbtPrem$linea)
Acce_Subte<-fread("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Accesubte.csv",encoding = "UTF-8")
Red_Tren<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Trenes.geojson")
Red_CicloV<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Ciclovias.geojson")
EcoBici<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/EcoBici.geojson")
Transp_x_C<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/TranspxC.geojson")
TranspBarras<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/T_X_c_Barras.geojson")
SillaDeRuedas <- makeIcon(
  iconUrl = "https://images.vexels.com/media/users/3/129039/isolated/preview/9b90dadb8432f24bd49b439e8438f071-icono-plano-de-silla-de-ruedas-by-vexels.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 5, iconAnchorY = 30)
palColec <- colorNumeric(palette = "YlOrRd", domain= Transp_x_C$Colecx100)
palSubte<- colorNumeric(palette = "YlOrRd", domain= Transp_x_C$Subtex100)
palTren <- colorNumeric(palette = "YlOrRd", domain= Transp_x_C$Trenx100)
palCicloV<-colorNumeric(palette = "YlOrRd", domain= Transp_x_C$CicloVx100)
palEcoB<-colorNumeric(palette = "YlOrRd", domain= Transp_x_C$EcoBx100)

#Mapa escuela
getColor_Escuela <- function(Muestra_escuelas) {
  sapply(Muestra_escuelas$num_niv, function(num_niv) {
    if(num_niv == 1) {
      "green"
    } else if(num_niv == 2) {
      "red"
    } else if(num_niv == 3) {
      "blue"
    } else {
      "orange"
    } })
}

icons_Escuela <- awesomeIcons(
  icon = 'ios-close',
  library = 'ion',
  markerColor = getColor_Escuela(Muestra_escuelas)
)

labels_esc <- sprintf("<strong>%s</strong><br/>%s  <sup></sup>",
                      Muestra_escuelas$COMUNA,
                      Muestra_escuelas$NIVOF)%>%
  lapply(htmltools::HTML)
bins <- c(0,1,2,3,4)
pal_EEE<- colorBin(c("#6d9d37", "red", "#35aee6", "#fb8e33"), 
                   domain = Muestra_escuelas$num_niv, bins = bins)

#Mapa hospitales
getColor <- function(Hospitales_reducido) {
  sapply(Hospitales_reducido$TINUM, function(TINUM) {
    if(TINUM == 1) {
      "green"
    } else if(TINUM == 2) {
      "red"
    } else {
      "orange"
    } })
}
icons <- awesomeIcons(
  icon = 'ios-close',
  library = 'ion',
  markerColor = getColor(Hospitales_reducido)
)

#Mapa viviendas
pal_Viv_Geo <- colorNumeric(palette = "YlOrRd", domain= Viv_Com_Geo$Cmrc_f_)
labels_Viv_Geo <- sprintf("<strong>%s</strong><br/>%s %% viviendas ocupadas por uso comercial <sup></sup>",
                          Comunas$Comuna, Viv_Com_Geo$Cmrc_f_) %>% lapply(htmltools::HTML)

