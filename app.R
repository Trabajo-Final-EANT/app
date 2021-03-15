
library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(data.table)
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


rm(list = ls())

#Archivos
Pobreza <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Pobreza.csv")
NBI <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/NBI.csv")
Pobreza_anual <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Pobreza_anual.csv")
Poblacion_Edad <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Poblacion_Edad.csv")
Piramide <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/PiramidePoblacion.csv")
Esc_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/EscCom.csv")
Muestra_escuelas <- st_read("MuestraEsc.shp")
#Muestra_escuelas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/MuestraEsc.geojson")
Hosp_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospCom.csv")
Hospitales_reducido <- st_read("HospitalesR.shp")
#Hospitales_reducido <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospitalesR.geojson")
Comunas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Comunas.geojson")
hacinamiento <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/hacinamiento.csv")
Viviendas <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Viviendas.csv")
Viv_Com2 <- st_read("Viv_com.shp")

Mapa_Cul<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/MapaCul.geojson?token=AST4NRR6XW2CCSUYC6MPX7LAJ6MXU")
Cul_x_C<-st_read("Cultura/Espacios_x_Comuna.geojson")
pal<- colorFactor(c("#c23c3c","#e08d07", "#c7fa39", "#02d606", "#00dfe3", 
                    "#752957"), domain = c("Bibliotecas", "Centro Cultural", 
                                           "Comercios","Esp. Públicos","Esp. de Formacion",
                                           "Exhibicion"))
coroPal<-colorNumeric(palette = "PuRd", domain= Cul_x_C$relativo)
Comunas <- st_read("Comunas/Comunas.shp")
##Transporte
Red_Bondis<-st_read("Transporte/Colectivos.geojson")
Red_Subte<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Subte.geojson?token=AST4NRQRQ7S2YLNN4HNO5ZLAJ6M4S")
Acce_Subte<-fread("Transporte/Accesubte.csv",encoding = "UTF-8")
Red_Tren<-st_read("Transporte/Trenes.geojson")
Red_CicloV<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Ciclovias.geojson?token=AST4NRRPKTYT4GSGG7QZATTAJ6M2E")
EcoBici<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/EcoBici.geojson?token=AST4NRWAPJO37B5ZT5DC7YTAJ6M3U")
Transp_x_C<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/TranspxC.geojson?token=AST4NRRKNTF6LWQW5CEZOA3AJ6M6I")
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
                      Muestra_escuelas$NIVELMODAL)%>%
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
pal_Viv2 <- colorNumeric(palette = "YlOrRd", domain= Viv_Com2$Cmrc_f_)
labels_Viv2 <- sprintf("<strong>%s</strong><br/>%s %% viviendas ocupadas por uso comercial <sup></sup>",
                       Viv_Com2$Comuna, Viv_Com2$Cmrc_f_) %>% lapply(htmltools::HTML)


#UI
ui <- fluidPage(
    
    theme = shinytheme("united"),
    
    
    titlePanel("Merecer la ciudad: el derecho al uso y disposición del espacio urbano en la Ciudad Autónoma de Buenos Aires"),
    
    tabsetPanel(
        tabPanel("Introducción",
                 tabPanel("Introducción",
                          textOutput(outputId = "Prueba"))),
        tabPanel("Indagando la población",
                 navlistPanel(
                     tabPanel("Pobreza",
                              highchartOutput(outputId = "G_Pob"),
                              br(),
                              highchartOutput(outputId = "G_Pob2")),
                     tabPanel("NBI",
                              plotOutput(outputId = "G_NBI")),
                     tabPanel("Datos demográficos",
                              highchartOutput(outputId = "G_demo"),
                              br(),
                              plotlyOutput(outputId = "G_Pir"),
                              selectInput(inputId = "input_fecha",
                                          choices = Piramide$Año,
                                          label = "Seleccione año del censo",
                                          selected = NULL))
                     )),
        tabPanel("Desarrollo humano",
                 navlistPanel(
                     tabPanel("Escuelas",
                              highchartOutput(outputId = "G_Esc"),
                              selectInput(inputId = "input_comuEscuela",
                                          choices = Esc_Com$Comuna,
                                          label = "Seleccione comuna"),
                              br(),
                              leafletOutput(outputId = "M_Escuelas")),
                     tabPanel("Hospitales",
                              highchartOutput(outputId = "G_Hosp"),
                              br(),
                              leafletOutput(outputId = "M_Hospitales")),
                     tabPanel("Cultura",
                              plotlyOutput((outputId= "BarrasCul")),
                              br(),
                              leafletOutput(outputId = "MapaCul"),
                              br(),
                              leafletOutput(outputId = "CoroCul"))
                 )),
        tabPanel("Vivienda",
                 navlistPanel(
                   tabPanel("Hacinamiento",
                            highchartOutput(outputId = "G_HNC"),
                            br(),
                            highchartOutput(outputId = "G_HC")),
                   tabPanel("Condición de ocupación",
                            highchartOutput(outputId = "G_Vivienda"),
                            br(),
                            leafletOutput(outputId = "M_Vivienda"))
                 )),
         tabPanel("Transporte",
                 navlistPanel(
                     tabPanel("Colectivo",
                              leafletOutput(outputId = "Recorrido_Bondis"),
                              br(),
                              leafletOutput(outputId= "Coro_Bondis"),
                              br(),
                              plotOutput(outputId = "Distr_Bondis")),

                     tabPanel("Subterraneo/Metro",
                              leafletOutput(outputId = "Recorrido_Subte"),
                              br(),
                              leafletOutput(outputId = "Coro_Subte"),
                              plotOutput(outputId = "Distr_Subte")),

                     tabPanel("Tren/Ferrocarril",
                              leafletOutput(outputId = "Recorrido_Trenes"),
                              br(),
                              leafletOutput(outputId= "Coro_Trenes"),
                              br(),
                              plotOutput(outputId = "Distr_Trenes")),

                     tabPanel("Bicicletas",
                              leafletOutput(outputId = "Recorrido_Bicicletas"),
                              br(),
                              leafletOutput(outputId= "Coro_CicloV"),
                              br(),
                              leafletOutput(outputId="Coro_EcoB"),
                              plotOutput(outputId = "Distr_Bicicletas")
                             ))
               )
    )
                 


#Server
server <- function(input, output) {
    
                pir_filt <- reactive({
                    pir_filt = Piramide[Piramide$Año == input$input_fecha,]
                    pir_filt
                })
                
                esc_filt <- reactive({
                    esc_filt = Esc_Com[Esc_Com$Comuna == input$input_comuEscuela,]
                    esc_filt
                })
                
                
        output$Prueba <- renderText({
                    "esto es la prueba"
                })

        output$G_Pob <- renderHighchart({
            Evolucion_POB2 <- hchart(Pobreza, "bar", hcaes(x = Año, y = Pobreza_total, group = TRIM))  %>% 
                hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Personas en situación de Pobreza e Indigencia, por año y trimestre.")%>%
                hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2015-2019)")%>%
                hc_yAxis(title = list(text = "Situación de pobreza (en %)"),
                         labels = list(format = "{value}%")) %>%
                hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_google())
                })
        
        output$G_Pob2 <- renderHighchart({
            losdos <- highchart() %>%
                hc_add_series(Pobreza_anual, "line", hcaes(x = Año, y = mean_pob), name = "Pobreza") %>%
                hc_add_series(Pobreza_anual, "line", hcaes(x = Año, y = mean_ind), name = "Indigencia") %>% 
                hc_add_theme(hc_theme_google()) %>%
                hc_title(text = "Evolución de Pobreza e Indigencia.")%>%
                hc_subtitle(text = "Promedio anual. Ciudad Autónoma de Buenos Aires (2015-2019)")%>%
                hc_yAxis(title = list(text = "Situación de pobreza e indigencia"),
                         labels = list(format = "{value}%")) %>%
                hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))
                })
        
        output$G_NBI <- renderPlot({
            BarrasNBI<- ggplot(NBI,mapping = aes(
                x=reorder(Comuna, NBI),
                y=NBI,fill=Comuna))+
                geom_col()+
                scale_fill_manual(values = c("grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","grey33","limegreen"))+
                geom_text(aes(label = NBI),vjust = 2, size = 3.5)+
                theme_bw()+
                theme(legend.position = "none")+
                labs(title="Porcentaje de hogares con NBI por Comuna. 2010",
                     x="Comunas",
                     caption = "Fuente: Censo Nacional de Población, hogares y viviendas (INDEC) 2010.")
                BarrasNBI
                })
        
        output$G_demo <- renderHighchart({
            G_Pob_Edad=  hchart(Poblacion_Edad, "line",
                                hcaes(x = Año, y= Poblacion,
                                      group = Edad)) %>%
                hc_title(text = "Cantidad de Poblacion por grupo etario(1960-2010)")%>%
                hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (1960-2010)")%>%
                hc_yAxis(title = list(text = "Poblacion"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente: Instituto Nacional de Estadisticas y Censos", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_ffx())
                })
        
        output$G_Pir <- renderPlotly({
            Pir <- ggplot(pir_filt(), mapping=aes(x= grupo_edad, y= Poblacion, fill=sexo))+
                geom_col(alpha=.7)+
                labs(title="Piramide poblacional. Ciudad Autonoma de Buenos Aires",
                     x="",
                     y="Cantidad de Poblacion",
                     caption="Fuente: Instituto Nacional de Estadisticas y Censos")+
                scale_fill_manual(values=c("#561759","#099CDB"))+
                theme(legend.position = "right",
                      strip.text = element_text(size = 14, face = "bold"))+
                coord_flip()+
                theme_classic()
                ggplotly(Pir)
                })
        
        output$G_Esc <- renderHighchart({
            Grafico_esc <- hchart(Esc_Com, "bar", hcaes(x = Comuna, y = Escuelas, group = Comuna))  %>%
                hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Cantidad de escuelas por comuna.")%>%
                hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2020)")%>%
                hc_yAxis(title = list(text = "Cantidad de escuelas"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente Data Buenos Aires- GCBA", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_flat())
                })
        
        output$M_Escuelas <- renderLeaflet({
            Geo_esc <- leaflet(data = Muestra_escuelas) %>% 
                setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addAwesomeMarkers(~long, ~lat, icon = icons_Escuela, label = labels_esc, labelOptions = labelOptions(textsize = "15px"))%>%
                addPolylines(data = Comunas, color="#2F4AFF", opacity = 1, weight = 2) %>%
                addLegend(pal=pal_EEE, 
                          values = ~num_niv,
                          opacity = 1, 
                          title = "Cantidad de niveles ofrecidos por escuela.",
                          labFormat = labelFormat(suffix=""),
                          position = "bottomleft")
            Geo_esc
                })
        
        output$G_Hosp <- renderHighchart({
            Grafico_hosp <- hchart(Hosp_Com, "bar", hcaes(x = Comuna, y = Hospitales, group = Hospitales))  %>% hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Cantidad de hospitales por comuna.")%>%
                hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2020)")%>%
                hc_yAxis(title = list(text = "Cantidad de hospitales"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente Data Buenos Aires- GCBA", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_flat())
                })
        
        output$M_Hospitales <- renderLeaflet({
            leaflet(data = Hospitales_reducido) %>% 
                setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addAwesomeMarkers(~long, ~lat, icon = icons, label = ~as.character(TIPO)) %>%
                addPolylines(data = Comunas, color="#2F4AFF", opacity = 1, weight = 2)
                })
        
        output$G_HNC <- renderHighchart({
          Evolucion_HNC <- hchart(hacinamiento, "line", 
                                  hcaes(x = Año, y= Hacinamiento_no_critico, 
                                        group = Comunas)) %>%
            hc_title(text = "Hacinamiento no crítico por comuna")%>%
            hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2010-2018)")%>%
            hc_yAxis(title = list(text = "Hacinamiento no crítico (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
            })
        
        output$G_HC <- renderHighchart({
          Evolucion_HC <- hchart(hacinamiento, "line", 
                                 hcaes(x = Año, y= Hacinamiento_critico, 
                                       group = Comunas)) %>%
            hc_title(text = "Hacinamiento crítico por comuna")%>%
            hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2010-2018)")%>%
            hc_yAxis(title = list(text = "Hacinamiento crítico (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
            })
        
        output$G_Vivienda <- renderHighchart({
          Grafico_viv2 <- highchart() %>%
            hc_add_series(Viviendas, "column", 
                          hcaes(x = Comuna, y = Todos_presentes, color = Todos_presentes), 
                          name = "Viviendas ocupadas") %>%
            hc_add_series(Viviendas, "column", 
                          hcaes(x = Comuna, y = Todos_ausentes, color = Todos_ausentes), 
                          name = "Viviendas desocupadas") %>%
            hc_plotOptions(column = list(dataLabels = list(enabled = T))) %>%
            hc_xAxis(title = list(text = "Comunas")) %>%
            hc_yAxis(labels = list(format = "{value}%")) %>%
            hc_title(text = "Distribución porcentual de viviendas según condición de ocupación, por comuna.") %>%
            hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2010)") %>%
            hc_yAxis(title = list(text = "Distribución porcentual de viviendas (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, 
                       text = "Fuente: Censo 2010 (INDEC)", 
                       style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_economist())
            })
        
        output$M_Vivienda <- renderLeaflet({
          Geo_viv2 <- leaflet(Viv_Com2$geometry) %>% 
            setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolylines(data = Comunas, color="#FC750F", opacity = 1, weight = 2)%>%
            addPolygons(data = Comunas$geometry, weight = 2, opacity = 1,
                        color = "#F5F1BA", dashArray = "3",
                        fillOpacity = 0.5,
                        highlight = highlightOptions(weight = 5, color = "#F5C9BA",
                                                     dashArray = "3",
                                                     fillOpacity = 0.7,
                                                     bringToFront = F),
                        label = labels_Viv2) %>%  
            addCircleMarkers(data = Viv_Com2,
                             color = ~pal_Viv2(Cmrc_f_),
                             stroke = FALSE,
                             fillOpacity = 1) %>%
            addLegend(data = Viv_Com2,
                      "bottomright", 
                      pal = pal_Viv2, 
                      values = ~Cmrc_f_,
                      title = "Comercios, oficinas y consultorios",
                      opacity = 1) %>%
            addLabelOnlyMarkers(data = Comunas,
                                ~lat,~long,
                                label =  ~as.character(Comuna), 
                                labelOptions = labelOptions(noHide = T, size=1,
                                                            direction='top',textOnly = F))
            })
    output$BarrasCul<-renderPlotly({
      BarrasCul<-ggplot(Cul_x_C)+
        geom_col(aes(x=reorder(Comuna,Cantidad), y=Cantidad, fill=Tipo), width = 0.7)+
        scale_fill_manual(values=c("#c23c3c","#e08d07", "#c7fa39", "#02d606", "#00dfe3", "#752957"))+
        guides(fill=FALSE)+
        labs(title="Distribucion de Espacios Culturales por comuna segun tipo de espacio",
             subtitle = "CABA.2021",
             x="Comuna",
             y= "Cantidad",
             caption=
               "Fuente= https://data.buenosaires.gob.ar/dataset/espacios-culturales")+
        theme_bw()
      ggplotly(BarrasCul)
    })

    output$MapaCul<-renderLeaflet({
      MapaCultura<-leaflet() %>%
        setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
        addProviderTiles(providers$CartoDB.Positron) %>%  
        addCircleMarkers(data = Mapa_Cul,
                         color = ~pal(Tipo),
                         stroke = FALSE,
                         fillOpacity = 0.5)%>%
        addLegend(data = Mapa_Cul,
                  "bottomright", 
                  pal = pal, 
                  values = ~Tipo,
                  title = "Tipo de Espacio",
                  opacity = 1)%>%
        addPolygons(data=Comunas,
                    color = "#444444",
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.3,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))%>%
        addLabelOnlyMarkers(data = Comunas,
                            ~lat,~long,
                            label =  ~as.character(Comuna), 
                            labelOptions = labelOptions(noHide = T, size=1,
                                                        direction='top',textOnly = F))
      MapaCultura
    })

    output$CoroCul<-renderLeaflet({
      CoropCul<-leaflet(Cul_x_C) %>% 
        setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addPolygons(
          fillColor = ~coroPal(relativo),
          weight = 1,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE))%>%
        addLegend(pal=coroPal, 
                  values = ~relativo,
                  opacity = 0.7, 
                  title = "Porcentaje del total de Espacios",
                  position = "bottomleft")%>%
        addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                              labelOptions = labelOptions(noHide = T, size=1,
                                                          direction='top',textOnly = F))
      CoropCul
    })


    output$Recorrido_Bondis<-renderLeaflet({
        MapaBondis<-leaflet() %>%
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolylines(data = Red_Bondis, color="#09ed46", opacity = .3, weight = .5)%>%
            addLegend(position = "topright", colors = c("#09ed46"), labels = c("Red de Colectivos en CABA"))%>%
            addPolygons(data=Comunas,
                        color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.25,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
        MapaBondis
    })
    output$Coro_Bondis<-renderLeaflet({
        CoroBondis<-leaflet(Transp_x_C) %>% 
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addPolygons(
                fillColor = ~palColec(Colecx100),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))%>%
            addLegend(pal=palColec, 
                      values = ~Colecx100,
                      opacity = 0.7, 
                      title = "Porcentaje del total de Paradas",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                  labelOptions = labelOptions(noHide = T, size=1,
                                                              direction='top',textOnly = F))
        CoroBondis
    })
    output$Distr_Bondis<-renderPlot({
        BarrasBondis<-ggplot(Transp_x_C,mapping = aes(
            reorder(Comuna, Colectivo),
            Colectivo))+
            geom_col(fill="#09ed46",
                     color="black")+
            geom_text(aes(label = Colectivo), 
                      vjust = 2, size = 3.5)+
            labs(title ="Paradas de Colectivo por Comuna",
                 x="Comuna",
                 y="Cantidad de Paradas",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_classic()
        BarrasBondis 
    })


    #SUBTE
    output$Recorrido_Subte<-renderLeaflet({
        MapaSubte<-leaflet() %>%
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolylines(data = Red_Subte , color ="#eb34d5",  opacity = 2, weight = 3)%>%
            addMarkers(data= Acce_Subte, ~long, ~lat, icon = SillaDeRuedas)%>%
            addLegend(position = "topright", colors = c("#eb34d5"), labels = c("Subte"))%>%
            addPolygons(data=Comunas,
                        color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.25,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
        MapaSubte
    })
    output$Coro_Subte<-renderLeaflet({
        CoroSubte<-leaflet(Transp_x_C) %>% 
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addPolygons(
                fillColor = ~palSubte(Subtex100),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))%>%
            addLegend(pal = palSubte, 
                      values = ~Subtex100,
                      opacity = 0.7, 
                      title = "Porcentaje del total de 
            Estaciones de Subte",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers( ~lat,~long, label =  ~as.character(Comuna), 
                                  labelOptions = labelOptions(
                                      noHide = T, size=1,
                                      direction='top',textOnly = F))
        CoroSubte
    })
    output$Distr_Subte<-renderPlot({
        BarrasSubte<-ggplot(Transp_x_C,mapping = aes(
            reorder(Comuna, Subte),
            Subte)) +
            geom_col(fill="#eb34d5",
                     color="black")+
            geom_text(aes(label = Subte), 
                      vjust = 2.1, size = 3.5)+
            labs(title = "Estaciones de Subte por Comuna",
                 x="Comuna",
                 y="Cantidad de Estaciones",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_classic()
        BarrasSubte
    })     


    #TREN 
        output$Recorrido_Trenes<-renderLeaflet({
            MapaTrenes<-leaflet() %>%
                setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolylines(data = Red_Tren, color="#30c9fc", opacity = .4, weight = 3)%>%
                addLegend(position = "topright", colors = c("#30c9fc"), labels = c("Tren"))%>%
                addPolygons(data=Comunas,
                            color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.25,
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE))
            MapaTrenes
        })
        output$Coro_Trenes<-renderLeaflet({
            CoroTrenes<-leaflet(Transp_x_C) %>% 
                setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                addPolygons(
                    fillColor = ~palTren(Trenx100),
                    weight = 1,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE))%>%
                addLegend(pal = palTren, 
                          values = ~Trenx100,
                          opacity = 0.7, 
                          title = "Porcentaje del total de Estaciones de Tren",
                          labFormat = labelFormat(suffix="%"),
                          position = "bottomleft")%>%
                addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                      labelOptions = labelOptions(noHide = T, size=1,
                                                                  direction='top',textOnly = F))
            CoroTrenes
        })
        output$Distr_Trenes<-renderPlot({
            BarrasTrenes<-ggplot(Transp_x_C,mapping = aes(
                reorder(Comuna, Tren),
                Tren)) +
                geom_col(fill="#30c9fc",
                         color="black")+
                geom_text(aes(label = Tren),
                          vjust = 2, size = 3.5)+
                labs(caption ="Estaciones de Tren por Comuna",
                     x="Comuna",
                     y="Cantidad de Estaciones")+
                theme_classic()
            BarrasTrenes
        })    


        #BICICLETAS     
        output$Recorrido_Bicicletas<-renderLeaflet({
                MapaBicis<-leaflet() %>%
                    setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
                    addProviderTiles(providers$CartoDB.Positron) %>%  
                    addMarkers(data=EcoBici, clusterOptions = markerClusterOptions())%>%
                    addPolylines(data = Red_CicloV, color = "#c7730c", opacity = 1, weight = 1)%>%
                    addPolygons(data=Comunas,
                                color = "#444444",
                                weight = 1,
                                smoothFactor = 0.5,
                                opacity = 1.0,
                                fillOpacity = 0.25,
                                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                    bringToFront = TRUE))
                MapaBicis
            })
            output$Coro_CicloV<-renderLeaflet({
                CoroCicloV<-leaflet(Transp_x_C) %>% 
                    setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
                    addProviderTiles(providers$CartoDB.Positron)%>%
                    addPolygons(
                        fillColor = ~palCicloV(CicloVx100),
                        weight = 1,
                        opacity = 1,
                        color = "black",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE))%>%
                    addLegend(pal = palCicloV, 
                              values = ~CicloVx100,
                              opacity = 0.7, 
                              title = "Porcentaje del total de Ciclovias",
                              labFormat = labelFormat(suffix="%"),
                              position = "bottomleft")%>%
                    addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                          labelOptions = labelOptions(noHide = T,
                                                                      direction='top',textOnly = F))
                CoroCicloV
            })
            output$Coro_EcoB<-renderLeaflet({
                CoroEcoB<-leaflet(Transp_x_C) %>% 
                    setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
                    addProviderTiles(providers$CartoDB.Positron)%>%
                    addPolygons(
                        fillColor = ~palEcoB(EcoBx100),
                        weight = 1,
                        opacity = 1,
                        color = "black",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE))%>%
                    addLegend(pal = palEcoB, 
                              values = ~EcoBx100,
                              opacity = 0.7, 
                              title = "Porcentaje del total de Ecobicis",
                              labFormat = labelFormat(suffix="%"),
                              position = "bottomleft")%>%
                    addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                          labelOptions = labelOptions(noHide = T,
                                                                      direction='top',textOnly = F))
                CoroEcoB  
            })
            output$Distr_Bicicletas<-renderPlot({
                BarrasBici<-ggplot(Transp_x_C)+
                    geom_col(mapping = aes(
                        x=reorder(Comuna,Ciclovias),
                        y=Ciclovias),
                        fill= "tan1",
                        width = .5,
                        position = position_nudge(x = -0.225))+
                    geom_col(mapping = aes(
                        x=Comuna,
                        y=Ecobicis),
                        fill= "slategray4",
                        width = 0.5,
                        position = position_nudge(x = 0.225))+
                    labs(title ="Servicios de Bicicleta segun Comuna.",
                         x="Comuna",
                         y="Cantidad",
                         caption = "Fuente: https://data.buenosaires.gob.ar")+
                    theme_classic()+
                    coord_flip()
                BarrasBici        
          
}
            
            
# Run the application 
shinyApp(ui = ui, server = server)

