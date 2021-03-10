
library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)
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
Hosp_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospCom.csv")
Hospitales_reducido <- st_read("HospitalesR.shp")
Comunas <- st_read("Comunas.shp")


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
                     )
                     ),
        tabPanel("Servicios Públicos",
                 navlistPanel(
                     tabPanel("Escuelas",
                              highchartOutput(outputId = "G_Esc"),
                              selectInput(inputId = "input_comuEscuela",
                                          choices = Esc_Com$Comuna,
                                          label = "Seleccione comuna"),
                              br(),
                              leafletOutput(outputId = "M_escuelas")),
                     tabPanel("Hospitales",
                              highchartOutput(outputId = "G_Hosp"),
                              br(),
                              leafletOutput(outputId = "M_hospitales"))
                 )
                 )
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
                hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2020)")%>%
                hc_yAxis(title = list(text = "Cantidad de escuelas"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente Data Buenos Aires- GCBA", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_flat())
                })
        
        output$M_Escuelas <- renderLeaflet({
            leaflet(data = Muestra_escuelas) %>% 
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
                })
        
        output$G_Hosp <- renderHighchart({
            Grafico_hosp <- hchart(Hosp_Com, "bar", hcaes(x = Comuna, y = Hospitales, group = Hospitales))  %>% hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Cantidad de hospitales por comuna.")%>%
                hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2020)")%>%
                hc_yAxis(title = list(text = "Cantidad de hospitales"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente Data Buenos Aires- GCBA", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_flat())
                })
        
        output$M_Hospitales <- renderLeaflet({
            leaflet(data = Hospitales_reducido) %>% 
                setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addAwesomeMarkers(~long.x, ~lat.x, icon = icons, label = ~as.character(TIPO)) %>%
                addPolylines(data = Comunas, color="#2F4AFF", opacity = 1, weight = 2)
                })
}
            
            
            

# Run the application 
shinyApp(ui = ui, server = server)

