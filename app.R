
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


rm(list = ls())

#Archivos
Pobreza <- read_csv("Pobreza.csv")
NBI <- read_csv("NBI.csv")
Pobreza_anual <- read_csv("Pobreza_anual.csv")
Poblacion_Edad <- read_csv("Poblacion_Edad.csv")
Piramide <- read_csv("PiramidePoblacion.csv")

Esc_Com <- read_csv("EscCom.csv")

Muestra_escuelas <- read_csv("Escuelas.csv")
Muestra_escuelas <- Muestra_escuelas[,c(-14)]

Hosp_Com <- read_csv("HospCom.csv")

Hospitales_reducido <- read_csv("Hospitales.csv")
Hospitales_reducido <-Hospitales_reducido[,c(1,2,3,4,5)]


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
                                          label = "Seleccione año del censo"))
                     )
                     ),
        tabPanel("Servicios Públicos",
                 navlistPanel(
                     tabPanel("Escuelas",
                              highchartOutput(outputId = "G_Esc"),
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
            Grafico_esc <- hchart(Esc_Com, "bar", hcaes(x = Comuna, y = Escuelas, group = Comuna))  %>% hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Cantidad de escuelas por comuna.")%>%
                hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (2020)")%>%
                hc_yAxis(title = list(text = "Cantidad de escuelas"),
                         labels = list(format = "{value}")) %>%
                hc_credits(enabled = TRUE, text = "Fuente Data Buenos Aires- GCBA", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_flat())
                })
        
        output$M_Escuelas <- renderLeaflet({
            Geo_esc <- leaflet(data = Muestra_escuelas) %>% 
                setView(long = -58.445531, lat = -34.606653, zoom = 11) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addMarkers(~long, ~lat)
            Geo_esc
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
            Geo_hosp <- leaflet(data = Hospitales_reducido) %>% 
                setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addMarkers(~long, ~lat)
            Geo_hosp
        })
}
            
            
            

# Run the application 
shinyApp(ui = ui, server = server)


