
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(hrbrthemes)
library(leaflet)
library(gganimate)
library(readr)
library(data.table)
library(plotly)

#Archivos
Pobreza <- fread("Pobreza.csv")
NBI <- read_csv("NBI.csv")
Pobreza_anual <- read_csv("Pobreza_anual.csv")
Poblacion_Edad <- read_csv("Poblacion_Edad.csv")
Piramide <- read_csv("PiramidePoblacion.csv")


#UI
ui <- fluidPage(
    
    theme = shinytheme("darkly"),
    
    titlePanel("Merecer la ciudad: el derecho al uso y disposición del espacio urbano en la Ciudad Autónoma de Buenos Aires"),
    
    tabsetPanel(
        tabPanel("Introducción",
                 navlistPanel(
                     tabPanel("Introducción",
                              textOutput(outputId = "Prueba")),
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
            Evolucion_POB2 <- hchart(Pobreza, "bar", hcaes(x = Año, y = Pobreza_total, group = TRIM))  %>% hc_add_theme(hc_theme_gridlight()) %>%
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
        
}
            
            
            

# Run the application 
shinyApp(ui = ui, server = server)


