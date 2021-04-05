
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
options(scipen=10)


rm(list = ls())

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
EscPal<-colorQuantile("OrRd", domain= EdadEsc_x_Esc$Prop, n=4)
Hosp_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospCom.csv")
Hospitales_reducido <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospitalesR.geojson")
Comunas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Comunas.geojson")
hacinamiento <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/hacinamiento.csv")
Viviendas <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Viviendas.csv")
Viv_Com_Geo <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/VIVCOM.geojson")
Regimen<-fread("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen.csv",encoding = "UTF-8")
#Regimen18<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen18.geojson",options = "ENCODING=WINDOWS-1252")
Regimen18<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen18.geojson",options = "ENCODING=UTF-8")
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
Red_Subte<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Subte.geojson?token=AST4NRQRQ7S2YLNN4HNO5ZLAJ6M4S")
Acce_Subte<-fread("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Accesubte.csv",encoding = "UTF-8")
Red_Tren<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Trenes.geojson")
Red_CicloV<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Ciclovias.geojson")
EcoBici<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/EcoBici.geojson")
Transp_x_C<-st_read("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/TranspxC.geojson")
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


#UI
ui <- fluidPage(
    
    theme = shinytheme("united"),
    
    
    titlePanel(strong("Merecer la ciudad: el derecho al uso y disposicion del espacio urbano en la Ciudad Autonoma de Buenos Aires")),
    
    tabsetPanel(
        tabPanel("Introduccion",
                 tabPanel("Introduccion",
                          textOutput(outputId = "Prueba"),
                          hr(),
                          helpText("A los fines de precisar el análisis, cabe aclarar que, por la ley orgánica nº 1777 de 2005, la Ciudad Autonoma de Buenos Aires se divide territorialmente y administrativamente en 15 comunas. 
                              Entre otras atribuciones, estas unidades elaboran sus propios planes de acción para el mantenimiento de los espacios verdes y de las vías de tránsito secundarias; 
                              desarrollan acciones para la administración de su patrimonio y ejecución de su presupuesto anual; 
                              evaluan las demandas y necesidades sociales; 
                              gestionan las políticas comunitarias; 
                              implementan un sistema multidisciplinario de mediación para la resolución de conflictos; 
                              convocan a audiencia pública para debatir asuntos de interés general y llaman a consultas populares no vinculantes. 
                              Cabe considerar que no pueden crear impuestos, tasas, contribuciones, ni tomar créditos; 
                              sumado a que su accionar no puede contradecir el interés general de la ciudad."),
                          helpText("Las comunas estan integradas por distintos barrios, a saber:",
                                   br(),
                                     "-Comuna 1: Retiro, San Nicolas, Puerto Madero, San Telmo, Monserrat y Constitucion.",
                                   br(),
                                   "-Comuna 2: Recoleta.",
                                   br(),
                                   "-Comuna 3: San Cristobal y Balvanera.",
                                   br(),
                                   "-Comuna 4: Boca, Barracas, Parque Patricios y Nueva Pompeya.",
                                   br(),
                                   "-Comuna 5: Almagro y Boedo.",
                                   br(),
                                   "-Comuna 6: Caballito.",
                                   br(),
                                   "-Comuna 7: Flores y Parque Chacabuco.",
                                   br(),
                                   "-Comuna 8: Villa Soldati, Villa Riachuelo y Villa Lugano.",
                                   br(),
                                   "-Comuna 9: Parque Avellaneda, Liniers y Matadero.",
                                   br(),
                                   "-Comuna 10: Villa Real, Monte Castro, Versalles, Floresta, Velez Sarfield y Villa Luro.",
                                   br(),
                                   "-Comuna 11: Villa Gral. Mitre, Villa Devoto, Villa del Parque y Villa Santa Rita.",
                                   br(),
                                   "-Comuna 12: Coghlan, Saavedra, Villa Urquiza y Villa Pueyrredon.",
                                   br(),
                                   "-Comuna 13: Belgrano, Nuñez y Colegiales.",
                                   br(),
                                   "-Comuna 14: Palermo.",
                                   br(),
                                   "-Comuna 15: Chacarita, Villa Crespo, Paternal, Villa Ortuzar, Agronomia y Parque Chas."))),
        tabPanel("Indagando la poblacion",
                 navlistPanel(
                   tabPanel("Datos demograficos",
                            h3(strong("Distribucion etaria de la poblacion de la Ciudad.")),
                            helpText("Caracterizar el grado y el tipo de acceso a la ciudad del que gozan los habitantes de la Ciudad
                            Autonoma de Buenos Aires, vuelve ineludible una previa caracterizacion de su poblacion. Para hacerlo resulta
                            pertinente atenerse, en un primer momento, a la descripcion de la estructura demografica de la ciudad, indagando
                            la distribucion de la poblacion segun su edad y sexo."),
                            highchartOutput(outputId = "G_demo"),
                            br(),
                            helpText("En este grafico se observan varios aspectos interesantes de destacar: el primero es que el
                            volumen de la poblacion se ha mantenido practicamente constante desde 1947, ubicandose en valores que
                            varian alrededor de los 2.9 millones de habitantes. Sin embargo, como segunda observacion, cabe notar
                            que sí ha cambiado la composicion etaria de la poblacion. En este sentido, vemos que desde 1960 el rango
                            de edad “+60” va en ascenso, mientras que los rangos de 15 a 44 años decrecen marcadamente. Manteniendose
                            constante el tamaño absoluto de la poblacion, parece licito hablar de un envejecimiento sostenido de la poblacion de la CABA.",
                            br(),
                            br(),
                            "Si bien, estas tendencias parecen revertirse segun los datos del ultimo censo, sería apresurado sacar conclusiones 
                            al respecto. En la actualidad, o al menos de acuerdo con los datos censales de 2010, el rango etario con menor 
                            representación es el de los menores de 15 años, mientras que en los valores más altos se observa al doble rango 15-45 
                            y, seguido, al rango +60. "),
                            br(h4(strong("Pirámide poblacional de la Ciudad Autonoma de Buenos Aires (1855-2010)."))),
                            plotlyOutput(outputId = "G_Pir"),
                            selectInput(inputId = "input_fecha",
                                        choices = Piramide$Año,
                                        label = "Seleccione año del censo",
                                        selected = NULL),
                            helpText("El ejercicio de observar las piramides censo a censo, revela la dinamica de
                            la distribucion de sexo y género en la Ciudad de Buenos Aires. Por ejemplo, la piramide de 1869
                            muestra una poblacion altamente masculinizada producto de los flujos migratorios provenientes de Europa. 
                            En la piramide de 2010, se encuentra una poblacion levemente feminizada, tendencia que se especifica a 
                            mayor edad. Por otro lado, ya desde 1980 estamos en condiciones de hablar de una poblacion “muy envejecida” 
                            puesto que el grupo mayor de 65 años, alcanza el 15% del total de la poblacion. En la actualidad este porcentaje
                            supera el 16% del conjunto, comprobandose la tendencia al envejecimiento que fue observada en el grafico anterior.")),
                   tabPanel("Pobreza e indigencia",
                              h3(strong("Distribución porcentual anual de Pobreza e Indigencia en la Ciudad.")),
                              helpText("Para introducir el analisis propuesto, tambien es preciso contemplar el contexto socio-histórico en que se desarrollaron
                              muchos de los indicadores estudiados. En principio, cabe considerar los procesos políticos, sociales y económicos que tuvieron
                              lugar en las últimas décadas de la historia argentina, los cuales atravesaron la sociedad y propiciaron profundos cambios en 
                              su estructura.",
                              br(),
                              br(),
                              "Entre otras consecuencias, el proceso de reestructuracion neoliberal de la década del 90, traducido en una desregulación
                              económica y descentralización del Estado, devino en un incremento de los índices de pobreza y marginalidad de la población
                              (Svampa, 2005). Aunque los mecanismos de exclusión fueron diversos, es sustancial tener en cuenta el fuerte incremento de
                              la pobreza reciente (predominante en el universo pobre desde 1995), seguida por la pobreza crónica y acompañada por los ya
                              valores importantes de la pobreza estructural (Arakaki, 2011).",
                              br(),
                              highchartOutput(outputId = "G_Pob"),
                              helpText("Si bien en los años siguientes existió una mejora en los indicadores de pobreza por ingreso, en ningún caso se
                              retorno a los pisos preexistentes. En el caso de la Ciudad de Buenos Aires, en el año 2006, contaba con un 12.7% de personas
                              pobres (EPH- INDEC, 2007). Del análisis de los datos, surge que 10 años después se registran los mayores índices de pobreza en
                              el período comprendido entre el primer trimestre del 2015 y el primero del 2019 (llegando a superar el 18%).  Asimismo, si bien
                              se observa una baja en los porcentajes del año 2017, es posible distinguir un aumento sostenido desde esa fecha en adelante."),
                              highchartOutput(outputId = "G_Pob2"))),
                     tabPanel("NBI",
                              h3(strong("Porcentaje de hogares con NBI por Comuna (2010).")),
                              plotOutput(outputId = "G_NBI"),
                              helpText("En este grafico se observa que la Comuna 1 concentra la mayor cantidad de poblacion con NBI. 
                              Sin embargo, las Comunas 4, 3, y 8 poseen valores que superan la media por 5 puntos porcentuales, mostrando que es el sur de la ciudad el que se ve mayormente afectado en lo que a la satisfaccion de sus necesidades basicas respecta.",
                              br(),
                              br(),
                              "Por otra parte, cabe destacar la amplitud de los valores en un rango de 14.2 puntos porcentuales. 
                               Las Comunas 12, 13 y 11 son las que muestran menores valores de NBI, respectivamente, evidenciando un fuerte contraste entre el norte y el sur de la ciudad."),
                              ),
                   tabPanel("Ficha tecnica",
                            textOutput(outputId = "TecnicaPob"),
                            h4(strong("Linea de pobreza e indigencia")),
                            helpText("La linea de pobreza es el valor monetario de una Canasta Basica Total de bienes y servicios capaz de satisfacer un conjunto de necesidades alimentarias y no alimentarias consideradas esenciales. 
                            Se denomina pobres a los hogares cuyos ingresos no alcanzan dicha linea o valor, y a la poblacion incluida en ellos.",
                            br(),
                            br(),
                            "Por su parte, la linea de indigencia es el valor monetario de una Canasta Basica de Alimentos, de costo minimo, capaz de satisfacer un umbral elemental de necesidades energeticas y proteicas. 
                            Se considera indigentes a los hogares cuyos ingresos no alcanzan dicha linea o valor, y a la poblacion incluida en ellos."),
                            hr(),
                            h4(strong("Necesidades Basicas Insatisfechas (NBI)")),
                            helpText("Los hogares y la población que poseen Necesidades Básicas Insatisfechas presentan, al menos, uno de los siguiente indicadores de privacion:",
                            br(),
                            "Hacinamiento critico;",
                            br(),
                            "Vivienda, siendo hogares que tienen lugar en una vivienda de tipo inconveniente (pieza de inquilinato, vivienda precaria u otro tipo);",
                            br(),
                            "Condiciones sanitarias (hogares que no tienen ningun tipo de retrete);",
                            br(),
                            "Asistencia escolar (hogares con menores en edad escolar (6-12) que no asisten a la escuela);",
                            br(),
                            "Capacidad de subsistencia (hogares que tienen 4 o mas personas por miembro ocupado y ademas, cuyo jefe no haya completado tercer grado de escolaridad primaria.)")),
                            hr()
                   )),
        tabPanel("Desarrollo humano",
                 navlistPanel(
                     tabPanel("Escuelas",
                              h3(strong("Distribucion de escuelas de la Ciudad.")),
                              highchartOutput(outputId = "G_Esc"),
                              helpText("Se observa que.."),
                              br(h4(strong("Mapa de escuelas de la Ciudad, según cantidad de niveles ofrecidos."))),
                              leafletOutput(outputId = "M_Escuelas"),
                              br(h4(strong("Poblacion en edad escolar(5-19) por cantidad de escuelas"))),
                              leafletOutput(outputId = "EdEsc_x_Esc")),
                     tabPanel("Hospitales",
                              h3(strong("Distribucion de hospitales de la Ciudad.")),
                              helpText("El presente apartado indaga en los hospitales de la ciudad. 
                                       Específicamente, distingue tres tipos de hospitales: de agudos, de niños y especializados.",
                              br(),
                              br(),
                              "Los", em("hospitales de agudos"), "brindan asistencia a la salud en clinica medica, pediatria, traumatologia, 
                              cardiologia, dermatologia, ginecologia, obstetricia, cirugia y otras especialidades. Ademas realizan estudios complementarios 
                              (radiologia, mamografias, tomografia, laboratorio, ecografias y otros estudios de diagnostico y prevencion de enfermedades).",
                              br(),
                              br(),
                              "Por su parte, los", em("hospitales especializados"), "monovalentes brindan asistencia a la salud en diferentes especialidades: gastroenterologia; 
                              infectologia; maternidad; odontologia; oncologia; oftalmologia; quemados; rehabilitacion psicofísica; rehabilitacion respiratoria; salud mental; y zoonosis.",
                              br(),
                              br(),
                              "Finalmente, los", em("hospitales de niños"), "cuentan con atencion ambulatoria, internacion, diagnostico y tratamiento; 
                              asi como tambien, diferentes especialidades medicas pediatricas. "),
                              highchartOutput(outputId = "G_Hosp"),
                              br(),
                              helpText("El analisis de la distribucion de hospitales, muestra que la comuna 8 es la que mayor cantidad de instituciones posee. Aunque la comuna 6 cuenta con 4 hospitales, 
                              y es seguida por la comuna 2 con 3 hospitales, llama la atención que las restantes comunas no cuenten con mas de 2 hospitales. Ademas, cabe destacar el caso de la comuna 1 
                              que carece de este tipo de servicios."),
                              br(h4(strong("Mapa de hospitales de la Ciudad, según su especialización."))),
                              leafletOutput(outputId = "M_Hospitales"),
                              helpText("En el mapa es posible explorar la distribucion de tipo de hospitales por comuna. Se encuentra que entre aquellas comunas 
                              que poseen un unico hospital, la mayoria cuenta con hospitales de agudos, salvo las comunas 5 y 13 que tienen entre su geografía hospitales especializados.",
                              br(),
                              br(),
                              "Respecto a los 3 hospitales de niños, 2 de ellos se localizan en la comuna 8 y el tercero en la comuna 2."),
                              br()),
                     tabPanel("Cultura",
                              h3(strong("Distribucion de espacios culturales de la Ciudad.")),
                              plotlyOutput((outputId= "BarrasCul")),
                              helpText("La distribucion de espacios culturales en la ciudad muestra que la mayor concentracion se radica, marcadamente, en la comuna 1. 
                              Seguidamente se encuentran las comunas 2, 14 y 3, sin embargo, estas poseen una cantidad notoriamente menor de comercios, 
                              espacios de exhibicion, bibliotecas y espacios de formacion.",
                              br()),
                              br(h4(strong("Distribucion geografica de los espacios culturales."))),
                              leafletOutput(outputId = "MapaCul"),
                              br(),
                              helpText("En cuanto a las comunas que cuentan con menor presencia de estos espacios, se trata de aquellas que se localizan en el suroeste de la ciudad: 
                              8, 10, 9 y 11. Especificamente, la comuna 8 es la que posee el valor minimo, teniendo en cuenta que en el marco de su territorio hay menos de 100 espacios culturales."),
                              br(),
                              h4(strong("Distribucion porcentual de los espacios culturales.")),
                              leafletOutput(outputId = "CoroCul"))
                 )),
        tabPanel("Vivienda",
                 navlistPanel(
                   tabPanel("Hacinamiento",
                            h3(strong("Distribución porcentual de hacinamiento por comuna.")),
                            helpText("La situacion de", em("hacinamiento"), "expresa la importancia relativa de los hogares, o de la poblacion en ellos, en los que hay dos o mas personas por cuarto en la vivienda (hacinados)."),
                            br(),
                            highchartOutput(outputId = "G_HNC"),
                            br(),
                            helpText("Si bien los valores mas actuales muestran que es la comuna 8 la que posee mayor porcentaje de hacinamiento, en el periodo comprendido en los ultimos diez años se observan 
                                     altos valores en las comunas 4, 1 y 7.",
                            br(),
                            br(),
                            "Asimismo, del analisis del periodo bajo estudio se desprende que las comunas 2, 13, 5, 6 y 12 presentan valores que no superan el 10% de la poblacion en condicion de hacinamiento. Estas unidades se corresponden con el 
                            norte y el centro de la ciudad. "),
                            hr(),
                            helpText("Por su parte, los hogares con", em("hacinamiento crítico"), "expresan la importancia relativa de los hogares en los que hay más de tres personas por cuarto de la vivienda."),
                            highchartOutput(outputId = "G_HC"),
                            helpText("aca va lo de hacinamiento critico"),
                            br()),
                   tabPanel("Condicion de ocupacion",
                            h3(strong("Distribución porcentual de viviendas, según condicion de ocupacion.")),
                            highchartOutput(outputId = "G_Vivienda"),
                            br(h4(strong("Mapa de viviendas de la Ciudad, ocupadas con fines comerciales."))),
                            leafletOutput(outputId = "M_Vivienda")),
                   tabPanel("Regimen de Tenencia",
                            selectInput(inputId = "input_AÑO",
                                        choices = Regimen$año,
                                        label = "Seleccione año",
                                        selected = TRUE),
                            plotOutput(outputId= "BarrasRegimen"),
                            br(),
                            selectInput(inputId = "input_Regimen",
                                        choices = Regimen18$Regimen_Tenencia,
                                        label = "Seleccion Régimen",
                                        selected = TRUE),
                            leafletOutput(outputId = "CoroRegimen")),
                   tabPanel("Precio del metro cuadrado",
                            leafletOutput(outputId = "P_x_m2"))
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
)

#Server
server <- function(input, output) {
#####################REACTIVE###################################
                pir_filt <- reactive({
                    pir_filt = Piramide[Piramide$Año == input$input_fecha,]
                    pir_filt
                })
                
                Reg_filt<-reactive({
                  Regimen%>%filter(año %in% input$input_AÑO)
                })
                
                Reg_filt2<-reactive({
                  Regimen18%>%filter(Regimen_Tenencia %in% input$input_Regimen)
                })
                

#############################INTRODUCCION##############################                
        output$Prueba <- renderText({
                    "prueba"
                })

                
##################################POBLACION##################################
          output$G_demo <- renderHighchart({
              G_Pob_Edad=  hchart(Poblacion_Edad, "line",
                                      hcaes(x = Año, y= Poblacion,
                                            group = Edad)) %>%
                    hc_title(text = "Cantidad de Poblacion por grupo etario(1960-2010)")%>%
                    hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (1960-2010)")%>%
                    hc_yAxis(title = list(text = "Poblacion"),
                             labels = list(format = "{value}")) %>%
                    hc_credits(enabled = TRUE, text = "Fuente: Instituto Nacional de Estadisticas y Censos", style = list(fontSize = "12px"))%>%
                    hc_add_theme(hc_theme_ffx())
                })
                
          output$G_Pir <- renderPlotly({
            Pir <- ggplot(pir_filt(), mapping=aes(x= grupo_edad, y= Poblacion, fill=sexo))+
                    geom_col(alpha=.7)+
                    labs(title="",
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
                
                
         output$G_Pob <- renderHighchart({
            Evolucion_POB2 <- hchart(Pobreza, "bar", hcaes(x = Año, y = Pobreza_total, group = TRIM))  %>% 
                hc_add_theme(hc_theme_gridlight()) %>%
                hc_title(text = "Personas en situacion de Pobreza e Indigencia, por año y trimestre.")%>%
                hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2015-2019)")%>%
                hc_yAxis(title = list(text = "Situacion de pobreza (en %)"),
                         labels = list(format = "{value}%")) %>%
                hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
                hc_add_theme(hc_theme_google())
                })
        
        output$G_Pob2 <- renderHighchart({
            losdos <- highchart() %>%
                hc_add_series(Pobreza_anual, "line", hcaes(x = Año, y = mean_pob), name = "Pobreza") %>%
                hc_add_series(Pobreza_anual, "line", hcaes(x = Año, y = mean_ind), name = "Indigencia") %>% 
                hc_add_theme(hc_theme_google()) %>%
                hc_title(text = "Evolucion de Pobreza e Indigencia.")%>%
                hc_subtitle(text = "Promedio anual. Ciudad Autonoma de Buenos Aires (2015-2019)")%>%
                hc_yAxis(title = list(text = "Situacion de pobreza e indigencia"),
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
                labs(title="",
                     x="Comunas",
                     caption = "Fuente: Censo Nacional de Poblacion, hogares y viviendas (INDEC) 2010.")
                BarrasNBI
                })
        output$TecnicaPob <- renderText({
          ""
           })


#######################################DESARROLLO HUMANO###########################################
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

        output$EdEsc_x_Esc<-renderLeaflet({
          CoroEscuelas<-leaflet(EdadEsc_x_Esc)%>% 
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addPolygons(
              fillColor = ~EscPal(Prop),
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
            addLegend(pal=EscPal,
                      values = ~Prop,
                      opacity = 0.7, 
                      title = "Poblacion en edad escolar por escuela",
                      position = "bottomleft")
          CoroEscuelas
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
                addAwesomeMarkers(~long, ~lat, icon = icons, label = ~as.character(TIPO2)) %>%
                addPolylines(data = Comunas, color="#2F4AFF", opacity = 1, weight = 2)
                })
        
        output$G_HNC <- renderHighchart({
          Evolucion_HNC <- hchart(hacinamiento, "line", 
                                  hcaes(x = Año, y= Hacinamiento_no_critico, 
                                        group = Comunas)) %>%
            hc_title(text = "Hacinamiento no critico por comuna")%>%
            hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2010-2018)")%>%
            hc_yAxis(title = list(text = "Hacinamiento no critico (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
            })
        
        output$G_HC <- renderHighchart({
          Evolucion_HC <- hchart(hacinamiento, "line", 
                                 hcaes(x = Año, y= Hacinamiento_critico, 
                                       group = Comunas)) %>%
            hc_title(text = "Hacinamiento critico por comuna")%>%
            hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2010-2018)")%>%
            hc_yAxis(title = list(text = "Hacinamiento critico (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
            })
        
        output$BarrasCul<-renderPlotly({
          BarrasCul <- ggplot(Cul_x_C)+
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
        
#############VIVIENDA###################################################
        
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
            hc_title(text = "Distribucion porcentual de viviendas segun condicion de ocupacion, por comuna.") %>%
            hc_subtitle(text = "Ciudad Autonoma de Buenos Aires (2010)") %>%
            hc_yAxis(title = list(text = "Distribucion porcentual de viviendas (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, 
                       text = "Fuente: Censo 2010 (INDEC)", 
                       style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_economist())
            })
        
        output$M_Vivienda <- renderLeaflet({
          Geo_comercios <- leaflet(Viv_Com_Geo) %>%
            setView(lng = -58.445531, lat = -34.606653, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.Positron, 
                             options = providerTileOptions(id = "mapbox.light",
                                                           accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
            addPolygons(data = Comunas,
                        fillColor = ~pal_Viv_Geo(Viv_Com_Geo$Cmrc_f_),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 5,
                                                     color = "#F5C9BA",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels_Viv_Geo,
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                    textsize = "15px",
                                                    direction = "auto")) %>%
            addLegend(data = Viv_Com_Geo,
                      "bottomright", 
                      pal = pal_Viv_Geo, 
                      values = ~Cmrc_f_,
                      title = "Comercios, oficinas y consultorios",
                      opacity = 2) %>%
            addLabelOnlyMarkers(data = Comunas,
                                ~lat,~long,
                                label =  ~as.character(Comuna), 
                                labelOptions = labelOptions(noHide = T, size=1,
                                                            direction='top',textOnly = F))
            })
        
        output$BarrasRegimen<-renderPlot({
          RegimenA<-ggplot(data=Reg_filt(), 
                           aes(x=comunas,
                               y=porcentaje, 
                               fill= Regimen_Tenencia))+
            scale_fill_manual(values = 
                                c("#34eb6b","#d334eb","#eaf51b"))+
            geom_col(width = .3)
          labs(title = "Regimen de Tenencia segun Comuna",
               subtitle = "CABA")+
            theme_bw()+
            coord_flip()
          RegimenA  
          })
          
        output$CoroRegimen<-renderLeaflet({
          CoroRegimen<-leaflet(Reg_filt2())%>%
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addLabelOnlyMarkers(~lat,~long, label =  ~as.character(Comuna), 
                                labelOptions = labelOptions(noHide = T, size=1,
                                                            direction='top',textOnly = F))%>%
            addLegend(pal=VivPal, 
                      values = ~porcentaje,
                      opacity = 0.7, 
                      title = "Porcentaje sobre el total de Hogares",
                      position = "bottomleft")
                      })
        
        observe({leafletProxy("CoroRegimen", data = Reg_filt2())%>%
            addPolygons(fillColor =
                          ~VivPal(porcentaje),
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
                          bringToFront = TRUE))
                          })
        output$P_x_m2<-renderLeaflet({
          CoroPreciom2<-leaflet(Precm2) %>% 
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addPolygons( 
              fillColor = ~palm2(US_x_m2),
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
            addLegend(pal=palm2, 
                      values = ~US_x_m2,
                      opacity = 0.7, 
                      title = "Precio promedio por m2",
                      labFormat = labelFormat(suffix="$"),
                      position = "bottomleft")
          
          CoroPreciom2
          })
    
#####################TRANSPORTE########################################
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
          })
}
            
            
# Run the application 
shinyApp(ui = ui, server = server)

