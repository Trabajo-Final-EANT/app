
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
EscPal<-colorNumeric("OrRd", domain= EdadEsc_x_Esc$Prop, n=4)
Hosp_Com <- read_csv("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospCom.csv")
Hospitales_reducido <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/HospitalesR.geojson")
Comunas <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Comunas.geojson")
hacinamiento <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/hacinamiento.csv")
Viviendas <- read_csv ("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/Viviendas.csv")
Viv_Com_Geo <- st_read("https://raw.githubusercontent.com/melinaschamberger/Aplicacion/main/VIVCOM.geojson")
Regimen<-fread("https://raw.githubusercontent.com/Trabajo-Final-EANT/Archivos/main/Regimen.csv",encoding = "UTF-8")
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


#UI
ui <- fluidPage(
    
    theme = shinytheme("united"),
    titlePanel(
      fluidRow(
        column(9,(h2(id="Titulo", "Acceder a la Ciudad: sobre la distribución de servicios en la Ciudad Autónoma de Buenos Aires"))), 
        column(3, (h6(tags$a("Melina Shamberger",href="https://www.linkedin.com/in/melina-schamberger"),align="right"))),
        column(3,(h6(tags$a("Ignacio Gomar", href="https://www.linkedin.com/in/ignacio-gomar"), align="right")))
        
      
        )
    ),
    
    
    
    tags$h2(tags$style("#Titulo{
                         color: #693fb5;
                         font-size: 24px;
                         font-style: italics;
                         }")),
    
    
               
    
    tabsetPanel(
        tabPanel("Introducción",
                 tabPanel("Introducción",
                          h3("Introducción"),
                          helpText(h5("Cuando nos referimos a una ciudad, podemos decir que estamos tanto ante una forma de vivir en común, como ante una forma de producir, caracterizada por concentrar en un espacio más bien reducido una gran diversidad de personas, actividades y culturas. 
                                      Sin embargo, esta forma de organización social no es posible sin vastos conjuntos de bienes colectivos en los que las ciudades se materializan. Es decir que, por un lado, se trata de una forma social que se distingue por concentrar lo distinto, por poner en relación los componentes más diversos del espacio social con la finalidad de reproducirse (Lefevbre, 1978). Mientras que, por otro, comprende el conjunto de bienes y servicios producidos que fungen de sustento material de la vida urbana. Entonces, si además de ser un espacio de vida, las ciudades son también un conjunto de elementos materiales dispuestos sobre este espacio, queda claro que lo urbano en conjunto se distribuye por un territorio determinado no de manera continua y plena, sino más bien discreta, desigual (Pirez, 2009)")),
                          hr(),
                          actionLink("BotonObjetivos",(h3("Objetivos"))),
                          textOutput("TextoObjetivos1"),
                          #br(),
                          textOutput("TextoObjetivos2"),
                          #br(),
                          textOutput("TextoObjetivos3"),
                          hr(),
                          actionLink("BotonUdA",(h3("Sobre las unidades de análisis: La ley de Comunas"))),
                          textOutput("TextoUdA1"),
                          textOutput("TextoUdA2"),
                          br(),
                          textOutput("TComuna1"),
                          br(),
                          textOutput("TComuna2"),
                          br(),
                          textOutput("TComuna3"),
                          br(),
                          textOutput("TComuna4"),
                          br(),
                          textOutput("TComuna5"),
                          br(),
                          textOutput("TComuna6"),
                          br(),
                          textOutput("TComuna7"),
                          br(),
                          textOutput("TComuna8"),
                          br(),
                          textOutput("TComuna9"),
                          br(),
                          textOutput("TComuna10"),
                          br(),
                          textOutput("TComuna11"),
                          br(),
                          textOutput("TComuna12"),
                          br(),
                          textOutput("TComuna13"),
                          br(),
                          textOutput("TComuna14"),
                          br(),
                          textOutput("TComuna15"),
                          br()
                    )),
        tabPanel("Estructura sociodemográfica",
                 navlistPanel(
                     tabPanel("Datos demográficos",
                              h3(strong("Distribución etaria de la población de la Ciudad.")),
                              helpText("Indagar el grado y el tipo de acceso a la ciudad del que gozan los habitantes de la Ciudad Autónoma de Buenos Aires, vuelve ineludible una previa caracterización de su población. Para hacerlo resulta pertinente atenerse, en un primer momento, a la descripcion de la estructura demografica de la ciudad, indagando la Distribución de la poblacion según su edad y sexo. "),
                              highchartOutput(outputId = "G_demo"),
                              br(),
                              helpText("En este gráfico se observan varios aspectos interesantes de destacar: el primero es que el volumen de la población se ha mantenido prácticamente constante desde 1947, ubicándose en valores que varían levemente en torno a los 2.9 millones de habitantes. Sin embargo, como segunda observación, cabe notar que sí ha cambiado la composición etaria de la población. En este sentido, vemos que desde 1960 el rango de edad “+60” va en ascenso, mientras que los rangos de 15 a 44 años decrecen marcadamente. Manteniéndose constante el tamaño absoluto de la población, parece lícito hablar de un envejecimiento sostenido de la población de la CABA."),
                              br(),
                              helpText("Si bien estas tendencias parecen revertirse según los datos del último censo, sería apresurado sacar conclusiones al respecto. En la actualidad, conforme los datos censales de 2010, el rango etario con menor representación es el de los menores de 15 años, mientras que en los valores más altos se observa al doble rango 15-45 y, seguido, al rango +60."),
                              br(),
                              br(),
                              br(h4(strong("Pirámide poblacional de la Ciudad Autónoma de Buenos Aires (1855-2010)."))),
                              selectInput(inputId = "input_fecha",
                                          choices = Piramide$Año,
                                          label = "Seleccione año del censo",
                                          selected = NULL),
                              plotlyOutput(outputId = "G_Pir"),
                              helpText("El ejercicio de observar las piramides censo a censo, revela la dinamica de la Distribución de sexo y género en la Ciudad de Buenos Aires. Por ejemplo, la pirámide de 1869 muestra una población altamente masculinizada, producto de los flujos migratorios provenientes de Europa. En la pirámide de 2010, se encuentra una población levemente feminizada, tendencia que se especifica a mayor edad. Por otro lado, ya desde 1980 es posible hablar de una población “muy envejecida”, puesto que el grupo mayor de 65 años alcanza el 15% del total de la población. Actualmente, este porcentaje supera el 16% del conjunto, comprobandose la tendencia al envejecimiento que fue observada en el gráfico anterior.")),
                     tabPanel("Pobreza e indigencia",
                              h3(strong("Distribución porcentual anual de Pobreza e Indigencia en la Ciudad.")),
                              helpText("Para introducir el análisis propuesto, también es preciso contemplar el contexto socio-histórico en que se desarrollaron muchos de los indicadores estudiados. En principio, cabe considerar los procesos políticos, sociales y económicos que tuvieron lugar en las últimas décadas de la historia argentina, los cuales atravesaron la sociedad y propiciaron profundos cambios en su estructura. ",
                              br(),
                              br(),
                              "Entre otras consecuencias, el proceso de reestructuración neoliberal de la década del 90, traducido en una desregulación económica y descentralización del Estado, devino en un incremento de los índices de pobreza y marginalidad de la población (Svampa, 2005). Aunque los mecanismos de exclusión fueron diversos, es sustancial tener en cuenta el fuerte incremento de la pobreza reciente (predominante en el universo pobre desde 1995), seguida por la pobreza crónica y acompañada por los ya valores importantes de la pobreza estructural (Arakaki, 2011). "),
                              br(),
                              highchartOutput(outputId = "G_Pob"),
                              helpText("Si bien en los años siguientes existió una mejora en los indicadores de pobreza por ingreso, en ningún caso se retorno a los pisos preexistentes. La Ciudad de Buenos Aires, en el año 2006, contaba con un 12.7% de personas pobres (EPH- INDEC, 2007). Del análisis de los datos, surge que 10 años después, en el año 2016, se registraron los mayores índices de pobreza en el período comprendido entre el primer trimestre del 2015 y el primero del 2019 (llegando a superar el 18%).  Asimismo, si bien se observa una baja en los porcentajes del año 2017, es posible distinguir un aumento sostenido desde esa fecha en adelante."),
                              highchartOutput(outputId = "G_Pob2")),
                     tabPanel("NBI",
                              h3(strong("Necesidades Básicas Insatisfechas.")),
                              helpText("En lo que respecta a la pobreza medida por NBI, presentó oscilaciones a nivel nacional que respondieron a los cambios estructurales propiciados por las políticas neoliberales. En el período comprendido entre 1998-2003, la población bajo esta condición superó el 10%; sin embargo, el 90% de estos hogares lo hicieron por el incumplimiento de sólo un indicador (generalmente, el de hacinamiento o la capacidad de subsistencia) (Arikaki, 2011).",
                                       br(),
                                       br(),
                                       "En el ámbito de la ciudad de Buenos Aires, en el año 2001, la población que mayormente se vió afectada por esta condición se ubicaba en la zona sur, donde se registraron comunas con valores de NBI superiores al 15%, llegando incluso -en un sector de la comuna 8- a superar el 20% (DGEyC, 2001). Esta relación asimétrica entre lo que sucede en el sur de la ciudad y aquello que sucede en el norte (donde el registro de NBI no superó el 10%); se modifica en los datos del 2010."),
                              plotOutput(outputId = "G_NBI"),
                              helpText("En el gráfico se observa que aunque las comunas 4, 3  y 8 tuvieron valores que superaron la media por 5 puntos porcentuales, fue la comuna 1 la que concentró mayor cantidad de población con NBI. Aun así, se sostuvo el rasgo distinguido entre la heterogeneidad de oportunidades de vida a las que acceden los dos extremos de la ciudad, comprobandose en la amplitud del rango de valores que fue de 14.2 puntos porcentuales. En este orden de ideas, se observa que fueron las comunas 12, 13 y 11 las que registraron menor cantidad de población con NBI, respectivamente. "),
                              hr()),
                     tabPanel("Ficha técnica",
                              actionLink("BotonPobr", h4("Línea de pobreza")),
                              br(),
                              textOutput("TextoPobr"),
                              hr(),
                              actionLink("BotonInd", h4("Línea de Indigencia")),
                              br(),
                              textOutput("TextoInd"),
                              hr(),
                              actionLink("BotonNBI",h4("Necesidades Básicas Insatisfechas (NBI)")),
                              br(),
                              textOutput("TextoNBI1"),
                              textOutput("TextoNBI2"),
                              textOutput("TextoNBI3"),
                              textOutput("TextoNBI4"),
                              textOutput("TextoNBI5"),
                              br(),
                              hr()
                 ))),
        tabPanel("Desarrollo humano",
                 navlistPanel(
                     tabPanel("Escuelas",
                              h3(strong("Distribución de escuelas de la Ciudad.")),
                              helpText("El orden impuesto en los 90 se relacionó con una 'modernización excluyente', gestando sus bases en la dualización de la sociedad y la economía. En dicho marco, se puso en marcha una reducción del gasto público que conllevó la descentralización administrativa y el traslado de competencias nacionales a los niveles provinciales y municipales (Svampa, 2005). Tal es el caso de la educación y los servicios de salud.",
                                       br(),
                                       br(),
                                       "En este sentido, si bien el proceso de privatización de la educación registraba valores en alza desde 1940, estos se vieron reforzados a partir de la citada reestructuración y, específicamente, el caso de la ciudad de Buenos Aires fue un epicentro de la cuestión (Judzik, Moschetti, 2016). A modo de ejemplo, basta considerar que en el año 2014 el 52% de la población de alumnos asistía a escuelas de gestión privada (DiNIECE, 2014).",
                                       br(),
                                       br(),
                                       "El siguiente gráfico muestra la distribución de escuelas por comuna en el año 2020 e incluye establecimientos públicos y privados. La comuna 4 es la que mayor cantidad de escuelas posee, seguida por la comuna 1. En paralelo, son las comunas 2,6 y 9, respectivamente, las que registran los valores más bajos en cantidad de centros educativos. El resto de las comunas posee una cantidad poco heterogénea, que varía entre los 170 y 220 establecimientos"),
                              highchartOutput(outputId = "G_Esc"),
                              br(),
                              h4(strong("Mapa de escuelas de la Ciudad, según cantidad de niveles ofrecidos.")),
                              helpText("Por su parte, el siguiente mapa expone la distribución geográfica de una muestra aleatoria de escuelas y la cantidad de niveles que cada una de ellas ofrece. A simple vista, se puede observar que la mayor parte de los establecimientos cuenta con sólo un nivel educativo, seguido por centros que poseen 2 o 3 niveles. Los establecimientos que ofrecen los 4 niveles (inicial, primario, secundario y superior) son minoría, teniendo una presencia marcadamente escasa en la zona sur de la ciudad."),
                              leafletOutput(outputId = "M_Escuelas"),
                              br(),
                              h4(strong("Distribución geográfica de personas en edad escolar (5-19), según cantidad de escuelas.")),
                              helpText("La relación entre la cantidad de escuelas que existe en cada comuna y el número de personas en edad escolar se puede observar en este otro mapa. Si bien el indicador no refleja el número de matriculados y matriculadas por centro educativo, evidencia que las comunas 8 y 7 (suroeste de la ciudad) cuentan con un número acotado de instituciones escolares, en relación a la población en edad escolar que registran."),
                              leafletOutput(outputId = "EdEsc_x_Esc"),
                              helpText("Aunque los datos recabados son un punto de partida interesante para explorar las condiciones de acceso a la educación en la ciudad, considerando los procesos de polarización creciente y la heterogeneidad de las características socioeconómicas de la población que integran las comunas, sería pertinente indagar -al interior de cada unidad territorial-  la matrícula registrada en las escuelas de gestión pública y de gestión privada (a modo de ejemplo, cabe tener en cuenta el caso de la comuna 1 que integra a los asentamientos informales ‘Villa 31’, ‘Villa 31bis’, ‘Barrio General San Martín’ y ‘Villa Rodrigo Bueno’, y a los barrios de Puerto Madero y Retiro). Si bien este análisis excede los límites del presente trabajo, no deja de ser una arista necesaria de contemplar para ampliar el conocimiento de la estructura social, económica y educativa de la ciudad."),
                              hr()),
                     tabPanel("Hospitales",
                              h3(strong("Distribución de hospitales de la Ciudad.")),
                              helpText("A diferencia de los procesos mencionados con antelación, la descentralización de los servicios de salud logró que 11 establecimientos nacionales -muchos de ellos planeados a escala nacional- fueran transferidos a la Ciudad de Buenos Aires (Stolkiner, 2003). De tal modo, en 2002, el sector estatal de la ciudad contaba con 2,91 camas cada mil habitantes (DGEyC, 2002). Sin embargo, según la investigación realizada por Stolkiner en el año 2003, muchas de las personas que hacían uso de los servicios de salud de la ciudad al momento del estudio provenían del primer y del segundo cordón del conurbano y, en términos porcentuales, más del 45% eran indigentes y más del 25% eran pobres. Es decir, la descentralización propició un traslado de la población más vulnerada a los hospitales de la ciudad, dificultando su acceso y agregando el costo del traslado.",
                                       br(),
                                       br(),
                                       "Siguiendo esta línea, del análisis de los datos de hospitales de la ciudad al año 2019, surge que la mayoría se encuentran situados en la comuna 4 que posee 13 unidades. Si bien esta es una de las que registra mayores valores de NBI, es preciso destacar que las restantes comunas con un gran porcentaje de población bajo esta condición cuentan con una notable cantidad inferior de hospitales: la comuna 3 posee 2 hospitales, la comuna 8 posee uno y la comuna 1 -valor máximo de NBI- no posee ninguno."),
                              highchartOutput(outputId = "G_Hosp"),
                              helpText("El análisis de la distribución de hospitales, reitera los datos señalados: la comuna 4 es la que mayor cantidad de instituciones posee y, aunque la comuna 6 cuenta con 4 hospitales, y es seguida por la comuna 2 con 3 hospitales, es preciso advertir que las restantes comunas no cuentan con más de 2 hospitales."),
                              br(),
                              h4(strong("Mapa de hospitales de la Ciudad, según su especialización.")),
                              leafletOutput(outputId = "M_Hospitales"),
                              helpText("En el  mapa se evidencia la distribución de hospitales públicos de la ciudad, según su tipo: de agudos, de niños y especializados. Se encuentra que entre aquellas comunas que poseen un único hospital, la mayoría cuenta con hospitales de agudos, salvo las comunas 5 y 13 que tienen entre su geografía hospitales especializados.  Respecto a los 3 hospitales de niños, 2 de ellos se localizan en la comuna 8 y el tercero en la comuna 2.",
                                       br(),
                                       br(),
                                       "Llama la atención la heterogeneidad en la distribución y concentración geográfica de las unidades hospitalarias, puesto que ello impacta en las condiciones de acceso a los servicios de salud y las vuelve poco equitativas. Es decir, en tanto la localización de la oferta urbana de servicios se vincula con la posibilidad de ejercer derechos sobre la ciudad, la falta de integración de los servicios de salud en distintas zonas del tejido urbano no deja de constituir un factor de desigualdad para quienes viven y habitan la ciudad.",
                                       br(),
                                       br(),
                                       "Asimismo, en la medida en que la localización del servicio configura su nivel de accesibilidad, la distribución de los hospitales muestra cómo la configuración territorial de la ciudad constituye en sí misma un mecanismo de desigualdad. Nuevamente, el análisis incipiente de esta cuestión, da cuenta de la importancia de indagar en otras variables de pertinencia en futuras investigaciones: población que asiste a los hospitales, capacidad de atención, infraestructura disponible, entre otras."),
                              hr()),
                     tabPanel("Cultura",
                              h3(strong("Distribución de espacios culturales de la Ciudad.")),
                              helpText("Como se dijo en la introducción, este trabajo entiende a la ciudad como una entidad doble: por un lado, la ciudad es una realidad práctico-sensible, un conjunto amplio de objetos dispuestos en un espacio reducido. Por el otro, la ciudad es una forma social de disponer lo diverso, lo divergente. La importancia de relevar la oferta cultural de una ciudad cobra sentido en esta doble definición puesto que la cultura se ubica entre estas dos dimensiones: aquello que llamamos", 
                              em("oferta"), "implica un conjunto de bienes, servicios, establecimientos e instituciones localizados a lo ancho de la ciudad; lo “cultural”, por su parte expresa los rasgos distintivos de la comunidad que la produce.",
                                       br(),
                                       br(),
                                       "En este sentido, la concentración de la oferta en pocos espacios tiende hacia la  homogeneización de la cultura, a la par que dificulta el acceso a los ciudadanos más alejados de esos espacios. Es por esto que el presente apartado releva la distribución geográfica de la oferta cultural. Por último, y teniendo en cuenta que el acceso a un bien urbano cualquiera, no solo tiene condicionantes geográficos sino también monetarios o institucionales, se ofrece una clasificación de estos espacios según su criterio de acceso."),
                              plotlyOutput((outputId= "BarrasCul")),
                              helpText("La distribución de espacios culturales en la ciudad muestra que la mayor concentración se radica, marcadamente, en la comuna 1. Seguidamente se encuentran las comunas 2, 14 y 3, sin embargo, estas poseen una cantidad notoriamente menor de comercios, espacios de exhibición, bibliotecas y espacios de formación.",
                                       br(),
                                       br(),
                                       "En cuanto a las comunas que cuentan con menor presencia de estos espacios, se trata de aquellas que se localizan en la periferia sudoeste de la ciudad: 8, 10, 9 y 11. Especificamente, la comuna 8 es la que posee el valor mínimo, teniendo en cuenta que en el marco de su territorio hay menos de 20 espacios culturales."),
                              br(),
                              h4(strong("Distribución geográfica de los espacios culturales.")),
                              leafletOutput(outputId = "MapaCul"),
                              br(),
                              helpText("Se ve entonces que la distribución cultural presenta una marcada dinámica centro-periferia que deja parcialmente excluidas del acceso a la oferta cultural a las comunas 7, 8, 9, 10, 11 y 12. Por otro lado, en la mayoría de las comunas predomina el tipo de espacio cultural “Comercio” (que engloba ferias, librerías, bares y disquerías). Mientras tanto, escasean los Espacios Públicos (monumentos, calesitas, sitios históricos) -con la notable excepción de la comuna 2-, donde son mayoría. Finalmente, el tipo de espacio más subrepresentado son los Espacios de Formación (que incluyen escuelas técnicas y artísticas, talleres e institutos de arte, universidades públicas, etc.)."),
                              h4(strong("Distribución porcentual de los espacios culturales.")),
                              leafletOutput(outputId = "CoroCul")),
                     tabPanel("Ficha técnica",
                              actionLink("BotonHospis", h4("Sobre los tipos de Hospitales")),
                              hr(),
                              textOutput("TextoHospis"),
                              br(),
                              br(),
                              textOutput("TextoHospis1"),
                              br(),
                              br(),
                              textOutput("TextoHospis2"),
                              br(),
                              br(),
                              textOutput("TextoHospis3"),
                              br()
                              )
                 )),
        tabPanel("Vivienda",
                 navlistPanel(
                     tabPanel("Hacinamiento",
                              h3(strong("Distribución porcentual de hacinamiento por comuna.")),
                              helpText("El análisis precedente muestra claros indicios de la fuerte desigualdad que trajeron consigo las políticas neoliberales implementadas desde la década del 90’ en Argentina. En dicho marco, el mercado como mecanismo de acceso a bienes y servicios se volvió primacía y muchos de los derechos, vinculados al bienestar económico, a la convivencia social y a la vida digna, quedaron excluidos para una gran parte de la sociedad (Svampa, 2005).",
                                       br(),
                                       br(),
                                       "El derecho al uso y disposición del espacio urbano fue uno de los tantos vulnerados, entendiendo por ello no sólo al goce de la propiedad, sino también al ejercicio de las oportunidades sociales y económicas ligadas a la localización de una vivienda o una actividad (Oszlak, 2017).  En este sentido, la vivienda se constituye como el bien urbano fundamental, puesto que la capacidad de fijar residencia en la ciudad (o en sus alrededores cercanos) habilita a los habitantes -al menos, en teoría- a gozar de otro amplio conjunto de bienes (cultura, educación, salud, etc). A su vez, la forma y ubicación de los lugares donde se asientan sus habitantes, también marca a fuego la manera en que estos satisfacen sus necesidades mediante los bienes urbanos.",
                                       br(),
                                       br(), 
                                       "Además, una vivienda digna debe garantizar y satisfacer a sus habitantes un conjunto de necesidades, entre las que se cuentan “el resguardo y la protección ante las inclemencias climáticas, la seguridad en la tenencia, el desarrollo personal y familiar, un espacio adecuado, la accesibilidad, la ubicación y el transporte, la privacidad, la cultura y la identidad, las condiciones de salubridad mínimas y el abastecimiento en forma segura de los servicios públicos esenciales” (Defensoría del Pueblo de la Ciudad Autónoma de Buenos Aires, 2009).",
                                       br(),
                                       highchartOutput(outputId = "G_HNC"),
                                       helpText("En consecuencia, indagar los niveles de hacinamiento en la ciudad resulta por demás relevante para conocer la distribución de diversos bienes y servicios urbanos. El siguiente gráfico muestra que la comuna 8 es la que mayor porcentaje de hacinamiento posee, aunque, considerando el período comprendido en los últimos 10 años, se advierten altos valores en las comunas 4, 1 y 7 (dos de las cuales registraron valores de NBI superiores a la media).",
                                                br(),
                                                "Por otro lado, se observa que las comunas 2, 13, 5, 6 y 12 presentan valores que no superan el 10% de la poblacion en condición de hacinamiento (centro y norte de la ciudad)."),
                                       highchartOutput(outputId = "G_HC"),
                                       helpText("El hacinamiento crítico, por su parte, tiene mayor presencia en la comuna 8 (6.8%), y se distancia de la comuna 1 en 2 puntos porcentuales (4.3%). Cabe contemplar que, en varios de los casos, no se registran valores posteriores al 2016 respecto a este indicador. Sin embargo, es posible advertir que la tendencia a una menor problemática habitacional se sitúa en el norte y centro de la ciudad."),
                                       hr())),
                     tabPanel("Condición de ocupación",
                              h3(strong("Distribución porcentual de viviendas, según condición de ocupación.")),
                              helpText("La identificación de la condición de ocupación permite clasificar a las viviendas en habitadas y deshabitadas, según el uso residencial habitual. La cantidad de viviendas desocupadas en cada comuna, muestra cómo se distribuye la ocupación del espacio, mediante un bien que está mercantilizado y resulta escaso. De tal modo, la jerarquización del espacio, impuesta por los sectores de mayores ingresos durante las últimas décadas, evidencia que ciertos sectores fueron destinados a fines comerciales, de turismo y recreación, dejando a otros en el abandono. En este sentido, la reducción de viviendas disponibles constituye un elemento desplazador de las zonas “centrales” de la ciudad, obligando a quienes poseen pocos recursos a ocupar otros espacios y restringiendo el acceso a dichas zonas a quienes “merecen” habitarlas (Oszlak, 2017).",
                                       br(),
                                       br(),
                                       "El análisis de los datos volcados en el siguiente gráfico muestra que son las comunas 2 y 14 las que mayor porcentaje de viviendas desocupadas poseen. Si bien ninguna cuenta con mas de 2% de hacinamiento, es evidente que los barrios de Palermo y Recoleta concentran los espacios menos destinados a la vivienda."),
                              highchartOutput(outputId = "G_Vivienda"),
                              helpText("Las comunas con mayor porcentaje de ocupación de viviendas, son las mismas que poseen los valores mas altos en lo que al nivel hacinamiento respecta."),
                              h4(strong("Mapa de viviendas de la Ciudad, ocupadas con fines comerciales.")),
                              leafletOutput(outputId = "M_Vivienda"),
                              helpText("La distribución geográfica de los espacios destinados a fines mercantiles, refuerza la perspectiva indagada en los párrafos precedentes. Aquellas zonas que se jerarquizan, no solo lo hacen mediante la restricción en la disponibilidad de viviendas, sino también a través de la concentración de servicios de los que dichos habitantes pueden gozar con mayor facilidad.",
                                       br(),
                                       br(),
                                       "Claramente, la comuna 2 es en la que mayor porcentaje de viviendas se destinan a fines comerciales (distanciandose en 9 puntos porcentuales de la comuna 5, el valor máximo siguiente del indicador)."),
                              hr()),
                     tabPanel("Régimen de Tenencia",
                              h3(strong("Distribución porcentual anual de hogares, según régimen de tenencia.")),
                              helpText("El régimen de tenencia se vincula con el derecho a una seguridad sobre la vivienda, contando así con protección legal ante desalojos forzados, hostigamientos y distintas amenazas. El régimen adquiere tres formas: propietario, inquilino o irregular.",
                                       br(),
                                       br(),
                                       "La tenencia irregular de la vivienda se vincula, directamente, con un carácter precario de acceso a la misma. En términos opuestos, acceder de modo regular conlleva condiciones de seguridad y estabilidad."),
                              selectInput(inputId = "input_AÑO",
                                          choices = Regimen$año,
                                          label = "Seleccione año",
                                          selected = TRUE),
                              selectInput(inputId = "input_Regimen1",
                                          choices = Regimen$Regimen_Tenencia,
                                          label = "Seleccione Régimen",
                                          selected = TRUE),
                              plotOutput(outputId= "BarrasRegimen"),
                              helpText("Nuevamente, se observa que las comunas 8 y 4, respectivamente, son las que mayor porcentaje de tenencia precaria poseen. Si bien estos valores han variado en los últimos diez años,los valores máximos se advierten sostenidamente en dichas unidades geográficas (también acompañadas por la comuna 1)."),
                              br(),
                              h4(strong("Distribución geografica de hogares, según tipo de Régimen de tenencia.")),
                              selectInput(inputId = "input_Regimen2",
                                          choices = Regimen18$Regimen_Tenencia,
                                          label = "Seleccione Régimen",
                                          selected = TRUE),
                              leafletOutput(outputId = "CoroRegimen"),
                              helpText("La distribución geográfica de los tipos de tenencia muestra que el mayor porcentaje de viviendas ocupadas por inquilinos o arrendatarios se registra en la zona norte y centro de la ciudad. Además, cabe destacar el caso de la comuna 1, que posee altos valores de inquilinato y, al mismo tiempo, marcados registros de hacinamiento y de tenencia precaria."),
                              hr()),
                     tabPanel("Precio del metro cuadrado",
                              h3(strong("Valor del Metro Cuadrado en la Ciudad")),
                              helpText("Los procesos de exclusión y desplazamiento urbano, también se relacionan con los valores impuestos en cada zona por el mercado inmobiliario. Es decir, una fuerte valorización del suelo de un determinado espacio lo jerarquiza respecto al resto y, de manera consecuente, lo restringe para aquella población que no es capaz de acceder por sus condiciones socioeconómicas.",
                                       br(),
                                       br(),
                                       "El valor del metro cuadrado, en su distribución geográfica, refuerza el análisis planteado en apartados precedentes: la comuna 2 es la que mayor cantidad de viviendas destinadas a fines mercantiles posee, es la que registra mayores viviendas desocupadas y es la que posee el valor mas alto de metro cuadrado."),
                              leafletOutput(outputId = "P_x_m2"),
                              helpText("La polarización enunciada a lo largo del trabajo se evidencia también en el valor de la tierra de la comuna 8, contando -en el 2010- con un promedio menor a $1000. Además, la marcada distinción que existe entre la zona sur y norte de la ciudad queda expuesta en la distribución de colores a lo largo y ancho del mapa."),
                              hr()),
                     tabPanel("Ficha técnica",
                              textOutput(outputId = "TecnicaViv"),
                              actionLink("BotonHacinamiento",h4(strong("Hacinamiento"))),
                              textOutput("TextoHacinamiento"),
                              br(),
                              textOutput("TextoHacinamiento1"),
                              hr(),
                              actionLink("BotonCondicion", h4(strong("Condición de Ocupacion"))),
                              textOutput("TextoCondicion"),
                              hr())
                     )),
        tabPanel("Transporte",
                 navlistPanel(
                     tabPanel("Colectivos",
                              actionLink("BotonTransporte",(h4("Sobre los servicios de transporte"))),
                              textOutput("TextoTransporte"),
                              br(),
                              textOutput("TextoTransporte1"),
                              br(),
                              textOutput("TextoTransporte2"),
                              hr(),
                              helpText("Los colectivos presentan la particularidad de poder realizar recorridos más flexibles y distancias más cortas. Esto porque no se ajustan precisamente a la descripción ofrecida, sobre los servicios urbanos. Si bien su tarifa es subsidiada, y la construcción de rutas es imprescindible, el Estado no se encuentra directamente involucrado en su producción, que es bastante accesible para medianas empresas locales(Torres, 1991). El relativamente bajo nivel de inversión que requieren, puede asegurar una tasa de ganancia media a estas empresas, por lo que la red de colectivos es, por lejos, la más ramificada de todas. Su característica fundamental para el sistema de transporte urbano es que permiten vincular a las áreas más alejadas de los núcleos urbanos, con verdaderos servicios urbanos de mediana o larga distancia como los trenes o ferrocarriles."),
                              h4(strong("Distribución de redes de colectivos en la ciudad.")),
                              leafletOutput(outputId = "Recorrido_Bondis"),
                              br(),
                              helpText("Lo primero que se observa es que el grueso de la red de colectivos, conecta a todo el conurbano bonaerense con la capital. La conexión se efectúa a través de diversos puntos, pero casi siempre a través de vías principales, como pueden ser la Autopista Ricchieri, el Acceso Norte o la Avenida Rivadavia. Una vez que un colectivo ingresa a la ciudad, suele continuar en la misma dirección por la que proviene, que normalmente es hacia la Comuna 1. Efectivamente, se aprecia que el grueso de las líneas de colectivos comienzan a confluir en las Comunas 6 o 5, para terminar de concentrarse en la Comuna 3 y continuar hacia la Comuna 1 donde se ubica el microcentro porteño."),
                              hr(),
                              br(h4(strong("Distribución porcentual de paradas, ponderada por area de las comunas."))),
                              leafletOutput(outputId= "Coro_Bondis"),
                              br(),
                              helpText("El mapa de Distribuciónes ajustado por el área de las comunas, refuerza lo señalado en el apartado anterior.
Las vías de acceso a la ciudad son múltiples, aunque se concentran en ciertas arterias principales que aquí pueden ubicarse con claridad en las comunas 12, 10 y 4 (aún más, teniendo en cuenta su gran tamaño). Luego, estas vías se concentran en las comunas centrales (6, 5, 3 , 2 y 1). Por último, se destaca la relativa ausencia de paradas en la Comuna 8."),
                              br(h4(strong("Cantidad de paradas por comuna."))),
                              plotOutput(outputId = "Distr_Bondis")),
                     
                     tabPanel("Subterraneo/Metro",
                              h3(strong("Subterraneos y Premetro")),
                              hr(),
                              helpText("Este servicio se ajusta con precisión a la conceptualización introducida al inicio de la pestaña. Los subterráneos implican una inversión gigantesca de capital y un larguísimo ciclo de circulación. Es por esta razón que, aunque luego puedan intervenir empresas contratistas, ha sido siempre el Estado el principal promotor en su desarrollo. Efectivamente todo lo concerniente a las extensiones de los recorridos, como al desarrollo de nuevas líneas tiene una normativa legal detrás. Actualmente, de acuerdo con la Ley 670 del año 2001 el GCBA debe hacer llevar adelante la construcción de al menos 22 km de subte conformados por 3 líneas nuevas. Otros proyectos tratados disponen la extensión de la línea H hasta Retiro, o la plena accesibilidad de todas las estaciones del servicio(Ministerio de Desarrollo Urbano: 2013)"),
                              br(),
                              h4("Distribución de redes de subtes en la ciudad."),
                              leafletOutput(outputId = "Recorrido_Subte_Prem"),
                              br(),
                              helpText("Los recorridos de los subtes muestran nuevamente una dirección centro-periferia, cortada por las líneas C y H. Esta tendencia vuelve a concentrar el transporte en las Comunas 1 y 3. En la vereda opuesta se encuentran las Comunas 9, 10 y 11 (que suman un total de 13 barrios) que no poseen ninguna estación de subterráneo. Luego se encuentra la comuna 4, donde la línea H apenas llega, aunque su proximidad al centro de la ciudad alivia esta situación. Por otro lado, la Comuna 8, cuenta exclusivamente con servicio de premetro, que aunque luego empalma en Virreyes con la Línea E, presenta ciertas particularidades que lo distinguen negativamente del servicio de subte(www.enelsubte.com, 2021)."),
                              br("Por último con respecto a la accesibilidad de sus estaciones, vemos que la línea E posee solamente 3 estaciones con ascensor, todas ellas en la cabecera del centro. Luego la línea C posee ascensores solo en las estaciones terminales. En un lugar intermedio se ubican las líneas B y D que poseen ascensores en varias estaciones del extremo del recorrido, pero ninguna en las estaciones del medio. Y por último las estaciones A y H presentan una cobertura más o menos absoluta de ascensores."),
                              br(h3(strong("Distribución porcentual de estaciones, ponderada por area de las comunas."))),
                              leafletOutput(outputId = "Coro_Subte"),
                              br(h4(strong("Cantidad de estaciones por comuna."))),
                              plotOutput(outputId = "Distr_Subte")),
                     
                     tabPanel("Tren/Ferrocarril",
                              h3(strong("Ferrocarril")),
                              helpText("El servicio de trenes es fundamental en cualquier ciudad moderna, puesto que está en condiciones de conectar la periferia lejana de una ciudad con su centro cívico y económico. Actualmente en la Ciudad de Buenos Aires cuenta con 7 líneas de tren que conectan partidos altamente poblados como Tigre, La Matanza, Lomas de Zamora o Quilmes con el centro de la capital en Retiro, Constitución , pero también con algunas zonas mas retiradas como Once, Chacarita o Nueva Pompeya.  En el año 2020 el CEM, en su Informe sobre el “Mercado Laboral Metropolitano”, relevo que cerca del 50% de los puestos de trabajo de la Ciudad de Buenos Aires son ocupados por residentes del Conurbano, mientras que casi el 10% de la población de la capital trabaja en el Gran Buenos Aires. Esto debe llevarnos hacia la pregunta por la utilidad de la división institucional entre la capital y su conurbano, puesto que en lo económico parecen funcionar como un sistema único."),
                              h4(strong("Distribución de redes de trenes en la ciudad.")),
                              leafletOutput(outputId = "Recorrido_Trenes"),
                              hr(),
                              helpText("Lo primero que debemos advertir al usuario es que al mapa que presentamos le falta por completo la Línea Urquiza, esto por una ausencia injustificada, probablemente un error, en las bases de datos del Gobierno de la Ciudad. Sobre este mapa cabe decir que el servicio que cubre la zona norte del AMBA, se diversifica ya dentro de la ciudad, por lo que las comunas 12, 13 y 14 resultan especialmente beneficiadas en cuanto a la diversidad de este servicio. En cambio, el servicio que cubre las zonas sur y oeste del área, no diversifica sus recorridos sino fuera de la capital. Por otro lado, llama la atención ver que es un servicio que recorre todas las comunas de la capital, aunque queda pendiente. ver si la presencia total de la red, se corresponde con la existencia de al menos una estación en cada comuna. Finalmente, cabe destacar que tanto la línea Urquiza, como la Sarmiento son las únicas que finalizan sus recorridos mas bien lejos de la comuna 1, derivando a sus pasajeros hacia las líneas B y A de subte respectivamente, así como a los colectivos. Esta última situación vuelve a las comunas 15 (especialmente al barrio de Chacarita) y 3 (especialmente en Once) en zonas muy densamente pobladas y comerciales, con los transportes usualmente colapsados durante los días de semana.  Algo similar aunque en menor medida, sucede en el barrio de Nueva Pompeya de la Comuna 4 a propósito de la línea Belgrano Sur. "),
                              br(),
                              helpText("Al observar la distribución de las estaciones dentro de la Ciudad de Buenos Aires, vemos que hay solo las comunas 2 y 5 (tener en cuenta que la ausente linea Urquiza cuenta con 4 estaciones en la Comuna 15, y una mas en la 11) sin ninguna estación, que son precisamente comunas céntricas, de las que antes destacamos su concentración de estaciones de subte y paradas de colectivos."),
                              hr(h4(strong("Distribución porcentual de estaciones, ponderada por area de las comunas."))),
                              leafletOutput(outputId= "Coro_Trenes"),
                              br(),
                              br(h4(strong("Cantidad de estaciones por comuna."))),
                              plotOutput(outputId = "Distr_Trenes")),
                     tabPanel("Bicicletas",
                              h3(strong("Ciclovias y Ecobicis")),
                              helpText("El servicio de bicicletas es el servicio urbano de transporte de origen más reciente en la Ciudad Autónoma de Buenos Aires. Su implementación responde a las demandas de grupos ciclistas de la ciudad, pero fundamentalmente al mas o menos reciente modelo ciudades verdes o sustentables, que tienen entre sus objetivos reducir las emisiones de carbono a la vez que fomentar la actividad física entre sus habitantes. En este sentido, es un objetivo inclaudicable de cualquier sistema de bicicletas que surja de estos principios, incentivar su uso por parte de los habitantes, en detrimento de colectivos o automóviles. 
En la ciudad de Buenos Aires, este servicio cuenta con dos patas. Por un lado el servicio de Ciclovías, carriles específicos para bicicletas, que data del año 1997 cuando se construyeron los primeros 8 km que unían los barrios de Palermo y Belgrano. A la fecha, la red comprende 250 km de extensión por buena parte de la capital. La segunda arista del servicio de bicicletas públicas compartidas llamado “Ecobici”. El servicio funciona de manera similar a otros sistemas análogos en varias ciudades del mundo, con la notable salvedad, de que el servicio es gratuito para todos los habitantes. En efecto, el servicio requiere que el interesado registre sus datos en una aplicación o página web, vía tarjeta de crédito o presencialmente en las oficinas de Tembici ubicadas en el barrio de Chacarita."),
                              br(),
                              helpText("Ahora realizaremos una breve cronología con sus hitos mas importantes:"),
                              br(),
                              helpText("2017: lanzamiento. Hasta inicios de 2019 fue implementado exclusivamente por el GCBA."),
                              br(),
                              helpText("01/2019: el servicio fue brindado a licitación a la empresa Tembici, y subsidiado a su vez, por el gobierno para mantener su gratuidad. La licitación tenía el objetivo de mejorar la calidad y del servicio llegar a tener 400 estaciones operacionales y 4000 bicicletas"),
                              br(),
                              helpText("08/2019: la demanda del servicio aumenta un 570%(A24) desde enero."),
                              br(),
                              helpText("10/2019: pico histórico del promedio mensual de oferta de bicicletas libres y disponibles: 1352."),
                              br(),
                              helpText("01/2020: se desamantelan unas 20 estaciones, todas ellas ubicadas en comunas del sur/sudoeste de la ciudad."),
                              br(),
                              helpText("02/2020: el promedio mensual de oferta de bicicletas llega a los valores mínimos desde 2017, ubicandose en 230."),
                              br(),
                              helpText("05/2020: el sistema cambia su modalidad. Ahora sólo seran gratuitos los viajes para residentes de la ciudad, de hasta 30 minutos. Antes podían serlo hasta 1 hora."),
                              br(),
                              helpText("03/2012: de ahora en más el servicio solo sera gratuito hasta 4 viajes semanales de 30 minutos. Superado ese límite se cobra una penalidad."),
                              hr(),
                              h4(strong("Distribución de ciclovias en la ciudad.")),
                              leafletOutput(outputId = "Recorrido_Bicicletas"),
                              br(),
                              br(h4(strong("Distribución porcentual de ciclovias, ponderada por area de las comunas."))),
                              leafletOutput(outputId= "Coro_CicloV"),
                              helpText("Como con el resto de los servicios, su prestación se concentra en las comunas 6, 5, 3, y 1, pero también se destacan con valores similares las comunas del norte (12, 13, 14 y 15). 
En la vereda opuesta se encuentran la comunas 8 -la menos provista por este servicio- seguida de cerca por las comunas 9, 10 y 11 que apenas si cuentan con ciclovías. Una explicación a esta concentración tan marcada, podría venir del hecho que las comunas que mas ciclovias tienen, son aquellas donde la población cuenta con mas bicicletas personales. De esta manera se esperaría que el servicio de Ecobicis provea especialmente a las comunas cuya población posee menos bicicletas. De acuerdo con esta hipótesis, estas serían las comunas 8, 9, 10 y 11. Veamos qué es lo que sucede con la distribución de Ecobicis.
"),
                              br(),
                              br(h4(strong("Distribución porcentual de Ecobicis, ponderada por area de las comunas."))),
                              leafletOutput(outputId="Coro_EcoB"),
                              helpText("Si bien puede que sea cierto que, las ciclovías se concentren allí donde la población cuenta con más bicicletes privadas, esto solo nos llevaría a concluir que el servicio de bicicletas públicas prestado por el GCBA y Tembici es fuertemente regresivo, puesto que ofrecería bicicletas públicas, allí donde su población ha podido satisfacer su demanda por vía del consumo privado. Por otro lado, si sucede que las comunas donde hay mas ciclovías no poseen una proporción mayor de bicicletas por habitante, entonces no parece haber una clara explicación de la sobreabundancia de ciclovías en esas zonas. Esta, creemos es una línea de investigación interesante para desarrollar, a partir de un relevamiento de la cantidad de bicicletas privadas por habitante, con la finalidad de entrever cuál es el criterio de distribución del servicio de bicicletas: ¿responde este a las necesidades de cada comuna? o ¿en cambio reproduce y refuerza las desigualdades de la estructura socio-económica?"),
                              br(h4(strong("Ecobicis (gris) y Ciclovias (naranaja) por comuna."))),
                              plotOutput(outputId = "Distr_Bicicletas")))),
        tabPanel("Cierre",
                 navlistPanel(
                     tabPanel("Conclusiones y disparadores",
                              h3(strong("Conclusiones Generales")),
                              helpText("A lo largo del trabajo, se indagaron indicadores de la Ciudad Autónoma de Buenos Aires que no sólo propusieron una aproximación a la distribución de bienes y servicios, sino también a las condiciones socioeconómicas y a las características sociodemográficas de sus habitantes. "),
                              br(),
                              helpText("El análisis partió del proceso de mercantilización y desregulación que implicaron las políticas neoliberales, detectando su fuerte impacto en la estructura socio-espacial de la ciudad. En este sentido, cabe mencionar que, en la última década, la Ciudad registró impactantes índices de pobreza e indigencia, siendo el año 2016 el que mayores valores tuvo. Asimismo, se contempló la incidencia de dichos cambios estructurales en los servicios educativos y de salud, mostrando que aquellas comunas que poseen peores condiciones socioeconómicas y habitacionales no son, en todos los casos, favorecidas con el acceso a este tipo de servicios (la comuna 1 no posee hospitales, y la comuna 8 tiene escasa cantidad de escuelas, en relación al número de población en edad escolar). 
"),
                              br(),
                              helpText("Teniendo en cuenta el proceso de jerarquización de espacios que devino en los últimos años; y la idea de que una concentración y delimitación de la oferta cultural dificulta y restringe su acceso a sectores de la población alejados de dichas coordenadas, se indagó en la distribución de espacios culturales en la ciudad. Del análisis de los datos, surge que es la comuna 1 la que mayor concentración de oferta cultural posee.)",
                              br(),
                              helpText("Esta observación no se explica solamente por la heterogeneidad propia de los habitantes de esa comuna(que combina barrios como el Padre Mujica con Puerto Madero), sino también a la luz del relevamiento de los servicios de transporte. En todos los casos, este muestra una fuerte tendencia de la Ciudad de Buenos Aires hacia la macrocefalia, cualidad que adquiere una urbanización que concentra gran parte de sus actividades en un único núcleo urbano. De esta manera se delimitan en la ciudad, zonas para habitar, y zonas para trabajar y consumir."),
                              br(),
                              helpText("En este sentido, la situación de las comuna 1 y 4 es paradójica: con valores altísimos de NBI y de ‘Tenencia precaria’, su población su ubica rodeada de servicios urbanos y de otras externalidades resultantes del proceso de centralización(como oferta cultural, o trabajo) de las que no puede gozar plenamente por no poder cumplir con los criterios (legales o mercantiles) delimitados para su acceso. Por ejemplo, poseer un domicilio legal, o tener capacidad de consumo."),
                              br(),
                              helpText("Finalmente, ante la relativa ausencia de prestaciones públicas, las comunas más alejadas del núcleo urbano, se juegan la calidad del acceso a la ciudad y a sus servicios, en su capacidad de suplir estas carencias a través del mercado. Estrategia que se vuelve especialmente difícil para la población de las comunas de la zona sur, como la 7 o la 8 que superan la media de NBI y de Tenencia Precaria.")),
                     tabPanel("Bibliografia y fuentes",
                              h3(strong("Bibliografia")),
                              hr(),
                              helpText("-Arakaki, A. (2011).  La pobreza en Argentina 1974-2006. Construcción y análisis de la información. Buenos Aires: CLACSO."),
                              helpText("-Carta Mundial por el Derecho a la Ciudad."),
                              helpText("-Defensoría del Pueblo (2015). La situación habitacional en la Ciudad Autónoma de Buenos Aires. Disponible en: https://www.defensoria.org.ar/wp-content/uploads/2015/09/SituacionHabitiacional-1.pdf"),
                              helpText("-Judzik, D., Moschetti, M. (2016). ¿Una segunda fase de privatización de la matrícula escolar?… RASE. Vol. 9. Nº2: Págs. 197-211."),
                              helpText("-Lefebvre, H. (1978). El derecho a la ciudad. Barcelona: Península."),
                              helpText("-Oszlak, O. (2017). Merecer la ciudad. Buenos Aires: EDUNTREF."),
                              helpText("-Pírez, P. (2009). Los servicios urbanos y el funcionamiento de la ciudad. EUDEBA, Ciudad Autónoma de Buenos Aires"),
                              helpText("-Pírez, P. (2016). Las heterogeneidades en la producción de la urbanización y los servicios urbanos en América Latina. Universidad de Bogota, Colombia"),
                              helpText("-Stolkiner, A. (2003). DESCENTRALIZACIÓN Y EQUIDAD EN SALUD: Estudio sobre la utilización de servicios de salud de la Ciudad de Buenos Aires por parte de usuarios pobres del conurbano bonaerense. Buenos Aires: CEDES."),
                              helpText("-Svampa, M. (2005). La sociedad excluyente. La Argentina bajo el signo del neoliberalismo. Buenos Aires: Taurus."),
                              helpText("-Topalov, C. (2006). La urbanizacion capitalista. Catedra de Sociologia Urbana, FSOC (UBA)"),
                              helpText("Torres, H. (1999). El mapa social de Buenos Aires(1940-1990). Serie Difusion N°3, FADU (UBA)"),
                              helpText(""),
                              hr(),
                              h3(strong("Fuentes")),
                              helpText("https://data.buenosaires.gob.ar/"),
                              helpText("-INDEC (2007). Incidencia de la pobreza y de la indigencia en 31 aglomerados urbanos. http://www.estadistica.ec.gba.gov.ar/dpe/images/SOCIEDAD/pob_tot_2sem06.pdf"),
                              helpText("https://www.estadisticaciudad.gob.ar/eyc/publicaciones/anuario_2003/Cap06/dyc6.htm"),
                              helpText("https://www.buenosaires.gob.ar/hospitales-generales-de-ninos"),
                              helpText("https://www.defensoria.org.ar/wp-content/uploads/2015/09/SituacionHabitiacional-1.pdf"),
                              helpText("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2015/04/ir_2014_696.pdf"),
                              helpText("https://www.a24.com/actualidad/ecobici-promedio-pierden-4-bicis-dia-02172020_r1QGhnb7L"),
                              helpText("https://www.enelsubte.com/noticias/berlin-la-ciudad-que-aposto-al-premetro/"),
                              helpText("https://www.buenosaires.gob.ar/sites/gcaba/files/analisis_urbano__lineas_proyectadas.pdf"),
                              helpText(""),
                              hr(),
                              br()
      
                     )))),
        tabPanel("Créditos",
                 mainPanel(
                     br(),
                     h3(strong("Autores")),
                     helpText(strong("Lic. Melina Shamberger")),
                     helpText("Licenciada en Sociología (UBA)"),
                     helpText(tags$a("LinkedIn",href="https://www.linkedin.com/in/melina-schamberger/")),
                     hr(),
                     helpText(strong("Ignacio Gomar")),
                     helpText("Estudiante de Sociologia (UBA)"),
                     helpText(tags$a("LinkedIn", href="www.linkedin.com/in/ignacio-gomar")),
                     hr(),
                     helpText("Este trabajo fue presentado como entrega final correspondiente al curso de 'Social Data Analitics' de EANT"),
                     hr(),
                     helpText(tags$a("Codigo y bases de datos", href="https://github.com/Trabajo-Final-EANT"))
                 ))
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
        Regimen%>%filter(año %in% input$input_AÑO)%>%
            filter(Regimen_Tenencia %in% input$input_Regimen1)
    })
    Reg_filt2<-reactive({
        Regimen18%>%filter(Regimen_Tenencia %in% input$input_Regimen2)
    })
    
    
    #############################INTRODUCCION##############################                

    Objetivos1<-eventReactive(input$BotonObjetivos,{
        "El presente trabajo tiene como objetivo indagar, mediante una primera aproximación, la distribución de diversos bienes y servicios urbanos a lo largo y a lo ancho de la Ciudad Autónoma de Buenos Aires. El lapso de tiempo comprendido en el análisis varía en función de la disponibilidad de datos, centrándose mayormente en el periodo 2010-2020. El interés por la cuestión proviene de una tradición que define el acceso a la ciudad como resultante de la forma en la que las redes de infraestructuras y servicios se disponen a través del territorio y del espacio social. En este sentido, el criterio de acceso a la ciudad se configura en un tiempo y espacio determinados -vale decir en una ciudad determinada-, y está indicado tanto por factores territoriales -de ubicación-, como institucionales y económicos (Oszlak: 2017). Dicho de otra manera, una correcta descripción del problema no solo debe preguntarse dónde se concentran estos bienes, sino también -y especialmente- por aquellos criterios legales y/o mercantiles que han sido definidos para su usufructo."
    })
    Objetivos2<-eventReactive(input$BotonObjetivos,{
        "De este modo, con el fin de alcanzar los mencionados propósitos, el trabajo parte de explorar y representar gráficamente la distribución de algunas dimensiones de la materialidad urbana que adquieren relevancia a la luz de los aportes de los autores considerados. Además, mediante el relevamiento de algunos indicadores clave, se caracteriza la estructura sociodemográfica de la población a efectos de saber si la estructura espacial de la ciudad es un correlato directo de su estructura social, o si, en cambio, esta relación requiere de una complejización adicional."
    })
    Objetivos3<-eventReactive(input$BotonObjetivos,{
      "Si bien se contemplan los límites propios de los datos disponibles (fundamentalmente en lo que respecta a su dimensión temporal y sus variables de análisis), y aquellos vinculados a la extensión y propuesta de esta instancia del Curso, el presente trabajo sugiere un punto de partida para avanzar en el análisis de lo urbano en la ciudad, considerando no sólo la cuestión habitacional, sino también la distribución de bienes y servicios que resulta constitutiva de la calidad de vida de quienes habitan el mencionado territorio. Desde este lugar, ofrece algunos cuestionamientos e interrogantes que buscan expandir los objetivos del proyecto. Por último, tenemos el deseo de que este trabajo sirva para motivar futuras y diversas investigaciones que utilicen de insumo el análisis y relevamiento de datos aquí expuesto."
    })
    output$TextoObjetivos1<- renderText({
        Objetivos1()
    })
    output$TextoObjetivos2<- renderText({
        Objetivos2()
    })
    output$TextoObjetivos3<- renderText({
        Objetivos3()
    })
    
    Unidades1<-eventReactive(input$BotonUdA,{
        "A los fines de precisar el análisis, cabe aclarar que, por la ley orgánica nº 1777 de 2005, la Ciudad Autónoma de Buenos Aires se divide territorialmente y administrativamente en 15 comunas. Entre otras atribuciones, estas unidades elaboran sus propios planes de acción para el mantenimiento de los espacios verdes y de las vías de tránsito secundarias; desarrollan acciones para la administración de su patrimonio y ejecución de su presupuesto anual; evalúan las demandas y necesidades sociales; gestionan las políticas comunitarias; implementan un sistema multidisciplinario de mediación para la resolución de conflictos; convocan a audiencia pública para debatir asuntos de interés general y llaman a consultas populares no vinculantes. Cabe considerar que no pueden crear impuestos, tasas, contribuciones, ni tomar créditos; sumado a que su accionar no puede contradecir el interés general de la ciudad. "
    })
    Unidades2<-eventReactive(input$BotonUdA,{
        "Las comunas están integradas por distintos barrios, a saber: "
    })
    Comuna1<-eventReactive(input$BotonUdA,{
        "-Comuna 1: Retiro, San Nicolas, Puerto Madero, San Telmo, Monserrat y Constitución."
    })
    Comuna2<-eventReactive(input$BotonUdA,{
        "-Comuna 2: Recoleta."
    })
    Comuna3<-eventReactive(input$BotonUdA,{
        "-Comuna 3: San Cristóbal y Balvanera."
    })
    Comuna4<-eventReactive(input$BotonUdA,{
        "-Comuna 4: Boca, Barracas, Parque Patricios y Nueva Pompeya." 
    })
    Comuna5<-eventReactive(input$BotonUdA,{
        "-Comuna 5: Almagro y Boedo."
    })
    Comuna6<-eventReactive(input$BotonUdA,{
        "-Comuna 6: Caballito."
    })
    Comuna7<-eventReactive(input$BotonUdA,{
        "-Comuna 7: Flores y Parque Chacabuco."
    })
    Comuna8<-eventReactive(input$BotonUdA,{
        "-Comuna 8: Villa Soldati, Villa Riachuelo y Villa Lugano."
    })
    Comuna9<-eventReactive(input$BotonUdA,{
        "-Comuna 9: Parque Avellaneda, Liniers y Mataderos."
    })
    Comuna10<-eventReactive(input$BotonUdA,{
        "-Comuna 10: Villa Real, Monte Castro, Versalles, Floresta, Vélez Sarsfield y Villa Luro."
    })
    Comuna11<-eventReactive(input$BotonUdA,{
        "-Comuna 11: Villa Gral. Mitre, Villa Devoto, Villa del Parque y Villa Santa Rita."  
    })
    Comuna12<-eventReactive(input$BotonUdA,{
        "-Comuna 12: Coghlan, Saavedra, Villa Urquiza y Villa Pueyrredón."
    })
    Comuna13<-eventReactive(input$BotonUdA,{
        "-Comuna 13: Belgrano, Nuñez y Colegiales."
    })
    Comuna14<-eventReactive(input$BotonUdA,{
        "-Comuna 14: Palermo."
    })
    Comuna15<-eventReactive(input$BotonUdA,{
        "-Comuna 15: Chacarita, Villa Crespo, Paternal, Villa Ortuzar, Agronomía y Parque Chas."
    })
    output$TextoUdA1<-renderText({
        Unidades1()
        })
    output$TextoUdA2<-renderText({
        Unidades2()
    })
    output$TComuna1<-renderText({
        Comuna1()
    })
    output$TComuna2<-renderText({
        Comuna2()
    })
    output$TComuna3<-renderText({
        Comuna3()
    })
    output$TComuna4<-renderText({
        Comuna4()
    })
    output$TComuna5<-renderText({
        Comuna5()
    })
    output$TComuna6<-renderText({
        Comuna6()
    })
    output$TComuna7<-renderText({
        Comuna7()
    })
    output$TComuna8<-renderText({
        Comuna8()
    })
    output$TComuna9<-renderText({
        Comuna9()
    })
    output$TComuna10<-renderText({
        Comuna10()
    })
    output$TComuna11<-renderText({
        Comuna11()
    })
    output$TComuna12<-renderText({
        Comuna12()
    })
    output$TComuna13<-renderText({
        Comuna13()
    })
    output$TComuna14<-renderText({
        Comuna14()
    })
    output$TComuna15<-renderText({
        Comuna15()
    })
    ##################################POBLACION##################################
    output$G_demo <- renderHighchart({
        G_Pob_Edad=  hchart(Poblacion_Edad, "line",
                            hcaes(x = Año, y= Poblacion,
                                  group = Edad)) %>%
            hc_title(text = "Cantidad de Población por grupo etario(1960-2010)")%>%
            hc_subtitle(text = "Ciudad Autónoma de Buenos Aires (1960-2010)")%>%
            hc_yAxis(title = list(text = "Poblacion"),
                     labels = list(format = "{value}")) %>%
            hc_credits(enabled = TRUE, text = "Fuente: Instituto Nacional de Estadísticas y Censos", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
    })
    
    output$G_Pir <- renderPlotly({
        Pir <- ggplot(pir_filt(), mapping=aes(x= grupo_edad, y= Poblacion, fill=sexo))+
            geom_col(alpha=.7)+
            labs(title="",
                 x="",
                 y="Cantidad de Población",
                 caption="Fuente: Instituto Nacional de Estadísticas y Censos")+
            scale_fill_manual(values=c("#561759","#099CDB"))+
            theme(legend.position = "right",
                  strip.text = element_text(size = 14, face = "bold"))+
            coord_flip()+
            theme_half_open()
        Pir
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
            geom_text(aes(label = NBI, colour="#c603fc"),vjust = 2, size = 3.5)+
            theme_bw()+
            labs(title="",
                 x="Comunas",
                 caption = "Fuente: Censo Nacional de Población, hogares y viviendas (INDEC) 2010.")+
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.05)))+
            theme_minimal_grid(
                font_size = 12,
                color = "darkorchid1"
            )
        BarrasNBI
    })
    TxtPobr<-eventReactive(input$BotonPobr,{
        "La linea de pobreza es el valor monetario de una Canasta Basica Total de bienes y servicios capaz de satisfacer un conjunto de necesidades alimentarias y no alimentarias consideradas esenciales. 
                            Se denomina pobres a los hogares cuyos ingresos no alcanzan dicha linea o valor, y a la poblacion incluida en ellos."
    })
    output$TextoPobr<-renderText({
        TxtPobr()
        })
    
    TxtIndg<-eventReactive(input$BotonInd,{
        "Por su parte, la linea de indigencia es el valor monetario de una Canasta Basica de Alimentos, de costo minimo, capaz de satisfacer un umbral elemental de necesidades energeticas y proteicas. 
                            Se considera indigentes a los hogares cuyos ingresos no alcanzan dicha linea o valor, y a la poblacion incluida en ellos."
    })
    output$TextoInd<-renderText({
        TxtIndg()
    })
    
    TxtNBI<-eventReactive(input$BotonNBI,{
        "Los hogares y la población que poseen Necesidades Básicas Insatisfechas presentan, al menos, uno de los siguiente indicadores de privacion:"
    })
    output$TextoNBI<-renderText({
        TxtNBI()
    })
    TxtNBI1<-eventReactive(input$BotonNBI,{
        "Hacinamiento critico"
    })
    output$TextoNBI1<-renderText({
        TxtNBI1()
    })
    TxtNBI2<-eventReactive(input$BotonNBI,{
        "Vivienda, siendo hogares que tienen lugar en una vivienda de tipo inconveniente (pieza de inquilinato, vivienda precaria u otro tipo)"
    })
    output$TextoNBI2<-renderText({
        TxtNBI2()
    })
    TxtNBI3<-eventReactive(input$BotonNBI,{
        "Condiciones sanitarias (hogares que no tienen ningun tipo de retrete)"
    })
    output$TextoNBI3<-renderText({
        TxtNBI3()
    })
    TxtNBI4<-eventReactive(input$BotonNBI,{
        "Asistencia escolar (hogares con menores en edad escolar (6-12) que no asisten a la escuela)"
    })
    output$TextoNBI4<-renderText({
        TxtNBI4()
    })
    TxtNBI5<-eventReactive(input$BotonNBI,{
        "Capacidad de subsistencia (hogares que tienen 4 o mas personas por miembro ocupado y ademas, cuyo jefe no haya completado tercer grado de escolaridad primaria.)"
    })
    output$TextoNBI5<-renderText({
        TxtNBI5()
    })
    
    #######################################DESARROLLO HUMANO###########################################
    #Escuelas
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
                      title = "Población en edad escolar por escuela",
                      position = "bottomleft")
        CoroEscuelas
    })     
    
    #Hospitales        
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
            addAwesomeMarkers(~long, ~lat, icon = icons, label = ~as.character(TIPO2)) %>%
            addPolylines(data = Comunas, color="#2F4AFF", opacity = 1, weight = 2)
    })
    
    #Cultura       
    output$BarrasCul<-renderPlotly({
        BarrasCul <- ggplot(Cul_x_C)+
            geom_col(aes(x=reorder(Comuna,Cantidad), y=Cantidad, fill=Tipo), width = 0.7)+
            scale_fill_manual(values=c("#c23c3c","#e08d07", "#c7fa39", "#02d606", "#00dfe3", "#752957"))+
            guides(fill=FALSE)+
            labs(title="Distribución de Espacios Culturales según su tipo",
                 subtitle = "CABA.2021",
                 x="Comuna",
                 y= "Cantidad",
                 caption=
                     "Fuente= https://data.buenosaires.gob.ar/dataset/espacios-culturales")+
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.05)))+
            theme_minimal_vgrid()
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
    TxtHospis<-eventReactive(input$BotonHospis,{
        "Los hospitales de la Ciudad Autónoma de Buenos Aires comprenden tres tipos de hospitales: de agudos, de niños y especializados."   
    })
    TxtHospis1<-eventReactive(input$BotonHospis,{
        "Los 'hospitales de agudos' brindan asistencia a la salud en clínica médica, pediatría, traumatología, 
                              cardiología, dermatología, ginecología, obstetricía, cirugía y otras especialidades. Además realizan estudios complementarios (radiología, mamografías, tomografía, laboratorio, ecografías y otros estudios de diagnóstico y prevención de enfermedades).
"
    })
    TxtHospis2<-eventReactive(input$BotonHospis,{
        "Por su parte, los 'hospitales especializados' monovalentes brindan asistencia a la salud en diferentes especialidades: gastroenterologia; 
                              infectologia; maternidad; odontologia; oncologia; oftalmologia; quemados; rehabilitacion psicofísica; rehabilitacion respiratoria; salud mental; y zoonosis."
    })
    TxtHospis3<-eventReactive(input$BotonHospis,{
    "Finalmente, los 'hospitales de niños' cuentan con atencion ambulatoria, internacion, diagnostico y tratamiento; 
                              asi como tambien, diferentes especialidades medicas pediatricas. "
        })
    output$TextoHospis<-renderText({
        TxtHospis()
    })
    output$TextoHospis1<-renderText({
        TxtHospis1()
    })                         
    output$TextoHospis2<-renderText({
        TxtHospis2()
    })
    output$TextoHospis3<-renderText({
        TxtHospis3()
    })
    
    
    
    #############VIVIENDA###################################################
    #Hacinamiento      
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
            hc_yAxis(title = list(text = "Hacinamiento critico (en %)"),
                     labels = list(format = "{value}%")) %>%
            hc_credits(enabled = TRUE, text = "Fuente EAH (DGEyC-GCBA)", style = list(fontSize = "12px"))%>%
            hc_add_theme(hc_theme_ffx())
    })
    
    #condicion de ocupacion
    
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
    
    #Regimen       
    output$BarrasRegimen<-renderPlot({
        RegimenA<-ggplot(data=Reg_filt(), 
                         aes(x=comunas,
                             y=porcentaje,
                             fill= Regimen_Tenencia))+
            scale_fill_manual(values = 
                                 c("#e3eb00"))+
            geom_col(width = .6)+
            geom_text(aes(label = porcentaje), 
                      vjust = 2.1, size = 3.5)+
            labs(title = "",
                 subtitle = "CABA")+
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.1)))+
            theme_minimal_grid()
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
                      title = "% sobre total de 
                      hogares de la Comuna",
                      labFormat = labelFormat(suffix="%"),
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
    #Metro cuadrado        
    
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
                      labFormat = labelFormat(suffix="US$"),
                      position = "bottomleft")
        
        CoroPreciom2
    })
    TxtHacinamiento<-eventReactive(input$BotonHacinamiento,{
    "La situación de 'hacinamiento expresa la importancia relativa de los 
                            hogares, o de la población en ellos, en los que hay dos o mas personas por cuarto en la vivienda (hacinados)."
    })
    TxtHacinamiento1<-eventReactive(input$BotonHacinamiento,{
    "Por su parte, los hogares con 'hacinamiento crítico expresan la importancia relativa de 
                            los hogares en los que hay más de tres personas por cuarto de la vivienda."
    })
    TxtCondicion<-eventReactive(input$BotonCondicion,{"La condición de ocupación de las viviendas se releva en los Censos nacionales. 
                            Las viviendas habitadas incluyen aquellas donde, al menos alguno de sus moradores, estaba presente durante el operativo censal así como, 
                            aquellos casos donde residía un hogar pero cuyos miembros estaban ausentes ese día (temporalmente ausentes).  
                            Las deshabitadas están integradas por unidades habitacionales que en la fecha del operativo estaban desocupadas por estar en construcción, 
                            en alquiler o venta, por tratarse de viviendas construidas originalmente para ser habitadas pero que en 
                            ese momento tenían un uso no residencial (comercios, oficinas, consultorios, etc.), así como aquellas 
                            que son utilizadas en forma ocasional o temporal como período de vacaciones, fines de semana, etc."
    })
    output$TextoHacinamiento<-renderText({
        TxtHacinamiento()
    })
    output$TextoHacinamiento1<-renderText({
        TxtHacinamiento1()
    })
    output$TextoCondicion<-renderText({
        TxtCondicion()
    })
        #####################TRANSPORTE########################################
    TxtTransporte<-eventReactive(input$BotonTransporte,{
        "Los servicios de transporte de una ciudad se ubican dentro de lo que varios autores(Topalov, 1979; Pirez, 2016) han caracterizado como servicios urbanos. La principal característica de estos es que proveen a la población de una prestación que difícilmente pueda ser satisfecha por el mercado. Esto es así, a su vez, por varias razones: la primera es la ausencia de una demanda solvente, es decir que el nivel de inversión que su producción requiere es difícilmente compensado por la demanda que generarían librados al sistema de precios de mercado. La segunda razón reside en la larga duración del periodo llamado De rotación del capital , iniciado al momento de la inversión, y finalizado con el reembolso por vía de la ganancia. A esta característica, que podría denominarse “mercantil”, se le añade una segunda de tipo “funcional”, vinculada con el carácter fundamental para la vida urbana de estos servicios. En efecto, al mencionar -por ejemplo- a la red de agua, electricidad, o transporte, existe una referencia a servicios indispensables para el funcionamiento de una aglomeración urbana y de un mercado. Estos servicios son necesarios para la supervivencia básica de las personas; sin ellos una ciudad carecería completamente de capacidad de producción y consumo. En conclusión, por las razones enumeradas, los servicios urbanos precisan de ser provistos, al menos parcialmente, por un mecanismo diferente al de la producción de mercancías; requieren de la producción estatal, que oriente su producción a fines colectivos y no particulares." 
    })
    TxtTransporte1<-eventReactive(input$BotonTransporte,{
        "Específicamente, relevar la red de transportes de una ciudad hecha luz sobre varios factores concernientes al acceso de sus diferentes zonas: al consumo, al trabajo, a la educación y a la recreación."
    })
    TxtTransporte2<- eventReactive(input$BotonTransporte,{
        "Primero, al mapear las redes de los diferentes transportes sobre el mapa es posible observar, con relativa claridad, la dirección de la red de transporte, es decir, de donde vienen y hacia dónde se dirigen los transportes, las personas. Luego, relevar la concentración de las estaciones o paradas según Comuna, nos permite indagar en distintos aspectos: i) qué núcleos urbanos se constituyen como centrales para la ciudad; ii) qué comunas y zonas están más o menos conectadas a esos centros urbanos; y por último, y como corolario, iii) qué sector de la población goza, en función de su ubicación espacial, de un acceso más completo a la totalidad urbana."
    })
    
    output$TextoTransporte<- renderText({
        TxtTransporte()
    })
    output$TextoTransporte1<- renderText({
        TxtTransporte1()
    })
    output$TextoTransporte2<- renderText({
        TxtTransporte2()
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
                      title = "",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                  labelOptions = labelOptions(noHide = T, size=1,
                                                              direction='top',textOnly = F))
        CoroBondis
    })
    
    output$Distr_Bondis<-renderPlot({
        BarrasBondis<-ggplot(TranspBarras,mapping = aes(
            reorder(Comuna, Colectivo),
            Colectivo))+
            geom_col(fill="#09ed46",
                     color="black")+
            geom_text(aes(label = Colectivo), 
                      vjust = 2, size = 3.5)+
            labs(title ="",
                 x="Comuna",
                 y="Cantidad de Paradas",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_minimal_grid(
                font_size = 12,
                color = "#cc00eb"
            )
        BarrasBondis 
    })
    
    #SUBTE
    output$Recorrido_Subte_Prem<-renderLeaflet({
        MapaSubte<-leaflet() %>%
            setView(lng = -58.445531, lat = -34.606653, zoom = 11)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolylines(data = Red_SbtPrem , color =~SubtePal(linea),  opacity = 2, weight = 3)%>%
            addMarkers(data= Acce_Subte, ~long, ~lat, icon = SillaDeRuedas)%>%
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
                      title = "",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers( ~lat,~long, label =  ~as.character(Comuna), 
                                 labelOptions = labelOptions(
                                     noHide = T, size=1,
                                     direction='top',textOnly = F))
        CoroSubte
    })
    
    output$Distr_Subte<-renderPlot({
        BarrasSubte<-ggplot(TranspBarras,mapping = aes(
            reorder(Comuna, Subte),
            Subte)) +
            geom_col(fill="#eb34d5",
                     color="black")+
            geom_text(aes(label = Subte), 
                      vjust = 2.1, size = 3.5)+
            labs(title = "",
                 x="Comuna",
                 y="Cantidad de Estaciones",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_minimal_grid(
                font_size = 12,
                color = "#34eb6e"
            )
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
                      title = "",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                  labelOptions = labelOptions(noHide = T, size=1,
                                                              direction='top',textOnly = F))
        CoroTrenes
    })
    
    output$Distr_Trenes<-renderPlot({
        BarrasTrenes<-ggplot(TranspBarras,mapping = aes(
            reorder(Comuna, Tren),
            Tren)) +
            geom_col(fill="#30c9fc",
                     color="black")+
            geom_text(aes(label = Tren),
                      vjust = 2, size = 3.5)+
            labs(caption ="Estaciones de Tren por Comuna",
                 x="Comuna",
                 y="Cantidad de Estaciones")+
            theme_minimal_grid(
                font_size = 12,
                color = "#fc30cc"
            )
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
                      title = "",
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
                      title = "",
                      labFormat = labelFormat(suffix="%"),
                      position = "bottomleft")%>%
            addLabelOnlyMarkers(  ~lat,~long, label =  ~as.character(Comuna), 
                                  labelOptions = labelOptions(noHide = T,
                                                              direction='top',textOnly = F))
        CoroEcoB  
    })
    
    output$Distr_Bicicletas<-renderPlot({
        BarrasBici<-ggplot(TranspBarras)+
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
            labs(title ="",
                 x="Comuna",
                 y="Cantidad",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_minimal_grid(
                font_size = 12,
                color = "#3030fc")
            scale_y_continuous(
                expand = expansion(mult = c(0, 0.05)))
        BarrasBici        
    })
    
    
    ###############CIERRE#############
    
    output$Cierre <- renderText({
        "prueba"
    })
    
}         

# Run the application 
shinyApp(ui = ui, server = server)
