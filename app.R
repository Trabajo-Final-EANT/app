
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
    
    
    titlePanel(strong("Merecer la ciudad: el derecho al uso y disposicion del espacio urbano en la Ciudad Autonoma de Buenos Aires")),
    
    tabsetPanel(
        tabPanel("Introduccion",
                 tabPanel("Introduccion",
                          h3("Marco Conceptual"),
                          helpText("Cuando nos referimos a una ciudad, podemos decir que 
                          estamos tanto ante una forma de vivir en comun, como ante una forma de producir, 
                          caracterizada por concentrar en un espacio mas bien reducido una gran diversidad
                          de personas, actividades y culturas. Sin embargo, esta forma de organizacion social 
                          no es posible sin vastos conjuntos de bienes colectivos en los que las ciudades 
                          se materializan. Es decir que, por un lado, se trata de una forma social que se 
                          distingue por concentrar lo distinto, por poner en relacion los componentes mas 
                          diversos del espacio social con la finalidad de reproducirse (Lefevbre, 1978). 
                          Mientras que, por otro, comprende el conjunto de bienes y servicios producidos 
                          que fungen de sustento material de la vida urbana. Entonces, si ademas de ser 
                          un espacio de vida, las ciudades son también un conjunto de elementos materiales 
                          dispuestos sobre este espacio, queda claro que lo urbano en conjunto se distribuye 
                          por un territorio determinado no de manera continua y plena, sino más bien discreta, 
                          desigual (Pirez, 2013)."),
                          hr(),
                          h3("Objetivos"),
                          helpText("El presente trabajo se enmarca en el Curso de Social Data Analytics 
                          de EANT y tiene como objetivo indagar, mediante una primera aproximacion, 
                          la distribucion de diversos bienes y servicios urbanos a lo largo y a lo ancho 
                          de la Ciudad Autónoma de Buenos Aires. El lapso de tiempo comprendido en el análisis 
                          varia en funcion de la disponibilidad de datos, centrandose mayormente en el periodo 
                          2010-2020. El interes por la cuestion proviene de una tradicion que define el acceso 
                          a la ciudad como resultante de la forma en la que las redes de infraestructuras y 
                          servicios se disponen a traves del territorio y del espacio social. En este sentido, 
                          el criterio de acceso a la ciudad se configura en un tiempo y espacio determinados 
                          -vale decir en una ciudad determinada-, y esta indicado tanto por factores 
                          territoriales -de ubicacion-, como institucionales y economicos (Oszlak: 2017). 
                          Dicho de otra manera, una correcta descripcion del problema no solo debe preguntarse 
                          donde se concentran estos bienes, sino tambien -y especialmente- por aquellos criterios
                          legales y/o mercantiles que han sido definidos para su usufructo.",
                          br(),
                          br(),
                          "De este modo, con el fin de alcanzar los mencionados propositos, el trabajo parte 
                          de explorar y representar graficamente la distribucion de algunas dimensiones de la 
                          materialidad urbana que adquieren relevancia a la luz de los aportes de los autores 
                          considerados. Ademas, mediante el relevamiento de algunos indicadores clave, se 
                          caracteriza la estructura sociodemografica de la poblacion a efectos de saber 
                          si la estructura espacial de la ciudad es un correlato directo de su estructura social,
                          o si, en cambio, esta relación requiere de una complejización adicional.",
                          br(),
                          br(),
                          "Si bien se contemplan los límites propios de los datos disponibles 
                          (fundamentalmente en lo que respecta a su dimension temporal y sus variables de 
                          analisis), y aquellos vinculados a la extension y propuesta de esta instancia 
                          del Curso, el presente trabajo sugiere un punto de partida para avanzar en el 
                          análisis de lo urbano en la Ciudad, considerando no sólo la cuestión habitacional, 
                          sino también la distribución de bienes y servicios que resulta constitutiva de la 
                          calidad de vida de quienes habitan el mencionado territorio. Desde este lugar, 
                          ofrece algunos cuestionamientos e interrogantes que buscan expandir los objetivos del 
                          proyecto. Por ultimo, tenemos el deseo de que este trabajo sirva para motivar futuras y 
                          diversas investigaciones que utilicen de insumo el análisis y relevamiento de datos 
                          aquí expuesto."),
                          hr(),
                          h4("Sobre las unidades de analisis"),
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
        tabPanel("Estructura sociodemografica",
                 navlistPanel(
                   tabPanel("Datos demograficos",
                            h3(strong("Distribucion etaria de la poblacion de la Ciudad.")),
                            helpText("Indagar el grado y el tipo de acceso a la ciudad del que gozan los habitantes de la Ciudad
                            Autonoma de Buenos Aires, vuelve ineludible una previa caracterizacion de su poblacion. Para hacerlo resulta
                            pertinente atenerse, en un primer momento, a la descripcion de la estructura demografica de la ciudad, explorando
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
                            mayor edad. Por otro lado, ya desde 1980 es posible hablar de una poblacion “muy envejecida” 
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
                              retorno a los pisos preexistentes. La Ciudad de Buenos Aires, en el año 2006, contaba con un 12.7% de personas
                              pobres (EPH- INDEC, 2007). Del análisis de los datos, surge que 10 años después se registran los mayores índices de pobreza en
                              el período comprendido entre el primer trimestre del 2015 y el primero del 2019 (llegando a superar el 18%).  Asimismo, si bien
                              se observa una baja en los porcentajes del año 2017, es posible distinguir un aumento sostenido desde esa fecha en adelante."),
                              highchartOutput(outputId = "G_Pob2"))),
                     tabPanel("NBI",
                              h3(strong("Necesidades Básicas Insatisfechas.")),
                              helpText("En lo que respecta a la pobreza medida por NBI, presento oscilaciones a nivel nacional que respondieron a los 
                              cambios estructurales propiciados por las políticas neoliberales. En el período comprendido entre 1998-2003, la población
                              bajo esta condición superó el 10%; sin embargo, el 90% de estos hogares lo hicieron por el incumplimiento de sólo un 
                              indicador (generalmente, el de hacinamiento o la capacidad de subsistencia) (Arikaki, 2011).",
                              br(),
                              br(),
                              "En el ámbito de la ciudad de Buenos Aires, en el año 2001, la población que mayormente se vió afectada por esta condición 
                              se ubicaba en la zona sur, donde se registraron comunas con más del 15% afectado por NBI, llegando incluso -en un sector de
                              la comuna 8- a superar el 20% (DGEyC, 2001). Esta relación asimétrica entre lo que sucede en el sur de la ciudad y aquello que
                              sucede en el norte (donde el registro de NBI no superó el 10%); se modifica en los datos del 2010."),
                              plotOutput(outputId = "G_NBI"),
                              helpText("En el gráfico se observa que aunque las comunas 4, 3  y 8 tuvieron valores que superaron la media por 5 
                              puntos porcentuales, fue la comuna 1 la que concentró mayor cantidad de población con NBI. Aun así, se sostuvo el rasgo distinguido 
                              entre la heterogeneidad de oportunidades de vida a las que acceden los dos extremos de la ciudad, comprobandose en la amplitud del 
                              rango de valores que fue de 14.2 puntos porcentuales. En este orden de ideas, se observa que fueron las comunas 12, 13 y 11 las que 
                              registraron menor cantidad de población con NBI, respectivamente. "),
                              hr()),
                   tabPanel("Ficha tecnica",
                            textOutput(outputId = "TecnicaPob"),
                            h4(strong("Linea de pobreza.")),
                            helpText("La linea de pobreza es el valor monetario de una Canasta Basica Total de bienes y servicios capaz de satisfacer un conjunto de necesidades alimentarias y no alimentarias consideradas esenciales. 
                            Se denomina pobres a los hogares cuyos ingresos no alcanzan dicha linea o valor, y a la poblacion incluida en ellos.",
                            br()),
                            hr(),
                            h4(strong("Linea de indigencia.")),
                            helpText("Por su parte, la linea de indigencia es el valor monetario de una Canasta Basica de Alimentos, de costo minimo, capaz de satisfacer un umbral elemental de necesidades energeticas y proteicas. 
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
                              helpText("El orden impuesto en los 90 se relacionó con una", em("modernización excluyente"), "gestando sus bases en la dualización de la 
                              sociedad y la economía. En dicho marco, se puso en marcha una reducción del gasto público que conllevó la descentralización administrativa 
                              y el traslado de competencias nacionales a los niveles provinciales y municipales (Svampa, 2005). Tal es el caso de la educación y 
                              los servicios de salud.",
                              br(),
                              br(),
                              "En este sentido, si bien el proceso de privatización de la educación registraba valores en alza desde 1940, estos se vieron reforzados a 
                              partir de la citada reestructuración y, específicamente, el caso de la ciudad de Buenos Aires fue un epicentro de la cuestión (Judzik, 
                              Moschetti, 2016). A modo de ejemplo, basta considerar que en el año 2014 el 52% de la población de alumnos asistía a escuelas de gestión 
                              privada (DiNIECE, 2014).",
                              br(),
                              br(),
                              "El siguiente gráfico muestra la distribución de escuelas por comuna en el año 2020 e incluye establecimientos públicos y privados. 
                              La comuna 4 es la que mayor cantidad de escuelas posee, seguida por la comuna 1. En paralelo, son las comunas 2,6 y 9, respectivamente, 
                              las que registran los valores más bajos en cantidad de centros educativos. El resto de las comunas posee una cantidad poco heterogénea, 
                              que varía entre los 170 y 220 establecimientos."),
                              highchartOutput(outputId = "G_Esc"),
                              br(),
                              h4(strong("Mapa de escuelas de la Ciudad, según cantidad de niveles ofrecidos.")),
                              helpText("Por su parte, el siguiente mapa expone la distribución geográfica de una muestra 
                                       aleatoria de escuelas y la cantidad de niveles que cada una de ellas ofrece. A simple 
                                       vista, se puede observar que la mayor parte de los establecimientos cuenta con sólo un 
                                       nivel educativo, seguido por centros que poseen 2 o 3 niveles. Los establecimientos que 
                                       ofrecen los 4 niveles (inicial, primario, secundario y superior) son minoría, teniendo 
                                       una presencia marcadamente escasa en la zona sur de la ciudad."),
                              leafletOutput(outputId = "M_Escuelas"),
                              br(),
                              h4(strong("Distribucion geografica de personas en edad escolar (5-19), segun cantidad de escuelas. ")),
                              helpText("La relación entre la cantidad de escuelas que existe en cada comuna y el 
                              número de personas en edad escolar se puede observar en este otro mapa. Si bien el 
                              indicador no refleja el número de matriculados y matriculadas por escuela, evidencia que las 
                              comunas 8 y 7 (suroeste de la ciudad) cuentan con un número acotado de instituciones escolares, 
                              en relación a la población en edad escolar que registran."),
                              leafletOutput(outputId = "EdEsc_x_Esc"),
                              helpText("Aunque los datos recabados son un punto de partida interesante para explorar las condiciones de acceso a la educación en 
                              la ciudad, considerando los procesos de polarización creciente y la heterogeneidad de las características socioeconómicas de la población
                              que integran las comunas, sería pertinente indagar -al interior de cada unidad territorial-  la matrícula registrada en las escuelas de 
                              gestión pública y de gestión privada (a modo de ejemplo, cabe tener en cuenta el caso de la comuna 1 que integra a los asentamientos 
                              informales ‘Villa 31’, ‘Villa 31bis’, ‘Barrio General San Martín’ y ‘Villa Rodrigo Bueno’, y a los barrios de Puerto Madero y Retiro). 
                              Si bien este análisis excede los límites del presente trabajo, no deja de ser una arista necesaria de contemplar para ampliar el 
                              conocimiento de la estructura social, económica y educativa de la ciudad."),
                              hr()),
                     tabPanel("Hospitales",
                              h3(strong("Distribucion de hospitales de la Ciudad.")),
                              helpText("A diferencia de los procesos mencionados con antelación, la descentralización de los servicios de salud logró que 11 
                              establecimientos nacionales -muchos de ellos planeados a escala nacional- fueran transferidos a la Ciudad de Buenos Aires 
                              (Stolkiner, 2003). De tal modo, en 2002, el sector estatal de la ciudad contaba con 2,91 camas cada mil habitantes (DGEyC, 2002). 
                              Sin embargo, según la investigación realizada por Stolkiner en el año 2003, muchas de las personas que hacían uso de los servicios 
                              de salud de la ciudad al momento del estudio provenían del primer y del segundo cordón del conurbano y, en términos porcentuales, 
                              más del 45% eran indigentes y más del 25% eran pobres. Es decir, la descentralización propició un traslado de la población más 
                              vulnerada a los hospitales de la ciudad, dificultando su acceso y agregando el costo del traslado.",
                              br(),
                              br(),
                              "Siguiendo esta línea, del análisis de los datos de hospitales de la ciudad al año 2019, surge que la mayoría se encuentran situados 
                              en la comuna 4 que posee 13 unidades. Si bien esta es una de las que registra mayores valores de NBI, es preciso destacar que las 
                              restantes comunas con un gran porcentaje de población bajo esta condición cuentan con una notable cantidad inferior de hospitales: 
                              la comuna 3 posee 2 hospitales, la comuna 8 posee uno y la comuna 1 -valor máximo de NBI- no posee ninguno."),
                              highchartOutput(outputId = "G_Hosp"),
                              helpText("El analisis de la distribucion de hospitales, muestra que la comuna 8 es la que mayor cantidad de instituciones posee. 
                              Aunque la comuna 6 cuenta con 4 hospitales, y es seguida por la comuna 2 con 3 hospitales, llama la atención que las restantes 
                              comunas no cuenten con mas de 2 hospitales. Ademas, cabe destacar el caso de la comuna 1 que directamente carece de servicios 
                              de este tipo. "),
                              br(),
                              h4(strong("Mapa de hospitales de la Ciudad, según su especialización.")),
                              leafletOutput(outputId = "M_Hospitales"),
                              helpText("En el  mapa se evidencia la distribución de hospitales públicos de la ciudad, según su tipo: de agudos, de niños
                              y especializados. Se encuentra que entre aquellas comunas que poseen un único hospital, la mayoría cuenta con hospitales de 
                              agudos, salvo las comunas 5 y 13 que tienen entre su geografía hospitales especializados.  
                              Respecto a los 3 hospitales de niños, 2 de ellos se localizan en la comuna 8 y el tercero en la comuna 2.",
                              br(),
                              br(),
                              "Llama la atención la heterogeneidad en la distribución y concentración geográfica de las unidades hospitalarias, puesto que ello
                              impacta en las condiciones de acceso a los servicios de salud y las vuelve poco equitativas. Es decir, en tanto la localización de
                              la oferta urbana de servicios se vincula con la posibilidad de ejercer derechos sobre la ciudad, la falta de integración de los servicios
                              de salud en distintas zonas del tejido urbano no deja de constituir un factor de desigualdad para quienes viven y habitan la ciudad.",
                              br(),
                              br(),
                              "Asimismo, la distribución de los hospitales muestra cómo la configuración territorial de la ciudad constituye en sí misma un 
                              mecanismo de desigualdad, en la medida en que la localización del servicio configura su nivel de accesibilidad. Nuevamente, el 
                              análisis incipiente de esta cuestión, da cuenta de la importancia de indagar en otras variables de pertinencia en futuras 
                              investigaciones: población que asiste a los hospitales, capacidad de atención, infraestructura disponible, entre otras. "),
                              hr()),
                     tabPanel("Cultura",
                              h3(strong("Distribucion de espacios culturales de la Ciudad.")),
                              helpText("Como se dijo en la introduccion, este trabajo entiende a la ciudad como una entidad doble: por un lado, la ciudad
                              es una realidad practico-sensible, un conjunto amplio de objetos dispuestos en un espacio reducido. Por el otro, la ciudad
                              es una forma social dentro de la cual se expresa lo diverso, lo divergente. La importancia de relevar la oferta cultural de una
                              ciudad cobra sentido en esta doble definicion puesto que la cultura se ubica entre estas dos dimensiones: aquello que 
                              llamamos", em("oferta"), "implica un conjunto de bienes, servicios, establecimientos e instituciones localizados a lo ancho de la ciudad; 
                              lo cultural, por su parte, expresa los rasgos distintivos de la comunidad que la produce.",
                              br(),
                              br(),
                              "En este sentido, la concentracion de la oferta en pocos espacios homogeniza la cultura, a la par que dificulta el acceso a los ciudadanos
                              más alejados de esos espacios. Es por esto que el presente apartado releva la distribucion geografica de la oferta cultural. 
                              Por ultimo, y teniendo en cuenta que el acceso a un bien urbano cualquiera, no solo tiene condicionantes geograficos sino tambien 
                              monetarios o institucionales, se ofrece una clasificación de estos espacios segun su criterio de acceso."),
                              plotlyOutput((outputId= "BarrasCul")),
                              helpText("La distribucion de espacios culturales en la ciudad muestra que la mayor concentracion se radica, marcadamente, en la comuna 1. 
                              Seguidamente se encuentran las comunas 2, 14 y 3, sin embargo, estas poseen una cantidad notoriamente menor de comercios, 
                              espacios de exhibicion, bibliotecas y espacios de formacion.",
                              br(),
                              br(),
                              "En cuanto a las comunas que cuentan con menor presencia de estos espacios, se trata de aquellas que se localizan en la periferia
                              sudoeste de la ciudad: 8, 10, 9 y 11. Especificamente, la comuna 8 es la que posee el valor mínimo, dado que en el marco
                              de su territorio hay menos de 20 espacios culturales."),
                              br(),
                              h4(strong("Distribucion geografica de los espacios culturales.")),
                              leafletOutput(outputId = "MapaCul"),
                              br(),
                              helpText("Se ve entonces que la distribucion cultural presenta una marcada dinamica centro-periferia que deja excluidas del acceso
                              a la oferta cultural a las comunas 7, 8, 9, 10, 11 y 12. Por otro lado, en casi todas la comunas el tipo de espacio cultural 
                              “Comercio” (que engloba ferias, librerias, bares y disquerías) predomina. Mientras tanto, escasean los Espacios Públicos 
                              (monumentos, calesitas, sitios historicos) -con la notable excepción de la comuna 2-, donde son mayoria. Finalmente, el tipo de 
                              espacio más subrepresentado son los Espacios de Formacion (que incluyen escuelas tecnicas y artisticas, talleres e institutos de 
                              arte, universidades publicas, etc.)."),
                              h4(strong("Distribucion porcentual de los espacios culturales.")),
                              leafletOutput(outputId = "CoroCul")),
                     tabPanel("Ficha tecnica",
                              h4(strong("Tipos de hospitales.")),
                              textOutput(outputId = "TecnicaDes"),
                              helpText("Los hospitales de la Ciudad Autonoma de Buenos Aires comprenden tres tipos de hospitales: de agudos, de niños y especializados.",
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
                              hr())
                 )),
        tabPanel("Vivienda",
                 navlistPanel(
                   tabPanel("Hacinamiento",
                            h3(strong("Distribución porcentual de hacinamiento por comuna.")),
                            helpText("El análisis precedente muestra claros indicios de la fuerte desigualdad que trajeron 
                            consigo las políticas neoliberales implementadas desde la década del 90’ en Argentina. En dicho 
                                     marco, el mercado como mecanismo de acceso a bienes y servicios se volvió primacía y muchos 
                                     de los derechos, vinculados al bienestar económico, a la convivencia social y a la vida digna, 
                                     quedaron excluidos para una gran parte de la sociedad (Svampa, 2005). ",
                            br(),
                            br(),
                            "El derecho al uso y disposición del espacio urbano fue uno de los tantos vulnerados, entendiendo por ello 
                            no sólo al goce de la propiedad, sino también al ejercicio de las oportunidades sociales y económicas ligadas 
                            a la localización de una vivienda o una actividad (Ozlack, 2017).  En este sentido, la vivienda se constituye 
                            como el bien urbano fundamental, puesto que la capacidad de fijar residencia en la ciudad (o en sus alrededores 
                            cercanos) habilita a los habitantes -al menos, en teoria- a gozar de otro amplio conjunto de bienes (cultura, 
                            educacion, salud, etc). A su vez, la forma y ubicacion de los lugares donde se asientan sus habitantes, 
                            también marca a fuego la manera en que estos satisfacen sus necesidades mediante los bienes urbanos.",
                            br(),
                            br(), 
                            "Además, una vivienda digna debe garantizar y satisfacer a sus habitantes un conjunto de necesidades, 
                            entre las que se cuentan “el resguardo y la protección ante las inclemencias climáticas, la seguridad 
                            en la tenencia, el desarrollo personal y familiar, un espacio adecuado, la accesibilidad, la ubicación 
                            y el transporte, la privacidad, la cultura y la identidad, las condiciones de salubridad mínimas y el 
                            abastecimiento en forma segura de los servicios públicos esenciales” (Defensoría del Pueblo de la Ciudad 
                            Autónoma de Buenos Aires, 2009).",
                            br(),
                            br(),
                            "En consecuencia, indagar los niveles de hacinamiento en la ciudad resulta por demás relevante 
                            para conocer la distribucion de diversos bienes y servicios urbanos. El siguiente gráfico muestra 
                            que la comuna 8 es la que mayor porcentaje de hacinamiento posee, aunque, considerando el período 
                            comprendido en los últimos 10 años, se advierten altos valores en las comunas 4, 1 y 7 (dos de las 
                            cuales registraron valores de NBI superiores a la media).
                            Por otro lado, se observa que las comunas 2, 13, 5, 6 y 12 presentan valores que 
                            no superan el 10% de la poblacion en condición de hacinamiento (centro y norte de la ciudad). "),
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
                            highchartOutput(outputId = "G_HC"),
                            helpText("aca va lo de hacinamiento critico"),
                            br()),
                   tabPanel("Condicion de ocupacion",
                            h3(strong("Distribución porcentual de viviendas, según condicion de ocupacion.")),
                            highchartOutput(outputId = "G_Vivienda"),
                            br(h4(strong("Mapa de viviendas de la Ciudad, ocupadas con fines comerciales."))),
                            leafletOutput(outputId = "M_Vivienda")),
                   tabPanel("Regimen de Tenencia",
                            br(h4(strong("Distribucion porcentual anual de hogares, segun regimen de tenencia."))),
                            selectInput(inputId = "input_AÑO",
                                        choices = Regimen$año,
                                        label = "Seleccione año",
                                        selected = TRUE),
                            selectInput(inputId = "input_Regimen1",
                                        choices = Regimen$Regimen_Tenencia,
                                        label = "Seleccione Regimen",
                                        selected = TRUE),
                            plotOutput(outputId= "BarrasRegimen"),
                            br(),
                            br(h4(strong("Distribucion porcentual de hogares, segun tipo de regimen de tenencia."))),
                            selectInput(inputId = "input_Regimen2",
                                        choices = Regimen18$Regimen_Tenencia,
                                        label = "Seleccione Régimen",
                                        selected = TRUE),
                            leafletOutput(outputId = "CoroRegimen")),
                   tabPanel("Precio del metro cuadrado",
                            h3(strong("Valor del Metro Cuadrado en la Ciudad")),
                            helpText("aca desarrollar"),
                            leafletOutput(outputId = "P_x_m2")),
                   tabPanel("Ficha tecnica",
                            textOutput(outputId = "TecnicaViv"),
                            h4(strong("Hacinamiento")),
                            helpText("La situacion de", em("hacinamiento"), "expresa la importancia relativa de los 
                            hogares, o de la poblacion en ellos, en los que hay dos o mas personas por cuarto en la vivienda (hacinados).",
                            br(),
                            br(),
                            "Por su parte, los hogares con", em("hacinamiento crítico"), "expresan la importancia relativa de 
                            los hogares en los que hay más de tres personas por cuarto de la vivienda."),
                            hr(),
                            h4(strong("Regimen de tenencia")),
                            helpText("explicar"))
                   )),
        tabPanel("Transporte",
                 navlistPanel(
                   tabPanel("Colectivo",
                            h3(strong("Distribución de redes de colectivos en la ciudad.")),
                            leafletOutput(outputId = "Recorrido_Bondis"),
                            br(),
                            br(h4(strong("Distribucion porcentual de paradas, ponderada por area de las comunas."))),
                            leafletOutput(outputId= "Coro_Bondis"),
                            br(),
                            br(h4(strong("Cantidad de paradas por comuna."))),
                            plotOutput(outputId = "Distr_Bondis")),
                   
                   tabPanel("Subterraneo/Metro",
                            h3(strong("Distribución de redes de subtes en la ciudad.")),
                            leafletOutput(outputId = "Recorrido_Subte"),
                            br(),
                            br(h4(strong("Distribucion porcentual de estaciones, ponderada por area de las comunas."))),
                            leafletOutput(outputId = "Coro_Subte"),
                            br(h4(strong("Cantidad de estaciones por comuna."))),
                            plotOutput(outputId = "Distr_Subte")),
                   
                   tabPanel("Tren/Ferrocarril",
                            h3(strong("Distribución de redes de trenes en la ciudad.")),
                            leafletOutput(outputId = "Recorrido_Trenes"),
                            br(),
                            br(h4(strong("Distribucion porcentual de estaciones, ponderada por area de las comunas."))),
                            leafletOutput(outputId= "Coro_Trenes"),
                            br(),
                            br(h4(strong("Cantidad de estaciones por comuna."))),
                            plotOutput(outputId = "Distr_Trenes")),
                   
                   tabPanel("Bicicletas",
                            h3(strong("Distribución de ciclovias en la ciudad.")),
                            leafletOutput(outputId = "Recorrido_Bicicletas"),
                            br(),
                            br(h4(strong("Distribucion porcentual de ciclovias, ponderada por area de las comunas."))),
                            leafletOutput(outputId= "Coro_CicloV"),
                            br(),
                            br(h4(strong("Distribucion porcentual de Ecobicis, ponderada por area de las comunas."))),
                            leafletOutput(outputId="Coro_EcoB"),
                            br(h4(strong("Servicios de bicicletas por comuna."))),
                            plotOutput(outputId = "Distr_Bicicletas")))),
        tabPanel("Cierre",
              navlistPanel(
                  tabPanel("Algunas conclusiones",
                                     textOutput(outputId = "Cierre"))))
                   
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
            output$Marco <- renderText({
                ""
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
#Escuelas
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

#Hospitales        
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
        
#Cultura       
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
        
        output$TecnicaDes <- renderText({
          ""
        })
        
#############VIVIENDA###################################################
#Hacinamiento      
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

#Regimen       
        output$BarrasRegimen<-renderPlot({
          RegimenA<-ggplot(data=Reg_filt(), 
                           aes(x=comunas,
                               y=porcentaje, 
                               fill= Regimen_Tenencia))+
            scale_fill_manual(values = 
                                c("#34eb6b","#d334eb","#eaf51b"))+
            geom_col(width = .3)+
            geom_text(aes(label = porcentaje), 
                      vjust = 2.1, size = 3.5)+
            labs(title = "",
                 subtitle = "CABA")+
            theme_bw()
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
                      labFormat = labelFormat(suffix="$"),
                      position = "bottomleft")
          
          CoroPreciom2
          })
        
        output$TecnicaViv <- renderText({
          ""
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
            theme_classic()
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
            labs(title ="Servicios de Bicicleta segun Comuna.",
                 x="Comuna",
                 y="Cantidad",
                 caption = "Fuente: https://data.buenosaires.gob.ar")+
            theme_classic()+
            coord_flip()
          BarrasBici        
          })


###############CIERRE#############

      output$Cierre <- renderText({
        "prueba"
         })

}         
            
# Run the application 
shinyApp(ui = ui, server = server)

