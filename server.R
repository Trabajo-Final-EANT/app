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
        ""
    })
    
} 
