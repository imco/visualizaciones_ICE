pacman::p_load(tmap,#libreria para generar los mapas
               tmaptools,#gadgets del mapa
               pryr,#manipulación de datos
               plyr,#manipulación de datos
               base,#manipulación de datos
               dplyr,#manipulación de datos
               ggplot2,#Graficas
               tidyr,#manipulación de datos
               tidyverse,#manipulación de datos
               shiny,#libreria para ejecutar la app
               bslib,#tema apps
               DT,#libreria para la tabla 
               shinyWidgets,# libreria de gadgets shiny app
               waiter,# waiter de la interfáz
               shinyBS#bootstrap(paginas emergentes)
)




# Shiny UI
ui <- navbarPage(#interfaz de estilo navegación
  title = tags$img(
    src = "data:image/png;base64,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"
    ,height="40px", width="120px"
    ),
  
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  
  
  #### Interfáz MAPA####
  
  tabPanel(#Primer panel "MAPA"
    "Mapa",
    setBackgroundImage(src = "https://wallpaperaccess.com/full/663025.jpg",
                       shinydashboard = FALSE),#Fondo de pantalla
    useWaiter(),#uso de animación de espera
    waiter_on_busy(#para presentar la animación cuando el servidor está ocupado
      html = spin_orbiter(),
      fadeout = TRUE,
      color= "#C0C0C0",
      image= "https://imco.org.mx/wp-content/uploads/2020/07/logo-imco-firma-02.png"#background de la animación
    ),
    
    bsModal(#Pagina emergente
      "informacion","",
      "info",#id del objeto
      textOutput("texto")# salida de contenido
    ),
    
    tmapOutput(#salida del mapa 
      "mapa",#id del objeto
      width = "100%", height = "580"
    ),
    
    absolutePanel(# panel deslizable que contiene los inputs de seleccion
      top = 400 , left = 100, draggable = TRUE, width = "15%",
      tags$div(
        tags$style(".picture{
               display: inline;
               vertical-align: middle;
               padding-left: 10px;
                       }")
        ),
      pickerInput(#input indidacor
        "indicador",#id del objeto
         list("Subíndice",#titulo
         actionLink(#vinculo de informacion que despliega ventana emergente
           "info",
           list(icon("question")#forma del vinculo
                          )
               )
      ),
      choices=unique(ICE2022$Subíndice),#opciones de selección
      selected="General"#preselcción
      ),
      
      selectInput(#input años
        "anio",#id del objeto
        "Año",#titulo
        choices = c("2021","2022"),#opciones de selección
        multiple = FALSE,
        selected = "2022"),
      
      downloadButton(#botón de descarga
        "download1", #id del objeto
        "Descarga el mapa")
    )
    
  ),
  
  
  #### Interfáz Graficador####
  
  tabPanel(#segundo panel de visualizacion "GRAFICADOR#
    "Graficador",
    "",
    fluidRow( #manejo de espacio del panel
     column(3,#distribución de espacio por columnas
    
     dropdown(#menú desplegable
      label = "Seleccione los datos", 
      animate= T,
        
      selectInput(#input subíndice
          "si",#id del input
          "Subindice",
          choices= unique(ICE2022$Subíndice),#opciones de selección
          multiple = F,
          selected=FALSE),#sin preselección
        
       selectInput(
         "entidad",#id del objeto
         "Entidad",
         choices=unique(ind_largo$entidad),#opciones
         multiple= TRUE,
         selected = FALSE
           ),
      
      selectInput(
        "var_x", #id del objeto
        "Indicador",
        choices="",
        multiple= F,
        selected = FALSE),
      
      downloadButton(#botón de descarga
        "download2",#id del objeto 
        "Descarga tus gráficas")
             )
             )
           ),
           
    
      fluidRow(#distribución del espacio del panel
      
          column(12, 
                 plotOutput("puntos")#salida de grafico ranking
                 ),
           
          column(6,
                 plotOutput("MYM")#salida de grafico barras 
                 ),
             
          column(6, 
                 plotOutput("Barras2")#salida de grafico barras 
             )
           ),
    
           fluidRow(#segunda sección de la página
             
             column(6,
                    plotOutput("Barras")#salida de grafico barras 
                    ),
             column(6,
                    plotOutput(outputId = "res_go"))#salida de grafico barras 
             
           )
  ),
  
  #### Interfáz Comparadpr####
  
  tabPanel(#tercer panel de visualización
    "Comparador",#titulo
    "",
    fluidRow(#tipo de manejo de espacio
      
      column(4,
             selectInput(#selector subíndice
               "subin",#id del objeto
               "Seleccione el subíndice",
               choices = unique(ICE2022$Subíndice),#opciones de selección
               multiple= T)
             ),
       column(4,
              selectInput(#selector entidad 
                "Ent",#id del objeto
                "Seleccione la entidad",
                choices = unique(ICE2022$Entidad),#opciones de selección
                multiple=T)
              ),
      column(4,
             downloadButton(#botón de descarga de la tabla
               "dt",#id del objeto
               "Descarga la tabla")#mensaje
             )
           ),
     DT::dataTableOutput("Tabla")#salida de la tabla
           
  )
)



####Servidor#####

server <- function(input, output, session) {
  #El servidor maneja entradas, salidas, y objetos
 
  #####MAPA#### 
  mapa<-reactive({#objeto reactivo "mapa"
    pt_subindice<-input$indicador#variables reactivas dependientes del valor de entrada
    pt_anio<-input$anio#variables reactivas dependientes del valor de entrada
    
    Posición<- filter(
      ICE2022SPT,
      Subíndice %in% pt_subindice,
      Año %in% pt_anio
      )# filtrando el data frame con los datos del ranking y los datos espaciales
       # mediante la selección del usuario
    
    tm_shape(Posición) +# función que toma el objeto spt y devuelve el mapa
      tm_style("cobalt")+
      tm_polygons(
        col="Nivel de Competitividad",id="Entidad",
        palette = c("Muy alta"= "#52966A",
                    "Alta"="#72CC80",
                    "Media alta"="#F7DC63",
                    "Media baja"="#F0A040", 
                    "Baja"="#D65470",
                    "Muy baja"="#762838"),
        legend.show= FALSE,
        popup.vars=c("Posición","Nivel de Competitividad"),
        title="Nivel de Competitividad"
      ) +
      tm_layout(
        "INDICE DE COMPETITIVIDAD ESTATAL 2022",
        title.color = "black",
        bg.color = "white",
        legend.text.color = "black",
        legend.title.color = "black",
        title.size = 0.8
      ) +
      tm_view(
        view.legend.position = "left"
      )+
      tm_view(
        view.legend.position = "bottom"
      )+
      tm_add_legend(
        type = c("fill"),
        labels = c("Muy alta",
                   "Alta",
                   "Media alta",
                   "Media baja", 
                   "Baja",
                   "Muy baja"),
        col = c("#52966A","#72CC80","#F7DC63","#F0A040","#D65470","#762838"),
        title = "Nivel de competitividad",
        group = Posición
      )
    
  })
  
  
  
  output$mapa<-renderTmap({
    #función que renderiza el maoa tomando nuestro objeto reactivo 
    #llama a la salida del objeto en la interfáz
      mapa()
  }
  )
  
  
####Gráfico de ranking####  
pts<-reactive({#objeto reactivo
  
  su<-input$si#variable temporal reactiva a la selección del usuario
    
  ptdt<-ICE2022%>%
  filter(Año == "2022", Subíndice %in% su)%>% 
    mutate(Abrev = factor(Abrev, levels = unique(Abrev)))%>%
    arrange("Abrev")#filtrado y orden a la base
  
  ggplot(ptdt)+#función que toma el data frame y devuleve la grafica
  geom_point(aes(x=Abrev,y=Puntaje, colour=`Nivel de Competitividad`,
                     size= 15))+
  geom_segment(aes(x = Abrev, xend = Abrev, y = 0, yend = Puntaje-0.6), color = "gray")+
  geom_text(aes(label = Posición, y=Puntaje+2,x=Abrev ))+
      scale_colour_manual(values=c("Muy alta"= "#52966A",
                                 "Alta"="#72CC80",
                                 "Media alta"="#F7DC63",
                                 "Media baja"="#F0A040", 
                                 "Baja"="#D65470",
                                 "Muy baja"="#762838"))+
  guides(size="none")+
      theme(axis.text.x = element_text(angle=90),
             axis.ticks = element_blank(),
            axis.text.y= element_blank(),
            plot.title = element_text(hjust = 0.5)
            )+
      labs(
        title = paste("Índice de Competitividad Estatal 2022,","subindice", su),
        y = NULL,
        x="Entidad",
        caption = "Fuente: IMCO, índice de competitividad estatal 2022",
        )
      
      
      
  }
  )

observe({
# función de reacción para cambio dinamico de la entrada de selección "Indicador"
 
   cam_var <- input$si#creando una variable reactiva a la seleción del usuario
  
  
  if (is.null(cam_var))
   cam_var <- character(0)# si no hay selección de subindice no hay datos
  
  
  updateSelectInput(#funcion de actualización del selector
    session, "var_x",#id del objeto a actualizar
    label = "Indicador",
    choices =  ind_largo$nomx[ind_largo$Subíndice == cam_var],#deplegando opciones actualizadas
    selected=""
  )
}
)
  
  
  output$puntos<-renderPlot({#renderizando el grafico de ranking
    pts()+
      theme(rect = element_rect(fill = "transparent"))
    
  },bg="transparent")
  
  ####Gráfico de barras indicador el mas alto y el mas bajo #####
  
  b1<- reactive({#objeto reactivo
    selx_pt<-input$var_x
    
    vr2<- filter(
      ind_largo,
      anios=="2022",
      nomx == selx_pt)%>%
      
      filter(
        Valor %in% c(max(Valor), min(Valor)
        )
      )#filtrado del data frame
    
    ggplot(vr2,# función que toma el data frame y devuelve el gráfico
           aes(x=entidad,
           y=Valor,
           fill=Unidades
           )
           ) +
      geom_bar(
        position= "dodge",stat="identity"
        )+
      facet_wrap(
        facets="anios"
        )+
      labs(
        subtitle = paste("El más alto y el más bajo"),
        title= paste(as.character(selx_pt)),
        y = NULL,
        x="Entidad",
        caption = "Fuente: IMCO, índice de competitividad estatal 2022")+
      theme(legend.position="bottom")+
      guides(fill=guide_legend(title = NULL))
    
  }
  )
  
  
  
  output$res_go <- renderPlot({#renderizando el gráfico
    req(input$var_x)#solo renderiza hasta que hay una selección
    b1()+
      theme(rect = element_rect(fill = "transparent"))  
    
  },bg="transparent")
  
  
  ####Gráfico de barras indicador selección del usuario #####
  
  b2<-reactive({#creando objeto reactivo
    
    selent_pt<-input$entidad#variables reactivas
    selx_pt<-input$var_x#variables reactivas
    
    vr1<-filter(
      ind_largo,
      entidad %in% selent_pt,
      anios %in% c("2021","2022"),
      nomx%in% selx_pt)#filtrando el data frame
    
    ggplot(#función que toma el data frame y devuelve el gráfico
      vr1,
      aes(x=entidad,
          y=Valor,
          fill=Unidades
          )
      ) +
      geom_bar(
        position= "dodge",
        stat="identity"
      )+
      facet_wrap(facets="anios")+
      labs(title = paste(as.character(selx_pt)),
                         subtitle= paste("Entidades seleccionadas"),
           x="Entidad",
           
           y = NULL,
           caption = "Fuente: IMCO, índice de competitividad estatal 2022")+
      guides(fill=guide_legend(title = NULL))+
      theme(legend.position="bottom")
    
    
    
  })
  
  
  output$Barras<- renderPlot({#función de renderizado del gráfico
    req(input$var_x)#requiere una selección para renderizar
    b2()+
      
      theme(rect = element_rect(fill = "transparent"))
    
  }
  
  ,bg="transparent")
  
  
  ####Gráfico de barras subíndice el mejor y el peor#####
  
  b3<-reactive({
    
    br_si<-input$si
   
     ICE2022%>%
      filter(
        Subíndice %in% br_si,
        Año=="2022")%>%
      filter(
        Posición %in% c(1,32))%>%#filtrado del data frame
      
      ggplot(aes(
          x=Entidad,
          fill=`Nivel de Competitividad`))+
      scale_fill_manual(values=c("Muy alta"= "#52966A",
                                 "Alta"="#72CC80",
                                 "Media alta"="#F7DC63",
                                 "Media baja"="#F0A040", 
                                 "Baja"="#D65470",
                                 "Muy baja"="#762838"))+
      geom_bar(position= "dodge", stat = "count")+
      geom_text(aes(label = Posición, y=0.50))+
      guides(fill=guide_legend("Nivel de competitividad"))+
      facet_wrap(facets="Año")+
      labs(title=paste("El mejor y el peor para el subindice",br_si),
           y="", x="Entidad",
           caption = "Fuente: IMCO, índice de competitividad estatal 2022")+
      theme( panel.grid = element_blank(),
             axis.ticks = element_blank(),
             axis.text.y= element_blank())
    
    
  })
  
  
  
  
  output$Barras2<-renderPlot({#renderizado del gráfico
    req(input$si,
        input$entidad)#requiere selección para renderizar
    
    b3()+
      theme(rect = element_rect(fill = "transparent"))
    
    
    
    
  },bg="transparent"
  )
  
  ####Gráfico de barras subíndice selección del usuario #####
  
  b4<-reactive({ 
    
    
    br_si<-input$si
    br_en<-input$entidad
    
    ICE2022%>%filter(Subíndice %in% br_si,
                     Entidad %in% br_en)%>%
  
   ggplot(aes(
     x=Entidad,
     fill=`Nivel de Competitividad`))+
      scale_fill_manual(values=c(
        "Muy alta"= "#52966A",
        "Alta"="#72CC80",
        "Media alta"="#F7DC63",
        "Media baja"="#F0A040", 
        "Baja"="#D65470",
        "Muy baja"="#762838"))+
      geom_bar(position= "dodge",stat="count")+
      geom_text(aes(label = Posición, y=0.50))+
      guides(fill=guide_legend("Nivel de competitividad"))+
      facet_wrap(facets = "Año")+
      labs(title=paste("Posición en el subindice",br_si),
           subtitle = "Entidades seleccionadas",
           y = "",x="Entidad",
           caption = "Fuente: IMCO, índice de competitividad estatal 2022")+
      theme(panel.grid = element_blank(),
            axis.text.y= element_blank(),
            axis.ticks = element_blank())
    
  })
  
  output$MYM<-renderPlot({#renderizanddo
    req(input$si,
        input$entidad)
    b4()+
      theme(rect = element_rect(fill = "transparent"))
    
  },bg="transparent"
  )
  
   
  #####comparador####
  
  tab<-reactive({#objeto reactivo
    
    ICE2022%>%
    select(Entidad, Posición,`Nivel de Competitividad`,Subíndice, Año)%>%
    filter(is.null(input$subin) | Subíndice %in% input$subin,
           is.null(input$Ent) | Entidad %in% input$Ent
    )# filtrado dinamico dependiente de la selección del usuario
  }
  )
  output$Tabla<- DT::renderDataTable({#renderizando la tabla
    
  tab()
    
    
    
  })
  
  
  
  

  
  ######Pagina desplegable de nformación####
  
output$texto<-renderText({#renderizando el texto mostrado en la pagina
    texto<- case_when(# creación dinamica del texto segun la entrada del usuario
      input$indicador == "Derecho" ~ 
        "El subindice de Derecho analiza el entorno de seguridad pública y 
  jurídica en las entidades federativas del país.
",
      input$indicador == "Medio Ambiente" ~ 
        "El subindice de Medio ambiente mide la capacidad de los estados 
  para relacionarse de manera sostenible y responsable con los recursos 
  naturales y su entorno.
",
      input$indicador == "Sociedad" ~ 
        "El subindice de Sociedad mide la calidad de vida de los habitantes 
  a través del acceso que tienen a bienes y servicios agrupados en 
  las siguientes tres áreas: inclusión, educación y salud.
",
      input$indicador == "Político" ~ 
        "El subindice de Sistema político mide el potencial de los sistemas
  políticos estatales para ser estables y funcionales.
",
      input$indicador == "Gobiernos" ~ 
        "El subindice de Gobiernos mide la forma en que los gobiernos
  estatales son capaces de influir positivamente en la competitividad
  de sus respectivos estados.
  ",
      input$indicador == "Factores" ~ 
        "El subindice de Mercado de factores mide la eficiencia del 
  principal factor de producción: el trabajo.
  ",
      input$indicador == "Economía" ~ 
        "El subindice de Economía mide las principales características 
   de las economías estatales, así como la situación del crédito
   para empresas y familias.
  ",
      input$indicador == "Precursores" ~ 
        "El subindice de Precursores engloba elementos relacionados con 
   los sectores financiero, de telecomunicaciones y de transporte.
  ",
      input$indicador == "Relaciones Internacionales" ~ 
        "El subindice de Relaciones internacionales mide el grado con 
  el cual los estados capitalizan su relación con el exterior 
  para elevar su competitividad.
  ",
      input$indicador == "Innovación" ~ 
        "El subindice de Innovación mide la capacidad de los estados 
  para competir con éxito en la economía, particularmente en 
  sectores de alto valor agregado, intensivos en conocimiento y 
  tecnología de punta.
  "
    )
    print(texto)
  })
  
  #####mapa descargable####
  
  mapades<-reactive({#objeto reactivo
    
    pt_subindice<-input$indicador
    pt_anio<-input$anio
    
    mapa()+#mapa de visualización como base
    tm_layout(
      main.title = paste("Índice de Compertitividad Estatal 2022,",
                         "subíndice", pt_subindice, "año", pt_anio
                ),#titulo dinamico a la selección del usuario
      title = "",
      main.title.color = "black",
      main.title.position = 0.1,
      title.color="black",
      bg.color = "white",
      legend.text.color = "black",
      legend.title.color = "black",
      legend.title.size = 1,
      main.title.size = 1.2)+
      tm_credits("Fuente: Instituto Mexicano para la Competitividad A.C",
                 position = "left",
                 col = "black")+
      tm_logo(
        "https://imco.org.mx/wp-content/uploads/2014/08/imco_html.png",
        height = 3,
        halign = "center",
        margin = 0.3,
        position = "right",
        just = NA
      )
  })
  
  output$download1 <- downloadHandler(#función de manejor de descarga
    filename = "mapa_ICE_IMCO.png",
    content = function(file) {
      tmap_save(mapades(), file)
    }
    
  )
 
  #####Descarga de Gráficos######
  
  b5<-reactive({#arreglo de gráficos dinámico a través de selección del usuario
    ipt<-input$var_x
    if  (ipt == "") {
    ggarrange(b4(),b3(),  nrow = 2)
    } else{
      ggarrange(b4(),b3(), b2(),b1(), ncol=2,nrow = 2,
                widths = 3, heights = 3)
    }
                 })
  
  
  
  output$download2 <- downloadHandler(#función de descarga de graficos
    filename = function() {
      "GraficasICE2022_IMCO.pdf"#nombre del archivo
    },
    content = function(file) {#contenido
      pdf(file, width= 12, heigh=14)#función que crea el archivo
   print(ggarrange(pts(),nrow = 2 ))
   print(b5())
      dev.off()
    }
  )
  
  #####descarga de la tabla ######
    
  output$dt <-downloadHandler(#función de descarga
    filename = function() { "ice2022.xlsx"},
    content = function(file) {
      write_xlsx(tab(), path = file
      )
      }

    
  )}#####fin del servidor###

# Run the application 
shinyApp(ui = ui, server = server)
