
pacman::p_load(tmap,
               tmaptools,
               sf,
               raster,
               rgeos,
               rgdal,
               pryr,
               plyr,
               readr,
               base,
               reshape2,
               dplyr,
               readxl,
               ggplot2,
               tidyr,
               tidyverse,
               ggridges,
               here,
               tibble,
               shiny,
               shinydashboard,
               formattable,
               shinythemes,
               bslib,
               data.table,
               DT,
               devtools,
               shinyWidgets)
here()




# Shiny UI
    ui <- navbarPage(title = "IMCO ICE 2021",
                     tabPanel("Mapa",
                              tmapOutput("mapa",
                                         width = "100%", height = "680"
                                         
                              ),
                              absolutePanel(
                                top = 400 , left = 100, draggable = TRUE, width = "20%",
                                selectInput("indicador", "Subíndice", 
                                            choices =unique(ranks$Subindice),
                                            multiple=FALSE),
                                selectInput("anio","Año",
                                            choices = c("2020","2019"),
                                            multiple = FALSE)
                              )
                     ),
                                
                              
                              
    tabPanel("Graficador","",
             fluidRow( 
               column(3,dropdown(label = "Seleccione los datos", 
                                 animate= T,
                 selectInput("si","Subindice",
                             choices=unique(ranks$Subindice),
                             multiple = F),
                 selectInput("entidad","Entidad",
                                      choices=unique(ind_largo$entidad),
                                      multiple= TRUE,
                             selected = "Ciudad de México"),
                selectInput("var_x", "Indicador",
                                      choices=unique(ind_largo$nomx),
                                      multiple= F)
                )
               )
             ),
               
             fluidRow(
               column(6,plotOutput("MYM"),
                 
               ),
               column(6, plotOutput("Barras2")
                      )
             ),
             fluidRow(
               column(6,plotOutput("Barras")),
               column(4, plotOutput("indicador"))
               
             )
    ),
             
    tabPanel("Comparador", "",
             fluidRow(
             
             column(4,selectInput(
               "subin", "Seleccione el subíndice",
                         choices = unique(ranks$Subindice),
                         selected = "GENERAL",
                         multiple= T)
                   ),
             column(4,selectInput(
                         "Ent","Seleccione la entidad",
                         choices = unique(ranks$Entidad),
                         selected = c(filter(ranks,
                                             Subindice == "GENERAL",
                                             Ranking %in% c(max(ranks$Ranking),
                                            Ranking %in% min(ranks$Ranking)))[,1]),
                         multiple=T)
             )
                   ),
             DT::dataTableOutput("Tabla")
             )
             )
             
    

server <- function(input, output, session) {
    

    
    
   output$mapa<-renderTmap({
      
     pt_subindice<-input$indicador
     pt_anio<-input$anio
     Ranking<- filter(ranks,Subindice==pt_subindice,
                     Año==pt_anio)%>%
     select(Entidad,Ranking,Grupo,Subindice,cv_inegi)
       Ranking<- inner_join(shp_estados, Ranking, by="cv_inegi")
     
  
     tm_shape(Ranking) + 
       tm_polygons(col="Grupo",id="Entidad",
               palette = c("#72CC80",
                                 "#52966A",
                                 "#F7DC63",
                                 "#F0A040", 
                                 "#D65470",
                                 "#762838"),
               alpha=0.7,
               legend.show= TRUE,
               popup.vars=c("Ranking","Grupo"),
                title="Grupo de Competitividad") +
        tm_layout("IMCO, INDICE DE COMPETITIVIDAD ESTATAL 2021", 
                  main.title.position = "center") +
        tm_view(view.legend.position = "left")+
        tm_view(view.legend.position = "bottom")
        
    
    })
  
   output$indicador<- renderPlot({
     selx_pt<-input$var_x
     
       filter(ind_largo,
              anios=="2020",
         nomx == selx_pt)%>%
       filter(valor %in% c(max(valor),
              min(valor)))%>%
     
            ggplot(aes(x=entidad,
                  y=valor,
                  fill=variable)) +
       geom_bar(position= "dodge",stat="identity")+
        facet_wrap(facets="anios")+
       labs(title = paste(selx_pt),
            y = NULL,
            caption = "Fuente: IMCO ICE2021")+
       theme(axis.text = element_text(angle=90))
     })
  
  
  
    output$Barras<- renderPlot({
      selent_pt<-input$entidad
      selx_pt<-input$var_x
      
      filter(ind_largo,
             entidad %in% selent_pt,
             anios %in% c("2019","2020"),
             nomx%in% selx_pt)%>%
        ggplot(aes(x=entidad,
                   y=valor,
                   fill=variable)) +
        geom_bar(position= "dodge",stat="identity"
                 )+
        facet_wrap(facets="anios")+
        labs(title = paste(selx_pt),
             y = NULL,
             caption = "Fuente: IMCO ICE2021")+
        theme(axis.text = element_text(angle=90))
    })
    
    
    
    output$Barras2<-renderPlot({
      br_si<-input$si
      
      ranks%>%filter(Subindice %in% br_si,
                     Año=="2020")%>%
        filter(Valor == c(max(Valor)
                          ,min(Valor)))%>%
        ggplot(aes(x=Entidad,
                   fill=Grupo
                   )) +
        geom_bar(position= "dodge", stat = "count")+
        geom_text(aes(label = Ranking, y=0.50))+
        guides(fill=guide_legend("Grupo de competitividad"))+
        facet_wrap(facets="Año")+
        labs(title="El mejor y el peor en el Ranking para el subindice",
             subtitle = paste(br_si),
             y = "Ranking",
             caption = "Fuente: IMCO ICE2021")+
        theme(axis.text = element_text(angle=90))
    }
    
    )
    
    
    output$MYM<-renderPlot({
      br_si<-input$si
     br_en<-input$entidad
      ranks%>%filter(Subindice %in% br_si,
                     Entidad %in% br_en)%>%
        ggplot(aes(x=Entidad,
                   fill=Grupo)) +
        geom_bar(position= "dodge",stat="count")+
        geom_text(aes(label = Ranking, y=0.50))+
        guides(fill=guide_legend("Grupo de competitividad"))+
        facet_wrap(facets = "Año")+
        labs(title="Calificacion obtenida en el Ranking",
             subtitle = paste(br_si),
             y = "Ranking",
             caption = "Fuente: IMCO ICE2021")+
        theme(axis.text = element_text(angle=90))
    }
      
    )
    
  
    
    output$Tabla<- DT::renderDataTable({
    tabla<- ranks%>%
      filter(Entidad %in% input$Ent,
             Subindice %in% input$subin)%>%
      select(Entidad, Ranking,Grupo,Subindice, Año)
     
    }
      
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
