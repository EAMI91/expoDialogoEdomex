#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
censo <- read_csv("data/censo.csv")
# censo %>% filter(NOM_LOC %in% c("Total AGEB urbana","Total del municipio")) %>% select(MUN, AGEB, NOM_LOC, POBTOT, TVIVPARHAB) %>%
#   write_excel_csv("data/censo.csv")
ageb <- rgdal::readOGR(dsn="data/15a.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() %>% 
  mutate(IRS = sample(c("Bajo", "Medio", "Alto"),prob = c(.2,.55,.35), 
                      replace = T, size = nrow(.)),
         apoyo1 = sample(seq(from = 0, to = .78, by = .08),
                         prob = rnorm(n=10) %>%  abs(), 
                         replace = T, size = nrow(.)),
         apoyo2 = sample(seq(from = 0, to = .22, by = .02),
                         prob = rnorm(n=12) %>%  abs(), 
                         replace = T, size = nrow(.))) %>% 
  left_join(censo %>% filter(NOM_LOC == "Total AGEB urbana") %>% 
              transmute(CVE_MUN = MUN, CVE_AGEB = AGEB, POBTOT, TVIVPARHAB))


cat_pct <- ageb %>% as_tibble %>% count(IRS) %>% mutate(pct = n/sum(n))

entidad <- rgdal::readOGR(dsn="data/15ent.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

municipio <- rgdal::readOGR(dsn="data/15mun.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() %>% 
  mutate(IRS = sample(c("Bajo", "Medio", "Alto"),prob = c(.2,.55,.35),
                      replace = T, size = nrow(.)),
         apoyo1 = sample(seq(from = 0, to = .78, by = .08),
                         prob = rnorm(n=10) %>%  abs(), 
                         replace = T, size = nrow(.)),
         apoyo2 = sample(seq(from = 0, to = .22, by = .02),
                         prob = rnorm(n=12) %>%  abs(), 
                         replace = T, size = nrow(.))) %>% 
  left_join(censo %>% filter(NOM_LOC == "Total del municipio") %>% 
              transmute(CVE_MUN = MUN, POBTOT, TVIVPARHAB))

dialogos <- st_read("data/dialogos.shp")
pal <- colorFactor(c( "#023047", "#FB8500", "#219EBC"), c("Bajo", "Medio", "Alto"))

ui <- tagList(
  
  includeCSS("www/tElectoral.css"),
  dashboardPage(
    header = dashboardHeader(title = "Diálogo Social"),
    sidebar = dashboardSidebar(#expand_on_hover = F,
      sidebarMenu(
        menuItem("Mapa",selected = F,
                 tabName = "mapa",
                 icon = icon("search")
        ),
        hidden(actionButton("regresar","Regresar"))
      )
    ),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "mapa", 
                tagList(
                  
                  fluidRow(
                    column(7,
                           progressBar(id = "progreso", value = nrow(dialogos), total = round(sum(as.numeric(municipio$TVIVPARHAB))*.01), 
                                       status = "primary", display_pct = TRUE, striped = TRUE, title = "Progreso")),
                    column(width = 5,
                           selectInput("municipio",NULL, choices = c("Todo" = "", sort(municipio$NOMGEO))) 
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(7, class = "shadowBox",
                           shinycssloaders::withSpinner(
                             leafletOutput("mapa",height = 600)
                           )
                    ),
                    column(5,
                           fluidRow(
                             column(12, class = "shadowBox",
                                    shinycssloaders::withSpinner(
                                      plotOutput("dif",height = 350)
                                    )
                             )
                           ),
                           fluidRow(
                             column(6, class = "shadowBox",
                                    shinycssloaders::withSpinner(
                                      plotOutput("barras",height = 250)
                                    )        
                             ),
                             column(6, class = "shadowBox",
                                    shinycssloaders::withSpinner(
                                      plotOutput("area",height = 250)
                                    )        
                             )
                           )
                    )
                    
                  )
                )
        )
      )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    leaflet(municipio) %>% addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(data = entidad, stroke = F, color = "gray50", group = "entidad") %>%
      hideGroup("entidad") %>% 
      addPolygons(stroke = T,weight = .5, color = ~pal(IRS),
                  label = ~NOMGEO, layerId = ~NOMGEO, group = "municipio",
                  highlightOptions = highlightOptions(weight = 2, 
                                                      bringToFront = T, color = "#db4471", opacity = 1)) %>% 
      addCircleMarkers(data = dialogos, radius = 1, clusterOptions = markerClusterOptions(), group = "Diálogos") %>% 
      addLegend(pal = pal, values = ~IRS) %>% 
      addLayersControl(overlayGroups = c("Diálogos"))
  })
  
  mapa <- leafletProxy("mapa")
  
  observeEvent(input$mapa_shape_click,{
    if(input$mapa_shape_click$group == "municipio"){
      updateSelectInput(session,"municipio", selected = input$mapa_shape_click)  
    }
  })
  
  observeEvent(slctDiag(),{
    updateProgressBar(session, "progreso", value = nrow(slctDiag()), total = round(sum(as.numeric(slctMun()$TVIVPARHAB))*.01))
  })
  
  slctMun <- eventReactive(input$municipio,{
    req(input$municipio)
    municipio %>% filter(NOMGEO == input$municipio)
  })
  
  select <- eventReactive(slctMun(),{
    ageb %>% filter(CVE_MUN == slctMun()$CVE_MUN)
  })
  
  
  
  observeEvent(select(),{
    shinyjs::show("regresar")
    
    
    bbox <- st_bbox(slctMun())
    content <- glue::glue("<b> Municipio: </b> {input$municipio} <br>
                          <b> Viviendas habitadas: </b> {scales::comma(as.numeric(slctMun()$TVIVPARHAB))} <br>
                          <b> Población total: </b> {scales::comma(as.numeric(slctMun()$POBTOT))} <br>
                          <b> Número de Diálogos: </b> {nrow(slctDiag())} <br>
                          <b> Conocimiento: </b> {scales::percent(runif(1))}
                          ")
    mapa %>% 
      hideGroup("municipio") %>%
      clearGroup("seleccionMun") %>%
      clearGroup("seleccionAgeb") %>%
      clearGroup("Diálogos") %>%
      clearGroup("texto") %>%
      showGroup("entidad") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
      addPolygons(data = slctMun(),  fill = F,
                  stroke = T,weight = 3, color = "black", group = "seleccionMun") %>% 
      addPolygons(data = select(),  stroke = T, weight = 1, color = ~pal(IRS),
                  label = ~CVE_AGEB, popup = ~glue::glue("<b> AGEB: </b> {CVE_AGEB} <br>
                          <b> Viviendas habitadas: </b> {scales::comma(as.numeric(TVIVPARHAB))} <br>
                          <b> Población total: </b> {scales::comma(as.numeric(POBTOT))}
                          "),
                  group = "seleccionAgeb") %>% 
      addCircleMarkers(data = slctDiag(), radius = 1, clusterOptions = markerClusterOptions(),
                       group = "Diálogos") %>% 
      addPopups(mean(c(bbox[[1]],bbox[[3]])), bbox[[4]], content,
                options = popupOptions(closeButton = FALSE),group = "texto"
      )
  })
  
  observeEvent(input$regresar,{
    updateSelectInput(session,"municipio", selected = "")
    updateProgressBar(session, "progreso", value = nrow(dialogos), total = round(sum(as.numeric(municipio$TVIVPARHAB))*.01))
    shinyjs::hide("regresar")
    bbox <- st_bbox(entidad)
    mapa %>% 
      hideGroup("entidad") %>% 
      showGroup("municipio") %>% 
      clearGroup("seleccionMun") %>% 
      clearGroup("seleccionAgeb") %>% 
      clearGroup("texto") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
      addCircleMarkers(data = dialogos, radius = 1, clusterOptions = markerClusterOptions(), group = "Diálogos")
    
  })
  
  output$barras <- renderPlot({
    if(input$municipio != ""){
      select() %>% as_tibble %>% count(IRS) %>% mutate(color = pal(IRS),
                                                       pct = n/sum(n)) %>% 
        ggplot(aes(x = reorder(IRS,n), y = pct)) + 
        ggchicklet::geom_chicklet(aes(fill = color), width = .5, alpha = .5) + 
        geom_errorbar(data = cat_pct, aes(ymin = pct, ymax =pct )) +
        coord_flip() +
        scale_fill_identity() +
        scale_y_continuous(labels = scales::percent) +
        labs(y = "Porcentaje de AGEB", x = NULL, title =  "Índice de rezago social") +
        theme_minimal()+
        theme(panel.grid.major.y= element_blank(),
              panel.grid.minor = element_blank(),
              text = element_text(family = "Poppins"))
    } else{
      municipio %>% as_tibble %>% count(IRS) %>%
        mutate(color = pal(IRS)) %>% 
        ggplot(aes(x = reorder(IRS, n), y = n, fill = color)) +
        ggchicklet::geom_chicklet(width = .5, alpha = .5) +
        coord_flip() +
        scale_fill_identity() +
        labs(y = "Municipios", x = NULL, title =  "Índice de rezago social") +
        theme_minimal()+
        theme(panel.grid.major.y= element_blank(),
              panel.grid.minor = element_blank(),
              text = element_text(family = "Poppins"))
    }
    
  })
  
  output$area <- renderPlot({
    if(input$municipio != ""){
      # browser()
      select() %>% as_tibble %>%  mutate(total_apoyo = apoyo1+apoyo2) %>%
        mutate(total_apoyo = apoyo1+apoyo2) %>% 
        gather(grupo, apoyo,apoyo1:apoyo2 ) %>% 
        mutate(grupo = case_when(grupo =="apoyo1"~"Apoyo previo",
                                 grupo== "apoyo2"~"Apoyo posterior")) %>% 
        ggplot(aes(x = fct_reorder(CVE_AGEB, apoyo), y = apoyo,
                   group = grupo, fill = grupo)) +
        # ggchicklet::geom_chicklet(width = 1, alpha = .5) +
        geom_area(stat = "identity", alpha = .7)+
        labs(x = "Agebs", y = NULL, fill = NULL,
             title =  "Apoyo previo y posterior") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("Apoyo previo" = "#023047",
                                     "Apoyo posterior" = "#219EBC"))+
        theme_minimal()+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x= element_blank(),
              legend.position = "bottom",
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              text = element_text(family = "Poppins"))
    } else{
      municipio %>% as_tibble %>% 
        mutate(total_apoyo = apoyo1+apoyo2) %>% 
        gather(grupo, apoyo,apoyo1:apoyo2 ) %>% 
        mutate(grupo = case_when(grupo =="apoyo1"~"Apoyo previo",
                                 grupo== "apoyo2"~"Apoyo posterior")) %>% 
        ggplot(aes(x = fct_reorder(CVE_MUN, apoyo), y = apoyo,
                   group = grupo, fill = grupo)) +
        # ggchicklet::geom_chicklet(width = 1, alpha = .5) +
        geom_area(stat = "identity", alpha = .7)+
        labs(x = "Municipios", y = NULL, fill = NULL,
             title =  "Apoyo previo y posterior") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("Apoyo previo" = "#023047",
                                     "Apoyo posterior" = "#219EBC"))+
        theme_minimal()+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x= element_blank(),
              legend.position = "bottom",
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              text = element_text(family = "Poppins"))
    }
    
  })
  
  slctDiag <- eventReactive(input$municipio,{
    auxi <- if(input$municipio == "") dialogos else dialogos %>% filter(NOMGEO == input$municipio)
    return(auxi)
  })
  
  output$dif <- renderPlot({
    validate(need(nrow(slctDiag())>0,
                  message = glue::glue("No se han realizado diálogos en {input$municipio}")
    ))
    
    
    aux <- slctDiag() %>% as_tibble  %>% dplyr::select(contains("a_")) %>% names %>%  
      map_df(~slctDiag() %>% as_tibble %>% count(across(.x)) %>% mutate(pct = n/sum(n),var = .x) %>% 
               rename(cat = 1)) %>% mutate(pct = if_else(cat == "Mala",-pct,pct))
    
    rectangulo <- if((aux %>% filter(cat == "Regular") %>% nrow) > 0) geom_rect(data = aux %>% filter(cat == "Regular"), 
                                                                                aes(xmin = as.numeric(factor(var))-.3,
                                                                                    xmax = as.numeric(factor(var))+.3, 
                                                                                    ymin = 1,ymax = 1+pct, fill = cat),
                                                                                alpha= .7, show.legend = F) else NULL
    
    aux %>% filter(cat != "Regular") %>% group_by(var) %>%
      mutate(nps = case_when(cat == "Buena"~pct, T~0), nps = sum(nps)) %>%
      ungroup() %>% 
      arrange(desc(nps)) %>% 
      ggplot() +
      ggchicklet::geom_chicklet(aes(x = fct_reorder(var,nps), y = pct, fill = cat),
                                alpha= .7,
                                width = .6) +
      rectangulo +
      geom_hline(yintercept = 1, linetype = "dotted")+
      scale_fill_manual(values = c("Mala" = "#DE6400",
                                   "Buena" = "#023047",
                                   "Regular" = "gray"))+
      scale_x_discrete(labels=c("a_1" = "De 18 a 29", "a_2" = "De 30 a 39",
                                "a_3" = "De 40 a 49", "a_4" = "De 50 a 59",
                                "a_5" = "60 y más"))+
      scale_y_continuous(
        # labels=scales::percent_format(accuracy = 1),
        breaks = c(-.5, 0, .5, 1, 1.2),
        labels = c("-50%", "0%", "50%", "0%", "20%"))+
      coord_flip() +
      labs(x = NULL, y = NULL, fill = NULL, title = "Opinión por grupos de edad") + 
      theme_minimal() + theme(legend.position = "bottom",
                              panel.grid.major.y= element_blank(),
                              text = element_text(family = "Poppins"))+
      geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
      geom_hline(yintercept = 0, color = "gray", size= .6)+
      geom_hline(yintercept = 1, color = "#FFFFFF", size = 1.2)+
      geom_hline(yintercept = 1, color = "#323232", linetype = "dotted", size = .7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
