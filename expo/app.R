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

ageb <- rgdal::readOGR(dsn="data/15a.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() %>% 
  mutate(categoria = sample(c("Bajo", "Medio", "Alto"),prob = c(.2,.55,.35), replace = T, size = nrow(.)))

cat_pct <- ageb %>% as_tibble %>% count(categoria) %>% mutate(pct = n/sum(n))

entidad <- rgdal::readOGR(dsn="data/15ent.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

municipio <- rgdal::readOGR(dsn="data/15mun.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() %>% 
  mutate(categoria = sample(c("Bajo", "Medio", "Alto"),prob = c(.2,.55,.35), replace = T, size = nrow(.)))

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
        )
      )
    ),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "mapa", 
                tagList(
                  fluidRow(
                    column(7,
                           hidden(actionButton("regresar","Regresar"))
                    ),
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
                                      plotOutput("barras",height = 400)
                                    )        
                             )
                           ),
                           fluidRow(
                             column(12, class = "shadowBox",
                                    shinycssloaders::withSpinner(
                                      plotOutput("dif",height = 200)
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
      addPolygons(stroke = T,weight = .5, color = ~pal(categoria),
                  label = ~NOMGEO, layerId = ~NOMGEO, group = "municipio",
                  highlightOptions = highlightOptions(weight = 2, 
                                                      bringToFront = T, color = "#db4471", opacity = 1)) %>% 
      addCircleMarkers(data = dialogos, radius = 1, clusterOptions = markerClusterOptions(), group = "Diálogos") %>% 
      addLegend(pal = pal, values = ~categoria) %>% 
      addLayersControl(overlayGroups = c("Diálogos"))
  })
  
  mapa <- leafletProxy("mapa")
  
  observeEvent(input$mapa_shape_click,{
    updateSelectInput(session,"municipio", selected = input$mapa_shape_click)
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
    mapa %>% 
      hideGroup("municipio") %>%
      clearGroup("seleccionMun") %>%
      clearGroup("seleccionAgeb") %>%
      clearGroup("Diálogos") %>%
      showGroup("entidad") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
      addPolygons(data = slctMun(),  fill = F,
                  stroke = T,weight = 3, color = "black", group = "seleccionMun") %>% 
      addPolygons(data = select(),  stroke = T, weight = 1, color = ~pal(categoria),
                  label = ~CVE_AGEB, 
                  group = "seleccionAgeb") %>% 
      addCircleMarkers(data = slctDiag(), radius = 1, clusterOptions = markerClusterOptions(),
                       group = "Diálogos")
  })
  
  observeEvent(input$regresar,{
    updateSelectInput(session,"municipio", selected = "")
    shinyjs::hide("regresar")
    bbox <- st_bbox(entidad)
    mapa %>% 
      hideGroup("entidad") %>% 
      showGroup("municipio") %>% 
      clearGroup("seleccionMun") %>% 
      clearGroup("seleccionAgeb") %>% 
      clearGroup("seleccionDialog") %>% 
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
      addCircleMarkers(data = dialogos, radius = 1, clusterOptions = markerClusterOptions(), group = "Diálogos")
    
  })
  
  output$barras <- renderPlot({
    if(input$municipio != ""){
      select() %>% as_tibble %>% count(categoria) %>% mutate(color = pal(categoria),
                                                             pct = n/sum(n)) %>% 
        ggplot(aes(x = reorder(categoria,n), y = pct)) + 
        ggchicklet::geom_chicklet(aes(fill = color), width = .5, alpha = .5) + 
        geom_errorbar(data = cat_pct, aes(ymin = pct, ymax =pct )) +
        coord_flip() +
        scale_fill_identity() +
        scale_y_continuous(labels = scales::percent) +
        labs(y = "Porcentaje de AGEB", x = NULL, title =  "Índice de rezago social") +
        theme_minimal()+
        theme(panel.grid.major.y= element_blank())
    } else{
      municipio %>% as_tibble %>% count(categoria) %>%
        mutate(color = pal(categoria)) %>% 
        ggplot(aes(x = reorder(categoria, n), y = n, fill = color)) +
        ggchicklet::geom_chicklet(width = .5, alpha = .5) +
        coord_flip() +
        scale_fill_identity() +
        labs(y = "Municipios", x = NULL, title =  "Índice de rezago social") +
        theme_minimal()+
        theme(panel.grid.major.y= element_blank())
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

    aux %>% filter(cat != "Regular") %>% group_by(var) %>%
      mutate(nps = case_when(cat == "Buena"~pct, T~0), nps = sum(nps)) %>%
      ungroup() %>% 
      arrange(desc(nps)) %>% 
      ggplot() +
      ggchicklet::geom_chicklet(aes(x = fct_reorder(var,nps), y = pct, fill = cat),
                                alpha= .7,
                                width = .6) +
      geom_rect(data = aux %>% filter(cat == "Regular"), 
                aes(xmin = as.numeric(factor(var))-.3,
                    xmax = as.numeric(factor(var))+.3, 
                    ymin = 1,ymax = 1+pct, fill = cat),
                alpha= .7, show.legend = F) +
      geom_hline(yintercept = 1, linetype = "dotted")+
      scale_fill_manual(values = c("Mala" = "#DE6400",
                                   "Buena" = "#023047",
                                   "Regular" = "gray"))+
      scale_x_discrete(labels=c("a_1" = "De 18 a 29", "a_2" = "De 30 a 39",
                               "a_3" = "De 40 a 49", "a_4" = "De 50 a 59",
                               "a_5" = "60 y más"))+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
      coord_flip() +
      labs(x = NULL, y = NULL, fill = NULL, title = "Opinión por grupos de edad") + 
      theme_minimal() + theme(legend.position = "bottom",
                              panel.grid.major.y= element_blank())+
      geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
      geom_hline(yintercept = 0, color = "gray", size= .6)+
      geom_hline(yintercept = 1, color = "#FFFFFF", size = 1.2)+
      geom_hline(yintercept = 1, color = "#323232", linetype = "dotted", size = .7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
