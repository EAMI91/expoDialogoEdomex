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
  mutate(categoria = sample(c("grupo 1", "grupo 2", "grupo 3"),prob = c(.2,.55,.35), replace = T, size = nrow(.))) %>% 
  left_join(censo %>% filter(NOM_LOC == "Total AGEB urbana") %>% 
                     transmute(CVE_MUN = MUN, CVE_AGEB = AGEB, POBTOT, TVIVPARHAB))


cat_pct <- ageb %>% as_tibble %>% count(categoria) %>% mutate(pct = n/sum(n))

entidad <- rgdal::readOGR(dsn="data/15ent.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

municipio <- rgdal::readOGR(dsn="data/15mun.shp",encoding = "CP1252") %>% 
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() %>% 
  mutate(categoria = sample(c("grupo 1", "grupo 2", "grupo 3"),prob = c(.2,.55,.35), replace = T, size = nrow(.))) %>% 
  left_join(censo %>% filter(NOM_LOC == "Total del municipio") %>% 
              transmute(CVE_MUN = MUN, POBTOT, TVIVPARHAB))

dialogos <- st_read("data/dialogos.shp")
pal <- colorFactor(c("#03045e", "#ff5400", "#ffc300"), c("grupo 1", "grupo 2", "grupo 3"))

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
      addPolygons(data = select(),  stroke = T, weight = 1, color = ~pal(categoria),
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
      select() %>% as_tibble %>% count(categoria) %>% mutate(color = pal(categoria),
                                                             pct = n/sum(n)) %>% 
        ggplot(aes(x = reorder(categoria,n), y = pct)) + 
        ggchicklet::geom_chicklet(aes(fill = color), width = .7, alpha = .5) + 
        geom_errorbar(data = cat_pct, aes(ymin = pct, ymax =pct )) +
        coord_flip() +
        scale_fill_identity() +
        scale_y_continuous(labels = scales::percent) +
        labs(y = "Porcentaje de AGEB", x = NULL) +
        theme_minimal()
    } else{
      municipio %>% as_tibble %>% count(categoria) %>%
        mutate(color = pal(categoria)) %>% 
        ggplot(aes(x = reorder(categoria, n), y = n, fill = color)) +
        ggchicklet::geom_chicklet(width = .7, alpha = .5) +
        coord_flip() +
        scale_fill_identity() +
        labs(y = "Municipios", x = NULL) +
        theme_minimal()
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
                                                                                aes(xmin = as.numeric(factor(var))-.4, 
                                                                                    xmax = as.numeric(factor(var))+.4, ymin = 1,ymax = 1+pct, fill = cat)) else NULL
    aux %>% filter(cat != "Regular") %>% group_by(var) %>% mutate(nps = sum(pct)) %>% 
      ggplot() +
      ggchicklet::geom_chicklet(aes(x = reorder(var,nps), y = pct, fill = cat)) +
      rectangulo +
      geom_hline(yintercept = 1, linetype = "dotted") +
      coord_flip() +
      labs(x = NULL, y = NULL, fill = NULL) + theme_minimal() + theme(legend.position = "bottom")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
