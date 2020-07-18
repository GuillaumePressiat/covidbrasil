

library(shiny)
library(RColorBrewer)
library(dplyr)
# library(rgdal)
library(leaflet)
library(sf)
library(rmapshaper)
# install.packages('rmapshaper')

dataset <- read.csv(file = "dataset_res.csv", stringsAsFactors = FALSE, encoding = 'latin1') %>%
  select(Codigo_IBGE, Sigla_Estado, cidade, longitude, latitude, Data, Total_Exames, Total_positivos, Indice_Positividade)
dataset$Data <- as.Date(dataset$Data)
dataset$Codigo_IBGE <- as.character(dataset$Codigo_IBGE)

# for one day first
dataset1 <- dataset

# %>% 
#   filter(Data == as.Date('2020-03-10'))

data_uf <- st_read('br_unidades_da_federacao', layer = 'BR_UF_2019')
# plot(data_uf)

# Summarise by UF
dataset1 <- dataset1 %>% 
  group_by(Data, Sigla_Estado) %>% 
  summarise(Total_positivos = sum(Total_positivos, na.rm = TRUE))

data_uf <- data_uf %>% ms_simplify(keep = 0.05)
#plot(data_uf)

data.p <- data_uf

casos <- data_uf %>% 
  left_join(dataset1, by = c('SIGLA_UF' = 'Sigla_Estado')) %>% 
  mutate(popup = paste0(SIGLA_UF," -", NM_UF, " : ", prettyNum(Total_positivos, big.mark = ",")))


data <- casos

pal_fun <- colorNumeric(scico::scico(n = 300, palette = "tokyo", direction = - 1, end = 0.85), data$Total_positivos, na.color = 'grey90')

data <- sf::st_transform(data,sp::CRS('+proj=longlat +datum=WGS84'))
data.p <- sf::st_transform(data.p,sp::CRS('+proj=longlat +datum=WGS84'))


# Just one map one day
tictoc::tic()
leaflet(data  %>%
          filter(Data == lubridate::as_date('2020-03-11'))) %>%
  #addTiles() %>%
  addProviderTiles("CartoDB", options = providerTileOptions(opacity = 1, minZoom = 3, maxZoom = 5), group = "Open Street Map") %>%
  #setView(lng = -100, lat = 40, zoom = 3) %>%
  addPolygons(color = 'white', weight = 1.4,
              group = 'base',
              fillColor = ~pal_fun(Total_positivos),
              fillOpacity = 1, stroke = 2,
              label = ~ popup) %>%
  addLegend("bottomleft", pal = pal_fun, values = ~Total_positivos,
            title = 'Confirmed', opacity = 1)
tictoc::toc()


ui <- bootstrapPage(
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
    #includeHTML("meta.html"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")),
    
  leafletOutput("covid", width = "100%", height = "100%"),
  
  absolutePanel(
    bottom = 20, left = 40, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
    titlePanel("Brasil | Covid"),
    # br(),
    em('data is available mouse on hover'),
    sliderInput("jour",h3(""),
                min = min(dataset1$Data), max = max(dataset1$Data), step = 1, 
                value = max(dataset1$Data),
                animate = animationOptions(interval = 1700, loop = FALSE)),
    

      
    shinyWidgets::prettyRadioButtons('sel_data', 'data', 
                                     choices = c('Total positivos'), 
                                     selected = 'Total positivos', 
                                     shape = "round", animation = "jelly",plain = TRUE,bigger = FALSE,inline = FALSE),
    #shinyWidgets::prettySwitch('pop', "Ratio / 100 000 inhabitants", FALSE),
    #em(tags$small("*à noter sur ce ratio : un patient peut être hospitalisé plus d'une fois")),
    #em(tags$small(br(), "Pour les décès, il s'agit de ceux ayant lieu à l'hôpital")),
    h5(tags$a(href = 'http://github.com/GuillaumePressiat', 'Guillaume Pressiat'), ' & ', 
       tags$a(href = 'http://github.com/fsvm78', 'fsvm78')),
    
    h5(em('Last update : ' , 'not available')),
    
    #br(),
    tags$small(
        tags$li(tags$a(href = 'http://www.fabiocrameri.ch/resources/ScientificColourMaps_FabioCrameri.png', 'Scientific colour maps'), ' with ',
                tags$a(href = 'https://cran.r-project.org/web/packages/scico/index.html', 'scico package'))))
  
  
)

server <- function(input, output) {
  
  # Confirmed, People_Hospitalized, Deaths, People_Tested
  get_data <- reactive({
    temp <- data[which(data$Data == input$jour),]
    if (input$sel_data == "Total positivos"){
      temp$val <- temp$Total_positivos
    } else if (input$sel_data == "People Hospitalized"){
      temp$val <- temp$People_Hospitalized
    } else if (input$sel_data == "People Tested"){
      temp$val <- temp$People_Tested
    } else if (input$sel_data == "Deaths"){
      temp$val <- temp$Deaths
    } else if (input$sel_data == "Recovered"){
      temp$val <- temp$Recovered
    } 
    
    
    
    temp$label <- prettyNum(temp$val, big.mark = ',')
    
    # if (input$pop){
    #   temp$val <- NA
    #   #temp$val <- (temp$val * 100000) / temp$POPESTIMATE2019
    #   #temp$label <- paste0(temp$label, '<br><em>', round(temp$val,1), ' / 100 000 inhab.</em><br>', prettyNum(temp$POPESTIMATE2019, big.mark = ','), ' inhabitants')
    # }
    
    
    return(temp)
    
  })
  
  values_leg <- reactive({
    temp <- data
    if (input$sel_data == "Total positivos"){
      temp$leg <- temp$Total_positivos
    } else if (input$sel_data == "People Hospitalized"){
      temp$leg <- temp$People_Hospitalized
    } else if (input$sel_data == "People Tested"){
      temp$leg <- temp$People_Tested
    } else if (input$sel_data == "Deaths"){
      temp$leg <- temp$Deaths
    } else if (input$sel_data == "Recovered"){
      temp$leg <- temp$Recovered
    } 
    
    # if (input$pop){
    #   temp$leg <- NA ;# (temp$leg * 100000) / temp$POPESTIMATE2019
    # }
    temp <- temp$leg
    # if (input$log){
    # temp <- log(temp)
    # temp[temp < 0] <- 0
    # }
    return(temp)
  })
  
  leg_title <- reactive({
    # if (input$pop){
    #   htmltools::HTML('Nb for<br>100,000<br>inhab.')
    # } else{
    #   'Nb'
    # }
    'Nb'
  })
  
  output$covid <- renderLeaflet({
    leaflet(data = data.p) %>%
      addProviderTiles("CartoDB", options = providerTileOptions(opacity = 1, minZoom = 3, maxZoom = 6), group = "Open Street Map") %>%
      
      addPolygons(group = 'base', 
                  fillColor = NA, 
                  color = 'white',
                  weight = 1.5)  %>%
      addLegend(pal = pal(), values = values_leg(), opacity = 1, title = leg_title(), 
                position = "topright", na.label = 'No&nbsp;data', )
  })
  
  
  
  pal <- reactive({
    
    if (input$sel_data != "Recovered"){
      return(colorNumeric(scico::scico(n = 300, palette = "tokyo", direction = - 1, end = 0.85), values_leg(), na.color = '#c1c1d7'))
    } else { 
      return(colorNumeric(scico::scico(n = 300, palette = "oslo", direction = - 1, begin = 0.2, end = 0.85), domain = values_leg(), na.color = '#808080'))
    }
  })
  
  
  observe({
    if(input$jour == min(dataset1$Data)){
      data <- get_data()
      leafletProxy('covid', data = data) %>%
        clearGroup('polygons') %>%
        addPolygons(group = 'polygons', 
                    fillColor = ~pal()(val), 
                    fillOpacity = 1, 
                    stroke = 2,
                    color = 'white',
                    weight = 1.5, label = ~ lapply(paste0("<b>", CD_UF, " - ", NM_UF, "</b><br>",Data, ' : ', label), htmltools::HTML))
    } else {
      data <- get_data()
      leafletProxy('covid', data = data) %>%
        #clearGroup('polygons') %>%
        addPolygons(group = 'polygons',
                    fillColor = ~pal()(val), 
                    fillOpacity = 1, 
                    stroke = 2,
                    color = 'white',
                    weight = 1.5, label = ~ lapply(paste0("<b>", CD_UF, " - ", NM_UF, "</b><br>", Data, ' : ', label), htmltools::HTML))
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

