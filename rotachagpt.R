library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Mapa Interativo com Atualização de Precipitação"),
  leafletOutput("mapa")
)

# Função para buscar dados meteorológicos
get_weather_data <- function() {
  # URL para a API (substitua "YOUR_API_KEY" pela sua chave de API)
  api_url <- "https://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&appid=e6ea32b7f44749d03b9f81d2f07eb928&units=metric"
  
  # Fazer a requisição para a API
  res <- GET(api_url)
  
  # Processar o resultado
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, "text"), flatten = TRUE)
    return(data)
  } else {
    return(NULL)
  }
}

# Servidor
server <- function(input, output, session) {
  # Função reativa para atualizar o mapa a cada 10 minutos
  observe({
    invalidateLater(600000, session)  # Atualiza a cada 10 minutos (600.000 ms)
    
    # Buscar dados meteorológicos
    weather_data <- get_weather_data()
    
    # Verificar se há dados
    if (!is.null(weather_data)) {
      # Extrair informações
      cidade <- weather_data$name
      temperatura <- weather_data$main$temp
      precipitação <- ifelse(!is.null(weather_data$rain$`1h`), weather_data$rain$`1h`, 0)
      
      # Criar o mapa
      output$mapa <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = -52.1, lat = -32.0, popup = paste("Cidade:", cidade, "<br>Temperatura:", temperatura, "°C<br>Precipitação:", precipitação, "mm"))
      })
    } else {
      # Caso não haja dados, exibir um mapa padrão
      output$mapa <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = -52.1, lat = -32.0, popup = "Sem dados disponíveis")
      })
    }
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)






library(leaflet)
library(sf)
library(osrm)
library(htmlwidgets)

# Função para criar ícones personalizados
create_icons <- function() {
  list(
    bombeiros = awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa'),
    saude = awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa'),
    distribuicao = awesomeIcons(icon = 'building', iconColor = 'white', markerColor = 'blue', library = 'fa'),
    abrigo = awesomeIcons(icon = 'bed', iconColor = 'white', markerColor = 'purple', library = 'fa')
  )
}

# Função para criar os pontos de interesse
create_pontos_interesse <- function() {
  data.frame(
    name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", 
             "Abrigo 01", "Abrigo 02", "Abrigo 03", "Abrigo 04", "Abrigo 05", "Abrigo 06", 
             "Abrigo 07", "Abrigo 08", "Abrigo 09", "Abrigo 10", "Abrigo 11", "Abrigo 12", 
             "Abrigo 13", "Abrigo 14", "Abrigo 15", "Abrigo 16"),
    lat = c(-32.035111, -32.184528, -32.114083, -32.04038, -31.99608, -32.02390, -32.07563, 
            -32.17724, -32.07512, -32.17828, -32.07817, -32.07855, -32.07700, -32.07217, 
            -32.04657, -32.09553, -32.03525, -32.07506, -32.16760),
    lon = c(-52.105528, -52.166111, -52.175278, -52.10052, -52.25032, -52.2487, -52.17996, 
            -52.15971, -52.17957, -52.14572, -52.19077, -52.19079, -52.19024, -52.16065, 
            -52.11592, -52.18284, -52.09896, -52.25398, -52.16963)
  )
}

# Função para criar os postos de saúde
create_postos_saude <- function() {
  data.frame(
    name = c("Posto de Saúde 1", "Posto de Saúde 3"),
    lat = c(-32.035444, -32.091111),
    lon = c(-52.102222, -52.135000)
  )
}

# Função para adicionar rotas ao mapa
add_route_to_map <- function(map, start, end, color) {
  rota <- osrmRoute(src = st_coordinates(start), dst = st_coordinates(end), returnclass = "sf")
  if (!is.null(rota)) {
    dist_km <- round(as.numeric(rota$distance), 2)  # Distância em km
    map <- map %>% 
      addPolylines(data = rota, color = color, weight = 2, opacity = 0.7, 
                   label = paste("Distância:", dist_km, "km"))
  }
  return(map)
}

# Função para adicionar áreas inundadas ao mapa
add_flood_areas <- function(map) {
  coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
  coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.10093, -32.04273, -52.10021, -32.04168, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
  coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10059, -32.04325, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
  
  area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
  area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
  area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
  
  map <- map %>%
    addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
    addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
    addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas")
  
  return(map)
}

# Função para adicionar círculo de inundação
add_inundation_circle <- function(map, lng, lat, radius_km) {
  ponto_inundacao <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  area_1km <- st_buffer(st_transform(ponto_inundacao, crs = 32722), dist = radius_km * 1000) # CRS 32722 é para usar metros
  area_1km <- st_transform(area_1km, crs = 4326) # Voltar para CRS 4326 para usar no Leaflet
  
  map <- map %>%
    addPolygons(data = area_1km, color = "cyan", weight = 2, fillOpacity = 0.2, label = paste("Inundação de", radius_km, "km")) %>%
    addCircleMarkers(lng = lng, lat = lat, radius = 5, color = "red", label = "Ponto Central")
  
  return(map)
}

# Função para adicionar a bússola com dados climáticos e hora de Brasília
add_compass_with_weather <- function(map, wind_speed, wind_direction, temperature, pressure, humidity) {
  wind_direction_label <- get_wind_direction_label(wind_direction)
  hora_brasilia <- format(Sys.time(), tz = "America/Sao_Paulo", usetz = TRUE)
  
  compass_css <- sprintf("
    <style>
      .compass-container {
        position: relative;
        text-align: center;
        margin-top: 10px;
      }
      .compass-icon {
        width: 48px;
        height: 48px;
        transform: rotate(%fdeg);
      }
      .weather-info {
        font-size: 12px;
        margin-top: 5px;
        color: #000;
        background-color: #fff;
        padding: 5px;
        border: 1px solid #ddd;
        border-radius: 3px;
      }
    </style>", wind_direction)
  
  compass_html <- sprintf("
    <div class='compass-container'>
      <img src='https://img.icons8.com/color/48/000000/compass.png' class='compass-icon'/>
      <div class='weather-info'>
        <div>Vento: %.1f km/h (%s)</div>
        <div>Temperatura: %.1f °C</div>
        <div>Pressão: %.1f hPa</div>
        <div>Umidade: %.1f%%</div>
        <div>Hora: %s</div>
      </div>
    </div>", wind_speed, wind_direction_label, temperature, pressure, humidity, hora_brasilia)
  
  map <- map %>% 
    addControl(html = paste(compass_css, compass_html), position = "topright")
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  # Ajustar o fuso horário para América/São_Paulo
  Sys.setenv(TZ="America/Sao_Paulo")
  
  # Dados e ícones
  pontos_interesse <- create_pontos_interesse()
  postos_saude <- create_postos_saude()
  icons <- create_icons()
  
  # Converter para objeto sf
  pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
  postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
  
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    # Adicionar ícones dos bombeiros, centro de distribuição e postos de saúde
    addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons$bombeiros, 
                      label = pontos_interesse$name[1:2], group = "Bombeiros") %>%
    addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons$saude, label = postos_saude$name, 
                      group = "Postos de Saúde") %>%
    addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons$distribuicao, label = "Centro de Distribuição", 
                      group = "Centro de Distribuição") %>%
    # Adicionar ícones dos abrigos
    addAwesomeMarkers(lat = pontos_interesse$lat[4:nrow(pontos_interesse)], lng = pontos_interesse$lon[4:nrow(pontos_interesse)], 
                      icon = icons$abrigo, label = pontos_interesse$name[4:nrow(pontos_interesse)], group = "Abrigos") %>%
    # Adicionar áreas inundadas
    add_flood_areas() %>%
    # Adicionar círculo de inundação de 1 km
    add_inundation_circle(-52.08428, -32.03216, 1) %>%
    
    # Adicionar o relógio dinâmico
    add_dynamic_clock() %>%
    
    # Adicionar controle de camadas
    addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    # Adicionar legenda
    addLegend(position = "topright", colors = c("blue", "red", "green", "purple", "cyan", "orange", "red"), 
              labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", "Abrigos", 
                         "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m"))
  
  # Adicionar a bússola com os dados climáticos e a hora
  mapa_interativo <- add_compass_with_weather(mapa_interativo, wind_speed = 50, wind_direction = 45, 
                                              temperature = 23.5, pressure = 1015.3, humidity = 60)
  
  # Adicionar rotas entre Bombeiros Centro e Abrigos / Postos de Saúde
  for (i in 4:nrow(pontos_interesse_sf)) {
    mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[1, ], pontos_interesse_sf[i, ], "blue")
  }
  for (i in 1:nrow(postos_saude_sf)) {
    mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[1, ], postos_saude_sf[i, ], "blue")
  }
  
  # Adicionar rotas entre Bombeiros Cassino e Abrigos / Postos de Saúde
  for (i in 4:nrow(pontos_interesse_sf)) {
    mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[2, ], pontos_interesse_sf[i, ], "red")
  }
  for (i in 1:nrow(postos_saude_sf)) {
    mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[2, ], postos_saude_sf[i, ], "red")
  }
  
  return(mapa_interativo)
}

# Exibir o mapa final
create_mapa_interativo()



library(leaflet)
library(sf)
library(osrm)
library(httr)
library(jsonlite)

# Definir sua chave de API
api_key <- "e6ea32b7f44749d03b9f81d2f07eb928"

# Função para obter os dados meteorológicos
get_weather_data <- function() {
  url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  
  return(data)
}

# Função para criar a tabela de informações meteorológicas
create_weather_table <- function(weather_data) {
  temperatura <- weather_data$main$temp
  feels_like <- weather_data$main$feels_like
  pressao <- weather_data$main$pressure
  umidade <- weather_data$main$humidity
  vento_speed <- weather_data$wind$speed
  vento_direcao <- weather_data$wind$deg
  nuvens <- weather_data$clouds$all
  
  # Criar tabela em HTML
  tabela_html <- sprintf("
  <table style='width:250px; font-size:12px; border:1px solid black;'>
    <tr><th colspan='2'>Dados Meteorológicos - Rio Grande</th></tr>
    <tr><td>Temperatura:</td><td>%.1f°C</td></tr>
    <tr><td>Sensação Térmica:</td><td>%.1f°C</td></tr>
    <tr><td>Pressão:</td><td>%d hPa</td></tr>
    <tr><td>Umidade:</td><td>%d%%</td></tr>
    <tr><td>Velocidade do Vento:</td><td>%.1f m/s</td></tr>
    <tr><td>Direção do Vento:</td><td>%d°</td></tr>
    <tr><td>Nuvens:</td><td>%d%%</td></tr>
  </table>
  ", temperatura, feels_like, pressao, umidade, vento_speed, vento_direcao, nuvens)
  
  return(tabela_html)
}

# Função para adicionar a tabela no mapa
add_weather_info_to_map <- function(map, tabela_html) {
  map <- map %>%
    addPopups(lng = -52.0976, lat = -32.0335, popup = tabela_html, options = popupOptions(closeButton = TRUE))
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  # Ajustar o fuso horário para América/São_Paulo
  Sys.setenv(TZ="America/Sao_Paulo")
  
  # Obter dados meteorológicos
  weather_data <- get_weather_data()
  
  # Criar a tabela de informações meteorológicas
  tabela_html <- create_weather_table(weather_data)
  
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12)
  
  # Adicionar as informações meteorológicas no mapa
  mapa_interativo <- add_weather_info_to_map(mapa_interativo, tabela_html)
  
  return(mapa_interativo)
}

# Exibir o mapa final
mapa <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML
saveWidget(mapa, file = "mapa_interativo_com_meteorologia.html")
library(leaflet)
library(httr)
library(jsonlite)
library(htmlwidgets)

# Definir sua chave de API
api_key <- "e6ea32b7f44749d03b9f81d2f07eb928"

# Função para obter os dados meteorológicos
get_weather_data <- function() {
  url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    print("Erro ao obter os dados meteorológicos.")
    return(NULL)
  }
}

# Função para criar a tabela de informações meteorológicas
create_weather_table <- function(weather_data) {
  if (is.null(weather_data)) return(NULL)
  
  temperatura <- weather_data$main$temp
  feels_like <- weather_data$main$feels_like
  pressao <- weather_data$main$pressure
  umidade <- weather_data$main$humidity
  vento_speed <- weather_data$wind$speed
  vento_direcao <- weather_data$wind$deg
  nuvens <- weather_data$clouds$all
  
  # Criar tabela em HTML
  tabela_html <- sprintf("
  <table style='width:250px; font-size:12px; border:1px solid black;'>
    <tr><th colspan='2'>Dados Meteorológicos - Rio Grande</th></tr>
    <tr><td>Temperatura:</td><td>%.1f°C</td></tr>
    <tr><td>Sensação Térmica:</td><td>%.1f°C</td></tr>
    <tr><td>Pressão:</td><td>%d hPa</td></tr>
    <tr><td>Umidade:</td><td>%d%%</td></tr>
    <tr><td>Velocidade do Vento:</td><td>%.1f m/s</td></tr>
    <tr><td>Direção do Vento:</td><td>%d°</td></tr>
    <tr><td>Nuvens:</td><td>%d%%</td></tr>
  </table>
  ", temperatura, feels_like, pressao, umidade, vento_speed, vento_direcao, nuvens)
  
  return(tabela_html)
}

# Função para adicionar a tabela no mapa
add_weather_info_to_map <- function(map, tabela_html) {
  if (!is.null(tabela_html)) {
    map <- map %>%
      addPopups(lng = -52.0976, lat = -32.0335, popup = tabela_html, options = popupOptions(closeButton = TRUE))
  } else {
    print("Tabela meteorológica não disponível.")
  }
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  # Obter dados meteorológicos
  weather_data <- get_weather_data()
  
  if (is.null(weather_data)) {
    print("Erro ao obter dados meteorológicos. O mapa será gerado sem informações.")
    return(leaflet() %>% addTiles() %>% setView(lng = -52.0976, lat = -32.0335, zoom = 12))
  }
  
  # Criar a tabela de informações meteorológicas
  tabela_html <- create_weather_table(weather_data)
  
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12)
  
  # Adicionar as informações meteorológicas no mapa
  mapa_interativo <- add_weather_info_to_map(mapa_interativo, tabela_html)
  
  return(mapa_interativo)
}

# Gerar o mapa interativo
mapa <- create_mapa_interativo()

# Verifique se o mapa foi criado corretamente
if (!is.null(mapa)) {
  # Salvar o mapa em um arquivo HTML
  saveWidget(mapa, file = "mapa_interativo_com_meteorologia.html")
  print("Mapa gerado com sucesso e salvo como 'mapa_interativo_com_meteorologia.html'.")
} else {
  print("Falha ao gerar o mapa.")
}

