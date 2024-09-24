#pacotes necessários
install.packages(c("leaflet", "sf", "osrm", "leaflet.extras", "ggplot2"))
library(leaflet)
library(sf)
library(osrm)
library(leaflet.extras)
library(ggplot2)

# Dados dos pontos de interesse

pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", 
           "Abrigo 01", "Abrigo 02", "Abrigo 03", "Abrigo 04", "Abrigo 05"),
  lat = c(-32.035111, -32.184528, -32.114083, -32.04038, -32.02390, -32.07563, -32.17724, -32.07512),
  lon = c(-52.105528, -52.166111, -52.175278, -52.10052, -52.2487, -52.17996, -52.15971, -52.17957)
)

postos_saude <- data.frame(
  name = c("Posto de Saúde 1", "Posto de Saúde 3"),
  lat = c(-32.035444, -32.091111),
  lon = c(-52.102222, -52.135000)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)

# Função para adicionar rotas
add_route_to_map <- function(map, start, end, color) {
  rota <- osrmRoute(src = st_coordinates(start), dst = st_coordinates(end), returnclass = "sf")
  if (!is.null(rota)) {
    map <- map %>% addPolylines(data = rota, color = color, weight = 2, opacity = 0.7)
  }
  return(map)
}

# Função para desenhar setas de vento
add_wind_arrow <- function(map, lat, lng, speed, direction) {
  angle <- direction * pi / 180
  dx <- cos(angle) * speed / 100  # Ajuste da seta
  dy <- sin(angle) * speed / 100
  map %>%
    addPolylines(lng = c(lng, lng + dx), lat = c(lat, lat + dy),
                 color = "black", weight = 2, opacity = 1)
}
# Áreas de inundação (20cm, 50cm, 1m)
coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.10021, -32.04168, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10059, -32.04325, -52.10800, -32.03265), ncol = 2, byrow = TRUE)

# Converter para sf
area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addTiles() %>%
  
  # Adicionar ícones de Bombeiros, Centro de Distribuição, Abrigos, Postos de Saúde
  addCircleMarkers(data = pontos_interesse_sf, color = "red", radius = 5, label = ~name, group = "Abrigos e Bombeiros") %>%
  addCircleMarkers(data = postos_saude_sf, color = "green", radius = 5, label = ~name, group = "Postos de Saúde") %>%
  
  # Adicionar áreas de inundação
  addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
  addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
  addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas") %>%
  
  # Adicionar rotas
  add_route_to_map(pontos_interesse_sf[1, ], pontos_interesse_sf[2, ], "blue") %>%
  add_route_to_map(pontos_interesse_sf[1, ], pontos_interesse_sf[3, ], "blue") %>%
  
  # Adicionar seta de vento
  add_wind_arrow(lat = -32.03, lng = -52.09, speed = 20, direction = 180) %>%
  
  # Adicionar legendas e controles de camadas
  addLegend(position = "topright", colors = c("blue", "red", "green", "cyan", "orange", "red"),
            labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", 
                       "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m")) %>%
  addLayersControl(overlayGroups = c("Abrigos e Bombeiros", "Postos de Saúde", "Áreas Inundadas"),
                   options = layersControlOptions(collapsed = FALSE))

# Exibir o mapa
mapa_interativo


















#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
install.packages("osrm")
install.packages(c("leaflet", "sf", "ggplot2", "leaflet.extras"))
install.packages("leaflet")
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(osrm)
library(sf)



# Dados dos pontos de interesse
pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
  lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
  lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
)

postos_saude <- data.frame(
  name = c("Posto de Saúde 1", "Posto de Saúde 3"),
  lat = c(-32.035444, -32.091111),
  lon = c(-52.102222, -52.135000)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)

# Exemplo de dados para Bombeiros Centro e Bombeiros Cassino
bombeiros_centro_coords <- data.frame(
  name = "Bombeiros Centro",
  lon = -52.105528,  # Coordenadas para Bombeiros Centro
  lat = -32.035111   # Coordenadas para Bombeiros Centro
)

bombeiros_cassino_coords <- data.frame(
  name = "Bombeiros Cassino",
  lon = -52.166111,  # Coordenadas para Bombeiros Cassino
  lat = -32.184528   # Coordenadas para Bombeiros Cassino
)

# Converter para objetos sf
bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)

# Função para traçar a rota e adicionar ao mapa
add_route_to_map <- function(map, start, end, color) {
  start_coords <- st_coordinates(start)
  end_coords <- st_coordinates(end)
  
  # Verificar se as coordenadas são válidas
  if (any(is.na(start_coords)) || any(is.na(end_coords))) {
    warning("Coordenadas inválidas detectadas. Rotas não adicionadas.")
    return(map)
  }
  
  # Adicionar a rota ao mapa
  map %>%
    addPolylines(
      lng = c(start_coords[1], end_coords[1]),
      lat = c(start_coords[2], end_coords[2]),
      color = color
    )
}

# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(data = pontos_interesse_sf, 
                   color = ~case_when(
                     name == "Centro de Distribuição" ~ "green",
                     name == "Abrigo no Centro" ~ "blue",
                     grepl("Corpo de Bombeiros", name) ~ "red"
                   ),
                   radius = 5, 
                   label = ~name) %>%
  addCircleMarkers(data = postos_saude_sf, color = "purple", radius = 5, label = ~name) %>%
  setView(lng = mean(pontos_interesse$lon), lat = mean(pontos_interesse$lat), zoom = 14)

# Adicionar rotas entre Corpo de Bombeiros e outros locais
for (bombeiro in c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino")) {
  for (local in pontos_interesse_sf$name[!grepl("Corpo de Bombeiros", pontos_interesse_sf$name)]) {
    mapa_interativo <- add_route_to_map(
      mapa_interativo,
      start = pontos_interesse_sf[pontos_interesse_sf$name == bombeiro, ],
      end = pontos_interesse_sf[pontos_interesse_sf$name == local, ],
      color = ifelse(bombeiro == "Corpo de Bombeiros Centro", "blue", "red")
    )
  }
}

# Adicionar rotas entre Abrigos e Postos de Saúde
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  for (posto in postos_saude_sf$name) {
    mapa_interativo <- add_route_to_map(
      mapa_interativo,
      start = pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
      end = postos_saude_sf[postos_saude_sf$name == posto, ],
      color = "orange"
    )
  }
}

# Dados dos pontos de interesse
pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
  lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
  lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
)

postos_saude <- data.frame(
  name = c("Posto de Saúde 1", "Posto de Saúde 3"),
  lat = c(-32.035444, -32.091111),
  lon = c(-52.102222, -52.135000)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)

# Exemplo de dados para Bombeiros Centro e Bombeiros Cassino
bombeiros_centro_coords <- data.frame(
  name = "Bombeiros Centro",
  lon = -51.15,  # Substitua pelos valores corretos
  lat = -30.05   # Substitua pelos valores corretos
)
bombeiros_cassino_coords <- data.frame(
  name = "Bombeiros Cassino",
  lon = -51.16,  # Substitua pelos valores corretos
  lat = -30.06   # Substitua pelos valores corretos
)

# Converter para objetos sf
bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)

# Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
add_route_to_map <- function(map, start, end, color) {
  start_coords <- st_coordinates(start)
  end_coords <- st_coordinates(end)
  
  # Calcular a rota com OSRM
  rota <- osrmRoute(
    src = start_coords,
    dst = end_coords,
    returnclass = "sf"
  )
  
  # Verifique se a rota foi calculada com sucesso
  if (!is.null(rota)) {
    # Adicionar a rota ao mapa
    map <- map %>%
      addPolylines(
        data = rota,
        color = color,
        weight = 2,
        opacity = 0.7
      )
  } else {
    warning("Falha ao calcular a rota.")
  }
  
  return(map)
}

# Adicionar rotas entre Bombeiros Centro e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

# Adicionar rotas entre Bombeiros Cassino e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "red"  # Cor para Bombeiros Cassino
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
    "red"  # Cor para Bombeiros Cassino
  )
}

# Adicionar rotas entre Abrigos e Postos de Saúde
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  for (posto in postos_saude_sf$name) {
    mapa_interativo <- add_route_to_map(
      mapa_interativo,
      pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
      postos_saude_sf[postos_saude_sf$name == posto, ],
      "orange"  # Cor para as rotas entre Abrigos e Postos de Saúde
    )
  }
}


# Dados dos pontos de interesse
pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
  lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
  lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
)

postos_saude <- data.frame(
  name = c("Posto de Saúde 1", "Posto de Saúde 3"),
  lat = c(-32.035444, -32.091111),
  lon = c(-52.102222, -52.135000)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)

# Coordenadas para Bombeiros Centro e Cassino
bombeiros_centro_coords <- data.frame(
  name = "Bombeiros Centro",
  lon = -52.105528,  # Coordenadas para Bombeiros Centro
  lat = -32.035111   # Coordenadas para Bombeiros Centro
)

bombeiros_cassino_coords <- data.frame(
  name = "Bombeiros Cassino",
  lon = -52.166111,  # Coordenadas para Bombeiros Cassino
  lat = -32.184528   # Coordenadas para Bombeiros Cassino
)

# Converter para objetos sf
bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)

# Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
add_route_to_map <- function(map, start, end, color) {
  start_coords <- st_coordinates(start)
  end_coords <- st_coordinates(end)
  
  # Calcular a rota com OSRM
  rota <- osrmRoute(
    src = start_coords,
    dst = end_coords,
    returnclass = "sf"
  )
  
  # Verifique se a rota foi calculada com sucesso
  if (!is.null(rota)) {
    # Adicionar a rota ao mapa
    map <- map %>%
      addPolylines(
        data = rota,
        color = color,
        weight = 2,
        opacity = 0.7
      )
  } else {
    warning("Falha ao calcular a rota.")
  }
  
  return(map)
}

# Adicionar rotas entre Bombeiros Centro e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

# Adicionar rotas entre Bombeiros Cassino e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "red"  # Cor para Bombeiros Cassino
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
    "red"  # Cor para Bombeiros Cassino
  )
}

# Adicionar rotas entre Abrigos e Postos de Saúde
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  for (posto in postos_saude_sf$name) {
    mapa_interativo <- add_route_to_map(
      mapa_interativo,
      pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
      postos_saude_sf[postos_saude_sf$name == posto, ],
      "orange"  # Cor para as rotas entre Abrigos e Postos de Saúde
    )
  }
}

# Definir a área de interesse (Rio Grande, por exemplo)
area <- opq(bbox = c(-52.3, -32.2, -51.9, -32)) %>%
  add_osm_feature(key = "highway")  # Extraindo apenas as estradas

# Baixar os dados OSM
ruas <- osmdata_sf(area)

# Filtrar as ruas principais
ruas_linhas <- ruas$osm_lines

# Mapa interativo com as ruas
mapa <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = ruas_linhas, color = "blue", weight = 1)

mapa

# Carregar os dados de rotas (supondo que você exportou as rotas como GeoJSON)
rotas_centro_dist <- st_read("rotas_centro_dist.geojson")
rotas_cassino_dist <- st_read("rotas_cassino_dist.geojson")
rotas_centro_abrigos <- st_read("rotas_centro_abrigos.geojson")
rotas_cassino_abrigos <- st_read("rotas_cassino_abrigos.geojson")

# Criar o mapa
mapa <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = rotas_centro_dist, color = "blue", weight = 3, group = "Rota Bombeiros Centro - Centro de Distribuição") %>%
  addPolylines(data = rotas_cassino_dist, color = "red", weight = 3, group = "Rota Bombeiros Cassino - Centro de Distribuição") %>%
  addPolylines(data = rotas_centro_abrigos, color = "green", weight = 3, group = "Rota Bombeiros Centro - Abrigos") %>%
  addPolylines(data = rotas_cassino_abrigos, color = "purple", weight = 3, group = "Rota Bombeiros Cassino - Abrigos") %>%
  addLegend(position = "topright", colors = c("blue", "red", "green", "purple"),
            labels = c("Bombeiros Centro - Centro de Distribuição", 
                       "Bombeiros Cassino - Centro de Distribuição",
                       "Bombeiros Centro - Abrigos", 
                       "Bombeiros Cassino - Abrigos"))

mapa

# Defina o diretório de trabalho onde os arquivos GeoJSON estão localizados
setwd("caminho/para/seu/diretório")

# Carregar os dados de rotas
rotas_centro_dist <- st_read("rotas_centro_dist.geojson")
rotas_cassino_dist <- st_read("rotas_cassino_dist.geojson")
rotas_centro_abrigos <- st_read("rotas_centro_abrigos.geojson")
rotas_cassino_abrigos <- st_read("rotas_cassino_abrigos.geojson")

# Criar o mapa
mapa <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = rotas_centro_dist, color = "blue", weight = 3, group = "Rota Bombeiros Centro - Centro de Distribuição") %>%
  addPolylines(data = rotas_cassino_dist, color = "red", weight = 3, group = "Rota Bombeiros Cassino - Centro de Distribuição") %>%
  addPolylines(data = rotas_centro_abrigos, color = "green", weight = 3, group = "Rota Bombeiros Centro - Abrigos") %>%
  addPolylines(data = rotas_cassino_abrigos, color = "purple", weight = 3, group = "Rota Bombeiros Cassino - Abrigos") %>%
  addLegend(position = "topright", colors = c("blue", "red", "green", "purple"),
            labels = c("Bombeiros Centro - Centro de Distribuição", 
                       "Bombeiros Cassino - Centro de Distribuição",
                       "Bombeiros Centro - Abrigos", 
                       "Bombeiros Cassino - Abrigos"))

# Verifique o diretório de trabalho atual
getwd()

# Defina um diretório de trabalho válido onde os arquivos GeoJSON estão localizados
setwd("C:/caminho/para/seu/diretório")  # Use o caminho absoluto correto para o diretório onde os arquivos estão localizados

# Carregar os dados de rotas
rotas_centro_dist <- st_read("rotas_centro_dist.geojson")
rotas_cassino_dist <- st_read("rotas_cassino_dist.geojson")
rotas_centro_abrigos <- st_read("rotas_centro_abrigos.geojson")
rotas_cassino_abrigos <- st_read("rotas_cassino_abrigos.geojson")

# Criar o mapa
mapa <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = rotas_centro_dist, color = "blue", weight = 3, group = "Rota Bombeiros Centro - Centro de Distribuição") %>%
  addPolylines(data = rotas_cassino_dist, color = "red", weight = 3, group = "Rota Bombeiros Cassino - Centro de Distribuição") %>%
  addPolylines(data = rotas_centro_abrigos, color = "green", weight = 3, group = "Rota Bombeiros Centro - Abrigos") %>%
  addPolylines(data = rotas_cassino_abrigos, color = "purple", weight = 3, group = "Rota Bombeiros Cassino - Abrigos") %>%
  addLegend(position = "topright", colors = c("blue", "red", "green", "purple"),
            labels = c("Bombeiros Centro - Centro de Distribuição", 
                       "Bombeiros Cassino - Centro de Distribuição",
                       "Bombeiros Centro - Abrigos", 
                       "Bombeiros Cassino - Abrigos"))

# Dados de exemplo
pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Centro de Distribuição"),
  lat = c(-32.035111, -32.114083),
  lon = c(-52.105528, -52.175278)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)

# Função para traçar a rota e adicionar ao mapa
add_route_to_map <- function(map, start, end, color) {
  start_coords <- st_coordinates(start)
  end_coords <- st_coordinates(end)
  
  # Calcular a rota com OSRM
  rota <- osrmRoute(
    src = start_coords,
    dst = end_coords,
    returnclass = "sf"
  )
  
  if (!is.null(rota)) {
    # Adicionar a rota ao mapa
    map <- map %>%
      addPolylines(
        data = rota,
        color = color,
        weight = 2,
        opacity = 0.7
      )
  } else {
    warning("Falha ao calcular a rota.")
  }
  
  return(map)
}

# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = pontos_interesse_sf, color = "blue", radius = 5, label = ~name)

# Adicionar rota de exemplo
mapa_interativo <- add_route_to_map(
  mapa_interativo,
  pontos_interesse_sf[1, ],
  pontos_interesse_sf[2, ],
  "red"
)

# Dados dos pontos de interesse
pontos_interesse <- data.frame(
  name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
  lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
  lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
)

postos_saude <- data.frame(
  name = c("Posto de Saúde 1", "Posto de Saúde 3"),
  lat = c(-32.035444, -32.091111),
  lon = c(-52.102222, -52.135000)
)

# Converter para objeto sf
pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)

# Coordenadas para Bombeiros Centro e Bombeiros Cassino
bombeiros_centro_coords <- data.frame(
  name = "Bombeiros Centro",
  lon = -52.105528,  # Coordenadas para Bombeiros Centro
  lat = -32.035111   # Coordenadas para Bombeiros Centro
)

bombeiros_cassino_coords <- data.frame(
  name = "Bombeiros Cassino",
  lon = -52.166111,  # Coordenadas para Bombeiros Cassino
  lat = -32.184528   # Coordenadas para Bombeiros Cassino
)

# Converter para objetos sf
bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)

# Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
add_route_to_map <- function(map, start, end, color) {
  start_coords <- st_coordinates(start)
  end_coords <- st_coordinates(end)
  
  # Calcular a rota com OSRM
  rota <- osrmRoute(
    src = start_coords,
    dst = end_coords,
    returnclass = "sf"
  )
  
  # Verifique se a rota foi calculada com sucesso
  if (!is.null(rota)) {
    # Adicionar a rota ao mapa
    map <- map %>%
      addPolylines(
        data = rota,
        color = color,
        weight = 2,
        opacity = 0.7
      )
  } else {
    warning("Falha ao calcular a rota.")
  }
  
  return(map)
}

# Adicionar rotas entre Bombeiros Centro e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_centro_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
    "blue"  # Cor para Bombeiros Centro
  )
}

# Adicionar rotas entre Bombeiros Cassino e Abrigos / Centros de Distribuição
for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
    "red"  # Cor para Bombeiros Cassino
  )
}

for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
  mapa_interativo <- add_route_to_map(
    mapa_interativo,
    bombeiros_cassino_sf,
    pontos_interesse
    
    
  # Dados dos pontos de interesse
    pontos_interesse <- data.frame(
      name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
      lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
      lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
    )
    
    postos_saude <- data.frame(
      name = c("Posto de Saúde 1", "Posto de Saúde 3"),
      lat = c(-32.035444, -32.091111),
      lon = c(-52.102222, -52.135000)
    )
    
    # Converter para objeto sf
    pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
    postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
    
    # Coordenadas para Bombeiros Centro e Bombeiros Cassino
    bombeiros_centro_coords <- data.frame(
      name = "Bombeiros Centro",
      lon = -52.105528,  # Coordenadas para Bombeiros Centro
      lat = -32.035111   # Coordenadas para Bombeiros Centro
    )
    
    bombeiros_cassino_coords <- data.frame(
      name = "Bombeiros Cassino",
      lon = -52.166111,  # Coordenadas para Bombeiros Cassino
      lat = -32.184528   # Coordenadas para Bombeiros Cassino
    )
    
    # Converter para objetos sf
    bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
    bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)
    
    # Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
    add_route_to_map <- function(map, start, end, color) {
      start_coords <- st_coordinates(start)
      end_coords <- st_coordinates(end)
      
      # Calcular a rota com OSRM
      rota <- osrmRoute(
        src = s

        # Dados dos pontos de interesse
        pontos_interesse <- data.frame(
          name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo no Centro"),
          lat = c(-32.035111, -32.184528, -32.114083, -32.040167),
          lon = c(-52.105528, -52.166111, -52.175278, -52.100444)
        )
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 3"),
          lat = c(-32.035444, -32.091111),
          lon = c(-52.102222, -52.135000)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Coordenadas para Bombeiros Centro e Bombeiros Cassino
        bombeiros_centro_coords <- data.frame(
          name = "Bombeiros Centro",
          lon = -52.105528,  # Coordenadas para Bombeiros Centro
          lat = -32.035111   # Coordenadas para Bombeiros Centro
        )
        
        bombeiros_cassino_coords <- data.frame(
          name = "Bombeiros Cassino",
          lon = -52.166111,  # Coordenadas para Bombeiros Cassino
          lat = -32.184528   # Coordenadas para Bombeiros Cassino
        )
        
        # Converter para objetos sf
        bombeiros_centro_sf <- st_as_sf(bombeiros_centro_coords, coords = c("lon", "lat"), crs = 4326)
        bombeiros_cassino_sf <- st_as_sf(bombeiros_cassino_coords, coords = c("lon", "lat"), crs = 4326)
        
        # Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
        add_route_to_map <- function(map, start, end, color) {
          start_coords <- st_coordinates(start)
          end_coords <- st_coordinates(end)
          
          # Calcular a rota com OSRM
          rota <- osrmRoute(
            src = start_coords,
            dst = end_coords
          )
          
          # Verifique se a rota foi calculada com sucesso
          if (!is.null(rota)) {
            # Adicionar a rota ao mapa
            map <- map %>%
              addPolylines(
                data = rota,
                color = color,
                weight = 2,
                opacity = 0.7
              )
          } else {
            warning("Falha ao calcular a rota.")
          }
          
          return(map)
        }
        
          # Adicionar rotas entre Bombeiros Centro e Abrigos / Centros de Distribuição
        for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(
            mapa_interativo,
            bombeiros_centro_sf,
            pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
            "blue"  # Cor para Bombeiros Centro
          )
        }
        
        for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(
            mapa_interativo,
            bombeiros_centro_sf,
            pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
            "blue"  # Cor para Bombeiros Centro
          )
        }
        
        # Adicionar rotas entre Bombeiros Cassino e Abrigos / Centros de Distribuição
        for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(
            mapa_interativo,
            bombeiros_cassino_sf,
            pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
            "red"  # Cor para Bombeiros Cassino
          )
        }
        
        for (centro_distribuicao in pontos_interesse_sf$name[grepl("Centro de Distribuição", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(
            mapa_interativo,
            bombeiros_cassino_sf,
            pontos_interesse_sf[pontos_interesse_sf$name == centro_distribuicao, ],
            "red"  # Cor para Bombeiros Cassino
          )
        }
        
        # Adicionar rotas entre Abrigos e Postos de Saúde
        for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
          for (posto in postos_saude_sf$name) {
            mapa_interativo <- add_route_to_map(
              mapa_interativo,
              pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ],
              postos_saude_sf[postos_saude_sf$name == posto, ],
              "orange"  # Cor para as rotas entre Abrigos e Postos de Saúde
            )
          }
        }
   
       # Dados dos pontos de interesse
        pontos_interesse <- data.frame(
          name = c("Corpo de Bombeiros Centro", "Centro de Distribuição"),
          lat = c(-32.035111, -32.114083),
          lon = c(-52.105528, -52.175278)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        
        # Coordenadas para Corpo de Bombeiros e Centro de Distribuição
        corpo_bombeiros_coords <- st_coordinates(pontos_interesse_sf[pontos_interesse_sf$name == "Corpo de Bombeiros Centro", ])
        centro_distribuicao_coords <- st_coordinates(pontos_interesse_sf[pontos_interesse_sf$name == "Centro de Distribuição", ])
        
        # Função para adicionar a rota ao mapa usando OSRM para seguir as ruas
        add_route_to_map <- function(map, start_coords, end_coords, color) {
          rota <- osrmRoute(
            src = start_coords,
            dst = end_coords
          )
          
          # Verifique se a rota foi calculada com sucesso
          if (!is.null(rota)) {
            # Adicionar a rota ao mapa
            map <- map %>%
              addPolylines(
                data = rota,
                color = color,
                weight = 2,
                opacity = 0.7
              )
          } else {
            warning("Falha ao calcular a rota.")
          }
          
          return(map)
        }
      
        # Adicionar a rota inicial do Corpo de Bombeiros ao Centro de Distribuição
        mapa_interativo <- add_route_to_map(
          mapa_interativo,
          corpo_bombeiros_coords,
          centro_distribuicao_coords,
          "blue"  # Cor para a rota inicial
        )
        
        # Adicionar pontos intermediários para simular mudanças de direção
        # Aqui você deve calcular pontos intermediários com base na lógica de virada à esquerda e direita
        # Estes pontos são apenas exemplos e devem ser ajustados para simular as quadras e mudanças de direção reais
        intermediate_points <- data.frame(
          lat = c(-32.035111, -32.037000, -32.038000, -32.040167),
          lon = c(-52.105528, -52.107000, -52.108000, -52.100444)
        )
        
        intermediate_sf <- st_as_sf(intermediate_points, coords = c("lon", "lat"), crs = 4326)
        
        # Adicionar a rota com pontos intermediários ao mapa
        for (i in 1:(nrow(intermediate_sf) - 1)) {
          start_coords <- st_coordinates(intermediate_sf[i, ])
          end_coords <- st_coordinates(intermediate_sf[i + 1, ])
          
          mapa_interativo <- add_route_to_map(
            mapa_interativo,
            start_coords,
            end_coords,
            "green"  # Cor para as rotas intermediárias
          )
        }
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles()
        # Exibir o mapa final
        mapa_interativo          
    
        
        
        # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
          name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo 01", "Abrigo 02", "Abrigo 03", "Abrigo 04", "Abrigo 05", "Abrigo 06", "Abrigo 07", "Abrigo 08", "Abrigo 09", "Abrigo 10", "Abrigo 11", "Abrigo 12", "Abrigo 13", "Abrigo 14", "Abrigo 15", "Abrigo 16"),
          lat = c(-32.035111, -32.184528, -32.114083, -32.04038, -31.99608, -32.02390, -32.07563, -32.17724, -32.07512, -32.17828, -32.07817, -32.07855, -32.07700, -32.07217, -32.04657, -32.09553, -32.03525, -32.07506, -32.16760),
          lon = c(-52.105528, -52.166111, -52.175278, -52.10052, -52.25032, -52.2487, -52.17996, -52.15971, -52.17957, -52.14572, -52.19077, -52.19079, -52.19024, -52.16065, -52.11592, -52.18284, -52.09896, -52.25398, -52.16963)
        )
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 3"),
          lat = c(-32.035444, -32.091111),
          lon = c(-52.102222, -52.135000)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Função para adicionar rotas ao mapa
        add_route_to_map <- function(map, start, end, color) {
          start_coords <- st_coordinates(start)
          end_coords <- st_coordinates(end)
          
          rota <- osrmRoute(src = start_coords, dst = end_coords, overview = "full", returnclass = "sf")
          
          if (!is.null(rota)) {
            map <- map %>%
              addPolylines(data = rota, color = color, weight = 2, opacity = 0.7)
          } else {
            warning("Falha ao calcular a rota.")
          }
          
          return(map)
        }
        
        # Área inundada de 20 cm (Raio de 200m) na esquina da Rua Portugal com a Rua 15 de Novembro
        esquina_coords <- st_sfc(st_point(c(-52.11963, -32.03737)), crs = 4326)
        area_20cm <- st_buffer(st_transform(esquina_coords, crs = 32722), dist = 200)
        area_20cm <- st_transform(area_20cm, crs = 4326)
        
        # Área inundada de 1 metro (definida pelas coordenadas fornecidas)
        ponto_1m_inicial <- st_sfc(st_point(c(-52.09554, -32.02977)), crs = 4326)
        ponto_1m_final <- st_sfc(st_point(c(-52.10345, -32.02856)), crs = 4326)
        linha_1m <- st_sfc(st_linestring(rbind(st_coordinates(ponto_1m_inicial), st_coordinates(ponto_1m_final))), crs = 4326)
        area_1m <- st_buffer(st_transform(linha_1m, crs = 32722), dist = 200) # Buffer para simular a lâmina
        area_1m <- st_transform(area_1m, crs = 4326)
        
        # Área inundada de 50 cm (definida pelas coordenadas fornecidas)
        ponto_50cm <- st_sfc(st_point(c(-52.09420, -32.04687)), crs = 4326)
        area_50cm <- st_buffer(st_transform(ponto_50cm, crs = 32722), dist = 200)
        area_50cm <- st_transform(area_50cm, crs = 4326)
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar a área de 20cm
          addPolygons(data = area_20cm, color = "cyan", weight = 2, opacity = 0.5, fillOpacity = 0.2, label = "Lâmina de 20cm") %>%
          # Adicionar a área de 50cm
          addPolygons(data = area_50cm, color = "orange", weight = 2, opacity = 0.5, fillOpacity = 0.3, label = "Lâmina de 50cm") %>%
          # Adicionar a área de 1m
          addPolygons(data = area_1m, color = "red", weight = 2, opacity = 0.5, fillOpacity = 0.4, label = "Lâmina de 1m") %>%
          # Adicionar os pontos de interesse (Corpo de Bombeiros, Centro de Distribuição, Abrigos)
          addCircleMarkers(data = pontos_interesse_sf, color = "blue", radius = 5, label = ~name, group = "Abrigos") %>%
          # Adicionar os postos de saúde
          addCircleMarkers(data = postos_saude_sf, color = "purple", radius = 5, label = ~name, group = "Postos de Saúde")
        
        # Adicionar rotas entre Bombeiros Centro e Postos de Saúde / Abrigos
        for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[pontos_interesse_sf$name == "Corpo de Bombeiros Centro", ], pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ], "blue")
        }
        
        for (posto in postos_saude_sf$name) {
          mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[pontos_interesse_sf$name == "Corpo de Bombeiros Centro", ], postos_saude_sf[postos_saude_sf$name == posto, ], "blue")
        }
        
        # Adicionar rotas entre Bombeiros Cassino e Postos de Saúde / Abrigos
        for (abrigo in pontos_interesse_sf$name[grepl("Abrigo", pontos_interesse_sf$name)]) {
          mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[pontos_interesse_sf$name == "Corpo de Bombeiros Cassino", ], pontos_interesse_sf[pontos_interesse_sf$name == abrigo, ], "red")
        }
        
        for (posto in postos_saude_sf$name) {
          mapa_interativo <- add_route_to_map(mapa_interativo, pontos_interesse_sf[pontos_interesse_sf$name == "Corpo de Bombeiros Cassino", ], postos_saude_sf[postos_saude_sf$name == posto, ], "red")
        }
        
        # Adicionar legenda com os abrigos e os demais elementos
        mapa_interativo <- mapa_interativo %>%
          addLegend(position = "topright", colors = c("cyan", "orange", "red", "blue", "red", "purple"),
                    labels = c("Lâmina de 20cm", "Lâmina de 50cm", "Lâmina de 1m", "Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde")) %>%
          addLayersControl(overlayGroups = c("Abrigos", "Postos de Saúde"), options = layersControlOptions(collapsed = FALSE))
        
        
        # Ler o arquivo GeoJSON dos limites dos bairros
        limites_bairros <- st_read("C:/Users/conta/Desktop/estatistica_ambiental/Professor Mauricio/efsnunes.github.io/export.geojson")
        
        # Filtrar apenas os polígonos e multipolígonos dos bairros
        limites_bairros_poligonos <- limites_bairros[st_geometry_type(limites_bairros) %in% c("POLYGON", "MULTIPOLYGON"), ]
        
        # Dividir os bairros em cinco grandes setores (exemplo arbitrário, ajuste conforme necessário)
        limites_bairros_poligonos$setor <- cut(1:nrow(limites_bairros_poligonos), breaks = 5, labels = paste("Setor", 1:5))
        
        # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
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
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 2", "Posto de Saúde 3", "Posto de Saúde 4", "Posto de Saúde 5"),
          lat = c(-32.035444, -32.05587, -32.03030, -32.04629, -32.04902),
          lon = c(-52.102222, -52.14871, -52.10209, -52.11008, -52.11341)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Definir ícones personalizados
        icons_bombeiros <- awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa')
        icons_saude <- awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa')
        icons_distribuicao <- awesomeIcons(icon = 'building', iconColor = 'white', markerColor = 'blue', library = 'fa') 
        icons_abrigo <- awesomeIcons(icon = 'bed', iconColor = 'white', markerColor = 'purple', library = 'fa')
        
        # Função para adicionar rotas ao mapa com a distância percorrida
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
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar ícones dos bombeiros, centros de distribuição e postos de saúde
          addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons_bombeiros, 
                            label = pontos_interesse$name[1:2], group = "Bombeiros") %>%
          addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons_saude, label = postos_saude$name, 
                            group = "Postos de Saúde") %>%
          addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons_distribuicao, label = "Centro de Distribuição", 
                            group = "Centro de Distribuição") %>%
          # Adicionar ícones dos abrigos
          addAwesomeMarkers(lat = pontos_interesse$lat[4:nrow(pontos_interesse)], lng = pontos_interesse$lon[4:nrow(pontos_interesse)], 
                            icon = icons_abrigo, label = pontos_interesse$name[4:nrow(pontos_interesse)], group = "Abrigos")
        
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
        
        # Adicionar áreas inundadas
        coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
        coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.10093, -32.04273, -52.10021, -32.04168, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
        coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10059, -32.04325, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
        
        area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
        area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
        area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
        
        Inicialize o mapa
        var mymap = L.map('mapid').setView([latitude, longitude], zoomLevel);
        
        Adiciona a camada de tiles (camada base do mapa)
        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
          attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        }).addTo(mymap);
        
        Adicione a bússola com direção e velocidade do vento
        L.windBarbs([latitudeVento, longitudeVento], {
          speed: 15,  // Velocidade do vento em nós
          direction: 220,  // Direção do vento em graus
          strokeColor: '#000000',  // Cor da seta
        }).addTo(mymap);
        
        Adicione as áreas inundadas
        var inundacao = L.polygon([
          [lat1, lon1],
          [lat2, lon2],
          [lat3, lon3],
          // Continue com as coordenadas das áreas
        ], {
          color: 'blue',
          fillOpacity: 0.5
        }).addTo(mymap);
        
        Continue adicionando outras camadas, como rotas e áreas de inundação
        
        
        # Adicionar tudo ao mapa
        mapa_interativo <- mapa_interativo %>%
          addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas") %>%
          addPolygons(data = limites_bairros_poligonos, color = "purple", weight = 2, fillOpacity = 0.5, label = ~setor, group = "Setores") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas", "Setores"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = c("blue", "red", "green", "purple", "cyan", "orange", "red"), 
                    labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", "Abrigos", 
                               "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m"))
        
        
        # Definir a cidade e o filtro para limites administrativos
        query <- opq("Rio Grande, Rio Grande do Sul") %>%
          add_osm_feature(key = "boundary", value = "administrative") %>%
          add_osm_feature(key = "admin_level", value = "10")  # Nível 10 geralmente corresponde a bairros
        
        # Executar a consulta
        bairros_osm <- osmdata_sf(query)
        
        # Extrair os polígonos dos bairros
        bairros_poligonos <- bairros_osm$osm_multipolygons
        
        # Verificar se há geometria POLYGON/MULTIPOLYGON
        bairros_poligonos <- bairros_poligonos[st_geometry_type(bairros_poligonos) %in% c("POLYGON", "MULTIPOLYGON"), ]
        
        # Plotar o mapa básico para verificar
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = bairros_poligonos, color = "purple", weight = 2, fillOpacity = 0.5, label = ~name)
        
        
        # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
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
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 2", "Posto de Saúde 3", "Posto de Saúde 4", "Posto de Saúde 5"),
          lat = c(-32.035444, -32.05587, -32.03030, -32.04629, -32.04902),
          lon = c(-52.102222, -52.14871, -52.10209, -52.11008, -52.11341)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Definir ícones personalizados
        icons_bombeiros <- awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa')
        icons_saude <- awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa')
        icons_distribuicao <- awesomeIcons(icon = 'building', iconColor = 'white', markerColor = 'blue', library = 'fa') 
        icons_abrigo <- awesomeIcons(icon = 'bed', iconColor = 'white', markerColor = 'purple', library = 'fa')
        
        # Função para adicionar rotas ao mapa com a distância percorrida
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
        
        # Função para adicionar a Rosa dos Ventos
        add_wind_rose <- function(map, center_lng, center_lat, speed) {
          # Direções principais da Rosa dos Ventos
          directions <- c(0, 45, 90, 135, 180, 225, 270, 315) # N, NE, E, SE, S, SW, W, NW
          direction_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
          colors <- c("blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue")
          
          # Tamanho do vetor ajustado à velocidade
          vector_length <- speed / 10 # Ajuste do comprimento com base na velocidade
          
          for (i in seq_along(directions)) {
            angle_rad <- directions[i] * (pi / 180)  # Converter para radianos
            dx <- vector_length * cos(angle_rad) / 100
            dy <- vector_length * sin(angle_rad) / 100
            end_lng <- center_lng + dx
            end_lat <- center_lat + dy
            map <- map %>%
              addPolylines(lng = c(center_lng, end_lng), lat = c(center_lat, end_lat), 
                           color = colors[i], weight = 3, opacity = 0.8, group = direction_labels[i],
                           label = paste("Vento:", direction_labels[i], "-", speed, "km/h"))
          }
          return(map)
        }
        
        # Criar o mapa base com a Rosa dos Ventos
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar ícones dos bombeiros, centros de distribuição e postos de saúde
          addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons_bombeiros, 
                            label = pontos_interesse$name[1:2], group = "Bombeiros") %>%
          addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons_saude, label = postos_saude$name, 
                            group = "Postos de Saúde") %>%
          addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons_distribuicao, label = "Centro de Distribuição", 
                            group = "Centro de Distribuição") %>%
          # Adicionar ícones dos abrigos
          addAwesomeMarkers(lat = pontos_interesse$lat[4:nrow(pontos_interesse)], lng = pontos_interesse$lon[4:nrow(pontos_interesse)], 
                            icon = icons_abrigo, label = pontos_interesse$name[4:nrow(pontos_interesse)], group = "Abrigos")
        
        # Adicionar a Rosa dos Ventos ao mapa (Centro da Rosa dos Ventos: Rio Grande)
        mapa_interativo <- add_wind_rose(mapa_interativo, center_lng = -52.08428, center_lat = -32.03216, speed = 50)
        
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
        
        # Adicionar áreas inundadas
        coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
        coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.10093, -32.04273, -52.10021, -32.04168, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
        coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10059, -32.04325, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
        
        area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
        area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
        area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
        
        mapa_interativo <- mapa_interativo %>%
          addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = c("blue", "red", "green", "purple", "cyan", "orange", "red"), 
                    labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", "Abrigos", 
                               "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m"))
       
        
        # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
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
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 2", "Posto de Saúde 3", "Posto de Saúde 4", "Posto de Saúde 5"),
          lat = c(-32.035444, -32.05587, -32.03030, -32.04629, -32.04902),
          lon = c(-52.102222, -52.14871, -52.10209, -52.11008, -52.11341)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Definir ícones personalizados
        icons_bombeiros <- awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa')
        icons_saude <- awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa')
        icons_distribuicao <- awesomeIcons(icon = 'building', iconColor = 'white', markerColor = 'blue', library = 'fa') 
        icons_abrigo <- awesomeIcons(icon = 'bed', iconColor = 'white', markerColor = 'purple', library = 'fa')
        
        # Função para adicionar rotas ao mapa com a distância percorrida
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
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar ícones dos bombeiros, centros de distribuição e postos de saúde
          addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons_bombeiros, 
                            label = pontos_interesse$name[1:2], group = "Bombeiros") %>%
          addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons_saude, label = postos_saude$name, 
                            group = "Postos de Saúde") %>%
          addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons_distribuicao, label = "Centro de Distribuição", 
                            group = "Centro de Distribuição") %>%
          # Adicionar ícones dos abrigos
          addAwesomeMarkers(lat = pontos_interesse$lat[4:nrow(pontos_interesse)], lng = pontos_interesse$lon[4:nrow(pontos_interesse)], 
                            icon = icons_abrigo, label = pontos_interesse$name[4:nrow(pontos_interesse)], group = "Abrigos")
        
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
        
        # Adicionar áreas inundadas
        coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
        coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.10093, -32.04273, -52.10021, -32.04168, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
        coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10059, -32.04325, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
        
        area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
        area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
        area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
        
        # Adicionar circunferência de 1 km de raio com lâmina de água de 20 cm
        ponto_inundacao_20cm <- st_sfc(st_point(c(-52.08428, -32.03216)), crs = 4326)
        area_1km_20cm <- st_buffer(st_transform(ponto_inundacao_20cm, crs = 32722), dist = 1000) # Buffer de 1 km
        area_1km_20cm <- st_transform(area_1km_20cm, crs = 4326)
        
        # Adicionar tudo ao mapa
        mapa_interativo <- mapa_interativo %>%
          addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas") %>%
          addPolygons(data = area_1km_20cm, color = "blue", weight = 2, fillOpacity = 0.2, label = "Inundação 1km - 20cm", group = "Áreas Inundadas") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = c("blue", "red", "green", "purple", "cyan", "orange", "red", "blue"), 
                    labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", "Abrigos", 
                               "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m", "Inundação 1km - 20cm"))
        
       
        # Ler o arquivo GeoJSON
        limites_bairros <- st_read("C:\\Users\\conta\\Desktop\\estatistica_ambiental\\Professor Mauricio\\efsnunes.github.io\\export.geojson")
        
        # Criar o mapa base e adicionar os bairros
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          addPolygons(data = limites_bairros, color = "purple", weight = 2, fillOpacity = 0.5, label = ~name, group = "Bairros") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas", "Bairros"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = "purple", labels = "Bairros")
        
        
        
        # Ler o arquivo GeoJSON
        limites_bairros <- st_read("C:\\Users\\conta\\Desktop\\estatistica_ambiental\\Professor Mauricio\\efsnunes.github.io\\export.geojson")
        
        # Verificar os tipos de geometria presentes no arquivo
        st_geometry_type(limites_bairros)
        # Filtrar apenas os polígonos e multipolígonos
        limites_bairros_poligonos <- limites_bairros %>% 
          filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
        # Criar o mapa base e adicionar os bairros
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          addPolygons(data = limites_bairros_poligonos, color = "purple", weight = 2, fillOpacity = 0.5, label = ~name, group = "Bairros") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas", "Bairros"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = "purple", labels = "Bairros")
        
        
        # Ler o arquivo GeoJSON (certifique-se de que o caminho esteja correto)
        limites_bairros <- st_read("C:/Users/conta/Desktop/estatistica_ambiental/Professor Mauricio/efsnunes.github.io/export.geojson")
        
        # Verificar os tipos de geometria presentes no arquivo
        st_geometry_type(limites_bairros)
        
        # Filtrar apenas os polígonos e multipolígonos
        limites_bairros_poligonos <- limites_bairros[st_geometry_type(limites_bairros) %in% c("POLYGON", "MULTIPOLYGON"), ]
        
        # Criar o mapa base e adicionar os bairros
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          addPolygons(data = limites_bairros_poligonos, color = "purple", weight = 2, fillOpacity = 0.5, label = ~name, group = "Bairros") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas", "Bairros"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = "purple", labels = "Bairros")
        
        
        
        # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
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
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 3"),
          lat = c(-32.035444, -32.091111),
          lon = c(-52.102222, -52.135000)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Definir ícones personalizados
        icons_bombeiros <- awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa')
        icons_saude <- awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa')
        icons_distribuicao <- awesomeIcons(icon = 'warehouse', iconColor = 'white', markerColor = 'blue', library = 'fa')
        icons_abrigo <- awesomeIcons(icon = 'bed', iconColor = 'white', markerColor = 'purple', library = 'fa')
        
        # Função para adicionar rotas ao mapa
        add_route_to_map <- function(map, start, end, color) {
          rota <- osrmRoute(src = st_coordinates(start), dst = st_coordinates(end), returnclass = "sf")
          if (!is.null(rota)) {
            map <- map %>% addPolylines(data = rota, color = color, weight = 2, opacity = 0.7)
          }
          return(map)
        }
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar ícones dos bombeiros, centros de distribuição e postos de saúde
          addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons_bombeiros, 
                            label = pontos_interesse$name[1:2], group = "Bombeiros") %>%
          addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons_saude, label = postos_saude$name, 
                            group = "Postos de Saúde") %>%
          addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons_distribuicao, label = "Centro de Distribuição", 
                            group = "Centro de Distribuição") %>%
          # Adicionar ícones dos abrigos
          addAwesomeMarkers(lat = pontos_interesse$lat[4:nrow(pontos_interesse)], lng = pontos_interesse$lon[4:nrow(pontos_interesse)], 
                            icon = icons_abrigo, label = pontos_interesse$name[4:nrow(pontos_interesse)], group = "Abrigos")
        
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
        
        # Adicionar áreas inundadas
        coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
        coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
        coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
        
        area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
        area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
        area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
        
        mapa_interativo <- mapa_interativo %>%
          addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm", group = "Áreas Inundadas") %>%
          addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m", group = "Áreas Inundadas") %>%
          addLayersControl(overlayGroups = c("Bombeiros", "Postos de Saúde", "Abrigos", "Áreas Inundadas"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(position = "topright", colors = c("blue", "red", "green", "purple", "cyan", "orange", "red"), 
                    labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Postos de Saúde", "Abrigos", 
                               "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m"))
        
       # Criar os dados dos pontos de interesse
        pontos_interesse <- data.frame(
          name = c("Corpo de Bombeiros Centro", "Corpo de Bombeiros Cassino", "Centro de Distribuição", "Abrigo 01", "Abrigo 02", "Abrigo 03", "Abrigo 04", "Abrigo 05", "Abrigo 06", "Abrigo 07", "Abrigo 08", "Abrigo 09", "Abrigo 10", "Abrigo 11", "Abrigo 12", "Abrigo 13", "Abrigo 14", "Abrigo 15", "Abrigo 16"),
          lat = c(-32.035111, -32.184528, -32.114083, -32.04038, -31.99608, -32.02390, -32.07563, -32.17724, -32.07512, -32.17828, -32.07817, -32.07855, -32.07700, -32.07217, -32.04657, -32.09553, -32.03525, -32.07506, -32.16760),
          lon = c(-52.105528, -52.166111, -52.175278, -52.10052, -52.25032, -52.2487, -52.17996, -52.15971, -52.17957, -52.14572, -52.19077, -52.19079, -52.19024, -52.16065, -52.11592, -52.18284, -52.09896, -52.25398, -52.16963)
        )
        
        postos_saude <- data.frame(
          name = c("Posto de Saúde 1", "Posto de Saúde 3"),
          lat = c(-32.035444, -32.091111),
          lon = c(-52.102222, -52.135000)
        )
        
        # Converter para objeto sf
        pontos_interesse_sf <- st_as_sf(pontos_interesse, coords = c("lon", "lat"), crs = 4326)
        postos_saude_sf <- st_as_sf(postos_saude, coords = c("lon", "lat"), crs = 4326)
        
        # Definir ícones personalizados
        icons_bombeiros <- awesomeIcons(icon = 'fire', iconColor = 'white', markerColor = 'red', library = 'fa')
        icons_saude <- awesomeIcons(icon = 'medkit', iconColor = 'white', markerColor = 'green', library = 'fa')
        icons_distribuicao <- awesomeIcons(icon = 'warehouse', iconColor = 'white', markerColor = 'blue', library = 'fa')
        
        # Função para adicionar rotas ao mapa
        add_route_to_map <- function(map, start, end, color) {
          rota <- osrmRoute(src = st_coordinates(start), dst = st_coordinates(end), returnclass = "sf")
          if (!is.null(rota)) {
            map <- map %>% addPolylines(data = rota, color = color, weight = 2, opacity = 0.7)
          }
          return(map)
        }
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles() %>%
          # Adicionar ícones dos bombeiros, centros de distribuição e postos de saúde
          addAwesomeMarkers(lat = pontos_interesse$lat[1:2], lng = pontos_interesse$lon[1:2], icon = icons_bombeiros, label = pontos_interesse$name[1:2]) %>%
          addAwesomeMarkers(lat = postos_saude$lat, lng = postos_saude$lon, icon = icons_saude, label = postos_saude$name) %>%
          addAwesomeMarkers(lat = -32.114083, lng = -52.175278, icon = icons_distribuicao, label = "Centro de Distribuição") 
        
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
        
        # Adicionar áreas inundadas
        coords_20cm <- matrix(c(-52.11963, -32.03737, -52.12163, -32.03737, -52.12163, -32.03937, -52.11963, -32.03937, -52.11963, -32.03737), ncol = 2, byrow = TRUE)
        coords_50cm <- matrix(c(-52.07862, -32.04986, -52.08677, -32.04339, -52.10020, -32.04168, -52.10148, -32.04470, -52.07862, -32.04986), ncol = 2, byrow = TRUE)
        coords_1m <- matrix(c(-52.10800, -32.03265, -52.11057, -32.02828, -52.14844, -32.05355, -52.15638, -32.05147, -52.10800, -32.03265), ncol = 2, byrow = TRUE)
        
        area_20cm <- st_sfc(st_polygon(list(coords_20cm)), crs = 4326)
        area_50cm <- st_sfc(st_polygon(list(coords_50cm)), crs = 4326)
        area_1m <- st_sfc(st_polygon(list(coords_1m)), crs = 4326)
        
        mapa_interativo <- mapa_interativo %>%
          addPolygons(data = area_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Lâmina de 20cm") %>%
          addPolygons(data = area_50cm, color = "orange", weight = 2, fillOpacity = 0.3, label = "Lâmina de 50cm") %>%
          addPolygons(data = area_1m, color = "red", weight = 2, fillOpacity = 0.4, label = "Lâmina de 1m") %>%
          addLegend(position = "topright", colors = c("blue", "red", "cyan", "orange", "red"), 
                    labels = c("Rota Bombeiros Centro", "Rota Bombeiros Cassino", "Lâmina 20cm", "Lâmina 50cm", "Lâmina 1m"))
        
        # Coordenadas e parâmetros de vento (exemplo)
        latitude <- -32.03
        longitude <- -52.09
        wind_speed <- 15  # Velocidade do vento
        wind_direction <- 220  # Direção do vento (em graus)
        
        # Função para desenhar setas de vento
        add_wind_arrow <- function(map, lat, lng, speed, direction) {
          angle <- direction * pi / 180  # Converte para radianos
          dx <- cos(angle) * speed / 10  # Escala o tamanho da seta
          dy <- sin(angle) * speed / 10
          
          map %>%
            addPolylines(lng = c(lng, lng + dx), lat = c(lat, lat + dy),
                         color = "black", weight = 2, opacity = 1)
        }
        
        # Crie o mapa e adicione uma seta representando a direção do vento
        map <- leaflet() %>%
          addTiles() %>%
          setView(lng = longitude, lat = latitude, zoom = 12)
        
        # Adiciona seta de vento no mapa
        map <- add_wind_arrow(map, latitude, longitude, wind_speed, wind_direction)
        
        
        # Coordenadas das áreas inundadas (exemplo)
        areas_inundadas <- matrix(c(
          -32.03, -52.09,
          -32.04, -52.10,
          -32.05, -52.11
        ), ncol = 2, byrow = TRUE)
        
        # Adicionando as áreas inundadas ao mapa
        map <- map %>%
          addPolygons(lng = areas_inundadas[, 2], lat = areas_inundadas[, 1],
                      color = "blue", fillOpacity = 0.5)
        
        
        
        ajuste para que produza um unico mapa interativo, sendo priorizado 
        as rotas,a reas de inundação, a identificação dos postos de emergencia 
        e bombeiros, abrigos e centro de distribuição, e tambem as legendas, a 
        bussola também é importante.