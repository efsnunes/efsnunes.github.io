library(leaflet)
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

# Exibir o mapa final
mapa_interativo

install.packages("osrm")
library(leaflet)
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

# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addTiles()

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

# Exibir o mapa interativo
mapa_interativo







library(leaflet)
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

# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addTiles()

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

# Exibir o mapa interativo
mapa_interativo







library(osmdata)
library(sf)
library(leaflet)

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










library(leaflet)
library(sf)

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







library(leaflet)
library(sf)

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

mapa




# Verifique o diretório de trabalho atual
getwd()

# Defina um diretório de trabalho válido onde os arquivos GeoJSON estão localizados
setwd("C:/caminho/para/seu/diretório")  # Use o caminho absoluto correto para o diretório onde os arquivos estão localizados
# Liste todos os arquivos no diretório de trabalho
list.files()
library(leaflet)
library(sf)

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

mapa

################################################################
################################################################
################################################################

library(leaflet)
library(osrm)
library(sf)

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

# Exibir o mapa final
mapa_interativo

library(leaflet)
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

# Criar o mapa base
mapa_interativo <- leaflet() %>%
  addTiles()

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
    
    
library(leaflet)
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

        
        
        
        library(leaflet)
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
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles()
        
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
        
        # Exibir o mapa interativo
        mapa_interativo

        
        
        
        
        
        
        
        library(leaflet)
        library(osrm)
        library(sf)
        
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
        
        # Criar o mapa base
        mapa_interativo <- leaflet() %>%
          addTiles()
        
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
        
        # Exibir o mapa interativo
        mapa_interativo
        
                
    
    