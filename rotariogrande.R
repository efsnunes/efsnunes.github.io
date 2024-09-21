library(leaflet)
library(sf)
library(osrm)

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
icons_distribuicao <- awesomeIcons(icon = 'building', iconColor = 'white', markerColor = 'blue', library = 'fa') # Ícone de edifício
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
# Definir o ponto central
ponto_inundacao_20cm <- st_sfc(st_point(c(-52.08428, -32.03216)), crs = 4326)

# Criar um buffer de 1 km ao redor do ponto (distância em metros)
area_1km_20cm <- st_buffer(st_transform(ponto_inundacao_20cm, crs = 32722), dist = 1000) # CRS 32722 é para usar metros
area_1km_20cm <- st_transform(area_1km_20cm, crs = 4326) # Voltar para CRS 4326 para usar no Leaflet

# Criar o mapa com o círculo de inundação
mapa_inundacao <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = area_1km_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Inundação de 20cm") %>%
  addCircleMarkers(lng = -52.08428, lat = -32.03216, radius = 5, color = "red", label = "Ponto Central") %>%
  addLegend(position = "topright", colors = "cyan", labels = "Área Inundada (1 km - 20cm)")

# Exibir o mapa
mapa_inundacao


# Adicionar círculo de 1km com inundação de 20cm ao mapa anterior
mapa_interativo <- mapa_interativo %>%
  addPolygons(data = area_1km_20cm, color = "cyan", weight = 2, fillOpacity = 0.2, label = "Inundação de 20cm") %>%
  addCircleMarkers(lng = -52.08428, lat = -32.03216, radius = 5, color = "red", label = "Ponto Central") %>%
  addLegend(position = "topright", colors = "cyan", labels = "Área Inundada (1 km - 20cm)")

# Exibir o mapa
mapa_interativo


