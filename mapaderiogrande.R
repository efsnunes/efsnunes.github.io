# Instalação dos pacotes necessários
install.packages("sf")
install.packages("ggplot2")
install.packages("geobr")
install.packages("osmdata")
install.packages("tmap")  # Opcional, mas útil para visualizações interativas
install.packages("dplyr")  # Para manipulação de dados

# Carregar os pacotes
library(ggplot2)
library(dplyr)
library(sf)
library(geobr)
library(osmdata)
library(tmap)  # Usado para visualização interativa de mapas

# Baixar o shapefile do estado do Rio Grande do Sul
rs_map <- read_state(code_state = "RS", year = 2020)

# Plotar o mapa do estado do Rio Grande do Sul
ggplot() +
  geom_sf(data = rs_map, fill = "lightblue", color = "black") +
  labs(title = "Mapa do Rio Grande do Sul") +
  theme_minimal()

# Baixar o shapefile do município de Rio Grande
rio_grande_map <- read_municipality(code_muni = 4315602, year = 2020)

# Plotar o mapa do município de Rio Grande
ggplot() +
  geom_sf(data = rio_grande_map, fill = "lightgreen", color = "black") +
  labs(title = "Mapa do Município de Rio Grande") +
  theme_minimal()

# Carregar o shapefile do município de Rio Grande (verificar caminho do arquivo)
rio_grande <- st_read("C:/Users/conta/Documents/shapefile.shp")

# Verificar o CRS do shapefile
print(st_crs(rio_grande))

# Se o CRS não for 4326, transformar para 4326
if (st_crs(rio_grande)$epsg != 4326) {
  rio_grande <- st_transform(rio_grande, crs = 4326)
}

# Carregar coordenadas dos pontos
coordenadas <- read.csv("coordenadasriogrande.csv")

# Transformar o dataframe em um objeto sf (pontos)
pontos_sf <- st_as_sf(coordenadas, coords = c("X", "Y"), crs = 4326)

# Realizar a interseção para verificar se os pontos estão dentro do polígono
pontos_dentro <- st_intersects(pontos_sf, rio_grande, sparse = FALSE)

# Filtrar os pontos que estão dentro do município
pontos_rio_grande <- coordenadas[apply(pontos_dentro, 1, any), ]

# Ver os pontos que pertencem a Rio Grande
print(pontos_rio_grande)

# Definir a área de interesse (Rio Grande, RS)
rio_grande_bbox <- getbb("Rio Grande, Rio Grande do Sul, Brazil")

# Consultar as ruas de Rio Grande
ruas_rio_grande <- opq(bbox = rio_grande_bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Extrair as linhas das ruas
ruas <- ruas_rio_grande$osm_lines

# Plotar as ruas de Rio Grande
ggplot() +
  geom_sf(data = ruas, color = "black") +
  labs(title = "Ruas de Rio Grande - RS") +
  theme_minimal()

# Visualização interativa das ruas de Rio Grande
tmap_mode("view")
tm_shape(ruas) +
  tm_lines() +
  tm_layout(title = "Ruas de Rio Grande - RS")

# Encontrar os pontos de interseção entre as ruas
cruzamentos <- st_intersection(st_geometry(ruas))

# Convertê-los em pontos para plotagem
cruzamentos_pontos <- st_cast(cruzamentos, "POINT")

# Plotar as ruas e marcar os cruzamentos
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos, color = "red", size = 2) +  # Marcar os cruzamentos
  labs(title = "Cruzamentos das Ruas de Rio Grande - RS") +
  theme_minimal()

# Converter os cruzamentos em um data frame com as coordenadas
cruzamentos_df <- st_coordinates(cruzamentos_pontos) %>% as.data.frame()

# Salvar em um arquivo CSV
write.csv(cruzamentos_df, "cruzamentos_rio_grande.csv", row.names = FALSE)

# Salvar o mapa em um arquivo PNG com maior tamanho e resolução
ggsave("mapa_cruzamentos_rio_grande.png", 
       plot = last_plot(), 
       width = 15,  # Largura em polegadas
       height = 10,  # Altura em polegadas
       dpi = 300)  # Resolução em DPI (300 para alta resolução)

# Obter os setores censitários de Rio Grande
setores_rio_grande <- read_census_tract(code_muni = 4315602, year = 2010)

# Plotar os setores censitários de Rio Grande
ggplot() +
  geom_sf(data = setores_rio_grande, fill = "lightblue", color = "black") +
  labs(title = "Setores Censitários de Rio Grande") +
  theme_minimal()

# Consultar os limites dos bairros
bairros_rio_grande <- opq(bbox = rio_grande_bbox) %>%
  add_osm_feature(key = "admin_level", value = "9") %>%  # Admin level 9 costuma ser nível de bairros
  osmdata_sf()

# Extrair as fronteiras dos bairros
bairros <- bairros_rio_grande$osm_multipolygons

# Plotar os bairros de Rio Grande
ggplot() +
  geom_sf(data = bairros, fill = "lightgreen", color = "black") +
  labs(title = "Bairros de Rio Grande") +
  theme_minimal()

# Exemplo fictício de abrigos
abrigos <- data.frame(
  lat = c(-32.0346, -32.0356, -32.0340),
  lon = c(-52.0915, -52.0930, -52.0950)
)

# Converter para objeto espacial
abrigos_sf <- st_as_sf(abrigos, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Plotar o mapa com bairros e pontos de abrigos
ggplot() +
  geom_sf(data = bairros, fill = "lightgreen", color = "black") +  # Desenhar os bairros
  geom_sf(data = abrigos_sf, color = "red", size = 3) +  # Marcar os abrigos
  labs(title = "Bairros de Rio Grande e Abrigos") +
  theme_minimal()

# Plotando as ruas e rotulando com o nome
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf_text(data = ruas, aes(label = name), size = 3, color = "blue") +  # Adicionar os nomes das ruas
  labs(title = "Ruas de Rio Grande - RS") +
  theme_minimal()

# Filtrar apenas ruas classificadas como "primary" ou "secondary"
ruas_principais <- ruas %>% filter(highway %in% c("primary", "secondary"))

# Plotar apenas ruas principais e rotulá-las
ggplot() +
  geom_sf(data = ruas, color = "grey") +  # Desenhar todas as ruas em cinza
  geom_sf(data = ruas_principais, color = "black", size = 1) +  # Destacar as ruas principais
  geom_sf_text(data = ruas_principais, aes(label = name), size = 3, color = "blue") +  # Adicionar rótulos apenas às principais
  labs(title = "Ruas Principais de Rio Grande - RS") +
  theme_minimal()
# Selecionar ruas de interesse, por exemplo, a cada 10ª rua
ruas_amostra <- ruas %>% slice(seq(1, n(), by = 10))

# Plotar as ruas e rotular apenas as ruas selecionadas
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar todas as ruas
  geom_sf_text(data = ruas_amostra, aes(label = name), size = 3, color = "blue") +  # Adicionar rótulos apenas para a amostra
  labs(title = "Ruas de Rio Grande - RS (Amostra de Nomes)") +
  theme_minimal()
# Ajustar a área de plotagem para uma região específica (usando bounding box)
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar todas as ruas
  geom_sf_text(data = ruas, aes(label = name), size = 3, color = "blue") +  # Adicionar rótulos
  labs(title = "Ruas de uma região de Rio Grande - RS") +
  coord_sf(xlim = c(-52.1, -52.0), ylim = c(-32.1, -32.0)) +  # Definir limites do mapa (zoom)
  theme_minimal()

# Definir a área de interesse (bounding box) para aumentar o zoom
# Use os limites da cidade de Rio Grande, ajustando conforme necessário
limites_bbox <- c(xmin = -52.2, xmax = -52.0, ymin = -32.2, ymax = -32.0)

# Plotar as ruas e os pontos de interseção com zoom
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos, color = "red", size = 2) +  # Marcar os cruzamentos
  labs(title = "Cruzamentos das Ruas de Rio Grande - RS") +
  coord_sf(xlim = c(limites_bbox["xmin"], limites_bbox["xmax"]), 
           ylim = c(limites_bbox["ymin"], limites_bbox["ymax"])) +  # Aplicar zoom
  theme_minimal(base_size = 16)  # Aumentar o tamanho do texto
# Salvar o mapa em um arquivo PNG com maior tamanho e resolução
ggsave("mapa_cruzamentos_rio_grande_zoom.png", 
       plot = last_plot(), 
       width = 15,  # Largura em polegadas
       height = 10,  # Altura em polegadas
       dpi = 300)  # Resolução em DPI (300 para alta resolução)
tmap_mode("view")
tm_shape(ruas) +
  tm_lines() +
  tm_shape(cruzamentos_pontos) +
  tm_dots(col = "red", size = 0.1) +
  tm_layout(title = "Cruzamentos das Ruas de Rio Grande - RS")



# Coordenadas dos pontos extremos e sede
extremos <- data.frame(
  name = c("Norte", "Sul", "Leste", "Oeste", "Sede Municipal"),
  lat = c(-31.7839, -32.6625, -32.0278, -32.0278, -32.0278),
  lon = c(-52.0645, -52.6972, -52.0645, -52.6972, -52.097)
)

# Converter para objeto espacial
extremos_sf <- st_as_sf(extremos, coords = c("lon", "lat"), crs = 4326)

# Plotar o mapa com os pontos extremos e a sede
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = extremos_sf, color = "red", size = 3) +  # Marcar os pontos extremos
  geom_sf_text(data = extremos_sf, aes(label = name), nudge_y = 0.01, size = 4) +  # Adicionar rótulos
  labs(title = "Pontos Extremos e Sede Municipal de Rio Grande") +
  theme_minimal()




# Definir os limites do município de Rio Grande (coordenadas dos pontos extremos)
limites_bbox <- st_bbox(c(
  xmin = -52.6972, xmax = -52.0645, 
  ymin = -32.6625, ymax = -31.7839),
  crs = st_crs(4326)
)

# Definir a área de interesse no OpenStreetMap para obter as ruas
ruas_rio_grande <- opq(bbox = limites_bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Extrair as ruas do conjunto de dados
ruas <- ruas_rio_grande$osm_lines

# Verificar a estrutura dos dados de ruas
print(ruas)

# Encontrar os cruzamentos entre as ruas
cruzamentos <- st_intersection(st_geometry(ruas))

# Converter os cruzamentos em pontos
cruzamentos_pontos <- st_cast(cruzamentos, "POINT")

# Plotar as ruas e os cruzamentos com base nos limites geográficos definidos
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos, color = "red", size = 2) +  # Marcar os cruzamentos
  labs(title = "Cruzamentos das Ruas dentro dos Limites de Rio Grande - RS") +
  coord_sf(xlim = c(limites_bbox["xmin"], limites_bbox["xmax"]), 
           ylim = c(limites_bbox["ymin"], limites_bbox["ymax"])) +  # Aplicar os limites geográficos
  theme_minimal()



# Coordenadas fornecidas
# a) Sede municipal
sede <- data.frame(
  name = "Sede Municipal",
  lat = -32 - (1/60) - (40/3600),  # 32º01'40" S
  lon = -52 - (5/60) - (49/3600)   # 52º05'49" W
)

# b) Pontos extremos
extremos <- data.frame(
  name = c("Norte", "Sul", "Leste", "Oeste"),
  lat = c(-31 - (47/60) - (2/3600), -32 - (39/60) - (45/3600), 
          -32 - (3/60) - (50/3600), -52 - (41/60) - (50/3600)),
  lon = c(-52 - (5/60) - (49/3600), -52 - (41/60) - (50/3600), 
          -52 - (3/60) - (50/3600), -52 - (41/60) - (50/3600))
)

# Combinar todos os pontos
pontos_limite <- rbind(sede, extremos)

# Ajustar para criar um polígono retangular com base nos pontos extremos
# Definir os limites máximos e mínimos
xmin <- min(extremos$lon)
xmax <- max(extremos$lon)
ymin <- min(extremos$lat)
ymax <- max(extremos$lat)

# Criar um data frame com os quatro cantos do retângulo
limites <- data.frame(
  lon = c(xmin, xmax, xmax, xmin, xmin),
  lat = c(ymin, ymin, ymax, ymax, ymin)
)

# Converter para objeto sf (polígono)
limites_sf <- st_as_sf(limites, coords = c("lon", "lat"), crs = 4326) %>%
  st_geometry() %>%
  st_polygon(list(as.matrix(limites))) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# Visualizar o polígono dos limites
ggplot() +
  geom_sf(data = limites_sf, fill = NA, color = "blue", linetype = "dashed") +
  geom_sf(data = sede, aes(x = lon, y = lat), color = "red", size = 3) +
  geom_sf_text(data = sede, aes(x = lon, y = lat, label = name), nudge_y = 0.01, size = 4) +
  labs(title = "Limites do Município de Rio Grande") +
  theme_minimal()
# Definir a área de interesse (bounding box) com base nos limites
bounding_box <- st_bbox(limites_sf)

# Consultar as ruas de Rio Grande dentro da bounding box
ruas_rio_grande <- opq(bbox = bounding_box) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Extrair as linhas das ruas
ruas <- ruas_rio_grande$osm_lines

# Certificar-se de que as ruas estão no CRS correto
ruas <- st_transform(ruas, crs = 4326)
# Encontrar os pontos de interseção entre as ruas
cruzamentos <- st_intersection(st_geometry(ruas))

# Filtrar apenas os pontos de interseção (excluir linhas)
cruzamentos_pontos <- st_cast(cruzamentos, "POINT")

# Remover possíveis duplicatas e pontos fora dos limites
cruzamentos_pontos <- cruzamentos_pontos[limites_sf, ]

# Converter para objeto sf
cruzamentos_pontos_sf <- st_as_sf(cruzamentos_pontos)

# Verificar alguns pontos
print(head(cruzamentos_pontos_sf))
# Plotar as ruas e os cruzamentos com zoom na área de interesse
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos_sf, color = "red", size = 2) +  # Marcar os cruzamentos
  geom_sf(data = limites_sf, fill = NA, color = "blue", linetype = "dashed") +  # Limites do município
  labs(title = "Cruzamentos das Ruas de Rio Grande - RS") +
  coord_sf(xlim = c(bounding_box["xmin"], bounding_box["xmax"]), 
           ylim = c(bounding_box["ymin"], bounding_box["ymax"])) +  # Aplicar zoom
  theme_minimal(base_size = 16)  # Aumentar o tamanho do texto
# Converter os cruzamentos em um data frame com as coordenadas
cruzamentos_df <- st_coordinates(cruzamentos_pontos_sf) %>% as.data.frame()

# Adicionar nomes das colunas
colnames(cruzamentos_df) <- c("Longitude", "Latitude")

# Salvar em um arquivo CSV
write.csv(cruzamentos_df, "cruzamentos_rio_grande.csv", row.names = FALSE)
# Definir o modo interativo
tmap_mode("view")

# Criar o mapa interativo
tm_shape(ruas) +
  tm_lines(col = "black") +
  tm_shape(cruzamentos_pontos_sf) +
  tm_dots(col = "red", size = 0.5) +
  tm_layout(title = "Cruzamentos das Ruas de Rio Grande - RS")
# Coordenadas dos pontos extremos e sede
extremos <- data.frame(
  name = c("Norte", "Sul", "Leste", "Oeste", "Sede Municipal"),
  lat = c(-31 - (47/60) - (2/3600), -32 - (39/60) - (45/3600), 
          -32 - (3/60) - (50/3600), -32 - (3/60) - (50/3600), -32 - (1/60) - (40/3600)),
  lon = c(-52 - (5/60) - (49/3600), -52 - (41/60) - (50/3600), 
          -52 - (3/60) - (50/3600), -52 - (41/60) - (50/3600), -52 - (5/60) - (49/3600))
)

# Converter para objeto sf
extremos_sf <- st_as_sf(extremos, coords = c("lon", "lat"), crs = 4326)

# Plotar o mapa com os limites, ruas, cruzamentos e pontos extremos
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos_sf, color = "red", size = 2) +  # Marcar os cruzamentos
  geom_sf(data = limites_sf, fill = NA, color = "blue", linetype = "dashed") +  # Limites do município
  geom_sf(data = extremos_sf, color = "green", size = 3) +  # Pontos extremos e sede
  geom_sf_text(data = extremos_sf, aes(label = name), nudge_y = 0.01, size = 4, color = "green") +  # Rótulos dos pontos
  labs(title = "Cruzamentos das Ruas de Rio Grande - RS com Limites e Pontos Extremos") +
  coord_sf(xlim = c(bounding_box["xmin"], bounding_box["xmax"]), 
           ylim = c(bounding_box["ymin"], bounding_box["ymax"])) +  # Aplicar zoom
  theme_minimal(base_size = 16)  # Aumentar o tamanho do texto

# Plotar as ruas e os cruzamentos com zoom na área de interesse
ggplot() +
  geom_sf(data = ruas, color = "black") +  # Desenhar as ruas
  geom_sf(data = cruzamentos_pontos_sf, color = "red", size = 2) +  # Marcar os cruzamentos
  geom_sf(data = limites_sf, fill = NA, color = "blue", linetype = "dashed") +  # Desenhar os limites
  labs(title = "Cruzamentos das Ruas dentro dos Limites de Rio Grande - RS") +
  theme_minimal()

# Salvar o mapa em um arquivo PNG com maior tamanho e resolução
ggsave("mapa_cruzamentos_limites_rio_grande.png", 
       plot = last_plot(), 
       width = 15,  # Largura em polegadas
       height = 10,  # Altura em polegadas
       dpi = 300)  # Resolução em DPI (300 para alta resolução)






install.packages("mapview")
library(mapview)
# Definir o nome do município e buscar os dados de ruas via OpenStreetMap
municipio <- "Rio Grande, Brazil"

# Obter os dados de ruas (highways) para Rio Grande
ruas_rio_grande <- opq(bbox = getbb(municipio)) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# Extrair a camada de linhas (ruas)
ruas <- ruas_rio_grande$osm_lines
# Definir um ponto central aproximado de Rio Grande (Centro)
ponto_central <- st_point(c(-52.0997, -32.035)) # Coordenadas do centro de Rio Grande
ponto_central_sf <- st_sfc(ponto_central, crs = 4326)

# Definir um buffer (raio de 5 km) para limitar a área central
area_central <- st_buffer(ponto_central_sf, dist = 0.05) # Aproximadamente 5km

# Filtrar as ruas dentro da área central
ruas_area_central <- st_intersection(ruas, area_central)
# Encontrar cruzamentos (interseções) entre as ruas
intersecoes <- st_intersection(ruas_area_central)

# Extrair os pontos de interseção
cruzamentos <- st_collection_extract(intersecoes, "POINT")
# Visualizar os cruzamentos em um mapa
mapview(cruzamentos)

# Exportar os cruzamentos para CSV
cruzamentos_coords <- st_coordinates(cruzamentos)
cruzamentos_df <- as.data.frame(cruzamentos_coords)

# Salvar como CSV
write.csv(cruzamentos_df, "cruzamentos_rio_grande.csv", row.names = FALSE)





# Projete as geometrias para um sistema de coordenadas plano (UTM zona 22S para Rio Grande)
ruas_proj <- st_transform(ruas, crs = 32722) # UTM zona 22S
area_central_proj <- st_transform(area_central, crs = 32722)

# Filtrar as ruas dentro da área central projetada
ruas_area_central <- st_intersection(ruas_proj, area_central_proj)

# Encontrar interseções entre as ruas
intersecoes <- st_intersection(ruas_area_central)

# Extrair apenas os pontos de interseção
# Como as interseções podem ser LINESTRING, vamos converter para POINTS
cruzamentos <- st_cast(intersecoes, "POINT", warn = FALSE)

# Verifique a classe de cruzamentos
class(cruzamentos) # Deve ser "sf" e "POINT"

# Visualizar os cruzamentos em um mapa
mapview(cruzamentos)

# Exportar os cruzamentos para CSV
cruzamentos_coords <- st_coordinates(cruzamentos)
cruzamentos_df <- as.data.frame(cruzamentos_coords)

# Salvar os cruzamentos como CSV
write.csv(cruzamentos_df, "cruzamentos_rio_grande.csv", row.names = FALSE)
# Definir um diretório onde você tem permissão para salvar arquivos
setwd("caminho_para_seu_diretorio")

