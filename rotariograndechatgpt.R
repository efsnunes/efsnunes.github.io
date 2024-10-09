# Obter o horário atual no fuso de Brasília (BRT)
horario_brasilia <- Sys.time()
horario_brasilia <- format(horario_brasilia, tz = "America/Sao_Paulo", usetz = TRUE)

# Exibir o horário
print(horario_brasilia)

# Instalar pacotes (se ainda não tiver)
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("leaflet")

# Carregar pacotes
library(httr)
library(jsonlite)
library(leaflet)
# Definir sua chave de API
api_key <- "e6ea32b7f44749d03b9f81d2f07eb928"

# URL para obter a temperatura de Rio Grande
url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)

# Fazer a requisição
response <- GET(url)

# Processar o resultado
data <- fromJSON(content(response, "text"))

# Extrair a temperatura atual
temperatura <- data$main$temp
cidade <- data$name

# Exibir a temperatura
print(paste("Temperatura em", cidade, ":", temperatura, "°C"))
# Criar o mapa interativo com a temperatura
mapa <- leaflet() %>%
  addTiles() %>%  # Adicionar camada de mapa
  setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%  # Focar em Rio Grande
  addMarkers(lng = -52.0976, lat = -32.0335, 
             popup = paste("Temperatura atual: ", temperatura, "°C"))

# Exibir o mapa
mapa
library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

ui <- fluidPage(
  leafletOutput("mapa")
)

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    
    # Obter dados de temperatura
    url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    temperatura <- data$main$temp
    
    # Criar o mapa
    leaflet() %>%
      addTiles() %>%
      setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
      addMarkers(lng = -52.0976, lat = -32.0335, 
                 popup = paste("Temperatura atual: ", temperatura, "°C"))
  })
  
  # Atualizar a cada 10 minutos (600000 milissegundos)
  invalidateLater(600000, session)
}

shinyApp(ui, server)
# Definir sua chave de API
api_key <- "e6ea32b7f44749d03b9f81d2f07eb928"

# URL para obter os dados de Rio Grande
url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)

# Fazer a requisição
response <- GET(url)

# Processar o resultado
data <- fromJSON(content(response, "text"))

# Extrair a precipitação, se disponível
if (!is.null(data$rain)) {
  precipitacao <- data$rain$`1h`  # Precipitação na última 1 hora
} else {
  precipitacao <- 0  # Se não houver dados de chuva
}

cidade <- data$name

# Exibir a precipitação
print(paste("Precipitação em", cidade, ":", precipitacao, "mm na última 1 hora"))
# Criar o mapa interativo com a precipitação
mapa <- leaflet() %>%
  addTiles() %>%  # Adicionar camada de mapa
  setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%  # Focar em Rio Grande
  addMarkers(lng = -52.0976, lat = -32.0335, 
             popup = paste("Precipitação atual: ", precipitacao, "mm na última 1 hora"))

# Exibir o mapa
mapa
library(shiny)
library(leaflet)
library(httr)
library(jsonlite)

ui <- fluidPage(
  leafletOutput("mapa")
)

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    
    # Obter dados de precipitação
    url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Rio%20Grande,BR&units=metric&appid=", api_key)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    
    if (!is.null(data$rain)) {
      precipitacao <- data$rain$`1h`
    } else {
      precipitacao <- 0
    }
    
    # Criar o mapa
    leaflet() %>%
      addTiles() %>%
      setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
      addMarkers(lng = -52.0976, lat = -32.0335, 
                 popup = paste("Precipitação atual: ", precipitacao, "mm na última 1 hora"))
  })
  
  # Atualizar a cada 10 minutos (600000 milissegundos)
  invalidateLater(600000, session)
}

shinyApp(ui, server)
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
library(lubridate)
today()
now()  
ymd_hms()
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
  chuva <- ifelse(!is.null(weather_data$rain$`1h`), weather_data$rain$`1h`, 0) # Verifica precipitação de 1 hora
  
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
    <tr><td>Precipitação (última 1h):</td><td>%.1f mm</td></tr>
  </table>
  ", temperatura, feels_like, pressao, umidade, vento_speed, vento_direcao, nuvens, chuva)
  
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
saveWidget(mapa, file = "caminho/desejado/mapa_interativo_com_meteorologia.html")
getwd()
library(leaflet)
library(lubridate)
library(htmlwidgets)

# Função para adicionar o relógio dinâmico ao mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Atual:</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var currentTime = now.toLocaleString('pt-BR', { hour: 'numeric', minute: 'numeric', second: 'numeric', hour12: false });
      document.getElementById('current_time').textContent = currentTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função para criar o mapa interativo
create_mapa_interativo <- function() {
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Ponto central")
  
  # Adicionar o relógio dinâmico ao mapa
  mapa_interativo <- add_dynamic_clock(mapa_interativo)
  
  return(mapa_interativo)
}

# Gerar o mapa interativo
mapa <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML
saveWidget(mapa, file = "mapa_com_relogio.html")

library(leaflet)
library(lubridate)
library(htmlwidgets)

# Função para adicionar o relógio dinâmico ao mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Atual (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var offset = -3;  // Fuso horário de Brasília (BRT)
      var utc = now.getTime() + (now.getTimezoneOffset() * 60000);
      var brtTime = new Date(utc + (3600000 * offset));
      var hours = brtTime.getHours().toString().padStart(2, '0');
      var minutes = brtTime.getMinutes().toString().padStart(2, '0');
      var seconds = brtTime.getSeconds().toString().padStart(2, '0');
      var currentTime = hours + ':' + minutes + ':' + seconds;
      document.getElementById('current_time').textContent = currentTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função para criar o mapa interativo
create_mapa_interativo <- function() {
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Ponto central")
  
  # Adicionar o relógio dinâmico ao mapa
  mapa_interativo <- add_dynamic_clock(mapa_interativo)
  
  return(mapa_interativo)
}

# Gerar o mapa interativo
mapa <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML
saveWidget(mapa, file = "mapa_com_relogio.html")
library(leaflet)
library(lubridate)
library(htmlwidgets)

# Função para adicionar o relógio dinâmico ao mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Atual (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var brtTime = now.toLocaleTimeString('pt-BR', { timeZone: 'America/Sao_Paulo' });
      document.getElementById('current_time').textContent = brtTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função para criar o mapa interativo
create_mapa_interativo <- function() {
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Ponto central")
  
  # Adicionar o relógio dinâmico ao mapa
  mapa_interativo <- add_dynamic_clock(mapa_interativo)
  
  return(mapa_interativo)
}

# Gerar o mapa interativo
mapa <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML
saveWidget(mapa, file = "mapa_com_relogio.html")




library(leaflet)
library(lubridate)
library(htmlwidgets)

# Função para adicionar o relógio dinâmico ao mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Atual (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var brtTime = now.toLocaleTimeString('pt-BR', { timeZone: 'America/Sao_Paulo' });
      document.getElementById('current_time').textContent = brtTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função para criar o mapa interativo
create_mapa_interativo <- function() {
  # Criar o mapa base
  mapa_interativo <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Ponto central")
  
  # Adicionar o relógio dinâmico ao mapa
  mapa_interativo <- add_dynamic_clock(mapa_interativo)
  
  return(mapa_interativo)
}

# Gerar o mapa interativo
mapa <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML
saveWidget(mapa, file = "mapa_com_relogio.html")
toLocaleTimeString()
# Adicionando um relógio dinâmico no mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Exata:</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var currentTime = now.toLocaleTimeString('pt-BR', { timeZone: 'America/Sao_Paulo' });
      document.getElementById('current_time').textContent = currentTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}
library(leaflet)

# Função para adicionar o relógio dinâmico no mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Exata:</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var currentTime = now.toLocaleTimeString('pt-BR', { timeZone: 'America/Sao_Paulo' });
      document.getElementById('current_time').textContent = currentTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  mapa <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Rio Grande") %>%
    add_dynamic_clock()  # Adicionar o relógio dinâmico
  
  return(mapa)
}

# Criar o mapa
mapa_interativo <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML para visualizar no navegador
library(htmlwidgets)
saveWidget(mapa_interativo, "mapa_interativo_com_relogio.html")

# Exibir mensagem de sucesso
print("Mapa gerado com sucesso e salvo como 'mapa_interativo_com_relogio.html'.")
library(leaflet)

# Função para adicionar o relógio dinâmico no mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Exata (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    setInterval(function() {
      var now = new Date();
      var options = { timeZone: 'America/Sao_Paulo', hour: '2-digit', minute: '2-digit', second: '2-digit' };
      var currentTime = now.toLocaleTimeString('pt-BR', options);
      document.getElementById('current_time').textContent = currentTime;
    }, 1000);
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  mapa <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Rio Grande") %>%
    add_dynamic_clock()  # Adicionar o relógio dinâmico
  
  return(mapa)
}

# Criar o mapa
mapa_interativo <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML para visualizar no navegador
library(htmlwidgets)
saveWidget(mapa_interativo, "mapa_interativo_com_relogio.html")

# Exibir mensagem de sucesso
print("Mapa gerado com sucesso e salvo como 'mapa_interativo_com_relogio.html'.")
library(leaflet)

# Função para adicionar o relógio dinâmico no mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Exata (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    function updateTime() {
      var now = new Date();
      var offset = -3 * 60;  // Offset de Brasília (-3 horas)
      var localTime = new Date(now.getTime() + offset * 60000);
      var hours = String(localTime.getUTCHours()).padStart(2, '0');
      var minutes = String(localTime.getUTCMinutes()).padStart(2, '0');
      var seconds = String(localTime.getUTCSeconds()).padStart(2, '0');
      var currentTime = hours + ':' + minutes + ':' + seconds;
      document.getElementById('current_time').textContent = currentTime;
    }
    setInterval(updateTime, 1000);
    updateTime();
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  mapa <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Rio Grande") %>%
    add_dynamic_clock()  # Adicionar o relógio dinâmico
  
  return(mapa)
}

# Criar o mapa
mapa_interativo <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML para visualizar no navegador
library(htmlwidgets)
saveWidget(mapa_interativo, "mapa_interativo_com_relogio.html")

# Exibir mensagem de sucesso
print("Mapa gerado com sucesso e salvo como 'mapa_interativo_com_relogio.html'.")



library(leaflet)

# Função para adicionar o relógio dinâmico no mapa
add_dynamic_clock <- function(map) {
  clock_html <- "
  <div id='clock-container' style='background-color: white; padding: 5px; border-radius: 5px;'>
    <div style='font-size: 12px;'>Hora Exata (BRT):</div>
    <div id='current_time' style='font-size: 14px; font-weight: bold;'>00:00:00</div>
  </div>
  <script>
    function updateTime() {
      var now = new Date();
      var options = { timeZone: 'America/Sao_Paulo', hour: '2-digit', minute: '2-digit', second: '2-digit' };
      var timeString = now.toLocaleTimeString('pt-BR', options);
      document.getElementById('current_time').textContent = timeString;
    }
    setInterval(updateTime, 1000);
    updateTime();
  </script>
  "
  
  map <- map %>% 
    addControl(html = clock_html, position = "bottomright")
  
  return(map)
}

# Função principal para criar o mapa interativo
create_mapa_interativo <- function() {
  mapa <- leaflet() %>%
    addTiles() %>%
    setView(lng = -52.0976, lat = -32.0335, zoom = 12) %>%
    addMarkers(lng = -52.0976, lat = -32.0335, popup = "Rio Grande") %>%
    add_dynamic_clock()  # Adicionar o relógio dinâmico
  
  return(mapa)
}

# Criar o mapa
mapa_interativo <- create_mapa_interativo()

# Salvar o mapa em um arquivo HTML para visualizar no navegador
library(htmlwidgets)
saveWidget(mapa_interativo, "mapa_interativo_com_relogio_atualizado.html")

# Exibir mensagem de sucesso
print("Mapa gerado com sucesso e salvo como 'mapa_interativo_com_relogio_atualizado.html'.")

