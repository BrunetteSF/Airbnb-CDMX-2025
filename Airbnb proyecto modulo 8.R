# app completo: Shiny dashboard Airbnb CDMX
# Incluye: visualizaciones originales + regresión lineal múltiple + mapas de precio promedio por zona + conclusiones

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(lubridate)
library(scales)
library(skimr)
library(broom)
library(viridis)

# Lectura de datos
raw <- read_csv("listings.csv.gz", show_col_types = FALSE)

# Funciones auxiliares
parse_host_since <- function(x){
  parsed <- parse_date_time(x, orders = c("Ymd", "ymd", "Y-m-d", "d/m/Y", "m/d/Y", "Y"))
  as_date(parsed)
}

filtrar_outliers <- function(df, var){
  if (nrow(df) < 10) return(df)
  vals <- df[[var]]
  if (all(is.na(vals)) || length(vals) < 3) return(df)
  outs <- boxplot.stats(vals)$out
  df %>% filter(!(!!sym(var) %in% outs))
}

# Preparación y limpieza 
lista <- raw %>%
  mutate(
    price = as.numeric(str_replace_all(price, "[^0-9.]", "")),
    precio_persona = if_else(!is.na(accommodates) & accommodates > 0, price / accommodates, NA_real_),
    host_since = parse_host_since(host_since)
  ) %>%
  mutate_at(vars(matches("bedrooms|bathrooms_text|beds|host_is_superhost")), .funs = ~ .) %>%
  drop_na(bedrooms, bathrooms_text, beds, host_is_superhost)

# Filtrado de outliers por barrio y tipo de cuarto 
if ("precio_persona" %in% names(lista)) {
  lista_filtrada_2 <- lista %>%
    group_by(neighbourhood_cleansed) %>%
    group_modify(~ filtrar_outliers(.x, "precio_persona")) %>%
    ungroup()
  lista_filtrada_3 <- lista_filtrada_2 %>%
    group_by(room_type) %>%
    group_modify(~ filtrar_outliers(.x, "precio_persona")) %>%
    ungroup()
} else {
  lista_filtrada_3 <- lista
}

# Clasificación por zonas A/B/C
tipo_A <- c("Cuajimalpa de Morelos","Cuauhtémoc","Miguel Hidalgo")
tipo_B <- c("Benito Juárez","Coyoacán","Iztacalco","La Magdalena Contreras",
            "Milpa Alta","Tlalpan","Xochimilco","Álvaro Obregón")
tipo_C <- c("Azcapotzalco","Gustavo A. Madero","Iztapalapa",
            "Tláhuac","Venustiano Carranza")

lista_filtrada_3 <- lista_filtrada_3 %>%
  mutate(
    zona_tipo = case_when(
      neighbourhood_cleansed %in% tipo_A ~ "A",
      neighbourhood_cleansed %in% tipo_B ~ "B",
      neighbourhood_cleansed %in% tipo_C ~ "C",
      TRUE ~ "Otro"
    )
  )

# Filtrado adicional por zona_tipo para precio_persona
lista_filtrada_4 <- lista_filtrada_3 %>%
  group_by(zona_tipo) %>%
  group_modify(~ filtrar_outliers(.x, "precio_persona")) %>%
  ungroup()

# Cálculo de antigüedad y limpieza final
lista_filtrada_4 <- lista_filtrada_4 %>%
  mutate(
    antiguedad = if_else(!is.na(host_since), as.numeric(as_date("2023-06-01") - host_since) / 365, NA_real_)
  )

datos_filtrados <- lista_filtrada_4 %>%
  mutate(
    bathrooms_text = as.numeric(str_replace_all(as.character(bathrooms_text), "[^0-9.]", "")),
    amenities = if_else(!is.na(amenities), str_count(amenities, ",") + 1, 0),
    review_scores_rating = replace_na(review_scores_rating, 0),
    host_is_superhost = as.factor(host_is_superhost),
    host_identity_verified = as.factor(host_identity_verified),
    instant_bookable = as.factor(instant_bookable),
    zona_tipo = as.factor(zona_tipo),
    room_type = as.factor(room_type)
  ) %>%
  drop_na(bathrooms_text)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb CDMX", titleWidth = 380),
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Precios", tabName = "precios", icon = icon("chart-bar")),
      menuItem("Hosts", tabName = "hosts", icon = icon("users")),
      menuItem("Regresi\u00f3n", tabName = "regresion", icon = icon("chart-line")), 
      menuItem("Comparaciones", tabName = "comparaciones", icon = icon("chart-area")),
      menuItem("Tabla", tabName = "tabla", icon = icon("table")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Conclusiones", tabName = "conclusiones", icon = icon("check"))
    ),
    hr(),
    selectizeInput("neigh", "Selecciona barrio(s):",
                   choices = sort(unique(datos_filtrados$neighbourhood_cleansed)),
                   selected = unique(datos_filtrados$neighbourhood_cleansed)[1:6],
                   multiple = TRUE),
    selectInput("room_type", "Tipo de habitaci\u00f3n:",
                choices = c("Todos" = "all", levels(datos_filtrados$room_type)),
                selected = "all"),
    selectInput("zona_tipo", "Zona (tipo):",
                choices = c("Todos" = "all", levels(datos_filtrados$zona_tipo)),
                selected = "all"),
    sliderInput("price_range", "Rango de precio (USD):",
                min = floor(min(datos_filtrados$price, na.rm = TRUE)),
                max = ceiling(quantile(datos_filtrados$price, 0.99, na.rm = TRUE)),
                value = c(floor(min(datos_filtrados$price, na.rm = TRUE)),
                          ceiling(quantile(datos_filtrados$price, 0.90, na.rm = TRUE))),
                step = 10),
    sliderInput("reviews_range", "N\u00famero de rese\u00f1as (m\u00e1x):",
                min = 0,
                max = max(datos_filtrados$number_of_reviews, na.rm = TRUE),
                value = c(0, quantile(datos_filtrados$number_of_reviews, 0.90, na.rm = TRUE))),
    checkboxInput("superhost", "Solo superhosts", value = FALSE),
    hr()
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".box { border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.08); } .small-box { border-radius: 12px; } .content-wrapper { background: linear-gradient(180deg, #FFF9F2 0%, #F3F7FF 100%); }"))
    ),
    tabItems(
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("vb_total", width = 4),
                valueBoxOutput("vb_avgprice", width = 4),
                valueBoxOutput("vb_medprice", width = 4)
              ),
              fluidRow(
                box(title = "Histograma de precios ", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("hist_price", height = "400px"),
                    p(tags$strong("Insight Clave:"), "La distribuci\u00f3n general de precios en Airbnb CDMX est\u00e1 marcadamente sesgada hacia valores bajos, concentrando la mayor\u00eda de propiedades por debajo de $2,000 USD.")
                ),
                box(title = "Distribuci\u00f3n por tipo de habitaci\u00f3n", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("dens_room", height = "400px"),
                    p(tags$strong("Insight Clave:"), "Se observa una clara estratificación: las habitaciones compartidas y privadas muestran la distribución más económica, mientras que las propiedades completas presentan la mayor dispersión y abarcan los precios más altos.")
                )
              ),
              fluidRow(
                box(title = "Barrios (top 15)", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("table_barrios"),
                    p(tags$strong("Insight Clave:"), "Los barrios con precios promedio m\u00e1s altos son consistentemente Miguel Hidalgo, Cuauht\u00e9moc y Cuajimalpa de Morelos, lo que refleja su perfil socioeconómico y atractivo turístico/negocios.")
                ),
                box(title = "Num. propiedades por host", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("pie_hosts", height = "400px"),
                    p(tags$strong("Insight Clave:"), "La mayoría de los hosts (88.9%)tiene de 1 a 3 propiedades pero el restante (11.1%) controla casi la mitad del mercado.")
                )
              )
      ),
      
      tabItem(tabName = "precios",
              fluidRow(
                box(title = "Precio vs. Numero de rese\u00f1as", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("scatter_reviews", height = "420px"),
                    p(tags$strong("Insight Clave:"), "No existe una relaci\u00f3n lineal significativa entre el n\u00famero de rese\u00f1as y el precio. (correlación cercana a cero)")
                ),
                box(title = "Disponibilidad vs. Precio", status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("scatter_avail", height = "420px"),
                    p(tags$strong("Insight Clave:"), "El precio no var\u00eda seg\u00fan la disponibilidad en el an\u00e1lisis exploratorio.")
                )
              ),
              fluidRow(
                box(title = "Boxplot: Precio por tipo de habitaci\u00f3n", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("box_room", height = "420px"),
                    p(tags$strong("Insight Clave:"), "Diferencias claras entre tipos de habitaci\u00f3n.")
                )
              )
      ),
      
      tabItem(tabName = "hosts",
              fluidRow(
                box(title = "Top anfitriones (num propiedades)", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("top_hosts"),
                    p(tags$strong("Insight Clave:"), "Algunos hosts gestionan un n\u00famero muy elevado de propiedades, destacando el fenómeno de la concentración del mercado en gestores a gran escala.")
                ),
                box(title = "Relaci\u00f3n numero de propiedades — precio medio", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("host_price", height = "420px"),
                    p(tags$strong("Insight Clave:"), "Hosts profesionales muestran estrategias de precio diversificadas, sugiriendo que optimizan su cartera para capturar múltiples segmentos de mercado en lugar de uniformizar tarifas.")
                )
              ),
              fluidRow(
                box(title = "Detalles de concentraci\u00f3n ", status = "warning", solidHeader = TRUE, width = 12,
                    DTOutput("concentracion_table"),
                    p(tags$strong("Concentraci\u00f3n Detallada:"), "El mercado est\u00e1 concentrado en un subconjunto de hosts, el 11.1% controla casi la mitad (47.9%) de las propiedades.")
                )
              )
      ),
    #  5.4. Visualización del modelo ajustado (coeficientes, predicciones, diagnósticos)
    # 5.4.1 Resumen del modelo
    # 5.4.2. Visualización de coeficientes
    # 5.4.3. Gráfico observado vs. predicho
    # 5. 4.4. Gráfico de residuos del modelo
      tabItem(tabName = "regresion",
              fluidRow(
                box(title = "Resumen del modelo (summary)", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("resumen_modelo"),
                    p(tags$strong("Nota metodol\u00f3gica:"), "Se ajusta un modelo lineal m\u00faltiple: price ~ accommodates + bedrooms + bathrooms_text + number_of_reviews + zona_tipo + room_type.")
                ),
                box(title = "Coeficientes del modelo", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("coef_plot", height = "420px"),
                    p(tags$strong("Interpretaci\u00f3n:"), "Los coeficientes muestran el efecto marginal promedio en USD manteniendo otras variables constantes.")
                )
              ),
              fluidRow(
                box(title = "Predicciones: Observado vs Predicho", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("obs_pred_plot", height = "420px")
                ),
                box(title = "Diagn\u00f3stico: residuos vs Valores ajustados", status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("resid_plot", height = "420px")
                )
              )
      ),
      # Sección 5.1 Mapa interactivo de precios promedio por zona
      tabItem(tabName = "comparaciones",
              fluidRow(
                box(title = HTML("Mapa de precios promedio por zona<br><span style='font-size:14px; color:#003366;'>Resultados estimados mediante el modelo predictivo</span>"), status = "primary", solidHeader = TRUE, width = 6,
                    leafletOutput("mapa_zonas", height = "420px")
                ),
                
      # 5.2 Gráficas comparando precios medios por tipo de habitación          
                box(title = "Precio medio por tipo de habitaci\u00f3n", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("mean_room_plot", height = "420px")
                )
              ),
     # 5.3 Relación entre precio y número de reseñas
              fluidRow(
                box(title = "Relaci\u00f3n precio - rese\u00f1as (regresi\u00f3n simple)", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("precio_reviews", height = "420px")
                )
              )
      ),
      
      tabItem(tabName = "tabla",
              fluidRow(
                box(title = "Tabla interactiva  (filtros aplicados)", width = 12, solidHeader = TRUE, status = "primary",
                    DTOutput("tabla_completa", height = "600px"))
              )
      ),
      
      tabItem(tabName = "mapa",
              fluidRow(
                box(title = HTML("Ubicación de las propiedades<br><span style='font-size:14px; color:#003366;'>Datos originales</span>"), width = 12, solidHeader = TRUE, status = "primary",
                    uiOutput("map_ui"))
              )
      ),
      tabItem(
      tabName = "conclusiones",
      h2("Conclusiones del análisis"),
      #Introducción
      p("El análisis exploratorio y el modelo predictivo aplicados a los listados de Airbnb en la Ciudad de México permiten identificar patrones claros en los precios, los factores que los determinan y la estructura del mercado anfitrión. A continuación se presenta una síntesis integrada:"),
      
      #Caja 1: Sintesis
      box(
        width = 12, status = "primary", solidHeader = TRUE,
        title = "Síntesis del análisis exploratorio y modelado",
        
        h4("1. Patrones generales de precios"),
        tags$ul(
          tags$li("La distribución de precios es marcadamente ", tags$b("asimétrica hacia valores bajos"), " con la mayoría de las propiedades por debajo de 2,000 USD, lo que refleja un mercado con abundancia de alojamientos económicos y una menor proporción de listados premium."),
          tags$li("Existen ", tags$b("diferencias sustanciales entre los tipos de habitación"), ": los alojamientos completos concentran los precios más altos y con mayor variabilidad, mientras que las habitaciones privadas y compartidas se mantienen en rangos mucho más acotados."),
          tags$li("El análisis espacial confirma que la CDMX presenta ", tags$b("patrones de precio muy definidos por zona"), ": Miguel Hidalgo, Cuauhtémoc y Cuajimalpa se posicionan sistemáticamente como los barrios con tarifas más elevadas, alineados con su actividad empresarial, turística y su perfil socioeconómico."),
        ),
        h4("2. Características que influyen en el precio"),
        p("A partir del modelo de regresión lineal múltiple, se identificó que:"),
        tags$ul(
          tags$li("Variables estructurales como ", tags$b("número máximo de huespedes, número de recámaras y número de baños"), " tienen los mayores efectos positivos y significativos sobre el precio."),
          tags$li("El tipo de habitación impacta de forma importante y consistente: las propiedades completas incrementan el precio de manera marcada frente a habitaciones privadas o compartidas."),
          tags$li("El número de reseñas no muestra una relación lineal significativa con el precio, lo que coincide con lo observado en el análisis exploratorio."),
          tags$li("Las zonas tipo A (Miguel Hidalgo, Cuajimalpa, Cuauhtémoc) mantienen un efecto positivo sobre el nivel de precios, confirmando que la ubicación sigue siendo un determinante clave incluso al controlar por características físicas."),
        ),
        p("En conjunto, el modelo logra explicar adecuadamente la variabilidad del precio y produce estimaciones más estables, reduciendo el ruido y los valores extremos presentes en los datos originales."),
        
        h4("3. Estructura del mercado de hosts"),
        tags$ul(
          tags$li("Aunque el 90-3% de los anfitriones tiene entre 1 y 3 propiedades, el restante 9-7% controla cerca del ", tags$b("48% de las propiedades"), ", lo que evidencia una concentración relevante en operadores profesionalizados."),
          tags$li("Los hosts con múltiples propiedades presentan estrategias de precio más diversificadas, sugiriendo que segmentan sus listados para optimizar ingresos en distintos niveles de tarifa."),
        ),
        
        h4("4. Resultados del modelo predictivo"),
        tags$ul(
          tags$li("El gráfico observado vs. predicho muestra un buen alineamiento general, aunque persisten algunas desviaciones esperables dadas las fuertes colas en la distribución del precio."),
          tags$li("Los análisis de residuos sugieren que el modelo captura bien la tendencia central, pero podría beneficiarse de:"),
          tags$ul(
            tags$li("Transformaciones del precio (log-precio)."),
            tags$li("Modelos no lineales o más robustos para manejar heterogeneidad e interacciones."),
          ),
        ),
      ),
      
      #Caja 2: Conclusiones y recomendaciones
      box(title = "Conclusiones y recomendaciones", width = 12, solidHeader = TRUE, status = "danger",
          
          h4("1. Conclusiones generales"),
          p("En conjunto los resultados muestran que:"),
          tags$ul(
            tags$li("La ubicación, el tipo de propiedad y la capacidad del alojamiento son los factores dominantes que explican el precio."),
            tags$li("El mercado está parcialmente profesionalizado, con un pequeño grupo de hosts que controla gran parte del inventario."),
            tags$li("Los datos presentan alta heterogeneidad y outliers, pero el filtrado y el modelado reducen significativamente este ruido."),
            tags$li("Los patrones espaciales y estructurales permiten construir modelos predictivos útiles, tanto para análisis de mercado como para estrategias de pricing.")
          ),
          h4("2. Recomendaciones para siguientes pasos"),
          tags$ul(
            tags$li("Evaluar modelos alternativos (Random Forest, Gradient Boosting, GAM, XGBoost) que capturen no linealidades."),
            tags$li("Probar transformaciones del precio para mejorar supuestos de normalidad y homocedasticidad."),
            tags$li("Incluir nuevas variables si se tiene acceso a ellas: distancia a zonas de interés, transporte, demanda temporal, temporada y características de amenidades más detalladas."),
            tags$li("Realizar validación cruzada y medir desempeño predictivo (RMSE, MAE, R² ajustado) para determinar mejoras."),
          )
      )
     )
    
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  datos_reactive <- reactive({
    df <- datos_filtrados
    if (!is.null(input$neigh) && length(input$neigh) > 0) {
      df <- df %>% filter(neighbourhood_cleansed %in% input$neigh)
    }
    if (!is.null(input$room_type) && input$room_type != "all") {
      df <- df %>% filter(room_type == input$room_type)
    }
    if (!is.null(input$zona_tipo) && input$zona_tipo != "all") {
      df <- df %>% filter(zona_tipo == input$zona_tipo)
    }
    df <- df %>% filter(price >= input$price_range[1], price <= input$price_range[2])
    df <- df %>% filter(number_of_reviews >= input$reviews_range[1], number_of_reviews <= input$reviews_range[2])
    if (input$superhost) {
      df <- df %>% filter(as.character(host_is_superhost) %in% c("t", "TRUE", "yes","1"))
    }
    df
  })
  
  # Value boxes
  output$vb_total <- renderValueBox({
    n <- nrow(datos_reactive())
    valueBox(
      value = formatC(n, format = "d", big.mark = ","),
      subtitle = "Propiedades (filtro elegido)",
      icon = icon("home"),
      color = "purple"
    )
  })
  
  output$vb_avgprice <- renderValueBox({
    avg <- mean(datos_reactive()$price, na.rm = TRUE)
    valueBox(
      value = ifelse(is.na(avg), "NA", dollar(round(avg, 2))),
      subtitle = "Precio promedio (USD)",
      icon = icon("dollar-sign"),
      color = "olive"
    )
  })
  
  output$vb_medprice <- renderValueBox({
    med <- median(datos_reactive()$price, na.rm = TRUE)
    valueBox(
      value = ifelse(is.na(med), "NA", dollar(round(med, 2))),
      subtitle = "Mediana precio (USD)",
      icon = icon("balance-scale"),
      color = "maroon"
    )
  })
  
  # Gráficos existentes (hist, dens, tabla barrios, pie hosts, etc.)
  output$hist_price <- renderPlotly({
    df <- datos_reactive()
    p <- ggplot(df, aes(x = price)) +
      geom_histogram(aes(fill = after_stat(count)), bins = 50, color = "white", linewidth = 0.15) +
      scale_fill_gradientn(colors = c("#FFD700", "#EE0000")) +
      labs(title = "Distribuci\u00f3n general del precio (USD)", x = "Precio (USD)", y = "Frecuencia") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y")) %>% layout(legend = list(orientation = "h"))
  })
  
  output$dens_room <- renderPlotly({
    df <- datos_reactive()
    p <- ggplot(df, aes(x = price, fill = room_type)) +
      geom_density(alpha = 0.45) +
      scale_x_continuous(limits = c(0, if(nrow(df) > 0) quantile(df$price, 0.98, na.rm = TRUE) else 1)) +
      scale_fill_manual(values = c("#FFE93F", "#C63F41", "#A9E9AD", "#0086B3")) +
      labs(title = "Densidad de precios por tipo de habitaci\u00f3n", x = "Precio (USD)", y = "Densidad") +
      theme_classic()
    ggplotly(p)
  })
  
  output$table_barrios <- renderDT({
    df <- datos_reactive()
    precios_barrios <- df %>%
      group_by(neighbourhood_cleansed) %>%
      summarise(precio_medio = mean(price, na.rm = TRUE), conteo = n()) %>%
      arrange(desc(precio_medio)) %>%
      head(15)
    datatable(precios_barrios, options = list(pageLength = 15), rownames = FALSE) %>%
      formatCurrency("precio_medio", currency = "$", digits = 0)
  })
  
  output$pie_hosts <- renderPlotly({
    df <- datos_reactive()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos de hosts disponibles"))
    }
    porcentaje_hosts <- df %>%
      count(host_id, name = "num_propiedades") %>%
      mutate(rango_propiedades = ifelse(num_propiedades <= 3, "1-3 propiedades", "4 o m\u00e1s propiedades")) %>%
      count(rango_propiedades, name = "cantidad_hosts") %>%
      mutate(porcentaje = cantidad_hosts / sum(cantidad_hosts) * 100) %>%
      arrange(factor(rango_propiedades, levels = c("4 o m\u00e1s propiedades", "1-3 propiedades")))
    
    plot_ly(porcentaje_hosts, labels = ~rango_propiedades, values = ~cantidad_hosts, type = 'pie', textinfo = 'percent+value', insidetextfont = list(color = '#000000', size = 14), marker = list(colors = c("#E96E00", "#4CAF50")), hoverinfo = 'text', text = ~paste0('<b>', rango_propiedades, '</b><br>', round(porcentaje, 2), '% (', cantidad_hosts, ' hosts)')) %>%
      layout(title = 'Distribuci\u00f3n de hosts por n\u00famero de propiedades', showlegend = FALSE)
  })
  
  output$scatter_reviews <- renderPlotly({
    df <- datos_reactive()
    p <- ggplot(df, aes(x = number_of_reviews, y = price, text = paste0("Host: ", host_id, "<br>Precio: $", price))) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Relaci\u00f3n entre n\u00famero de rese\u00f1as y precio", x = "N\u00famero de rese\u00f1as", y = "Precio (USD)") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$scatter_avail <- renderPlotly({
    df <- datos_reactive() %>% filter(!is.na(availability_365))
    if (nrow(df) == 0) {
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No hay datos de availability_365") + theme_void()
      return(ggplotly(p))
    }
    p <- ggplot(df, aes(x = availability_365, y = price)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Disponibilidad (d\u00edas/a\u00f1o) vs Precio", x = "Disponibilidad (365)", y = "Precio (USD)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$box_room <- renderPlotly({
    df <- datos_reactive()
    p <- ggplot(df, aes(x = room_type, y = price, fill = room_type)) +
      geom_boxplot() +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(title = "Precio por tipo de habitaci\u00f3n", x = "Tipo de habitaci\u00f3n", y = "Precio (USD)") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$top_hosts <- renderDT({
    df <- datos_reactive()
    host_counts <- df %>% count(host_id, name = "num_propiedades") %>% arrange(desc(num_propiedades)) %>% head(20)
    datatable(host_counts, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$host_price <- renderPlotly({
    df <- datos_reactive()
    host_stats <- df %>% count(host_id, name = "num_propiedades") %>% left_join(df %>% group_by(host_id) %>% summarise(mean_price = mean(price, na.rm = TRUE)), by = "host_id") %>% distinct(host_id, .keep_all = TRUE)
    p <- ggplot(host_stats, aes(x = num_propiedades, y = mean_price)) + geom_jitter(alpha = 0.5, width = 0.2, height = 0) + geom_smooth(method = "lm", se = FALSE, color = "black") + labs(title = "N\u00ba propiedades por host vs. Precio medio", x = "N\u00ba propiedades", y = "Precio medio (USD)") + theme_minimal()
    ggplotly(p)
  })
  
  output$concentracion_table <- renderDT({
    df <- datos_reactive()
    host_counts <- df %>% count(host_id, name = "num_propiedades")
    analisis_concentracion <- host_counts %>% mutate(rango_hosts = case_when(num_propiedades == 1 ~ "1 propiedad", num_propiedades == 2 ~ "2 propiedades", num_propiedades == 3 ~ "3 propiedades", num_propiedades > 3 ~ "4 o m\u00e1s propiedades")) %>% group_by(rango_hosts) %>% summarise(cantidad_hosts = n(), total_propiedades = sum(num_propiedades)) %>% mutate(porcentaje_hosts = round((cantidad_hosts / sum(cantidad_hosts)), 4), porcentaje_propiedades = round((total_propiedades / sum(total_propiedades)), 4)) %>% arrange(factor(rango_hosts, levels = c("1 propiedad", "2 propiedades", "3 propiedades", "4 o m\u00e1s propiedades")))
    datatable(analisis_concentracion, options = list(pageLength = 10), rownames = FALSE) %>% formatPercentage(c("porcentaje_hosts", "porcentaje_propiedades"), digits = 2) %>% formatCurrency(c("cantidad_hosts", "total_propiedades"), currency = "", digits = 0)
  })
  
  output$tabla_completa <- renderDT({
    df <- datos_reactive()
    cols <- c("id", "name", "host_id", "neighbourhood_cleansed", "zona_tipo", "room_type", "price",
              "accommodates", "bedrooms", "beds", "bathrooms_text", "number_of_reviews",
              "review_scores_rating", "availability_365", "amenities")
    cols <- intersect(cols, names(df))
    datatable(df %>% select(all_of(cols)), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) %>% formatCurrency("price", currency = "$", digits = 0)
  })
  
  output$map_ui <- renderUI({
    if (all(c("latitude", "longitude") %in% names(datos_filtrados))) {
      leafletOutput("map", height = "650px")
    } else {
      tagList(h4("Datos de localizaci\u00f3n no disponibles."), p("."))
    }
  })
  
  output$map <- renderLeaflet({
    df <- datos_reactive()
    if (!all(c("latitude", "longitude") %in% names(df))) return(NULL)
    df_map <- df %>% filter(!is.na(latitude) & !is.na(longitude))
    pal <- colorNumeric(c("#FFE93F","#C63F41","#0086B3"), domain = df_map$price)
    leaflet(df_map) %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(~longitude, ~latitude, radius = ~pmin(12, pmax(4, price/200)), color = ~pal(price), stroke = FALSE, fillOpacity = 0.7, popup = ~paste0("<b>", name, "</b><br/>Precio: $", price, "<br/>Barrio: ", neighbourhood_cleansed, "<br/>Tipo: ", room_type)) %>% addLegend("bottomright", pal = pal, values = ~price, title = "Precio (USD)")
  })
  
  # ----------------------------
  # MODELO DE REGRESIÓN LINEAL MÚLTIPLE
  # ----------------------------
  modelo_lm <- reactive({
    df <- datos_reactive()
    # Requerimos suficientes observaciones por eso si tenemos menos de 20 observaciones no procede el modelo
    if (nrow(df) < 20) return(NULL)
    # Ajuste del modelo lineal múltiple (sin transformación de precio)
    lm(price ~ accommodates + bedrooms + bathrooms_text + number_of_reviews + zona_tipo + room_type, data = df)
  })
  
  output$resumen_modelo <- renderPrint({
    mod <- modelo_lm()
    if (is.null(mod)) {
      cat("No hay suficientes observaciones para ajustar el modelo con los filtros actuales.")
    } else {
      print(summary(mod))
    }
  })
  
  output$coef_plot <- renderPlotly({
    mod <- modelo_lm()
    if (is.null(mod)) return(NULL)
    coefs <- broom::tidy(mod) %>% filter(term != "(Intercept)")
    p <- ggplot(coefs, aes(x = reorder(term, estimate), y = estimate, color = estimate > 0)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
      scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) + 
      coord_flip() + theme_minimal() + labs(title = "Coeficientes del modelo (IC 95%)", x = "Variable", y = "Estimaci\u00f3n (USD)")
    ggplotly(p)
  })
  
  
  output$obs_pred_plot <- renderPlotly({
    mod <- modelo_lm()
    df <- datos_reactive()
    if (is.null(mod)) return(NULL)
    pred <- predict(mod, newdata = df)
    df2 <- df %>% mutate(pred = pred) %>% filter(!is.na(pred))
    p <- ggplot(df2, aes(x = pred, y = price, color = price - pred)) + 
     geom_point(alpha = 0.4) + 
     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + 
      scale_color_viridis_c(option = "rocket") +
     theme_minimal() + 
     labs(title = "Observado vs Predicho", x = "Precio predicho (USD)", y = "Precio observado (USD)", color = "Error")
    ggplotly(p)
  })
  
  
  
  
  output$resid_plot <- renderPlotly({
    mod <- modelo_lm()
    df <- datos_reactive()
    if (is.null(mod)) return(NULL)
    resid <- resid(mod)
    fitted <- fitted(mod)
    df2 <- tibble(fitted = fitted, resid = resid)
    p <- ggplot(df2, aes(x = fitted, y = resid, color = abs(resid))) + 
      geom_point(alpha = 0.4) + 
      geom_hline(yintercept = 0, linetype = "dashed") + 
      scale_color_viridis_c(option = "rocket") +
      theme_minimal() + labs(title = "Residuos vs Valores ajustados", x = "Valores ajustados", y = "Residuos",color = "Magnitud del residuo")
    ggplotly(p)
  })
  

  
  
  # ----------------------------
  # VISUALIZACIONES SECCIÓN 5
  # ----------------------------
  # 1) Mapa de precios promedio por zona 
  # # Sección 5.1 Mapa interactivo de precios promedio por zona
  
  output$mapa_zonas <- renderLeaflet({
    df <- datos_filtrados %>% filter(zona_tipo != "Otro")
    if (nrow(df) == 0) return(NULL)
    zona_stats <- df %>% group_by(zona_tipo) %>% summarise(precio_prom = mean(price, na.rm = TRUE), lat = mean(latitude, na.rm = TRUE), lon = mean(longitude, na.rm = TRUE), n = n())
    zona_stats <- zona_stats %>% filter(!is.na(lat) & !is.na(lon))
    if (nrow(zona_stats) == 0) return(NULL)
    pal <- colorNumeric(c("#FFD700", "#C63F41"), domain = zona_stats$precio_prom)
    leaflet(zona_stats) %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(~lon, ~lat, radius = ~pmax(8, pmin(30, precio_prom/50)), color = ~pal(precio_prom), stroke = FALSE, fillOpacity = 0.8, popup = ~paste0("Zona: ", zona_tipo, "<br>Precio promedio: $", round(precio_prom, 2), "<br>N: ", n)) %>% addLegend("bottomright", pal = pal, values = ~precio_prom, title = "Precio promedio (USD)")
  })
  

  # 5.2 Gráficas comparando precios medios por tipo de habitación
  output$mean_room_plot <- renderPlotly({
    df <- datos_filtrados %>% group_by(room_type) %>% summarise(media = mean(price, na.rm = TRUE), n = n())
    p <- ggplot(df, aes(x = room_type, y = media, fill = room_type)) + 
      geom_col() + 
      scale_color_viridis_c(option = "rocket") +
      theme_minimal() + 
      labs(title = "Precio medio por tipo de habitaci\u00f3n", y = "Precio medio (USD)", x = "Tipo de habitaci\u00f3n") + theme(legend.position = "none")
    ggplotly(p)
  })
  
  # 3) Relación precio - reseñas (versión enfocada)
  output$precio_reviews <- renderPlotly({
    df <- datos_filtrados
    p <- ggplot(df, aes(number_of_reviews, price)) + 
      geom_point(color = "#984ea3", alpha = 0.25, size = 2) + geom_smooth(method = "lm", color = "black") + 
      theme_classic() + 
      labs(title = "Relación entre precio y n\u00famero de reseñas", 
           x = "Número de reseñas", y = "Precio (USD)")
    ggplotly(p)
  })
  

  
  
}

# EJECUCIÓN
shinyApp(ui, server)
