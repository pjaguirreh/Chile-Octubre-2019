# Cargar librerías

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)

# Cargar datos 
data <- read_csv("WDIData.csv") %>% 
  select(-"Indicator Code") %>% 
  
  # Países OECD
  filter(`Country Name` %in% c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", 
           "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
           "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
           "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
           "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", 
           "United Kingdom", "United States")) %>% 
  
  # Indicadores seleccionados
  filter(`Indicator Name` %in% c("Government expenditure on education, total (% of GDP)",
                                 "Government expenditure on education, total (% of government expenditure)",
                                 "Government expenditure per student, primary (% of GDP per capita)",
                                 "Government expenditure per student, secondary (% of GDP per capita)",
                                 "Government expenditure per student, tertiary (% of GDP per capita)",
                                 "Unemployment, total (% of total labor force) (modeled ILO estimate)",
                                 "Research and development expenditure (% of GDP)",
                                 "Military expenditure (% of GDP)",
                                 "GDP per capita, PPP (current international $)")) %>% 
  
  # Pasar de "ancho" a "largo"
  pivot_longer("2012":"2018", names_to = "Year", values_to = "Value") %>% 
  
  # Imputación: Dejar en "2018" los últimos valores disponibles para cada país en cada indicador
  group_by(`Indicator Name`) %>% 
  fill(Value) %>% 
  ungroup() %>% 
  filter(Year == 2018) %>% 
  
  # Cambiar nombres de variables a español
  rename(
    "país" = "Country Name",
    `código_país` = "Country Code",
    "indicador" = "Indicator Name",
    `año` = "Year",
    "valor" = "Value"
  ) %>% 
  
  # Cambiar nombres de indicadores a español
  mutate(
    indicador = 
      recode(indicador,
        "Government expenditure on education, total (% of government expenditure)" = "Gasto en educación, total (% de gasto público)",
        "Government expenditure on education, total (% of GDP)" = "Gasto en educación, total (% PIB)", 
        "Government expenditure per student, primary (% of GDP per capita)" = "Gasto en educación por estudiante, primaria (% GDP per capita)",
        "Government expenditure per student, secondary (% of GDP per capita)" = "Gasto en educación por estudiante, secundaria (% GDP per capita)",
        "Government expenditure per student, tertiary (% of GDP per capita)" = "Gasto en educación por estudiante, superior (% GDP per capita)",
        "Military expenditure (% of GDP)" = "Gasto militar (% PIB)",
        "Research and development expenditure (% of GDP)" = "Gasto en investigación y desarrollo (% PIB)",
        "Unemployment, total (% of total labor force) (modeled ILO estimate)" = "Desempleo, total (% de la fuerza de trabajo)",
        "GDP per capita, PPP (current international $)" = "PIB per capita, PPP"),
    Chile = ifelse(`país` == "Chile", "Si", "No")
  )

ui <- fluidPage(
   
   titlePanel("Indicadores países"),
   
   sidebarLayout(
      sidebarPanel(
        
        # Barra de selección de indicador
         selectInput("indicador",
                     "Seleccione el indicador de interés:",
                     sort(unique(data$indicador))
                     ),
         h6("Datos obtenidos desde la web del Banco Mundial: https://data.worldbank.org/indicator"),
         h6("Para cada país se muestra la información correspondiente al último año disponible")
      ),
      
      mainPanel(
         plotlyOutput("graf", height = "550px")
      )
   )
)

server <- function(input, output) {
   
  # Generar gráfico principal
   output$graf <- renderPlotly({
     ggplotly(
       data %>% 
         filter(indicador == input$indicador) %>% 
         mutate(`país` = reorder(`país`, valor)) %>% 
         ggplot(aes(x = `país`, y = valor, fill = Chile)) +
         geom_col() +
         scale_fill_manual(values = c("Si" = "#E87474", "No" = "light blue")) +
         coord_flip() +
         theme_bw() +
         labs(x = "", y = "", title = input$indicador) +
         theme(
           legend.position = "none"
         )
     )
   })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)

