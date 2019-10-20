library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)
library(treemap)
library(d3treeR)

# Cargar datos Banco Mundial
data <- read_csv("Datos/WDIData.csv") %>% 
  
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
                                 "GINI index (World Bank estimate)",
                                 "Unemployment, total (% of total labor force) (modeled ILO estimate)",
                                 "Research and development expenditure (% of GDP)",
                                 "Military expenditure (% of GDP)",
                                 "GDP per capita, PPP (current international $)",
                                 "Hospital beds (per 1,000 people)",
                                 "Risk of impoverishing expenditure for surgical care (% of people at risk)"
  )) %>% 
  
  # Pasar de "ancho" a "largo"
  pivot_longer("2013":"2018", names_to = "Year", values_to = "Value") %>% 
  
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
    "valor" = "Value",
    "código_indicador" = "Indicator Code",
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
             "GINI index (World Bank estimate)" = "Coeficiente GINI (desigualdad)",
             "Military expenditure (% of GDP)" = "Gasto militar (% PIB)",
             "Research and development expenditure (% of GDP)" = "Gasto en investigación y desarrollo (% PIB)",
             "Unemployment, total (% of total labor force) (modeled ILO estimate)" = "Desempleo, total (% de la fuerza de trabajo)",
             "GDP per capita, PPP (current international $)" = "PIB per capita, PPP",
             "Risk of impoverishing expenditure for surgical care (% of people at risk)" = "Riesgo de gasto empobrecedor por atención quirúrgica (% de personas en riesgo)",
             "Hospital beds (per 1,000 people)" = "Camas de hospital (por cada 1.000 personas)"
      ),
    Chile = ifelse(`país` == "Chile", "Si", "No")
  ) %>% 
  select(`país`, indicador, valor, Chile)

# Cargar datos PNUD

presos <- read_csv("Datos/Prison population (per 100,000 people).csv",
                   skip = 1) %>% 
  transmute("país" = recode(Country, "Czechia" = "Czech Republic"),
            indicador = "Población penal (cada 100.000 personas)",
            valor = as.numeric(`2015`),
            Chile = ifelse(`país` == "Chile", "Si", "No")) %>% 
  filter(`país` %in%  c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                        "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania",
                        "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                        "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                        "United Kingdom", "United States"))

suicidios_mujer <- read_csv("Datos/Suicide rate, female (per 100,000 people).csv",
                            skip = 1) %>% 
  transmute("país" = recode(Country, "Czechia" = "Czech Republic"),
            indicador = "Tasa de suicidios, mujeres (cada 100.000 personas)",
            valor = as.numeric(`2015`),
            Chile = ifelse(`país` == "Chile", "Si", "No")) %>% 
  filter(`país` %in%  c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                        "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania",
                        "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                        "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                        "United Kingdom", "United States"))

suicidios_hombre <- read_csv("Datos/Suicide rate, male (per 100,000 people).csv",
                             skip = 1) %>% 
  transmute("país" = recode(Country, "Czechia" = "Czech Republic"),
            indicador = "Tasa de suicidios, hombres (cada 100.000 personas)",
            valor = as.numeric(`2015`),
            Chile = ifelse(`país` == "Chile", "Si", "No")) %>% 
  filter(`país` %in%  c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                        "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania",
                        "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                        "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                        "United Kingdom", "United States"))

ratio <- read_csv("Datos/Ratio of education and health expenditure to military expenditure.csv",
                  skip = 1) %>% 
  mutate(
    valor1 = as.numeric(`2013`),
    valor2 = as.numeric(`2014`),
    valor3 = as.numeric(`2015`)
  ) %>% 
  transmute("país" = recode(Country, "Czechia" = "Czech Republic"),
            indicador = "Ratio de gasto en educación más salud sobre gasto militar",
            valor = case_when(
              !is.na(valor3) ~ valor3,
              is.na(valor3) & !is.na(valor2) ~ valor2,
              is.na(valor3) & is.na(valor2) & !is.na(valor1) ~ valor1
            ),
            Chile = ifelse(`país` == "Chile", "Si", "No")) %>% 
  filter(`país` %in%  c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                        "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania",
                        "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                        "Portugal", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                        "United Kingdom", "United States"))

# Juntar datos Banco Mundial y PNUD

data <- bind_rows(data, presos, suicidios_hombre, suicidios_mujer, ratio)

# Cargar datos "Ruiz-Tagle"
ruiz <- read_csv("Datos/RuizTagleOct2019.csv")
