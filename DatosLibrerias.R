library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)
library(treemap)
library(d3treeR)

# Cargar datos 
data <- read_csv("WDIData.csv") %>% 
  
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
             "GINI index (World Bank estimate)" = "Índice GINI",
             "Military expenditure (% of GDP)" = "Gasto militar (% PIB)",
             "Research and development expenditure (% of GDP)" = "Gasto en investigación y desarrollo (% PIB)",
             "Unemployment, total (% of total labor force) (modeled ILO estimate)" = "Desempleo, total (% de la fuerza de trabajo)",
             "GDP per capita, PPP (current international $)" = "PIB per capita, PPP",
             "Risk of impoverishing expenditure for surgical care (% of people at risk)" = "Riesgo de gasto empobrecedor por atención quirúrgica (% de personas en riesgo)",
             "Hospital beds (per 1,000 people)" = "Camas de hospital (por cada 1.000 personas)"
      ),
    Chile = ifelse(`país` == "Chile", "Si", "No")
  )

ruiz <- read_csv("RuizTagleOct2019.csv")

p <- treemap(ruiz,
             index=c("grupo", "Item"),
             vSize="MillonesUS",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
) 

d <- d3tree3(p ,  rootname = "(Millones de USD)" )