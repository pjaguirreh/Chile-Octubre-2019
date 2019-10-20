# Cargar librerías

source("Script/DatosLibrerias.R", encoding = "UTF-8")

source("Script/treemapRT.R")

ui <- fluidPage(
   
   titlePanel("Jornadas de protestas en Chile - Octubre 2019"),
   
   sidebarLayout(
      sidebarPanel(
        
        # Barra de selección de indicador
         selectInput("indicador",
                     "Seleccione el indicador de interés:",
                     sort(unique(data$indicador)),
                     selected = "Coeficiente GINI (desigualdad)"
                     ),
         h6("Datos obtenidos desde la web del Banco Mundial: https://data.worldbank.org/indicator"),
         h6("Para cada país se muestra la información correspondiente al último año disponible")
      ),
      
      mainPanel(
        h4("Indicadores de Chile vs países OECD"),
        plotlyOutput("graf", height = "550px"),
        h4("Datos de 'Poniendo las cosas en contexto'"),
        h6("Documento escrito por Javier Ruiz-Tagle https://twitter.com/CedeusChile/status/1185940968740184071/photo/1"),
        d3tree3Output("d3")
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
   
   output$d3 <- renderD3tree3({
     d
   })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)

