library(shiny)
library(leaflet)
library(DT)

geo_data <- data.frame(
  City = c("City A", "City B", "City C"),
  Population = c(500000, 700000, 600000),
  AvgTemperature = c(75, 68, 80),
  Elevation = c(1000, 800, 1200),
  Latitude = c(28.6139, 19.0760, 13.0827),
  Longitude = c(77.2090, 72.8777, 80.2707)
)

ui <- fluidPage(
  titlePanel("Geographic Data Dashboard"),
  
  fluidRow(
    column(12,
           leafletOutput("map", height = 400))
  )
  
  fluidRow(
    column(6,
           plotOutput("scatter")),
    column(6,
           DTOutput("table"))
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(geo_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Population)/300,
        color = "blue",
        fillOpacity = 0.7,
        label = ~paste(
          "City:", City,
          "<br>Population:", Population,
          "<br>Avg Temp:", AvgTemperature,
          "<br>Elevation:", Elevation
        )
      )
  })
  
  output$scatter <- renderPlot({
    plot(
      geo_data$Population,
      geo_data$AvgTemperature,
      pch = 19,
      cex = 2,
      col = "red",
      xlab = "Population",
      ylab = "Average Temperature",
      main = "Population vs Average Temperature"
    )
    
    text(
      geo_data$Population,
      geo_data$AvgTemperature,
      labels = geo_data$City,
      pos = 3
    )
  })
  
  output$table <- renderDT({
    datatable(
      geo_data,
      options = list(pageLength = 5),
      caption = "Geographic Data Table"
    )
  })
  
}

shinyApp(ui, server)