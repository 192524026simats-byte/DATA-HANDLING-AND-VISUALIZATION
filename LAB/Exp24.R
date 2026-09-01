library(shiny)
library(DT)

# Hotel Booking Dataset
hotel <- data.frame(
  Booking_ID = c(1,2,3,4,5),
  Stay_Nights = c(2,5,3,7,4),
  Guests = c(2,4,1,3,2),
  Room_Type = c("Standard","Deluxe","Standard","Suite","Deluxe")
)

ui <- fluidPage(
  
  titlePanel("Hotel Booking Analysis Dashboard"),
  
  fluidRow(
    column(6,
           plotOutput("histPlot")),
    column(6,
           plotOutput("pieChart"))
  ),
  
  fluidRow(
    column(6,
           plotOutput("barChart")),
    column(6,
           plotOutput("scatterPlot"))
  ),
  
  fluidRow(
    column(12,
           DTOutput("hotelTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of Stay Nights
  output$histPlot <- renderPlot({
    
    hist(
      hotel$Stay_Nights,
      col = "skyblue",
      main = "Histogram of Stay Nights",
      xlab = "Stay Nights",
      ylab = "Frequency"
    )
    
  })
  
  # Pie Chart of Room Types
  output$pieChart <- renderPlot({
    
    room <- table(hotel$Room_Type)
    
    pie(
      room,
      col = c("orange","lightgreen","skyblue"),
      main = "Room Type Distribution"
    )
    
  })
  
  # Bar Chart of Guests per Booking
  output$barChart <- renderPlot({
    
    barplot(
      hotel$Guests,
      names.arg = hotel$Booking_ID,
      col = "steelblue",
      main = "Guests per Booking",
      xlab = "Booking ID",
      ylab = "Number of Guests"
    )
    
  })
  
  # Scatter Plot of Guests vs Stay Nights
  output$scatterPlot <- renderPlot({
    
    plot(
      hotel$Guests,
      hotel$Stay_Nights,
      pch = 19,
      col = "red",
      xlab = "Number of Guests",
      ylab = "Stay Nights",
      main = "Guests vs Stay Nights"
    )
    
    abline(
      lm(Stay_Nights ~ Guests, data = hotel),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Interactive Data Table
  output$hotelTable <- renderDT({
    
    datatable(
      hotel,
      options = list(pageLength = 5),
      caption = "Hotel Booking Data"
    )
    
  })
  
}

shinyApp(ui, server)