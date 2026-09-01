library(shiny)
library(DT)

# Airline Passenger Dataset
airline <- data.frame(
  Passenger_ID = c(1,2,3,4,5),
  Age = c(28,45,33,52,39),
  Flight_Hours = c(2,8,5,10,6),
  Satisfaction = c("High","Medium","High","Low","Medium")
)

ui <- fluidPage(
  
  titlePanel("Airline Passenger Analysis Dashboard"),
  
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
           DTOutput("airlineTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of Passenger Ages
  output$histPlot <- renderPlot({
    
    hist(
      airline$Age,
      col = "skyblue",
      main = "Histogram of Passenger Ages",
      xlab = "Age",
      ylab = "Frequency"
    )
    
  })
  
  # Pie Chart of Satisfaction Levels
  output$pieChart <- renderPlot({
    
    sat <- table(airline$Satisfaction)
    
    pie(
      sat,
      col = c("lightgreen","orange","pink"),
      main = "Passenger Satisfaction Levels"
    )
    
  })
  
  # Bar Chart of Flight Hours by Passenger
  output$barChart <- renderPlot({
    
    barplot(
      airline$Flight_Hours,
      names.arg = airline$Passenger_ID,
      col = "steelblue",
      main = "Flight Hours by Passenger",
      xlab = "Passenger ID",
      ylab = "Flight Hours"
    )
    
  })
  
  # Scatter Plot of Age vs Flight Hours
  output$scatterPlot <- renderPlot({
    
    plot(
      airline$Age,
      airline$Flight_Hours,
      pch = 19,
      col = "red",
      xlab = "Age",
      ylab = "Flight Hours",
      main = "Age vs Flight Hours"
    )
    
    abline(
      lm(Flight_Hours ~ Age, data = airline),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Interactive Data Table
  output$airlineTable <- renderDT({
    
    datatable(
      airline,
      options = list(pageLength = 5),
      caption = "Airline Passenger Data"
    )
    
  })
  
}

shinyApp(ui, server)