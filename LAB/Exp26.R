library(shiny)
library(DT)

# Restaurant Orders Dataset
restaurant <- data.frame(
  Order_ID = c(1,2,3,4,5),
  Items_Ordered = c(2,5,3,4,2),
  Bill_Amount = c(25,60,35,50,20),
  Dining_Type = c("Dine-In","Takeaway","Dine-In","Delivery","Takeaway")
)

ui <- fluidPage(
  
  titlePanel("Restaurant Orders Analysis Dashboard"),
  
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
           DTOutput("restaurantTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of Bill Amounts
  output$histPlot <- renderPlot({
    
    hist(
      restaurant$Bill_Amount,
      col = "skyblue",
      main = "Histogram of Bill Amounts",
      xlab = "Bill Amount",
      ylab = "Frequency"
    )
    
  })
  
  # Pie Chart of Dining Types
  output$pieChart <- renderPlot({
    
    dining <- table(restaurant$Dining_Type)
    
    pie(
      dining,
      col = c("orange","lightgreen","skyblue"),
      main = "Dining Type Distribution"
    )
    
  })
  
  # Bar Chart of Items Ordered by Order
  output$barChart <- renderPlot({
    
    barplot(
      restaurant$Items_Ordered,
      names.arg = restaurant$Order_ID,
      col = "steelblue",
      main = "Items Ordered by Order",
      xlab = "Order ID",
      ylab = "Items Ordered"
    )
    
  })
  
  # Scatter Plot of Items Ordered vs Bill Amount
  output$scatterPlot <- renderPlot({
    
    plot(
      restaurant$Items_Ordered,
      restaurant$Bill_Amount,
      pch = 19,
      col = "red",
      xlab = "Items Ordered",
      ylab = "Bill Amount",
      main = "Items Ordered vs Bill Amount"
    )
    
    abline(
      lm(Bill_Amount ~ Items_Ordered, data = restaurant),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Interactive Data Table
  output$restaurantTable <- renderDT({
    
    datatable(
      restaurant,
      options = list(pageLength = 5),
      caption = "Restaurant Orders Data"
    )
    
  })
  
}

shinyApp(ui, server)