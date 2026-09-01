library(shiny)
library(DT)

stock <- data.frame(
  Date = as.Date(c("2023-01-01","2023-01-02","2023-01-03")),
  StockA = c(100,105,110),
  StockB = c(150,152,148),
  StockC = c(120,118,122)
)

stock$PercentChange <- c(
  0,
  ((stock$StockA[2]-stock$StockA[1])/stock$StockA[1])*100,
  ((stock$StockA[3]-stock$StockA[2])/stock$StockA[2])*100
)

ui <- fluidPage(
  
  titlePanel("Stock Analysis Dashboard"),
  
  fluidRow(
    column(
      12,
      plotOutput("lineChart")
    )
  ),
  
  fluidRow(
    column(
      6,
      plotOutput("barChart")
    ),
    column(
      6,
      DTOutput("stockTable")
    )
  )
  
)

server <- function(input, output){
  
  output$lineChart <- renderPlot({
    
    plot(stock$Date, stock$StockA,
         type="o",
         col="blue",
         pch=16,
         ylim=range(c(stock$StockA,stock$StockB,stock$StockC)),
         xlab="Date",
         ylab="Stock Price",
         main="Stock Prices Over Time")
    
    lines(stock$Date, stock$StockB, type="o", col="red", pch=17)
    lines(stock$Date, stock$StockC, type="o", col="darkgreen", pch=15)
    
    legend("topleft",
           legend=c("Stock A","Stock B","Stock C"),
           col=c("blue","red","darkgreen"),
           pch=c(16,17,15),
           lty=1)
    
  })
  
  output$barChart <- renderPlot({
    
    barplot(stock$PercentChange,
            names.arg=stock$Date,
            col="orange",
            xlab="Date",
            ylab="Percentage Change (%)",
            main="Daily Percentage Change - Stock A")
    
  })
  
  output$stockTable <- renderDT({
    
    datatable(stock,
              options=list(pageLength=5),
              caption="Stock Price Data")
    
  })
  
}

shinyApp(ui, server)