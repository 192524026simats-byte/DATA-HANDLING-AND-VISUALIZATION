library(shiny)
library(DT)

# Water Samples Dataset
water <- data.frame(
  Sample_ID = c(1,2,3,4,5),
  pH_Level = c(7.2,6.8,7.5,6.5,7.1),
  Turbidity = c(3,5,2,7,4),
  Quality = c("Good","Fair","Good","Poor","Fair")
)

ui <- fluidPage(
  
  titlePanel("Water Quality Monitoring Dashboard"),
  
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
           DTOutput("waterTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of pH Levels
  output$histPlot <- renderPlot({
    
    hist(
      water$pH_Level,
      col="skyblue",
      main="Histogram of pH Levels",
      xlab="pH Level",
      ylab="Frequency"
    )
    
  })
  
  # Pie Chart of Water Quality Categories
  output$pieChart <- renderPlot({
    
    quality <- table(water$Quality)
    
    pie(
      quality,
      col=c("lightgreen","orange","pink"),
      main="Water Quality Categories"
    )
    
  })
  
  # Bar Chart of Turbidity by Sample
  output$barChart <- renderPlot({
    
    barplot(
      water$Turbidity,
      names.arg=water$Sample_ID,
      col="steelblue",
      main="Turbidity by Sample",
      xlab="Sample ID",
      ylab="Turbidity"
    )
    
  })
  
  # Scatter Plot of pH Level vs Turbidity
  output$scatterPlot <- renderPlot({
    
    plot(
      water$pH_Level,
      water$Turbidity,
      pch=19,
      col="red",
      xlab="pH Level",
      ylab="Turbidity",
      main="pH Level vs Turbidity"
    )
    
    abline(
      lm(Turbidity ~ pH_Level, data=water),
      col="blue",
      lwd=2
    )
    
  })
  
  # Interactive Data Table
  output$waterTable <- renderDT({
    
    datatable(
      water,
      options=list(pageLength=5),
      caption="Water Samples Data"
    )
    
  })
  
}

shinyApp(ui, server)