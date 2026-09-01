library(shiny)
library(DT)

library_data <- data.frame(
  User_ID = c(1,2,3,4,5),
  Books_Borrowed = c(2,5,3,6,1),
  Days_Kept = c(10,25,14,30,7),
  Fine_Amount = c(0,15,0,20,0)
)

library_data$Fine_Status <- ifelse(
  library_data$Fine_Amount > 0,
  "With Fine",
  "No Fine"
)

ui <- fluidPage(
  
  titlePanel("Library Borrowing Records Dashboard"),
  
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
           DTOutput("libraryTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram
  output$histPlot <- renderPlot({
    
    hist(
      library_data$Books_Borrowed,
      col="skyblue",
      main="Histogram of Books Borrowed",
      xlab="Books Borrowed",
      ylab="Frequency"
    )
    
  })
  
  # Pie Chart
  output$pieChart <- renderPlot({
    
    fine <- table(library_data$Fine_Status)
    
    pie(
      fine,
      col=c("lightgreen","orange"),
      main="Users With and Without Fines"
    )
    
  })
  
  # Bar Chart
  output$barChart <- renderPlot({
    
    barplot(
      library_data$Fine_Amount,
      names.arg=library_data$User_ID,
      col="steelblue",
      main="Fine Amount by User",
      xlab="User ID",
      ylab="Fine Amount"
    )
    
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    
    plot(
      library_data$Days_Kept,
      library_data$Fine_Amount,
      pch=19,
      col="red",
      xlab="Days Kept",
      ylab="Fine Amount",
      main="Days Kept vs Fine Amount"
    )
    
    abline(
      lm(Fine_Amount ~ Days_Kept, data=library_data),
      col="blue",
      lwd=2
    )
    
  })
  
  # Data Table
  output$libraryTable <- renderDT({
    
    datatable(
      library_data,
      options=list(pageLength=5),
      caption="Library Borrowing Records"
    )
    
  })
  
}

shinyApp(ui, server)