library(shiny)
library(DT)

# Social Media Posts Dataset
social <- data.frame(
  Post_ID = c(1,2,3,4,5),
  Likes = c(120,200,150,300,180),
  Comments = c(15,30,20,40,25),
  Shares = c(10,20,12,35,18)
)

ui <- fluidPage(
  
  titlePanel("Social Media Engagement Analysis Dashboard"),
  
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
           DTOutput("socialTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of Likes
  output$histPlot <- renderPlot({
    
    hist(
      social$Likes,
      col = "skyblue",
      main = "Histogram of Likes",
      xlab = "Likes",
      ylab = "Frequency"
    )
    
  })
  
  # Pie Chart of Total Engagement Components
  output$pieChart <- renderPlot({
    
    engagement <- c(
      sum(social$Likes),
      sum(social$Comments),
      sum(social$Shares)
    )
    
    pie(
      engagement,
      labels = c("Likes","Comments","Shares"),
      col = c("orange","lightgreen","skyblue"),
      main = "Total Engagement Components"
    )
    
  })
  
  # Bar Chart of Comments by Post
  output$barChart <- renderPlot({
    
    barplot(
      social$Comments,
      names.arg = social$Post_ID,
      col = "steelblue",
      main = "Comments by Post",
      xlab = "Post ID",
      ylab = "Comments"
    )
    
  })
  
  # Scatter Plot of Likes vs Shares
  output$scatterPlot <- renderPlot({
    
    plot(
      social$Likes,
      social$Shares,
      pch = 19,
      col = "red",
      xlab = "Likes",
      ylab = "Shares",
      main = "Likes vs Shares"
    )
    
    abline(
      lm(Shares ~ Likes, data = social),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Interactive Data Table
  output$socialTable <- renderDT({
    
    datatable(
      social,
      options = list(pageLength = 5),
      caption = "Social Media Posts Data"
    )
    
  })
  
}

shinyApp(ui, server)