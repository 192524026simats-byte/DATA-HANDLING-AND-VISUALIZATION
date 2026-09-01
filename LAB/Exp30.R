library(shiny)
library(DT)

# Music Streaming Dataset
music <- data.frame(
  Song_ID = c(1,2,3,4,5),
  Duration = c(3.5,4.2,3.8,5.0,4.1),
  Streams = c(150,200,180,250,170),
  Genre = c("Pop","Rock","Pop","Hip-Hop","Rock")
)

ui <- fluidPage(
  
  titlePanel("Music Streaming Analysis Dashboard"),
  
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
           DTOutput("musicTable"))
  )
  
)

server <- function(input, output){
  
  # 1. Histogram of Song Durations
  output$histPlot <- renderPlot({
    
    hist(
      music$Duration,
      col = "skyblue",
      main = "Histogram of Song Durations",
      xlab = "Duration (Minutes)",
      ylab = "Frequency"
    )
    
  })
  
  # 2. Pie Chart of Genre Distribution
  output$pieChart <- renderPlot({
    
    genre <- table(music$Genre)
    
    pie(
      genre,
      col = c("orange","lightgreen","skyblue"),
      main = "Genre Distribution"
    )
    
  })
  
  # 3. Bar Chart of Average Streams by Genre
  output$barChart <- renderPlot({
    
    avg <- aggregate(Streams ~ Genre,
                     data = music,
                     mean)
    
    barplot(
      avg$Streams,
      names.arg = avg$Genre,
      col = c("red","green","blue"),
      main = "Average Streams by Genre",
      xlab = "Genre",
      ylab = "Average Streams (000s)"
    )
    
  })
  
  # 4. Scatter Plot of Duration vs Streams
  output$scatterPlot <- renderPlot({
    
    plot(
      music$Duration,
      music$Streams,
      pch = 19,
      col = "purple",
      xlab = "Duration (Minutes)",
      ylab = "Streams (000s)",
      main = "Duration vs Streams"
    )
    
    abline(
      lm(Streams ~ Duration, data = music),
      col = "blue",
      lwd = 2
    )
    
  })
  
  # Interactive Data Table
  output$musicTable <- renderDT({
    
    datatable(
      music,
      options = list(pageLength = 5),
      caption = "Music Streaming Data"
    )
    
  })
  
}

shinyApp(ui, server)