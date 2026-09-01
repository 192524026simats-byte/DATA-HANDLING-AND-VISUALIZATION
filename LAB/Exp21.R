library(shiny)
library(DT)

movie <- data.frame(
  Movie_ID = c(1,2,3,4,5),
  Genre = c("Action","Comedy","Drama","Action","Comedy"),
  Rating = c(4.5,3.8,4.2,4.7,3.5),
  Duration = c(120,90,140,130,95)
)

ui <- fluidPage(
  
  titlePanel("Movie Ratings Analysis Dashboard"),
  
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
           DTOutput("movieTable"))
  )
  
)

server <- function(input, output){
  
  output$histPlot <- renderPlot({
    
    hist(
      movie$Rating,
      col="skyblue",
      main="Histogram of Movie Ratings",
      xlab="Rating",
      ylab="Frequency"
    )
    
  })
  
  output$pieChart <- renderPlot({
    
    genre <- table(movie$Genre)
    
    pie(
      genre,
      col=c("orange","lightgreen","skyblue"),
      main="Genre Distribution"
    )
    
  })
  
  output$barChart <- renderPlot({
    
    avg <- aggregate(Rating ~ Genre,
                     data=movie,
                     mean)
    
    barplot(
      avg$Rating,
      names.arg=avg$Genre,
      col=c("red","green","blue"),
      main="Average Rating by Genre",
      xlab="Genre",
      ylab="Average Rating"
    )
    
  })
  
  output$scatterPlot <- renderPlot({
    
    plot(
      movie$Duration,
      movie$Rating,
      pch=19,
      col="purple",
      xlab="Duration (Minutes)",
      ylab="Rating",
      main="Duration vs Rating"
    )
    
    abline(
      lm(Rating ~ Duration, data=movie),
      col="red",
      lwd=2
    )
    
  })
  
  output$movieTable <- renderDT({
    
    datatable(
      movie,
      options=list(pageLength=5),
      caption="Movie Ratings Data"
    )
    
  })
  
}

shinyApp(ui, server)