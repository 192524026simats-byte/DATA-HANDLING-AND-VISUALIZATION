library(shiny)
library(DT)

# Daily Fitness Activity Dataset
fitness <- data.frame(
  User_ID = c(1,2,3,4,5),
  Steps = c(7000,10000,8500,12000,6500),
  Calories_Burned = c(250,400,320,500,220),
  Active_Minutes = c(40,60,50,75,35)
)

# Activity Level Categories
fitness$Activity_Level <- ifelse(
  fitness$Steps < 8000, "Low",
  ifelse(fitness$Steps <= 10000, "Medium", "High")
)

ui <- fluidPage(
  
  titlePanel("Fitness Tracker Analysis Dashboard"),
  
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
           DTOutput("fitnessTable"))
  )
  
)

server <- function(input, output){
  
  # Histogram of Daily Steps
  output$histPlot <- renderPlot({
    
    hist(
      fitness$Steps,
      col="skyblue",
      main="Histogram of Daily Steps",
      xlab="Steps",
      ylab="Frequency"
    )
    
  })
  
  # Pie Chart of Activity Levels
  output$pieChart <- renderPlot({
    
    activity <- table(fitness$Activity_Level)
    
    pie(
      activity,
      col=c("orange","lightgreen","skyblue"),
      main="Activity Level Categories"
    )
    
  })
  
  # Bar Chart of Calories Burned by User
  output$barChart <- renderPlot({
    
    barplot(
      fitness$Calories_Burned,
      names.arg=fitness$User_ID,
      col="steelblue",
      main="Calories Burned by User",
      xlab="User ID",
      ylab="Calories Burned"
    )
    
  })
  
  # Scatter Plot of Steps vs Calories Burned
  output$scatterPlot <- renderPlot({
    
    plot(
      fitness$Steps,
      fitness$Calories_Burned,
      pch=19,
      col="red",
      xlab="Steps",
      ylab="Calories Burned",
      main="Steps vs Calories Burned"
    )
    
    abline(
      lm(Calories_Burned ~ Steps, data=fitness),
      col="blue",
      lwd=2
    )
    
  })
  
  # Interactive Table
  output$fitnessTable <- renderDT({
    
    datatable(
      fitness,
      options=list(pageLength=5),
      caption="Daily Fitness Activity Data"
    )
    
  })
  
}

shinyApp(ui, server)