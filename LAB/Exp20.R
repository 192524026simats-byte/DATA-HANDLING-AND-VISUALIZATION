library(shiny)
library(DT)

hospital <- data.frame(
  Patient_ID = c(1,2,3,4,5),
  Age = c(25,40,35,50,29),
  Waiting_Time = c(2,5,1,7,3),
  Appointment_Status = c("Attended","Missed","Attended","Missed","Attended")
)

ui <- fluidPage(
  
  titlePanel("Hospital Appointment Analysis Dashboard"),
  
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
           DTOutput("hospitalTable"))
  )
  
)

server <- function(input, output){
  
  output$histPlot <- renderPlot({
    
    hist(
      hospital$Age,
      col="skyblue",
      main="Histogram of Patient Ages",
      xlab="Age",
      ylab="Frequency"
    )
    
  })
  
  output$pieChart <- renderPlot({
    
    status <- table(hospital$Appointment_Status)
    
    pie(
      status,
      col=c("lightgreen","orange"),
      main="Appointment Status Distribution"
    )
    
  })
  
  output$barChart <- renderPlot({
    
    barplot(
      hospital$Waiting_Time,
      names.arg=hospital$Patient_ID,
      col="steelblue",
      main="Waiting Time by Patient",
      xlab="Patient ID",
      ylab="Waiting Time (Days)"
    )
    
  })
  
  output$scatterPlot <- renderPlot({
    
    plot(
      hospital$Age,
      hospital$Waiting_Time,
      pch=19,
      col="red",
      xlab="Age",
      ylab="Waiting Time (Days)",
      main="Age vs Waiting Time"
    )
    
    abline(
      lm(Waiting_Time ~ Age, data=hospital),
      col="blue",
      lwd=2
    )
    
  })
  
  output$hospitalTable <- renderDT({
    
    datatable(
      hospital,
      options=list(pageLength=5),
      caption="Hospital Appointment Data"
    )
    
  })
  
}

shinyApp(ui, server)