library(shiny)
library(ggplot2)
library(GGally)
library(DT)

patient <- data.frame(
  Patient_ID = c("P1","P2","P3","P4","P5"),
  Age = c(25,40,55,35,60),
  BMI = c(22,28,30,26,32),
  BP = c(120,135,145,130,150),
  Cholesterol = c(180,210,240,200,260)
)

ui <- fluidPage(
  
  titlePanel("Patient Health Risk Analysis Dashboard"),
  
  fluidRow(
    column(6,
           plotOutput("scatterMatrix")),
    column(6,
           plotOutput("qqPlot"))
  ),
  
  fluidRow(
    column(6,
           plotOutput("ecdfPlot")),
    column(6,
           plotOutput("barChart"))
  ),
  
  fluidRow(
    column(12,
           DTOutput("patientTable"))
  )
  
)

server <- function(input, output){
  
  output$scatterMatrix <- renderPlot({
    
    ggpairs(patient[,c("Age","BMI","BP","Cholesterol")])
    
  })
  
  output$qqPlot <- renderPlot({
    
    qqnorm(patient$Cholesterol,
           main="Q-Q Plot of Cholesterol")
    qqline(patient$Cholesterol,col="red",lwd=2)
    
  })
  
  output$ecdfPlot <- renderPlot({
    
    plot(ecdf(patient$Cholesterol),
         main="ECDF of Cholesterol",
         xlab="Cholesterol",
         ylab="ECDF",
         col="blue",
         lwd=2)
    
  })
  
  output$barChart <- renderPlot({
    
    avg <- c(
      mean(patient$Age),
      mean(patient$BMI),
      mean(patient$BP),
      mean(patient$Cholesterol)
    )
    
    barplot(avg,
            names.arg=c("Age","BMI","BP","Cholesterol"),
            col=c("skyblue","orange","green","pink"),
            xlab="Health Indicators",
            ylab="Average Value",
            main="Average Health Indicators")
    
  })
  
  output$patientTable <- renderDT({
    
    datatable(patient,
              options=list(pageLength=5),
              caption="Patient Health Data")
    
  })
  
}

shinyApp(ui,server)
