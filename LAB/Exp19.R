library(shiny)
library(ggplot2)
library(DT)

mobile <- data.frame(
  User_ID = c("U01","U02","U03","U04","U05","U06"),
  Gender = c("Male","Female","Male","Female","Male","Female"),
  Age = c(20,22,19,21,23,20),
  Screen_Time = c(4.5,6.0,3.2,7.1,2.8,5.4),
  App_Usage_Count = c(18,25,12,30,10,22),
  Data_Used = c(2.4,3.8,1.6,4.5,1.2,3.1),
  Satisfaction = c(3,5,3,5,2,4),
  Usage_Date = as.Date(c(
    "2025-01-08",
    "2025-01-08",
    "2025-02-11",
    "2025-02-11",
    "2025-03-14",
    "2025-03-14"
  ))
)

ui <- fluidPage(
  
  titlePanel("Mobile App Usage Analysis Dashboard"),
  
  fluidRow(
    column(6, plotOutput("histPlot")),
    column(6, plotOutput("scatterPlot"))
  ),
  
  fluidRow(
    column(6, plotOutput("barPlot")),
    column(6, DTOutput("mobileTable"))
  )
  
)

server <- function(input, output){
  
  output$histPlot <- renderPlot({
    
    par(mfrow=c(1,2))
    
    hist(
      mobile$Screen_Time,
      col="skyblue",
      main="Histogram of Screen Time",
      xlab="Screen Time (Hours)",
      ylab="Frequency"
    )
    
    plot(
      density(mobile$Screen_Time),
      col="red",
      lwd=2,
      main="Density Plot",
      xlab="Screen Time (Hours)"
    )
    
  })
  
  output$scatterPlot <- renderPlot({
    
    corr <- cor(mobile$Data_Used, mobile$Screen_Time)
    
    ggplot(
      mobile,
      aes(Data_Used, Screen_Time)
    ) +
      geom_point(size=4,color="blue") +
      geom_smooth(method="lm",se=FALSE,color="red") +
      labs(
        title=paste(
          "Data Used vs Screen Time\nCorrelation =", round(corr,2)
        ),
        x="Data Used (GB)",
        y="Screen Time (Hours)"
      )
    
  })
  
  output$barPlot <- renderPlot({
    
    avg <- aggregate(
      Satisfaction ~ Gender,
      data=mobile,
      mean
    )
    
    bp <- barplot(
      avg$Satisfaction,
      names.arg=avg$Gender,
      col=c("orange","green"),
      ylim=c(0,6),
      xlab="Gender",
      ylab="Average Satisfaction",
      main="Average Satisfaction by Gender"
    )
    
    text(
      bp,
      avg$Satisfaction+0.2,
      labels=round(avg$Satisfaction,1)
    )
    
  })
  
  output$mobileTable <- renderDT({
    
    datatable(
      mobile,
      options=list(pageLength=6),
      caption="Mobile App Usage Data"
    )
    
  })
  
}

shinyApp(ui, server)