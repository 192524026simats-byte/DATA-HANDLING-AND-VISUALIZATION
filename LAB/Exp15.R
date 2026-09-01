library(shiny)
library(ggplot2)
library(DT)
library(zoo)

student <- data.frame(
  Student_ID = c("S01","S02","S03","S04","S05","S06"),
  Gender = c("Male","Female","Male","Female","Male","Female"),
  Study_Hours = c(2.0,3.5,1.5,4.0,2.8,3.0),
  Attendance = c(78,90,70,95,85,92),
  Math_Score = c(62,80,55,90,72,82),
  Science_Score = c(65,85,58,92,74,86),
  Exam_Date = as.Date(c("2025-01-10","2025-01-10","2025-02-12",
                        "2025-02-12","2025-03-15","2025-03-15"))
)

student$Month <- format(student$Exam_Date,"%b")

ui <- fluidPage(
  
  titlePanel("Student Mini Data Analysis Dashboard"),
  
  fluidRow(
    column(6,plotOutput("histPlot")),
    column(6,plotOutput("boxPlot"))
  ),
  
  fluidRow(
    column(6,plotOutput("scatterPlot")),
    column(6,plotOutput("linePlot"))
  ),
  
  fluidRow(
    column(12,DTOutput("studentTable"))
  )
  
)

server <- function(input, output){
  
  output$histPlot <- renderPlot({
    
    ggplot(student,aes(Math_Score))+
      geom_histogram(binwidth=10,fill="skyblue",color="black")+
      labs(title="Histogram of Math Scores",
           x="Math Score",
           y="Frequency")
    
  })
  
  output$boxPlot <- renderPlot({
    
    ggplot(student,aes(Gender,Science_Score,fill=Gender))+
      geom_boxplot()+
      labs(title="Science Score by Gender",
           x="Gender",
           y="Science Score")
    
  })
  
  output$scatterPlot <- renderPlot({
    
    ggplot(student,
           aes(Study_Hours,
               Math_Score,
               color=Gender))+
      geom_point(size=3)+
      geom_smooth(method="lm",se=FALSE,color="black")+
      labs(title="Study Hours vs Math Score",
           x="Study Hours",
           y="Math Score")
    
  })
  
  output$linePlot <- renderPlot({
    
    avg <- aggregate(Math_Score~Exam_Date,student,mean)
    avg$MA <- rollmean(avg$Math_Score,k=2,fill=NA)
    
    ggplot(avg,aes(Exam_Date,Math_Score))+
      geom_line(color="blue",size=1)+
      geom_point(size=3)+
      geom_line(aes(y=MA),color="red",linetype="dashed",size=1)+
      labs(title="Monthly Average Math Score",
           x="Exam Date",
           y="Average Math Score")
    
  })
  
  output$studentTable <- renderDT({
    
    datatable(student,
              options=list(pageLength=6),
              caption="Student Mini Data")
    
  })
  
}

shinyApp(ui,server)