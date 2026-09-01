library(shiny)
library(ggplot2)
library(DT)
library(zoo)

student <- data.frame(
  Student_ID = c("S1","S2","S3","S4","S5"),
  Age = c(19,21,20,22,23),
  Study_Hours = c(12,8,15,10,7),
  Attendance = c(90,70,95,85,60),
  Test_Score = c(85,70,92,80,65),
  Participation_Score = c(8,7,9,8,6)
)

student$Attendance_Group <- cut(
  student$Attendance,
  breaks = quantile(student$Attendance,
                    probs = seq(0,1,0.25),
                    na.rm = TRUE),
  include.lowest = TRUE
)

ui <- fluidPage(
  
  titlePanel("Student Academic Performance Dashboard"),
  
  fluidRow(
    column(6,
           plotOutput("areaPlot")),
    column(6,
           plotOutput("boxPlot"))
  ),
  
  fluidRow(
    column(6,
           plotOutput("densityPlot")),
    column(6,
           DTOutput("studentTable"))
  )
  
)

server <- function(input, output){
  
  output$areaPlot <- renderPlot({
    
    mat <- rbind(student$Test_Score,
                 student$Participation_Score)
    
    x <- 1:nrow(student)
    
    plot(x,
         mat[1,],
         type="n",
         ylim=c(0,max(colSums(mat))),
         xlab="Students",
         ylab="Scores",
         main="Stacked Area Chart")
    
    polygon(c(x,rev(x)),
            c(rep(0,length(x)),rev(mat[1,])),
            col="skyblue",
            border=NA)
    
    polygon(c(x,rev(x)),
            c(mat[1,],rev(colSums(mat))),
            col="orange",
            border=NA)
    
    lines(x,colSums(mat),lwd=2)
    
    axis(1,
         at=x,
         labels=student$Student_ID)
    
    legend("topleft",
           legend=c("Test Score",
                    "Participation Score"),
           fill=c("skyblue","orange"))
    
  })
  
  output$boxPlot <- renderPlot({
    
    ggplot(student,
           aes(x=Attendance_Group,
               y=Study_Hours,
               fill=Attendance_Group))+
      geom_boxplot()+
      labs(title="Study Hours by Attendance Quartiles",
           x="Attendance Quartile",
           y="Study Hours")
    
  })
  
  output$densityPlot <- renderPlot({
    
    ggplot(student,
           aes(Test_Score))+
      geom_density(fill="lightgreen",
                   alpha=0.6)+
      geom_rug()+
      labs(title="Density Plot of Test Scores",
           x="Test Score",
           y="Density")
    
  })
  
  output$studentTable <- renderDT({
    
    datatable(student,
              options=list(pageLength=5),
              caption="Student Academic Performance Data")
    
  })
  
}

shinyApp(ui,server)