library(shiny)

# Import Dataset
data <- read.csv("Online_Learning_Activity.csv",
                 stringsAsFactors = FALSE)

# Convert Date
data$Login_Date <- as.Date(data$Login_Date)

ui <- fluidPage(
  
  titlePanel("Online Learning Activity Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("gender","Gender",
                  choices=c("All",unique(data$Gender))),
      
      selectInput("course","Course",
                  choices=c("All",unique(data$Course)))
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Histogram & Boxplot",
                 
                 plotOutput("hist"),
                 plotOutput("box")),
        
        tabPanel("Scatter Plot",
                 
                 plotOutput("scatter")),
        
        tabPanel("Monthly Trend",
                 
                 plotOutput("line")),
        
        tabPanel("Student Table",
                 
                 tableOutput("table"))
      )
    )
  )
)

server <- function(input, output){
  
  d <- reactive({
    
    x <- data
    
    if(input$gender!="All")
      x <- subset(x, Gender==input$gender)
    
    if(input$course!="All")
      x <- subset(x, Course==input$course)
    
    x
  })
  
  # 1 Histogram
  output$hist <- renderPlot({
    
    hist(d()$Quiz_Score,
         col="skyblue",
         main="Quiz Score Distribution",
         xlab="Quiz Score",
         ylab="Frequency")
    
  })
  
  # Boxplot
  output$box <- renderPlot({
    
    boxplot(Quiz_Score~Course,
            data=d(),
            col=c("orange","lightgreen"),
            main="Quiz Score by Course",
            xlab="Course",
            ylab="Quiz Score")
    
  })
  
  # 2 Scatter Plot
  output$scatter <- renderPlot({
    
    symbols(d()$Study_Time,
            d()$Quiz_Score,
            circles=d()$Videos_Watched/5,
            inches=0.3,
            bg="blue",
            fg="black",
            xlab="Study Time (hrs)",
            ylab="Quiz Score",
            main="Study Time vs Quiz Score")
    
  })
  
  # 3 Monthly Trend
  output$line <- renderPlot({
    
    m <- aggregate(Quiz_Score~format(Login_Date,"%Y-%m"),
                   data,
                   mean)
    
    plot(m$Quiz_Score,
         type="o",
         xaxt="n",
         col="red",
         xlab="Month",
         ylab="Average Quiz Score",
         main="Monthly Average Quiz Score")
    
    axis(1,1:nrow(m),m[,1])
    
    ma <- stats::filter(m$Quiz_Score,
                        rep(1/2,2),
                        sides=1)
    
    lines(ma,col="blue",lwd=2)
    
    legend("topleft",
           legend=c("Average","Moving Average"),
           col=c("red","blue"),
           lty=1)
    
  })
  
  # 4 Table
  output$table <- renderTable({
    
    d()
    
  })
  
}

shinyApp(ui,server)