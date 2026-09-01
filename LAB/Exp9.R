library(shiny)

# Survey Responses Dataset
survey <- data.frame(
  SurveyID = c(1,2,3),
  Question1 = c("A","B","C"),
  Question2 = c("B","A","A"),
  Question3 = c("C","D","B")
)

# UI
ui <- fluidPage(
  titlePanel("Survey Responses Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("q","Select Question",
                  choices=c("Question1","Question2","Question3"))
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Grouped Bar Chart
        tabPanel("Grouped Bar Chart",
                 plotOutput("groupbar")),
        
        # Stacked Bar Chart
        tabPanel("Stacked Bar Chart",
                 plotOutput("stackbar")),
        
        # Survey Table
        tabPanel("Survey Table",
                 tableOutput("table"))
      )
    )
  )
)

# Server
server <- function(input, output){
  
  # 1. Grouped Bar Chart
  output$groupbar <- renderPlot({
    
    barplot(table(survey[[input$q]]),
            beside=TRUE,
            col="skyblue",
            main=paste("Responses for",input$q),
            xlab="Answer",
            ylab="Frequency")
    
  })
  
  # 2. Stacked Bar Chart
  output$stackbar <- renderPlot({
    
    tab <- rbind(
      table(factor(survey$Question1,levels=c("A","B","C","D"))),
      table(factor(survey$Question2,levels=c("A","B","C","D"))),
      table(factor(survey$Question3,levels=c("A","B","C","D")))
    )
    
    rownames(tab) <- c("Question1","Question2","Question3")
    
    barplot(t(tab),
            col=rainbow(4),
            legend=colnames(tab),
            main="Overall Survey Responses",
            xlab="Questions",
            ylab="Frequency")
    
  })
  
  # 3. Survey Table
  output$table <- renderTable({
    survey
  })
  
}

# Run Dashboard
shinyApp(ui, server)