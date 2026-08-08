library(shiny)

# Customer Demographics Dataset
customer <- data.frame(
  CustomerID = c(1,2,3),
  Age = c(28,35,42),
  Gender = c("Female","Male","Female"),
  Income = c(50000,60000,75000)
)

# UI
ui <- fluidPage(
  titlePanel("Customer Demographics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender","Select Gender",
                  choices=c("All", unique(customer$Gender)))
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Bar Chart
        tabPanel("Bar Chart",
                 plotOutput("bar")),
        
        # Pie Chart
        tabPanel("Pie Chart",
                 plotOutput("pie")),
        
        # Customer Table
        tabPanel("Customer Table",
                 tableOutput("table"))
      )
    )
  )
)

# Server
server <- function(input, output){
  
  # Filter Data
  data <- reactive({
    if(input$gender=="All")
      customer
    else
      subset(customer, Gender==input$gender)
  })
  
  # 1. Bar Chart (Age Distribution)
  output$bar <- renderPlot({
    d <- data()
    
    barplot(d$Age,
            names.arg=d$CustomerID,
            col="skyblue",
            main="Customer Age Distribution",
            xlab="Customer ID",
            ylab="Age")
  })
  
  # 2. Pie Chart (Gender Distribution)
  output$pie <- renderPlot({
    pie(table(customer$Gender),
        col=c("pink","lightblue"),
        main="Gender Distribution")
  })
  
  # 3. Customer Table
  output$table <- renderTable({
    data()
  })
  
}

# Run Dashboard
shinyApp(ui, server)