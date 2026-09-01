library(shiny)

# Product Category Dataset
product <- data.frame(
  Category = c("Electronics","Clothing","Appliances"),
  Sales = c(50000,35000,40000)
)

# UI
ui <- fluidPage(
  
  titlePanel("Product Category Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("cat","Select Category",
                  choices=c("All",product$Category))
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # Pie Chart
        tabPanel("Pie Chart",
                 plotOutput("pie")),
        
        # Funnel Chart
        tabPanel("Funnel Chart",
                 plotOutput("funnel")),
        
        # Sales Table
        tabPanel("Sales Table",
                 tableOutput("table"))
      )
    )
  )
)

# Server
server <- function(input, output){
  
  # Filter Data
  data <- reactive({
    if(input$cat=="All")
      product
    else
      subset(product, Category==input$cat)
  })
  
  # 1. Pie Chart
  output$pie <- renderPlot({
    
    d <- data()
    
    pie(d$Sales,
        labels=paste(d$Category,"\n$",d$Sales),
        col=rainbow(nrow(d)),
        main="Sales Distribution by Category")
    
  })
  
  # 2. Funnel Chart (Horizontal Bar Style)
  output$funnel <- renderPlot({
    
    d <- product[order(product$Sales,decreasing=TRUE),]
    
    par(mar=c(5,8,4,2))
    
    barplot(rev(d$Sales),
            horiz=TRUE,
            names.arg=rev(d$Category),
            col=c("skyblue","orange","lightgreen"),
            main="Sales Funnel",
            xlab="Sales ($)")
    
  })
  
  # 3. Sales Table
  output$table <- renderTable({
    data()
  })
  
}

# Run Dashboard
shinyApp(ui, server)