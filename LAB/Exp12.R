library(shiny)
library(DT)

inventory <- data.frame(
  ProductID = c(1,2,3),
  ProductName = c("Product A","Product B","Product C"),
  Category = c("Electronics","Electronics","Accessories"),
  Quantity = c(250,175,300),
  Price = c(20,15,18)
)

ui <- fluidPage(
  
  titlePanel("Product Inventory Management Dashboard"),
  
  fluidRow(
    column(6,
           plotOutput("barplot")
    ),
    column(6,
           plotOutput("stackedbar")
    )
  ),
  
  fluidRow(
    column(12,
           DTOutput("inventoryTable")
    )
  )
  
)

server <- function(input, output){
  
  output$barplot <- renderPlot({
    
    barplot(
      inventory$Quantity,
      names.arg = inventory$ProductName,
      col = c("steelblue","orange","green"),
      xlab = "Product Name",
      ylab = "Quantity Available",
      main = "Quantity of Products in Inventory"
    )
    
  })
  
  output$stackedbar <- renderPlot({
    
    qty <- xtabs(Quantity ~ Category + ProductName, data = inventory)
    
    barplot(
      qty,
      beside = FALSE,
      col = c("skyblue","orange","green"),
      xlab = "Category",
      ylab = "Quantity",
      main = "Stacked Bar Chart by Product Category"
    )
    
    legend(
      "topright",
      legend = rownames(qty),
      fill = c("skyblue","orange","green")
    )
    
  })
  
  output$inventoryTable <- renderDT({
    
    datatable(
      inventory,
      options = list(pageLength = 5),
      caption = "Product Inventory Data"
    )
    
  })
  
}

shinyApp(ui, server)