library(ggplot2)
library(plotly)
library(shiny)
sales_data <- data.frame(
  Month = factor(c("January", "February", "March", "April", "May"), 
                 levels = c("January", "February", "March", "April", "May")),
  Sales = c(15000, 18000, 22000, 20000, 23000),
  Product = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  Ad_Budget = c(1200, 1500, 2000, 1800, 2100)
)
line_chart <- ggplot(sales_data, aes(x = Month, y = Sales, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Monthly Sales Trend",
       x = "Month",
       y = "Sales (in $)") +
  theme_minimal()

print(line_chart)
bar_chart <- ggplot(sales_data, aes(x = reorder(Product, -Sales), y = Sales, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Top-Selling Products",
       x = "Product",
       y = "Sales (in $)") +
  theme_minimal() +
  theme(legend.position = "none")

print(bar_chart)
scatter_plot <- ggplot(sales_data, aes(x = Ad_Budget, y = Sales)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dashed") +
  labs(title = "Advertising Budget vs. Monthly Sales",
       x = "Advertising Budget (in $)",
       y = "Sales (in $)") +
  theme_minimal()
print(scatter_plot)
ui <- fluidPage(    
  titlePanel("Sales Data Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("Explore the Monthly Sales Data interactively."),
      selectInput("chart_type", "Select Chart to View:",
                  choices = c("Line Chart (Monthly Sales)", 
                              "Bar Chart (Top Products)")),
      hr(),
      p("Hover over the charts to see exact data points!")
    ),
    mainPanel(
      plotlyOutput("interactive_plot")
    )
  )
)

server <- function(input, output) {
  
  output$interactive_plot <- renderPlotly({
    if (input$chart_type == "Line Chart (Monthly Sales)") {
      ggplotly(line_chart)
    } else {
      ggplotly(bar_chart)
    }
  })
}
shinyApp(ui = ui, server = server)