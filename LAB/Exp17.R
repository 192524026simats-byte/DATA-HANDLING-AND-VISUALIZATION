library(shiny)
library(ggplot2)
library(DT)

vehicle <- data.frame(
  Vehicle_ID = c("V1","V2","V3","V4","V5"),
  Engine_Size = c(1.5,2.0,3.0,2.5,1.8),
  Horsepower = c(110,150,250,200,130),
  Fuel_Efficiency = c(18,15,12,14,17),
  Top_Speed = c(180,200,250,220,190),
  Safety_Rating = factor(c(4,5,5,4,3))
)

ui <- fluidPage(
  
  titlePanel("Vehicle Performance Analysis Dashboard"),
  
  fluidRow(
    column(6,
           plotOutput("violinPlot")),
    column(6,
           plotOutput("scatterPlot"))
  ),
  
  fluidRow(
    column(6,
           plotOutput("heatmap")),
    column(6,
           DTOutput("vehicleTable"))
  )
  
)

server <- function(input, output){
  
  output$violinPlot <- renderPlot({
    
    ggplot(vehicle,
           aes(x=Safety_Rating,
               y=Fuel_Efficiency,
               fill=Safety_Rating))+
      geom_violin(trim=FALSE)+
      geom_boxplot(width=0.1,fill="white")+
      labs(title="Fuel Efficiency by Safety Rating",
           x="Safety Rating",
           y="Fuel Efficiency (km/l)")
    
  })
  
  output$scatterPlot <- renderPlot({
    
    ggplot(vehicle,
           aes(x=Horsepower,
               y=Top_Speed,
               color=Engine_Size))+
      geom_point(size=4)+
      geom_smooth(method="lm",
                  se=FALSE,
                  color="black")+
      labs(title="Horsepower vs Top Speed",
           x="Horsepower",
           y="Top Speed (km/h)",
           color="Engine Size")
    
  })
  
  output$heatmap <- renderPlot({
    
    cor_data <- cor(vehicle[,2:5])
    
    image(
      1:nrow(cor_data),
      1:ncol(cor_data),
      cor_data,
      axes=FALSE,
      xlab="",
      ylab="",
      main="Correlation Heatmap"
    )
    
    axis(1,
         at=1:nrow(cor_data),
         labels=colnames(cor_data))
    
    axis(2,
         at=1:ncol(cor_data),
         labels=colnames(cor_data))
    
  })
  
  output$vehicleTable <- renderDT({
    
    datatable(
      vehicle,
      options=list(pageLength=5),
      caption="Vehicle Performance Data"
    )
    
  })
  
}

shinyApp(ui,server)