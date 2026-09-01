library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

energy <- data.frame(
  Sector = c("Residential","Commercial","Industrial","Residential","Commercial","Industrial"),
  Region = c("North","South","West","East","North","South"),
  Month = c("Jan","Jan","Feb","Feb","Mar","Mar"),
  Temperature = c(15,24,20,18,28,30),
  Units_Consumed = c(320,540,880,350,610,920),
  Cost = c(2100,3600,5900,2300,4100,6200),
  Renewable_Usage = c(22,18,12,25,20,15),
  Peak_Hours = c(4,6,8,5,7,9)
)

ui <- fluidPage(
  
  titlePanel("Energy Consumption Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sector","Select Sector",
                  choices = c("All", unique(energy$Sector)),
                  selected = "All")
    ),
    
    mainPanel(
      
      fluidRow(
        column(6,plotOutput("histPlot")),
        column(6,plotOutput("scatterPlot"))
      ),
      
      fluidRow(
        column(6,plotOutput("barPlot")),
        column(6,DTOutput("table"))
      )
      
    )
  )
)

server <- function(input, output){
  
  filtered <- reactive({
    
    if(input$sector=="All"){
      energy
    }else{
      subset(energy,Sector==input$sector)
    }
    
  })
  
  output$histPlot <- renderPlot({
    
    par(mfrow=c(1,2))
    
    hist(filtered()$Units_Consumed,
         col="skyblue",
         main="Histogram",
         xlab="Units Consumed")
    
    plot(
      density(filtered()$Units_Consumed),
      col="red",
      lwd=2,
      main="Density Plot",
      xlab="Units Consumed"
    )
    
  })
  
  output$scatterPlot <- renderPlot({
    
    ggplot(filtered(),
           aes(x=Temperature,
               y=Units_Consumed,
               size=Peak_Hours))+
      geom_point(alpha=0.6,
                 colour="blue")+
      labs(
        title="Temperature vs Units Consumed",
        x="Temperature (°C)",
        y="Units Consumed"
      )
    
  })
  
  output$barPlot <- renderPlot({
    
    avg <- filtered() %>%
      group_by(Sector) %>%
      summarise(Average_Renewable=mean(Renewable_Usage))
    
    ggplot(avg,
           aes(x=Sector,
               y=Average_Renewable,
               fill=Sector))+
      geom_bar(stat="identity")+
      labs(
        title="Average Renewable Usage by Sector",
        x="Sector",
        y="Average Renewable Usage (%)"
      )
    
  })
  
  output$table <- renderDT({
    
    datatable(filtered(),
              options=list(pageLength=6),
              caption="Energy Consumption Data")
    
  })
  
}

shinyApp(ui, server)