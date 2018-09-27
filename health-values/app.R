library(shiny)
library(tidyverse)
health <- read_csv("../data/ghdx.csv")
values <- read_csv("../data/wvsData.csv")

valueCountries <- sort(unique(values$Country))

ui <- fluidPage(
  
  titlePanel("Global Women's Health and World Values"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("countries_vTab", 
                  "Countries", 
                  choices = c("All", valueCountries),
                  selected = "All",
                  multiple = TRUE),
      
      checkboxGroupInput("value1_vTab",
                         "Justifiable?",
                         choices = names(values)[c(-1, -5)],
                         selected = names(values[2]))
    ),
    
    mainPanel(
      
      plotOutput("valuesPlot",
                 height = "800px")
      
    )
  )
)

server <- function(input, output) {
  
  output$valuesPlot <- renderPlot({
    
    if(length(input$value1_vTab) == 1){
      
      # Single Value Analysis
      if(input$countries_vTab == "All" & 
         length(input$countries_vTab) == 1){
        
        val_filt <- values %>%
          select(Country, "value" = input$value1_vTab)
        
        ggplot(data = val_filt,
               aes(y = value,
                   x = Country)) + 
          geom_boxplot() +
          labs(y = input$value1_vTab)
        
      } else {
        
        val_filt <- values %>%
          filter(Country %in% input$countries_vTab) %>%
          select(Country, "value" = input$value1_vTab)
        
        ggplot(data = val_filt,
               aes(y = value,
                   x = Country)) + 
          geom_boxplot() +
          labs(y = input$value1_vTab)
        
      }
    } else {
      
      if(length(input$value1_vTab == 2)){
        
        # Relate two Values
        if(input$countries_vTab == "All" & 
           length(input$countries_vTab) == 1){
          
          val_filt <- values %>%
            select(Country, 
                   "value1" = input$value1_vTab[1],
                   "value2" = input$value1_vTab[2])
          
          ggplot(data = val_filt,
                 aes(x = value1,
                     y = value2)) +
            geom_jitter(alpha = 0.05) +
            labs(x = input$value1_vTab[1],
                 y = input$value1_vTab[2])
          
        } else {
          
          val_filt <- values %>%
            filter(Country %in% input$countries_vTab) %>%
            select(Country, 
                   "value1" = input$value1_vTab[1],
                   "value2" = input$value1_vTab[2])
          
          ggplot(data = val_filt,
                 aes(x = value1,
                     y = value2,
                     color = Country)) +
            geom_jitter(alpha = 0.05) +
            labs(x = input$value1_vTab[1],
                 y = input$value1_vTab[2])
          
        }
        
      }
      
    } 
    
  })
  
}

shinyApp(ui = ui, server = server)