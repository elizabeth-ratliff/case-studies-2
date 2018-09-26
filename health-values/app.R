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
                    selected = NULL)
      ),
      
      mainPanel(
        
        plotOutput("valuesPlot")
        
      )
   )
)

server <- function(input, output) {
   
  output$valuesPlot <- renderPlot({
    ggplot()
  })
  
}

shinyApp(ui = ui, server = server)