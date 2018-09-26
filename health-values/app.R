library(shiny)
library(tidyverse)
health <- read_csv("../data/ghdx.csv")
values <- read_csv("../data/wvsData.csv")

ui <- fluidPage(
   
   titlePanel("Global Women's Health and World Values"),
   
   sidebarLayout(
      sidebarPanel(
        
      ),
      
      mainPanel(
      )
   )
)

server <- function(input, output) {
   
}

shinyApp(ui = ui, server = server)