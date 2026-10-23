
library(shiny)

ui <- fluidPage(
  titlePanel("Baby Name Popularity Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Compare Five Names"),
      
      selectInput("name1", "Name 1", choices = character(0)),
      selectInput("name2", "Name 2", choices = character(0)),
      selectInput("name3", "Name 3", choices = character(0)),
      selectInput("name4", "Name 4", choices = character(0)),
      selectInput("name5", "Name 5", choices = character(0))
    ),
    
    mainPanel(
      width = 9,
      
      textOutput("data_status"),
      
      plotOutput(
        "xy_plot",
        height = "700px"
      )
    )
    
  )
)
