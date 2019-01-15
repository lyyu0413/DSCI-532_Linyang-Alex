library(shiny)
library(tidyverse)
mental_data <- read.csv("../data/cleaned_data.csv", stringsAsFactors = FALSE) %>% 
  select("Age":"phys_health_interview_score")
View(mental_data)
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Aspects of Mental Health in Tech"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageInput", "Select your age range of interest",
                  min = 5, max = 100, value = c(18, 40)),
      radioButtons("anonInput", "Perceived Mental Health Anonymity",
                   choices = c("Yes", "No", "Don't know"))
      
    ),
    mainPanel(
      plotOutput("demo_hist"),
      dataTableOutput("demo_table")
      # dataTableOutput("table")
      #tabsetPanel(type = "tabs",
                  #tabPanel("Histogram_Age", plotOutput("age_hist"))
      )
    )
    
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe(print(input$ageInput))
  
  mh_filtered <-  reactive(
    mental_data %>% 
      filter(Age > input$ageInput[1],
             Age < input$ageInput[2],
             anonymity == input$anonInput)
  )
  
  output$demo_hist <- renderPlot(
    mh_filtered() %>% 
      ggplot(aes(continent)) + 
      geom_bar(bins=20)
  )
  
  output$demo_table <- renderDataTable(
    mh_filtered())
    
    #this is for the table below
    
}

# Run the application 
shinyApp(ui = ui, server = server)

