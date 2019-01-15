library(shiny)
library(tidyverse)
mental_data <- read.csv("../data/cleaned_data.csv", stringsAsFactors = FALSE) %>% 
  select("Age":"phys_health_interview_score")
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Aspects of Mental Health in Tech"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageInput", "Select your age range of interest",
                  min = 5, max = 100, value = c(18, 40)),
      #### NOTE: How can we include an 'All' setting when we don't have it in our column for radioButtons?
      radioButtons("anonInput", "Is your anonymity protected at work if you seek help?",
                   choices = c("Yes", "No", "Don't know"),inline=T),
      radioButtons("famInput", "Do you have a family history of mental illness?", 
                   choices = c("Yes", "No"), inline=T)
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
             anonymity == input$anonInput,
             family_history == input$famInput)
  )
  
  output$demo_hist <- renderPlot(
    mh_filtered() %>% 
      ggplot(aes(Age)) + 
      geom_histogram(bins=20) + xlab("Age")
  )
  
  output$demo_table <- renderDataTable(
    mh_filtered())
    
    #this is for the table below
    
}

# Run the application 
shinyApp(ui = ui, server = server)

