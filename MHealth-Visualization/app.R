library(shiny)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)

mental_data <- read.csv("../data/cleaned_data.csv", stringsAsFactors = FALSE)
#  select("Age":"phys_health_interview_score")

states <- map_data("state")
x <- states$region
x <- stringr::str_to_title(x)
states$state <- state.abb[match(x,state.name)]


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Aspects of Mental Health in Tech"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Question_main", "Question-main:", c('Please Select a Question' = "",
                                                       'Have you sought treatment for a mental health condition?' = "treatment_score",
                                                       'If you have a mental health condition, do you feel that it interferes with your work?' = 'work_interfere_score',
                                                       "Do you know the options for mental health care your employer provides?" = 'care_options_score',
                                                       "Does your employer provide resources to learn more about mental health issues and how to seek help?" = "seek_help_score",
                                                       "Do you think that discussing a mental health issue with your employer would have negative consequences?" = "mental_health_interview_score",
                                                       "Do you think that discussing a physical health issue with your employer would have negative consequences?" = "phys_health_interview_score"
                                                       )),
      sliderInput("ageInput", "Select your age range of interest",
                  min = 5, max = 100, value = c(18, 40)),
      sliderInput("freedomInput", "How would you rate how free your work is?", min = -5, max = 5, value = c(-5, 5)),
      #sliderInput("freedomInput", "How much freedom do you feel you have to manage a mental illness in your workplace?",min=-5,max=5,value=c(-5,5)),
      #### NOTE: How can we include an 'All' setting when we don't have it in our column for radioButtons?
      radioButtons("famInput", "Do you have a family history of mental illness?", c("Yes" ='Yes',
                                                                                    "No" = 'No', 
                                                                                    "All condition included" = "All condition included")),
      
      radioButtons("anonInput", "Is your anonymity protected at work if you sought help?",
                  choices = c("Yes" = 'Yes',
                    "No"= 'No', 
                    "Don't know" = "Don't know", 
                    "All condition included" = "All condition included"),
                  selected = "All condition included"),
      radioButtons("treatmentInput", "Have you sought treatment for a mental health condition?", c("Yes" = 'Yes',
                                                                                                   "No" = 'No',
                                                                                                   "All condition included" = 'All condition included'))
     
    ),
    mainPanel(
      plotOutput("demo_map"),
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
  
  mh_filtered_age_workf <-  reactive(
    mental_data %>% 
      filter(Age > input$ageInput[1],
              Age < input$ageInput[2],
             work_freedom > input$freedomInput[1],
             work_freedom < input$freedomInput[2]) %>%
      select(Age, work_freedom,family_history,anonymity,treatment  ,Country, state,input$Question_main)
    )
  
  mh_filter_family <- reactive({
    
    if(input$famInput == 'All condition included'){
      mental_select <- mh_filtered_age_workf()
    }else{
      mental_select <- mh_filtered_age_workf() %>%
        filter(family_history == input$famInput)
    }
    mental_select
  })
  
  mh_filter_anon <- reactive({
    if (input$anonInput == 'All condition included'){
      mental_select <- mh_filter_family()
    }else{
      mental_select <- mh_filter_family()%>%
        filter(anonymity == input$anonInput)
    }
    mental_select
  })
  
  mh_filtered <- reactive({
    if(input$treatmentInput == 'All condition included'){
      mental_select <- mh_filter_anon()
    }else{
      mental_select <- mh_filter_anon() %>%
        filter(treatment == input$treatmentInput)
    }
    mental_select
  })
    
    
  
  mh_calculater_us <- reactive(
    mh_filtered() %>%
      filter(Country == 'United States') %>%
      group_by(state) %>%
      summarise(n = n(),
                avg_score = sum( is.na=TRUE)/n) %>%
      left_join(states, by ='state')
  )
  
  output$demo_map <- renderPlot(
    mh_calculater_us() %>%
      ggplot(aes(long, lat)) +
      geom_polygon(aes(group = group, fill = avg_score)) +
      coord_fixed())
  
  output$demo_hist <- renderPlot(
    mh_filtered() %>% 
      ggplot(aes(Age)) + 
      geom_histogram(bins=20) + xlab("Age")
  )
  
  output$demo_table <- renderDataTable(
    mh_filtered())
  
  
}
    
    

# output$pie_plot <- renderPlot(
#  mh_filtered() %>%
#    ggplot(aes(question, fill = groups))+
#    geom_bar()
#)
    
    #this is for the table below
    

# Run the application 
shinyApp(ui = ui, server = server)

