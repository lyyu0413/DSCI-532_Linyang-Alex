library(shiny)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(rsconnect)

mental_data <- read.csv("./data/cleaned_data.csv", stringsAsFactors = FALSE)
#  select("Age":"phys_health_interview_score")

states <- map_data("state")
x <- states$region
x <- stringr::str_to_title(x)
states$state <- state.abb[match(x,state.name)]


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Mental Heath Care Condition for Tech Companies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Question_main", "Please select one question that you are interested in and see other people's response towards it:", c("Do you know the options for mental health care your employer provides?" = 'care_options_score',
                                                       "Does your employer provide resources to learn more about mental health issues and how to seek help?" = "seek_help_score",
                                                       "Do you think that discussing a mental health issue with your employer would have negative consequences?" = "mental_health_interview_score",
                                                       "Do you think that discussing a physical health issue with your employer would have negative consequences?" = "phys_health_interview_score",
                                                       "Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?" = "anonymity_score"
                                                       )),
      sliderInput("ageInput", "People's age influence ",
                  min = 10, max = 60, value = c(18, 40)),
      sliderInput("freedomInput", "People's working freedom degree influence their responses towards the question, select the range you are interested in", min = 0, max = 5, value = c(0, 5)),
      #sliderInput("freedomInput", "How much freedom do you feel you have to manage a mental illness in your workplace?",min=-5,max=5,value=c(-5,5)),
      #### NOTE: How can we include an 'All' setting when we don't have it in our column for radioButtons?
      radioButtons("famInput", "Do you want to investigate the inpact of family illness history on people's response toward the question?", choices = c("People with family history of mental illness" ='Yes',
                                                                                    "People without family history of mental illness" = 'No', 
                                                                                    "All condition included" = "All condition included"),
                   selected = "All condition included"),
      
      
      radioButtons("treatmentInput", "People whoever think about taking mental health treatment may have different response towards the question, select the group you are interested in", choices = c("People want to have mental health treatment" = 'Yes',
                                                                                                   "People never think about having mental health treatment" = 'No',
                                                                                                   "All condition included" = 'All condition included'),
                   selected = "All condition included")
     
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
  
  mh_filtered_age_workf <-  reactive({
    mental_select <- mental_data %>% 
      filter(Age > input$ageInput[1],
              Age < input$ageInput[2],
             work_freedom > input$freedomInput[1],
             work_freedom < input$freedomInput[2]) %>%
      select(Age, work_freedom,family_history,treatment ,Country, state, input$Question_main)
    mental_select$question <- mental_select[,ncol(mental_select)]
    mental_select
    })
    
    
  
  mh_filter_family <- reactive({
    
    if(input$famInput == 'All condition included'){
      mental_select <- mh_filtered_age_workf()
    }else{
      mental_select <- mh_filtered_age_workf() %>%
        filter(family_history == input$famInput)
    }
    mental_select
  })
  
  
  mh_filtered <- reactive({
    if(input$treatmentInput == 'All condition included'){
      mental_select <- mh_filter_family()
    }else{
      mental_select <- mh_filter_family() %>%
        filter(treatment == input$treatmentInput)
    }
  })
  
  mh_calculater_us <- reactive({
    mh <- mh_filtered() %>%
      filter(Country == 'United States') %>%
      group_by(state) %>%
      summarise(n = n(),
                avg_score = sum(question, is.na=TRUE)/n)
      mh_sub <- left_join(states, mh, by ='state')
      mh_sub
  })
  
  output$demo_map <- renderPlot(
    mh_calculater_us() %>%
      ggplot(aes(long, lat)) +
      geom_polygon(aes(group = group, fill = avg_score), color = 'black', na.rm = TRUE)+
      coord_fixed() +
      scale_fill_gradientn(colours=c('cornflowerblue','hotpink'),na.value = "transparent",
                           breaks=c(0.075,0.925),labels=c("Negtive","Positive"),
                           limits=c(0,1)) +
      labs(title="the Mental Health Care condition for US tech companies, based on peoples response") + 
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.text = element_text(colour = "black", size = 14),
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.key.width = unit(1.5,"cm"))
    )
   

  
  output$demo_hist <- renderPlot({
    mh <- mh_filtered()
    mh$question <-as.factor(mh$question)
    mh_sub <- mh %>%
      group_by(Country, question) %>%
      summarise(n = n())
    mh_sub <- mh_sub %>% mutate(total = ifelse(Country == 'Canada', 56,
                                           ifelse(Country == 'United Kingdom', 139, 608)),
                                Responses = ifelse(question == 1, 'Yes',
                                                  ifelse(question == 0, 'No', 'Maybe'))) %>%
      mutate(norm_count = n/total)
    # did the factor relevel
    mh_sub$Responses <- as.factor(mh_sub$Responses)
    mh_sub$Responses <- relevel(mh_sub$Responses,"Yes")
    ggplot(mh_sub, aes(Country, norm_count, fill = Responses)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(title = "tech company Mental Health Care condition in different countries",
           y = "Proportion for different responses") +
      theme_bw()+
      theme(axis.title.x = element_blank(),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.text = element_text(colour = "black", size = 14),
            legend.title = element_text(size = 14),
            legend.spacing = unit(5,'cm'))
      
  })
  
  # output$demo_table <- renderDataTable(
   # mh_filtered())
  
  
}
    
    

# output$pie_plot <- renderPlot(
#  mh_filtered() %>%
#    ggplot(aes(question, fill = groups))+
#    geom_bar()
#)
    
    #this is for the table below
    

# Run the application 
shinyApp(ui = ui, server = server)

