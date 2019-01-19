library(shiny)
library(tidyverse)
library(ggmap)
library(maps)
library(rsconnect)

suppressPackageStartupMessages(library(ggplot2))
library(plotly)


mental_data <- read.csv("./data/cleaned_data.csv", stringsAsFactors = FALSE)
#  select("Age":"phys_health_interview_score")

states <- map_data("state")
x <- states$region
x <- stringr::str_to_title(x)
states$state <- state.abb[match(x,state.name)]


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Mental Health Conditions  Tech Companies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Question_main", "Please select one question that you are interested in and see other people's response towards it:", c("Do you know the options for mental health care your employer provides?" = 'care_options_score',
                                                       "Does your employer provide resources to learn more about mental health issues and how to seek help?" = "seek_help_score",
                                                       "Do you think that discussing a mental health issue with your employer would have negative consequences?" = "mental_health_interview_score",
                                                       "Do you think that discussing a physical health issue with your employer would have negative consequences?" = "phys_health_interview_score",
                                                       "Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?" = "anonymity_score"
                                                       )),
      sliderInput("ageInput", "Age",
                  min = 10, max = 60, value = c(18, 40)),
      radioButtons("genderInput", "Gender", choices = c("Male" ='M',
                                                              "Female" = 'F',
                                                              "All condition included" = "All condition included"),
                   selected = "All condition included"),
      sliderInput("freedomInput", "Working Flexibility", min = 0, max = 5, value = c(0, 5)),
      #sliderInput("freedomInput", "How much freedom do you feel you have to manage a mental illness in your workplace?",min=-5,max=5,value=c(-5,5)),
      #### NOTE: How can we include an 'All' setting when we don't have it in our column for radioButtons?
      radioButtons("famInput", "Family History", choices = c("Respondents with family history of mental illness" ='Yes',
                                                                                    "Respondents without family history of mental illness" = 'No', 
                                                                                    "All condition included" = "All condition included"),
                   selected = "All condition included"),
      
      
      radioButtons("treatmentInput", "Treatment for Mental Health", choices = c("Respondents have received mental health treatment" = 'Yes',
                                                                                                   "Respondents have never received mental health treatment" = 'No',
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
      select(Age, Gender, work_freedom,family_history,treatment ,Country, state, input$Question_main)
    mental_select$question <- mental_select[,ncol(mental_select)]
    mental_select
    })
    
  mh_filter_gender <- reactive({
    
    if(input$genderInput == 'All condition included'){
      mental_select <- mh_filtered_age_workf()
    }else{
      mental_select <- mh_filtered_age_workf() %>%
        filter(Gender == input$genderInput)
    }
    mental_select
  })
  
  mh_filter_family <- reactive({
    
    if(input$famInput == 'All condition included'){
      mental_select <- mh_filter_gender()
    }else{
      mental_select <- mh_filter_gender() %>%
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
  
  output$demo_map <- renderPlot({
    
      data.state <- as.data.frame(state.center)
      data.state <- data.state %>% mutate(abb = state.abb)
      
      ggplot(mh_calculater_us()) +
      geom_polygon(aes(long, lat,group = group, fill = avg_score), color = 'black', na.rm = TRUE)+
      geom_text(data = data.state,aes(x, y, label = abb), color = 'white')+
      coord_fixed() +
      scale_fill_gradientn(colours=c('cornflowerblue','hotpink'),na.value = "transparent",
                           breaks=c(0.075,0.925),labels=c("Negative","Positive"),
                           limits=c(0,1)) +
      labs(title="Positive & Negative Responses by U.S. States") + 
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
    })
   

  
  output$demo_hist <- renderPlot({
    mh <- mh_filtered()
    mh$question <-as.factor(mh$question)
    
    mh_sub <- mh %>%
      group_by(Country, question) %>%
      summarise(n = n())
    
    c_total <- mh_sub %>%
      group_by(Country) %>%
      summarise(total = sum(n))
    
    mh_sub_with_total <- left_join(mh_sub, c_total, by = 'Country')
    
    mh_final <- mh_sub_with_total %>%
      mutate(norm_count = round(n/total * 100,2)) %>%
      mutate(Responses = ifelse(question == 1, 'Yes',
                                ifelse(question == '0.5', 'Maybe', 'No')))
    
    # did the factor relevel
    mh_final$Responses <- as.factor(mh_final$Responses)
    mh_final$Responses <- relevel(mh_final$Responses,"Yes")
    ggplot(mh_final, aes(Country, norm_count, fill = Responses)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_text(aes(y = norm_count + .9,    # nudge above top of bar
                    label = paste0(norm_count, '%')),    # prettify
                position = position_dodge(width = 0.8), 
                size = 4)+
      labs(title = "Individual Responses by Country",
           y = "Proportion of different responses") +
      theme_bw()+
      theme(axis.title.x = element_blank(),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(colour = "black", size = 14),
            legend.title = element_text(size = 14),
            legend.spacing = unit(5,'cm'))
      
  })
  
  
}
    
    

# output$pie_plot <- renderPlot(
#  mh_filtered() %>%
#    ggplot(aes(question, fill = groups))+
#    geom_bar()
#)
    
    #this is for the table below
    

# Run the application 
shinyApp(ui = ui, server = server)

