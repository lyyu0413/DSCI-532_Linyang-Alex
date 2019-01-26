library(shiny)
library(tidyverse)
library(ggmap)
library(maps)
library(rsconnect)
library(sp)

# import packages

suppressPackageStartupMessages(library(ggplot2))
library(plotly)
library(leaflet)
library(geojsonio)

# import data for geom info
states_plot <- geojson_read("tempgeo.json", what = "sp")

# import data for analysis
mental_data <- read.csv("./data/cleaned_data.csv", stringsAsFactors = FALSE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Mental Health Conditions  Tech Companies"),
  sidebarLayout(
    sidebarPanel(
    # Question input define
      selectInput("Question_main", "Please select one question that you are interested in and see other people's response towards it:", c("Do you know the options for mental health care your employer provides?" = 'care_options_score',
                                                       "Does your employer provide resources to learn more about mental health issues and how to seek help?" = "seek_help_score",
                                                       "Do you think that discussing a mental health issue with your employer would have negative consequences?" = "mental_health_interview_score",
                                                       "Do you think that discussing a physical health issue with your employer would have negative consequences?" = "phys_health_interview_score",
                                                       "Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?" = "anonymity_score"
                                                       )),
      # Age input define
      sliderInput("ageInput", "Age",
                  min = 10, max = 60, value = c(18, 40)),
      # gender input define
      radioButtons("genderInput", "Gender", choices = c("Male" ='M',
                                                              "Female" = 'F',
                                                              "All" = "All"),
                   selected = "All"),
      # Workflexibility
      sliderInput("freedomInput", "Working Flexibility", min = 0, max = 5, value = c(0, 5)),
      #sliderInput("freedomInput", "How much freedom do you feel you have to manage a mental illness in your workplace?",min=-5,max=5,value=c(-5,5)),


      # Family history
      radioButtons("famInput", "Family History", choices = c("Respondents with family history of mental illness" ='Yes',
                                                                                    "Respondents without family history of mental illness" = 'No',
                                                                                    "All Respondents" = "All Respondents"),
                   selected = "All Respondents"),

      # previous treatment
      radioButtons("treatmentInput", "Treatment for Mental Health", choices = c("Respondents have received mental health treatment" = 'Yes',
                                                                                                   "Respondents have never received mental health treatment" = 'No',
                                                                                                   "All Respondents" = 'All Respondents'),
                   selected = "All Respondents"),

      # Added explaination for work flexibility
      p("*Working Flexibility is a self-defined variable, which takes in to consideration of five separate variables in the original survey: self_employed, wellness_program, benefits, remote_work, leave",style = "font-family: 'times'; font-si16pt")

    ),
    mainPanel(
      h4("Positive & Negative Responses by U.S. States"),
      leafletOutput("mymap",height = 400),
      h4("Individual Responses by Country"),
      plotOutput("demo_hist")


      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

  observe(print(input$ageInput))

  # filters based on the input from the sliderbar
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

    if(input$genderInput == 'All'){
      mental_select <- mh_filtered_age_workf()
    }else{
      mental_select <- mh_filtered_age_workf() %>%
        filter(Gender == input$genderInput)
    }
    mental_select
  })

  mh_filter_family <- reactive({

    if(input$famInput == 'All Respondents'){
      mental_select <- mh_filter_gender()
    }else{
      mental_select <- mh_filter_gender() %>%
        filter(family_history == input$famInput)
    }
    mental_select
  })


  mh_filtered <- reactive({
    if(input$treatmentInput == 'All Respondents'){
      mental_select <- mh_filter_family()
    }else{
      mental_select <- mh_filter_family() %>%
        filter(treatment == input$treatmentInput)
    }
  })


  # combine analysed data with geom information
  mh_calculater_us <- reactive({
    mh <- mh_filtered() %>%
      filter(Country == 'United States') %>%
      group_by(state) %>%
      summarise(n = n(),
                avg_score = sum(question, is.na=TRUE)/n)

      states <- map_data("state")
      x <- states$region
      x <- stringr::str_to_title(x)
      states$state <- state.abb[match(x,state.name)]
      states$region <- x

      mh_sub <- left_join(states, mh, by ='state')

      mh_sub <- mh_sub %>% select(region, avg_score) %>% unique()
      mh_sub

  })

  # make interactive map plots
  output$mymap <- renderLeaflet({
  # combine analysed data with geom information
    mh_sub <- mh_calculater_us()
    spatial_score <- sp::merge(states_plot, mh_sub, by.x = 'name', by.y = 'region')

    # initialize the map
    m <- leaflet(spatial_score) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('pk.eyJ1IjoibHl5dTA0MTMiLCJhIjoiY2pyOWtlOGtoMGIzNDN5cnB3eDQyeTlxaSJ9.eDus6kDq3Bn7sO5bYoTjVw')))

    # define lables and colours
    labels <- sprintf(
      "<strong>%s</strong><br/> average response score: %g",
      spatial_score$name, spatial_score$avg_score
    ) %>% lapply(htmltools::HTML)
    pal <- colorBin("YlOrRd", domain = spatial_score$avg_score, bins = 5)

    # add interactive layers to the plot
    m <- m %>% addPolygons(
      fillColor = ~pal(avg_score),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))

    # add ledegend
    m <- m %>% addLegend(pal = pal, values = ~avg_score, opacity = 0.7, title = "Averaged Response: Yes=higher score, No=lower score",
                    position = "bottomright")
    m




  })



  output$demo_hist <- renderPlot({

    mh <- mh_filtered()

    if (dim(mh)[1] < 10) {print("There is no enough match data")
      }else{
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


      cbPalette <- c("#009E73", "#F0E442", "#0072B2")

      m <- ggplot(mh_final, aes(Country, norm_count, fill = Responses)) +
       geom_bar(stat = 'identity', position = 'dodge') +
       geom_text(aes(y = norm_count + .9,    # nudge above top of bar
                    label = paste0(norm_count, '%')),    # prettify
                position = position_dodge(width = 0.8),
                size = 4)+
        scale_fill_manual(values=cbPalette)+
        labs(
           y = "Proportion of different responses") +
        theme_bw()+
        theme(axis.title.x = element_blank(),
            axis.line = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(colour = "black", size = 14),
            legend.title = element_text(size = 14),
            legend.spacing = unit(5,'cm'))}
      m
  })




}
          

# Run the application
shinyApp(ui = ui, server = server)
