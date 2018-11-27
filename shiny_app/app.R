library(shiny)
library(tidytext)
library(haven)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(plotly)
library(ggpubr)
library(tidyverse)

# Read in data
raw_data <- read_sav("33968-0001-Data.sav")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Enrollment in Infant Care Centers"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("characteristic",
                    "Select a characteristic:",
                    choices = c("Home Environment Stimulation" = "HOMESTIM_24",
                                 "Incidence of Maternal Depression" = "DEPRESSION_12"),
                     selected = "Home Environment Stimuulation")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("About this app", htmlOutput("about")),
                    tabPanel("Infant Center Quality vs. Family Income", htmlOutput("boxplot")),
                    tabPanel("Other graph", htmlOutput("scatterplot")))

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$about <- renderUI({

     str1 <- paste("Summary")
     str2 <- paste("I examine data from the Massachusetts Early Care and Education and School Readiness Study, which assess the specific factors in infant and preschool classrooms that promote school readiness.")
     str3 <- paste("Purpose")
     str4 <- paste("The goal of my project is to identify the best indicators of success both inside and outside of the classroom. In a real world context, this work would hopefully be useful to decision makers in crafting effective child care policies and strategies.")
     
     HTML(paste(h3(str1), p(str2), h3(str3), p(str4)))
   })
  
   output$boxplot <- renderPlotly({
    
     income_i_overall_12 <- raw_data %>%
       select(ICPSR_CHILDID, INCOMGROUP_12, I_OVERALL_12) %>%
       filter(!is.na(INCOMGROUP_12)) %>%
       mutate(INCOMGROUP_12 = as.character(INCOMGROUP_12),
              INCOMGROUP_12 = fct_recode(INCOMGROUP_12, "Low income" = "1",
                                         "Moderate income" = "2",
                                         "High income" = "3")) %>%
       group_by(ICPSR_CHILDID) %>%
       ggplot(aes(x = INCOMGROUP_12, y = I_OVERALL_12)) +
       geom_boxplot() +
       labs(title = "Overall Infant Center Quality Improves with Income Level",
            x = "Family Income Category",
            y = "Infant/Toddler Environment Rating Scale (ITERS) Score")
     
     income_i_overall_12
   })
   
   output$scatterplot <- renderPlotly({
     
     dep_social_dev_12 <- raw_data %>%
       select(ICPSR_CHILDID, DEPRESSION_12, GENDER_12, ASQSCORE_12) %>%
       filter(!is.na(GENDER_12)) %>%
       mutate(GENDER_12 = as.character(GENDER_12),
              GENDER_12 = fct_recode(GENDER_12, "Female" = "1",
                                     "Male" = "2")) %>%
       group_by(ICPSR_CHILDID) %>%
       ggplot(aes(x = DEPRESSION_12, y = ASQSCORE_12, color = GENDER_12)) + 
       geom_point() +
       scale_color_manual(values = c("pink", "blue")) +
       labs(title = "No Spike in Behavioral Problems at Higher Level of Maternal Depression 
            at 12 Months", 
            x = "Maternal Depression",
            y = "ASQ Score of Social Development", 
            color = "Gender")
     
     dep_social_dev_12
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

