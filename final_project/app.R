library(shiny)
library(tidytext)
library(haven)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(plotly)
library(ggpubr)
library(tidyverse)

raw_data <- read_sav("33968-0001-Data.sav") %>%
  select(ICPSR_CENTERID_PK, RDDAYWKLI_PK, TOYTOTAL_PK, PALS_PK, TOTALASQSCORE_PK) %>%
  mutate(RDDAYWKLI_PK = as.numeric(RDDAYWKLI_PK),
         TOYTOTAL_PK = as.numeric(TOYTOTAL_PK),
         PALS_PK = as.numeric(PALS_PK),
         TOTALASQSCORE_PK = as.numeric(TOTALASQSCORE_PK)) %>%
  filter(!is.na(RDDAYWKLI_PK),
         !is.na(TOYTOTAL_PK),
         !is.na(PALS_PK),
         !is.na(TOTALASQSCORE_PK)) %>%
  group_by(ICPSR_CENTERID_PK)

characteristic_choices <- c("Number of Books Read Per Week" = "RDDAYWKLI_PK",
                            "Number of Toys in Household" = "TOYTOTAL_PK")

outcome_choices <- c("PALS Academic Score" = "PALS_PK",
                     "ASQ Behavioral Score" = "TOTALASQSCORE_PK")

ui <- fluidPage(
   
   # Application title
   titlePanel("Educ Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       selectInput("characteristic",
                   "Select a characteristic:",
                   choices = characteristic_choices,
                   selected = "Number of Books Read Per Week"),
      selectInput("outcome",
                  "Select an outcome variable:",
                   choices = outcome_choices,
                   selected = "PALS Academic Score")
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     raw_data %>% 
       ggplot(aes_string(x = input$characteristic, y = input$outcome)) + geom_col()
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

