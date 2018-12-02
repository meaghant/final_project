library(shiny)
library(tidytext)
library(haven)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(plotly)
library(ggpubr)
library(tidyverse)

raw_data <- read_sav("33968-0001-Data.sav")

clean_data <- raw_data %>%
  select(ICPSR_CENTERID_PK, RDDAYWKLI_PK, NUBOOKSLI_PK, EDUCAT_PK, PALS_PK, TOTALASQSCORE_PK, INCOMELI_PK) %>%
  mutate(RDDAYWKLI_PK = as.character(RDDAYWKLI_PK),
         RDDAYWKLI_PK = fct_recode(RDDAYWKLI_PK, "0" = "0",
                                   "1" = "1",
                                   "2" = "2",
                                   "3" = "3",
                                   "4" = "4",
                                   "5" = "5",
                                   "6" = "6",
                                   "7" = "7"),
         NUBOOKSLI_PK = as.character(NUBOOKSLI_PK),
         NUBOOKSLI_PK = fct_recode(NUBOOKSLI_PK, "None" = "1",
                                   "1 to 9 books" = "2",
                                   "10 to 24 books" = "3",
                                   "25 to 49 books" = "4",
                                   "50 or more books" = "5"),
         EDUCAT_PK = case_when(EDUCAT_PK %in% 1:2 ~ "High school or less",
                                 INCOMELI_PK %in% 3:5 ~ "Some college/associate's or technical degree",
                                 INCOMELI_PK %in% 6:7 ~ "Graduate degree"),
         EDUCAT_PK = fct_relevel(EDUCAT_PK, c("High school or less", "Some college/associate's or technical degree", "Graduate degree")),
         PALS_PK = as.numeric(PALS_PK),
         TOTALASQSCORE_PK = as.numeric(TOTALASQSCORE_PK),
         INCOMELI_PK = case_when(INCOMELI_PK %in% 1:5 ~ "Under $50,000",
                                 INCOMELI_PK %in% 6:10 ~ "$50,000-100,000",
                                 INCOMELI_PK %in% 11 ~ "$100,000-150,000",
                                 INCOMELI_PK %in% 12 ~ "$150,000-200,000",
                                 INCOMELI_PK %in% 13 ~ "Over $200,000"),
         INCOMELI_PK = fct_relevel(INCOMELI_PK, c("Under $50,000", "$50,000-100,000", "$100,000-150,000", "$150,000-200,000", "Over $200,000")),
         INCOMELI_PK = as_vector(INCOMELI_PK)) %>%
  filter(!is.na(RDDAYWKLI_PK),
         !is.na(NUBOOKSLI_PK),
         !is.na(EDUCAT_PK),
         !is.na(PALS_PK),
         !is.na(TOTALASQSCORE_PK))

characteristic_choices <- c("Number of Books Read Per Week" = "RDDAYWKLI_PK",
                            "Number of Books in Home" = "NUBOOKSLI_PK",
                            "Maternal Education Level" = "EDUCAT_PK")

outcome_choices <- c("PALS Academic Performance Score" = "PALS_PK",
                     "ASQ Behavioral Score (higher = worse behavior)" = "TOTALASQSCORE_PK")

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
     
     clean_data %>% 
       group_by(ICPSR_CENTERID_PK) %>%
       ggplot(aes_string(x = input$characteristic, y = input$outcome)) + geom_boxplot() 
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

