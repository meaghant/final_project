library(shiny)
library(haven)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(shinythemes)
library(tidyverse)

# Read in data. I chose an abbreviation of the study (Massachusetts Early Care and Education and School Readiness -> MECESR) as the name of the sav file.

raw_data <- read_sav("MECESR_data.sav")

# My data was relatively clean and did not require the grouping of multiple data frames, so instead of cleaning the data in a separate file and reading it in with an RDS, I chose to clean the data within this app.
# I selected the variables I planned to use: the three non-school factor variables, the two outcome variables, and the unique child ID grouping variable. I mutated these variables to make them easily understandable for the user.
# There were many other non-school factor variables in the data set, such as number of days read to per week, incidence of maternal depression, and number of toys in the household. I limited myself to three non-school factor variables because I did not want the app to present too many options to the user. I selected the three non-school factor variables that I found most interesting.
# I chose to filter out N/A values because I wanted to compare only data that was complete on all metrics. I acknowledge that this will leave out certain respondents who, for example, refused to share their income or education level.

clean_data <- raw_data %>%
  select(ICPSR_CENTERID_PK, NUBOOKSLI_PK, EDUCAT_PK, INCOMELI_PK, PALS_PK, TOTALASQSCORE_PK) %>%
  filter(!is.na(NUBOOKSLI_PK),
         !is.na(EDUCAT_PK),
         !is.na(INCOMELI_PK),
         !is.na(PALS_PK),
         !is.na(TOTALASQSCORE_PK)) %>%
  mutate(NUBOOKSLI_PK = as.character(NUBOOKSLI_PK),
         NUBOOKSLI_PK = fct_recode(NUBOOKSLI_PK, "None" = "1",
                                   "1 to 9 books" = "2",
                                   "10 to 24 books" = "3",
                                   "25 to 49 books" = "4",
                                   "50 or more books" = "5"),
         EDUCAT_PK = case_when(EDUCAT_PK %in% 1:2 ~ "High school or less",
                               EDUCAT_PK %in% 3:5 ~ "Some college/non-bachelor's degree",
                               EDUCAT_PK %in% 6 ~ "Bachelor's degree",
                               EDUCAT_PK %in% 7 ~ "Graduate degree"),
         EDUCAT_PK = fct_relevel(EDUCAT_PK, c("High school or less", "Some college/non-bachelor's degree", "Bachelor's degree", "Graduate degree")),
         INCOMELI_PK = as.character(INCOMELI_PK),
         INCOMELI_PK = fct_collapse(INCOMELI_PK, "Under $50,000" = c("1","2","3","4","5"),
                                    "$50,000-100,000" = c("6","7","8","9","10"), 
                                    "$100,000-150,000" = "11",
                                    "$150,000-200,000" = "12",
                                    "Over $200,000" = "13"),
         INCOMELI_PK = fct_relevel(INCOMELI_PK, c("Under $50,000", "$50,000-100,000", "$100,000-150,000", "$150,000-200,000", "Over $200,000")),
         PALS_PK = as.numeric(PALS_PK),
         TOTALASQSCORE_PK = as.numeric(TOTALASQSCORE_PK))

# Drawing from the problem set 7 solution, I defined input choices for non-school factors and outcome outside of my UI.

nonschool_choices <- c("Number of Books in Home" = "NUBOOKSLI_PK",
                       "Maternal Education Level" = "EDUCAT_PK",
                       "Family Income Level" = "INCOMELI_PK")

outcome_choices <- c("PALS Literacy Score" = "PALS_PK",
                     "ASQ Social Development Score" = "TOTALASQSCORE_PK")

ui <- fluidPage(
  
  # I chose a dark navy theme and titled my app with a question.
  
  theme = shinytheme("superhero"),
  
  titlePanel("What Effect Do Non-School Factors have on Preschoolers' Literacy and Social Development Outcomes?"),
  
  # I created a sidebar that allows the user to select one of three non-school factors and one of two outcome variables. 
  # I elected to use a drop-down menu rather than radio buttons to avoid overcrowding the screen. 
  # Using Mr. Smith's app as an example, I added descriptions of each variable underneath the drop-down menus to make their meaning transparent to the user.
  
  sidebarLayout(
    sidebarPanel(
      selectInput("nonschool",
                  "Select a non-school factor:",
                  choices = nonschool_choices,
                  selected = "Number of Books in Home"),
      
      tags$h6(helpText("\"Number of Books in Home\" refers to the total number of children's books, including library books, in the respondents' home at time of response.
                       \"Maternal Education Level\" refers to the reported educational background of the mother.
                       \"Family Income Level\" refers to the total household income from all sources.")),
      br(),
      
      selectInput("outcome",
                  "Select an outcome variable:",
                  choices = outcome_choices,
                  selected = "PALS Literacy Score"),
      
      tags$h6(helpText("\"PALS Literacy Score\" refers to a child's score on the Phonological Awareness Literacy Screening test, a widely used measure that gauges ability in areas such as phonological awareness, alphabet recognition, concept of word, knowledge of letter sounds, and spelling.
                       \"ASQ Social Development Score\" refers to a child's score on the parent-completed Ages and Stages Questionnaire, which evaluates the areas of communication, gross motor skills, fine motor skills, problem solving, and personal-social skills. It should be noted that a higher score on this measure indicates greater incidence of problem behaviors."))
    ),
    
    # In the main panel, I created several tabs to keep my app looking clean: an "About" tab to introduce the app, an "Explore" tab for the data, a "Insights" tab to share my interpretations, and a "Learn more" tab for anyone interested in doing so.
    # I wrote up a very brief summary of my data and printed it beneath the boxplot in the "Explore" tab, but I wanted to preserve the majority of my interpretations of the data for the "Insights" tab. I chose to make this a separate tab to keep the app neat.
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About this app", htmlOutput("about")),
                  tabPanel("Explore the data", 
                           plotOutput("boxplot"),
                           h3("Interpretation of Findings"),
                           p("My hypotheses largely held true. Generally, with increases in number of household books, maternal education level, and family income level, there was an improvement in both literacy and social development outcomes, although this plateaued somewhat at the higher levels of privilege in each factor. Suprisingly, there was a stronger correlation between the non-school factor and the literacy outcome variable than with the social development outcome variable. This is perhaps surprising, given that all of these factors were removed from the classroom.")),
                  tabPanel("Insights", htmlOutput("insight")), 
                  tabPanel("Learn more", htmlOutput("learn")))
    )
  )
)

# I defined my server logic, producing a different output for each tab.
# Again following Mr. Smith's example, I created two reactive functions to print different x- and y-axis labels depending on which variables were selected. 

server <- function(input, output) {
  
  x_label <- reactive({
    req(input$nonschool)
    if(input$nonschool == "NUBOOKSLI_PK"){
      x_label <- "Number of Books in Home"
    } else if(input$nonschool == "EDUCAT_PK"){
      x_label <- "Maternal Education Level"
    } else if(input$nonschool == "INCOMELI_PK"){
      x_label <- "Family Income Level"
    }})
  
  y_label <- reactive({
    req(input$outcome)
    if(input$outcome == "PALS_PK"){
      y_label <- "PALS Literacy Score"
    } else if(input$outcome == "TOTALASQSCORE_PK"){
      y_label <- "ASQ Social Development Score (higher score = worse behavior)"
    }})
  
  # In the "About" tab, I pasted together strings of text describing my project and created headers and subtext to make it more readable.
  # The sentences are an adaptation of my one- and four-sentence summaries, followed by the hypotheses that drove my exploration of this data.
  
  output$about <- renderUI({
    
    str1 <- paste("Did you know...")
    str2 <- paste("...that non-school factors like parental education and income have more of an impact on preschoolers’ academic outcomes than their behavioral outcomes?")
    str3 <- paste("My Focus")
    str4 <- paste("I drew data from the Massachusetts Early Care and Education and School Readiness Study, which assesses the specific factors inside and outside of preschool classrooms that promote school readiness. I examined three non-school factors: number of books in the home, maternal education level, and family income level, and compared their effects on two outcome variables: literacy and social development. There was a stronger correlation between all three non-school factors and literacy outcomes than with social development outcomes. The substantial drop-offs in performance seen at the lowest levels of privilege on each non-school factor suggest a need for early education reform efforts directed towards those most in need.")
    str5 <- paste("Hypotheses")
    str6 <- paste("I hypothesized that preschool children who (1) are exposed to a greater number of books in their homes, (2) have mothers with a higher degree of education, and (3) come from higher-income households will have better literacy and social development outcomes.")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))
  })
  
  # In the "Data" tab, I piped the cleaned data into a boxplot of outcome vs. non-school factors. I chose to visualize these data with boxplots to communicate the spread of each variable. 
  # Again drawing from the solutions to problem set 7, I used aes_string instead of aes because selectInput stored both nonschool and outcome as strings. I also grouped by the unique child ID and rotated the x-axis labels by 30 degrees to make them more readable.
  
  output$boxplot <- renderPlot({
    
    clean_data %>% 
      group_by(ICPSR_CENTERID_PK) %>%
      ggplot(aes_string(x = input$nonschool, y = input$outcome)) + 
      geom_boxplot() +
      labs(x = x_label(),
           y = y_label()) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
  })
  
  # In the "Insights" tab, I followed a similar approach to that of the "About" tab. In addition to studying the boxplots, my interpretations of the data were informed by examining the study codebook (included in my Github repo). I tried to be thorough but not excessively long in my interpretations.
  
  output$insight <- renderUI({
    
    str1 <- paste("Number of Books Read Per Week")
    str2 <- paste("The number of books in the home was positively correlated with literacy outcomes. Preschoolers who came from 50+ book households outscored peers from households with fewer than 9 books by 1.5 standard deviations on the PALS literacy score.")
    str3 <- paste("A greater number of books was also correlated, though not as strongly, with improved social development. There were, however, a notable number of poorly-behaved children from households with many books, suggesting that exposure to books does not prevent the instance of poor behavior.")
    str4 <- paste("Maternal Education Level")
    str5 <- paste("Maternal education level had an even stronger positive correlation with improved literacy outcomes. The spread of the data was consistent between educational levels, but the average literacy score improved with each additional level of school completed by the student’s mother.")
    str6 <- paste("Children of mothers who completed only high school or less were significantly less well behaved than children of other mothers. However, there were minimal differences in the social development of children whose mothers completed some college or received an associate’s, technical, bachelor’s or graduate degree.")
    str7 <- paste("Family Income Level")
    str8 <- paste("There was also a slight positive correlation between family income level and literacy outcomes. The largest gap was between families with incomes below $50,000 and all other families, whose children all exhibited similar literacy scores.")
    str9 <- paste("Finally, children of parents from lower-income families performed less well on scores of social development. Again, a gap emerged between families living on less than $50,000, whose children had the worst social development scores, and other families, whose children performed similarly to each other. Six of the seven worst-behaved children were in the lowest income bracket.")
    str10 <- paste("Implications")
    str11 <- paste("Above a certain threshold – whether number of books in the home, maternal education level, or family income level – children seem to perform at roughly the same level on literacy and social development outcomes. The substantial drop-offs in performance seen at the lowest levels of privilege on each factor might incline policymakers to focus non-school reform efforts towards those most in need.")
    
    HTML(paste(h3(str1), p(str2), p(str3), h3(str4), p(str5), p(str6), h3(str7), p(str8), p(str9), h3(str10), p(str11)))
    
  })
  
  # In the "Learn more" tab, I again pasted together strings of text. I wanted to share more about the study from which my data was drawn and give credit to the researchers whose data I was using.
  # I also included a link to the data.
  
  output$learn <- renderUI({
    
    str1 <- paste("About the Massachusetts Early Care and Education and School Readiness Study")
    str2 <- paste("This study was conducted in Massachusetts from 2001 to 2008 by Wellesley College researchers. The study collected data at 12, 24, and 36 months, and in the year before kindergarten. I examine only the pre-kindergarten year data. The average age of the children in the study at this time was 59.79 months, or 4.98 years, with a standard deviation of 4.10 months. Of the 248 families originally recruited for the study, 233 remain. This represents an attrition rate of 3.6%.")
    str3 <- paste("The study was directed by Wendy Wagner Robeson, Ed.D., Joanne Roberts, Ph.D., and Nancy L. Marshall, Ed.D. Funding was provided by (1) the United States Department of Health and Human Services, Administration for Children and Families, Office of Planning, Research and Evaluation; (2) the United States Department of Health and Human Services, Administration for Children and Families, Office of Child Care; and (3) the Harold Benenson Memorial Research Fund.")
    str4 <- paste("Citation")
    str5 <- paste("Marshall, Nancy, Roberts, Joanne, and Wagner Robeson, Wendy. Massachusetts Early Care and Education and School Readiness Study, 2001-2008. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2013-04-05.")
    str6 <- a("https://doi.org/10.3886/ICPSR33968.v1.", href = "https://doi.org/10.3886/ICPSR33968.v1")
    
    HTML(paste(h3(str1), p(str2), p(str3), h3(str4), p(str5), p(str6)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

