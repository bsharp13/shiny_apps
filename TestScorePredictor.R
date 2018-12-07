
library(shiny)
library(tidyverse)

#-------------------------------------------------------------------------------

# Prep
#-------------------------------------------------------------------------------
# Load models
load('MathModel.rda')
load('ReadModel.rda')
load('WriteModel.rda')

# Read data
scores <- read_csv('~/Downloads/StudentsPerformance.csv')
colnames(scores) <- 
  c('Gender', 'Race', 'ParentEd', 'Lunch', 'Pre', 'Math', 'Read', 'Write')

select_scores <- scores %>% 
  filter(
    Gender == 'female',
    Race == 'group B',
    ParentEd == "bachelor's degree",
    Lunch == 'standard'
  )

# Plotting function
my_density <- function(subject = 'math') {
  
  if (subject == 'math') {
    select_scores$Subject <- select_scores$Math
  } else if (subject == 'reading') {
    select_scores$Subject <- select_scores$Read
  } else {
    select_scores$Subject <- select_scores$Write
  }
  
  select_scores %>% 
    ggplot(aes(x = Subject, color = Pre, fill = Pre)) +
    geom_density(alpha = 0.6)
  
}

#-------------------------------------------------------------------------------

# UI
#-------------------------------------------------------------------------------
ui <- fluidPage(
  title = 'Test Score Predictor',
  sidebarLayout(
    sidebarPanel(
      h2('Demographic Information'),
      fluidRow(
        selectInput(
          inputId = 'gender', 
          label = 'Gender', 
          choices = unique(scores$Gender)
        ),
        selectInput(
          inputId = 'race',
          label = 'Race',
          choices = unique(scores$Race)
        ),
        selectInput(
          inputId = 'parent_ed',
          label = 'Parent Education',
          choices = unique(scores$ParentEd)
        ),
        selectInput(
          inputId = 'lunch',
          label = 'Lunch',
          choices = unique(scores$Lunch)
        ),
        p(actionButton(inputId = 'predict', label = 'Predict Scores'))
      )
    ),
    mainPanel(
      plotOutput('mathplot'),
      plotOutput('readplot'),
      plotOutput('writeplot')
    )
  )
)

#-------------------------------------------------------------------------------

# Server
#-------------------------------------------------------------------------------

server <- function(input, output) {
  
  observeEvent(input$predict, {
    select_scores <- scores %>% 
      filter(
        Gender == input$gender,
        Race == input$race,
        ParentEd == input$parent_ed,
        Lunch == input$lunch
      )
  })
  
  output$mathplot <- renderPlot({
    my_density('math')
  })
  output$readplot <- renderPlot({
    my_density('reading')
  })
  output$writeplot <- renderPlot({
    my_density('writing')
  })
}

#-------------------------------------------------------------------------------

# Run app
#-------------------------------------------------------------------------------
shinyApp(ui, server)