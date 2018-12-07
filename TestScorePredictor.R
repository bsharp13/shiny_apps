
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

facet_options <- c(
  'Gender' = 'gender', 
  'Race' = 'race', 
  'Parent Education' = 'parent_ed', 
  'Lunch' = 'lunch'
)

# Plotting function
my_density <- function(subject = 'math', facet = 'gender') {
  
  if (subject == 'math') {
    scores$Subject <- scores$Math
  } else if (subject == 'reading') {
    scores$Subject <- scores$Read
  } else {
    scores$Subject <- scores$Write
  }
  
  if (facet == 'gender') {
    scores$Facet <- scores$Gender
  } else if (facet == 'race') {
    scores$Facet <- scores$Race
  } else if (facet == 'parent_ed') {
    scores$Facet <- scores$ParentEd
  } else {
    scores$Facet <- scores$Lunch
  }
  
  scores %>% 
    ggplot(aes(x = Subject, color = Pre, fill = Pre)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ Facet) +
    labs(y = '', x = 'Score') +
    scale_fill_manual(values = c('#1B9E77', '#7570B3')) +
    scale_color_manual(values = c('#1B9E77', '#7570B3')) +
    theme_bw()
  
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
      h2('Score Distributions'),
      selectInput(inputId = 'view_by', label = 'View by', facet_options),
      br(), br(),
      h4('Math'),
      plotOutput('mathplot'),
      br(),
      h4('Reading'),
      plotOutput('readplot'),
      br(),
      h4('Writing'),
      plotOutput('writeplot')
    )
  )
)

#-------------------------------------------------------------------------------

# Server
#-------------------------------------------------------------------------------

server <- function(input, output) {
  
  observeEvent(input$predict, {
    x <- 1
  })
  
  output$mathplot <- renderPlot({
    my_density('math', input$view_by)
  })
  output$readplot <- renderPlot({
    my_density('reading', input$view_by)
  })
  output$writeplot <- renderPlot({
    my_density('writing', input$view_by)
  })
}

#-------------------------------------------------------------------------------

# Run app
#-------------------------------------------------------------------------------
shinyApp(ui, server)