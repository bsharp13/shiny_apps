library(shiny)
library(magrittr)
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------------

# Prep
#-------------------------------------------------------------------------------

# Read in data
y2019 <- as.data.frame(read_csv('www/MeData.csv'))
for (i in seq_along(y2019)) {
  if (str_detect(colnames(y2019)[i], 'Bucket')) {
    y2019[,i] <- as.factor(y2019[,i])
  }
}

# Read HTML
my_header <- readLines('www/calendar_header.txt')

# Create labels 
month_labs <- format(ISOdate(2004, 1:12, 1), "%B")[1:month(today())]
cur_month <- month_labs[month(today())]
measure_labs <- c(
  'Running', 'Music', 'Reading', 'Finances', 'Mental Health' = 'Mental', 
  'Eating Out' = 'Eating', 'Episodes Watched' = 'Episodes', 'Learning'
)

# Identify good and bad behaviors
good  <- c('Running', 'Music', 'Reading')
bad   <- c('Finances', 'Eating', 'Episodes')

# Define color schemes
seq_good <- c('#EDF8FB', '#B3CDE3', '#8C96C6', '#8856A7', '#810F7C', '#CCCCCC')
seq_bad  <- c('#FEE5D9', '#FCAE91', '#FB6A4A', '#DE2D26', '#A50F15', '#CCCCCC')
diverging<- c('#CA0020', '#F4A582', '#F7F7F7', '#92C5DE', '#0571B0', '#CCCCCC')

# Define cutoffs
cutoffs <- data_frame(
  Running = c(2, 5, 7, 10),
  Music = c(15, 30, 45, 60),
  Reading = c(25, 50, 75, 100),
  Finances = c(10, 25, 50, 100),
  Mental = c(2, 4, 6, 8),
  Eating = c(1, 2, 3, 4),
  Episodes = c(3, 6, 9, 12),
  Learning = c(15, 30, 45, 60)
)

# Define functions
my_theme <- function() {
  theme(
    plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = 'none'
  )
}

yearly_plot <- function(col_scheme, bucket) { 
  y2019 %>% 
    ggplot(aes_string(x = 'Week', y = 'DayOfWeek', color = bucket)) +
    geom_point(shape = 15, size = 4.5) +
    scale_color_manual(values = col_scheme) +
    my_theme()
}

monthly_plot <- function(selected_month, col_scheme, bucket, lab) {
  
  y2019 %>% 
    filter(Month == selected_month) %>% 
    mutate(Week = max(Week) - Week + 1) %>%  
    ggplot(aes_string(x = 'DayOfWeek', y = 'Week', color = bucket)) + 
    geom_point(shape = 15, size = 45) +
    geom_text(aes_string(label = lab), col = '#000000', size = 8) +
    xlim(c(0.5, 7.5)) +
    ylim(c(0.5, 5.5)) +
    scale_color_manual(values = col_scheme) +
    my_theme()
  
}

update_bucket <- function(input_value, cutoff_field)  {
  if (input_value > max(cutoff_field)) {
    result <- length(cutoff_field) + 1
  } else {
    result <- max(which(input_value <= cutoff_field)) + 1
  }
  return(result)
}

#-------------------------------------------------------------------------------

# UI
#-------------------------------------------------------------------------------

ui <- fluidPage(
  theme = "calendar_style.css",
  tags$head(
    tags$link(
      rel = "stylesheet", 
      type = "text/css", 
      href = "https://fonts.googleapis.com/css?family=Assistant"
    )
  ),
  HTML('<title> 2019 Spare Time </title>'),
  HTML(my_header),
  
  sidebarLayout(
    sidebarPanel(
      h1('Daily Update'),
      br(),
      h3('Running'),
      fluidRow(
        column(4, numericInput('run', 'Distance run', 0)),
        column(4, textInput('run_shoes', 'Shoes', '')),
        column(4, textInput('run_route', 'Route', ''))
      ),
      h3('Music'),
      fluidRow(
        column(4, numericInput('minutes', 'Minutes played', 0)),
        column(8, textInput('guitar', 'Guitar played', ''))
      ),
      h3('Reading'),
      fluidRow(
        column(4, numericInput('pages', 'Pages read', 0)),
        column(8, textInput('book', 'Book read', ''))
      ),
      h3('Finances'),
      fluidRow(
        column(4, numericInput('amount', 'Amount spent', 0)),
        column(8, textInput('expense', 'Primary expense', ''))
      ),
      h3('Mental Health'),
      fluidRow(
        column(4, numericInput('headache', 'Quality', 0)),
        column(8, textInput('reason', 'Reason', ''))
      ),
      h3('Eating Out'),
      fluidRow(
        column(4, numericInput('ate_out', 'Ate Out', 0)),
        column(8, textInput('ate_where', 'Where?', ''))
      ),
      h3('Episodes Watched'),
      fluidRow(
        column(4, numericInput('episodes', 'Episodes', 0)),
        column(8, textInput('show', 'Main show', ''))
      ),
      h3('Learning Time'),
      fluidRow(
        column(4, numericInput('learning', 'Time Learning', 0)),
        column(8, textInput('subject', 'Subject', ''))
      ),
      br(),
      fluidRow(
        p(actionButton("update", "Update Day"))
      )
    ),
    mainPanel(
      h3('What would you like to see?'),
      column(4, selectInput('var', '', measure_labs, 'Running')),
      br(), br(), br(),
      h1('Year View'),
      plotOutput('this_year', height = '111px', width = '840px'),
      br(), br(),
      h1('Month View'),
      column(4, selectInput('month', 'Month', month_labs, cur_month)),
      plotOutput('this_month', height = '600px', width = '840px')
    )
  )
)

#-------------------------------------------------------------------------------

# Server
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  observeEvent(input$update, {
    # Update csv of yearly values
    update_row <- which(y2019$Date == today())
    
    # Update variables
    y2019[update_row, 'Running']        <- input$run
    y2019[update_row, 'RunShoe']        <- input$run_shoes
    y2019[update_row, 'RunRoute']       <- input$run_route
    y2019[update_row, 'Music']          <- input$minutes
    y2019[update_row, 'GuitarPlayed']   <- input$guitar
    y2019[update_row, 'Reading']        <- input$pages
    y2019[update_row, 'BookRead']       <- input$book
    y2019[update_row, 'Finances']       <- input$amount
    y2019[update_row, 'SpentOn']        <- input$expense
    y2019[update_row, 'Mental']         <- input$headache
    y2019[update_row, 'HeadacheReason'] <- input$reason
    y2019[update_row, 'Eating']         <- input$ate_out
    y2019[update_row, 'AteWhere']       <- input$ate_where
    y2019[update_row, 'Episodes']       <- input$episodes
    y2019[update_row, 'Show']           <- input$show
    y2019[update_row, 'Learning']       <- input$learning
    y2019[update_row, 'Subject']        <- input$subject
    
    # Update buckets
    for (i in seq_along(y2019)) {
      if (str_detect(colnames(y2019)[i], 'Bucket')) {
        y2019[,i] <- as.numeric(as.character(y2019[,i]))
      }
    }
    
    y2019[update_row, 'RunningBucket']  <- 
      update_bucket(input$run, cutoffs[,'Running'])
    y2019[update_row, 'MusicBucket']    <- 
      update_bucket(input$minutes, cutoffs[,'Music'])
    y2019[update_row, 'ReadingBucket']  <- 
      update_bucket(input$run, cutoffs[,'Reading'])
    y2019[update_row, 'FinancesBucket']  <- 
      update_bucket(input$amount, cutoffs[,'Finances'])
    y2019[update_row, 'MentalBucket']   <- 
      update_bucket(input$headache, cutoffs[,'Mental'])
    y2019[update_row, 'EatingBucket']   <- 
      update_bucket(input$ate_out, cutoffs[,'Eating'])
    y2019[update_row, 'EpisodesBucket'] <- 
      update_bucket(input$episodes, cutoffs[,'Episodes'])
    y2019[update_row, 'LearningBucket'] <-
      update_bucket(input$learning, cutoffs[,'Learning'])
    
    for (i in seq_along(y2019)) {
      if (str_detect(colnames(y2019)[i], 'Bucket')) {
        y2019[,i] <- as.factor(y2019[,i])
      }
    }
    
    # Rewrite csv of yearly values
    write.csv(y2019, 'www/MeData.csv', row.names = FALSE)
  })
  
  output$this_year <- renderPlot({ 
    
    if (input$var %in% good) {
      colors <- seq_good
    } else if (input$var %in% bad) {
      colors <- seq_bad
    } else {
      colors <- diverging
    }
    
    yearly_plot(colors, paste0(input$var, 'Bucket'))
    
  })
  
  output$this_month <- renderPlot({
    
    if (input$var %in% good) {
      colors <- seq_good
    } else if (input$var %in% bad) {
      colors <- seq_bad
    } else {
      colors <- diverging
    }
    
    monthly_plot(
      which(month_labs == input$month), 
      colors, 
      paste0(input$var, 'Bucket'), 
      input$var
    )
  })
  
}

#-------------------------------------------------------------------------------

# Run App
#-------------------------------------------------------------------------------
shinyApp(ui, server)
