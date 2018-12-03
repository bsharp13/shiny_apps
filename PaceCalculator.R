library(shiny)
library(ggplot2)

#------------------------------------------------------------------------------

# Compute function
#------------------------------------------------------------------------------
compute.pace <- function(dist, h, m, s, in.unit, out.unit) {
  
  k.m.ratio <- 0.621371
  time.in.seconds <- s + m * 60 + h * 60 * 60
  
  # Convert units if necessary
  if (in.unit == out.unit) {
    out.unit.distance <- dist
  } else if (in.unit == 'Kilometers') {
    out.unit.distance <- dist * k.m.ratio
  } else if (in.unit == 'Miles') {
    out.unit.distance <- dist / k.m.ratio
  }
  
  # Compute pace in seconds
  pace <- time.in.seconds / out.unit.distance
  
  # Convert seconds to actual time
  out.h <- floor(pace / 3600)
  out.m <- floor((pace - (out.h * 3600)) / 60)
  out.s <- round(pace - (out.h * 3600) - (out.m * 60))
  
  out.times <- c(out.h, out.m, out.s)
  if(out.h == 0) out.times <- c(out.m, out.s)
  
  # Formatting
  output <- paste0(
    sapply(out.times, function(i) formatC(i, width = 2, flag = 0)),
    collapse = ':'
  )
  
  output <- paste0(
    output,
    ifelse(out.unit == 'Miles', ' / mile', ' / km')
  )
  
  return(output)
}

base.plot <- function(label) {
  ggplot(data = data.frame(x = 1:9, y = 1:9)) +
    geom_text(aes(x = 5, y = 5), label = label, size = 10, color = '#337AB7') +
    theme_void()
}

#------------------------------------------------------------------------------

# UI
#------------------------------------------------------------------------------
ui <- fluidPage(
  br(),
  titlePanel('Pace Calculator'),
  br(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6, radioButtons(
            'race_unit', 
            'Race Unit', 
            choices = c('Kilometers', 'Miles'), 
            selected = 'Kilometers',
            inline = TRUE
          )
        ),
        column(
          6, radioButtons(
            'pace_unit', 
            'Pace Unit', 
            choices = c('Kilometers', 'Miles'), 
            selected = 'Kilometers',
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(12, numericInput('distance', 'Distance: ', value = 5))
      ),
      fluidRow(
        column(4, numericInput('hours', 'Hours: ', value = 0)),
        column(4, numericInput('minutes', 'Minutes: ', value = 30)),
        column(4, numericInput('seconds', 'Seconds: ', value = 0))
      ),
      fluidRow(
        column(
          3, p(actionButton(
            "calc",
            "Compute Pace",
            style = "color: #fff; background-color: #337ab7;"
          ))
        )
      )
    ),
    mainPanel(
      wellPanel(plotOutput('pacePlot', height = '252px'))
    )
  )
)

#------------------------------------------------------------------------------

# Server
#------------------------------------------------------------------------------
server <- function(input, output) {
  
  output_pace <- eventReactive(input$calc, {
    compute.pace(
      input$distance,
      input$hours,
      input$minutes,
      input$seconds,
      input$race_unit,
      input$pace_unit
    )
  })
  
  output$pacePlot <- renderPlot({
    base.plot(output_pace())
  })
}

#------------------------------------------------------------------------------

# Run App
#------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
