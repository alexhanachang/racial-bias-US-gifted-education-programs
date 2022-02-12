library(shiny)
library(tidyverse)


ui <- fluidPage(
  
  selectInput("race", "Race:", choices = unique(gifted_talented_enrollment_by_race$race)),
  
  plotOutput("plot1")
  
  )


server <- function(input, output) {
  
  race <- reactive({
    filter(gifted_talented_enrollment_by_race, TERRITORY == input$race)
  })
  
  output$plot1 <- renderPlot({
    ggplot(gifted_talented_enrollment_by_race, aes(x = prop_gifted_total, y = prop_gifted_race)) + 
      geom_jitter(color = "grey75", size = 0.4) +
      geom_smooth(color = "goldenrod1", se = F) +
      geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
      scale_x_continuous(name = "Proportion of all students in Gifted and Talented programs", limits = c(0, 0.5)) +
      scale_y_continuous(name = "Proportion of Black students in Gifted and Talented programs", limits = c(0,1)) +
      labs(title = "Representation of Black students in Gifted and Talented programs") +
      theme_minimal()
  })
  
  }

shinyApp(ui, server)