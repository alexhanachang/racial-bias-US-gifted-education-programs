library(shiny)
library(tidyverse)

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  
  selectInput("race", "Race:", choices = gifted_talented_enrollment_by_race$race),
  
  plotOutput("plot1")
  
  )


server <- function(input, output) {
  
  # race <- reactive({
  #   filter(gifted_talented_enrollment_by_race, TERRITORY == input$race)
  # })
  
  selected <- reactive({
    filter_var(gifted_talented_enrollment_by_race$race, input$race)
  })
  
  output$plot1 <- renderPlot({
    
    gifted_talented_enrollment_by_race %>% 
      ggplot(aes(x = prop_gifted_total, y = prop_gifted_race, color = race[selected(), ])) + 
      geom_smooth(se = F) + 
      scale_color_manual(values = c("firebrick1", "darkorange", "goldenrod1", 
                                    "darkolivegreen3", "cornflowerblue", "slateblue1")) +
      geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
      scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
      scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
      labs(title = "Representation of all students by race in GAT programs")
      
  })
  
  }

shinyApp(ui, server)