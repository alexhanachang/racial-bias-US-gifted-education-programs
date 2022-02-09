library(shiny)

ui <- fluidPage(
  titlePanel("MY APP"),
  fillPage(
    plotOutput("plot1")
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    RE_prop %>% 
      filter(race == "black") %>% 
      ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
      geom_jitter(color = "grey75", size = 0.4) + 
      geom_smooth(color = "goldenrod1", se = F) + 
      geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
      scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
      scale_y_continuous(name = "Proportion of Black students in GAT program", limits = c(0,1)) + 
      labs(title = "Representation of Black students in GAT programs")
  })
}

shinyApp(ui, server)