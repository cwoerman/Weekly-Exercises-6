library(shiny)
library(tidyverse)
library(rsconnect)
ui <- fluidPage(
  selectInput("state",
              "State",
              choices = unique(covid19$state)),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)
server <- function(input, output) {
  output$covidplot <- renderPlot({
    covid_weekly %>% 
      filter(state %in% c(input$state)) %>% 
    ggplot()
    aes(x = cases,
               y = new_cases_weekly,
               colour = state)+
      geom_line()+
      scale_y_continuous(trans = "log10",
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      geom_text(aes(label = state), check_overlap = TRUE)
    })
}
shinyApp(ui = ui, server = server)

