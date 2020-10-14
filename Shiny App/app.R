library(shiny)
library(tidyverse)
library(rsconnect)
ui <- fluidPage(
  selectInput("state",
              "State",
              choices = list(Alabama = "AL",Alaska = "AK",Arizona = "AZ",
                             Arkansas = "AR",California = "CA",Colorado = "CO",
                             Connecticut = "CT", Deleware = "DE",Florida = "FL",
                             Georgia = "GA", Hawaii = "HI", Idaho = "ID",
                             Illinois = "IL", Indiana = "IN", Iowa = "IA",
                             Kansas = "KN", Kentucky = "KY", Louisiana = "LA",
                             Maine = "ME", Maryland = "MD", Massachusetts = "MA",
                             Michigan = "MI", Minnesota = "MN", Mississippi = "MS",
                             Missouri = "MO", Montana = "MT",Nebraska = "NE",
                             Nevada = "NV",New_Hampshire= "NH", New_Jersey = "NJ",
                             New_Mexico = "NM", New_York = "NY", North_Carolina = "NC",
                             North_Dakota = "ND",Ohio = "OH",Oklahoma = "OK",
                             Oregon = "OR", Pennsylvania = "PA", Rhode_Island = "RI",
                             South_Carolina = "SC", South_Dakota = "SD", Tennessee = "TN",
                             Texas = "TX", Utah = "UT", Vermont = "VT", Virginia = "VA",
                             Washington = "WA", West_Virginia = "WV", Wisconsin = "WI",
                             Wyoming = "WY")),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)
server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid_weekly %>% 
      filter(state == c(input$state)) %>% 
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

