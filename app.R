library(shiny)
library(shinythemes)
library(markdown)

source("model.r")

# 1. SERVER --------------------------------------------------------------------

# Define server logic
server <- shinyServer(function(input, output, session) {
  
  # Predict
  observeEvent(input$button_predict, {
    predicted_text1 <- reactive(PredictNextWord(input$text_in))
    output$text_out_id1 <- predicted_text1
  })

  # Clear
  observeEvent(input$button_clear, {
    updateTextInput(session, "text_in",  value = " ")
  })
})

# 2. UI ------------------------------------------------------------------------

# Define UI for application
ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"),
  tags$hr(),
  titlePanel("Next Word Predictor"),
  tags$hr(),
  
  verticalLayout(
    tags$p(" "),
    tags$h3("Enter Search Text:"),
    h4(tags$textarea(id = "text_in", rows = 5, cols = 30, "Delete and Enter Text")),
    actionButton("button_predict",
                 label = "Predict",
                 icon = icon("arrow")),
    tags$p(" "),
    tags$h3("Predicted next word:"),
      h3(textOutput("text_out_id1")),
      "",
      actionButton("button_clear",
                   label = "Clear",
                   icon = icon("refresh"))
  ))
)

# 2. APP -----------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)