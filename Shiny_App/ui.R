# ui.R

# submit on github


library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Iris Random Forest Prediction"),
    sidebarPanel(h3("Instructions"),
                 h5("~ select the number of trees in your forest"),
                 numericInput('number_of_trees', 'number of trees', 10, min = 1, max = 100, step = 1 ),
                 h5("~ select the number of measurements of out of sample error to average into the displayed estimate"),
                 numericInput('number_of_validations','number of out of sample error estimates', 2, min = 1, max = 20, step = 1),
                 submitButton('Submit')),
    mainPanel(h2("Results"),
              h3("number of trees"),
              verbatimTextOutput("num_trees"),
              h3("number of out of sample error estimates"),
              verbatimTextOutput("num_repeats"),
              h3("out of sample error"),
              verbatimTextOutput("accuracy")
              )
  )
)