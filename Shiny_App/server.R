# server.R

library(shiny)
library(randomForest)
library(caret)

data("iris")


rf <- function(num_trees, num_repeats){
  
  
  print(num_repeats)
  
  average_oos_error <- 0
  
  for(run in 1:num_repeats){
    inTrain = createDataPartition(iris$Species, p = 3/4)[[1]]
    training = iris[ inTrain,]
    validating = iris[-inTrain,]
    answers <- validating$Species
    validating <- validating[,-c(5)]
    rf_machine <- randomForest(Species ~ ., data = training, ntree = num_trees)
    preds <- predict(rf_machine, validating)
    accuracy <- sum(preds == answers) / length(answers)
    average_oos_error <- average_oos_error + accuracy
  }
  
  average_oos_error <- average_oos_error / num_repeats
  
  return(average_oos_error)
}


shinyServer(
  function(input, output){
    num_t <- reactive({num_t <- input$number_of_trees})
    num_v <- reactive({num_v <- input$number_of_validations})
    output$num_trees <- reactive(num_t()) 
    output$num_repeats <- reactive(num_v())
    output$accuracy <- reactive(rf(num_t(),num_v()))
  }
)