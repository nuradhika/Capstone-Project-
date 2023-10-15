

library(shiny)
library(DT)

source('Prediction.R', local = TRUE)


function(input, output) {

        # predict the next word 
        wordPrediction <- reactive({
                text <- input$text
                wordPrediction <- get_prediction(text)
                
        })
        output$guess_1 <- renderText(wordPrediction()[1])
        output$guess_2 <- renderText(wordPrediction()[2])
        output$guess_3<- renderText(wordPrediction()[3])
        output$guess_4<- renderText(wordPrediction()[4])
                
        }
    


