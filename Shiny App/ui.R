
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
fluidPage(
        theme = bslib::bs_theme(
                bg = "#EAFAF1", 
                fg = "black", 
                base_font = "arial"),
                
    # Application title
    titlePanel(h1(HTML("<strong> Predict The Next Word</strong>"))),

    # interface with tabs 
    navbarPage("Next Word Prediction ",
               tabPanel("Shiny App",
                        fluidRow(
                                column(3),
                                column(6,
                                       tags$div(textInput("text", 
                                                          label = h3("Enter text:"),
                                                          value = ), 
                                       
                                                br(),
                                        tags$style(HTML("hr {border-top: 4px solid #004225;}")),
                                                h3("Predicted Next Word:"),
                                                tags$span(style="color:darkred",
                                                          tags$strong(tags$h3(textOutput("guess_1")))),
                                                br(),
                                                tags$hr(),
                                                h4("Second Guess:"),
                                                tags$span(style="color:grey",
                                                          tags$strong(tags$h4(textOutput("guess_2")))),
                                                br(),
                                                tags$hr(),
                                                h5("Third Guess:"),
                                                tags$span(style="color:grey",
                                                          tags$strong(tags$h5(textOutput("guess_3")))),
                                        
                                               br(),
                                               tags$hr(),
                                               h6("Fourth Guess:"),
                                                tags$span(style="color:grey",
                                                  tags$strong(tags$h6(textOutput("guess_4")))),
                                        
                                                br(),
                                                tags$hr(),                                    
                                                align="center")
                                ),
                                column(3)
                        ) 
        ),

        
        # rmarkdown document about the app
       tabPanel("Information", includeMarkdown("Document.Rmd")
        )           
))

