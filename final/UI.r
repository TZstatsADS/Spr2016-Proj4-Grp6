shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    h1 {
                    color: #000099;
                    }
                    
                    body {
                    background-color: #FFFFFF;
                    }
                    "))
    ),
  headerPanel("Movie Search"),
  sidebarLayout(position = "left", 
                sidebarPanel(
                  h4("Movie Search"),
                  textInput('moviename', 'What is the movie name', value = ""),
                  selectInput("Type", "Does your input contain error?", c("No", "Yes")),
                  submitButton("Submit")
                ),
                mainPanel(
                  h4("Summary"),
                  verbatimTextOutput("summary")
                )
  )
))