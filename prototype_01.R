#
# This is a Shiny web application. 
# 1st Prototype for the Biology Tool Kit
# 1/22/2019
#

library(shiny)
library(shinythemes)

ui <- navbarPage(
  "Computational Biology Bio-Informatics Toolkit",
  theme = shinytheme("united"),
  
 #---page tab settings---#
   tabsetPanel(type = "tab", position = "right",
               
      #SEARCH TAB
      tabPanel(title = "Search", 
               sidebarPanel(
                
                   condition = "input$id == 'Search'",
                   helpText("Welcome to the Biology Bio-Information ToolKit!
                            Use the options below to search."),
                   br(),
                   
                   #---user input for search---#
                   # textInput("option1", "select option1"),
                   # textInput("option2", "select option2"),
                   
                   selectInput("dataset", "Select a disease type: ", c("", "a", "b", "c", "d")),
                   selectInput("option2", "Select a tissue and cell line: ", c("", "a", "b", "c", "d")),
                   
                   helpText("Select your download format."),
                   radioButtons("type", "Format Type: ", 
                                choices = c("Excel (CSV)", "Text(TSV)", "Text(Space Separated)", "Doc")
                   ),
                   br(),
                   
                   helpText("Click the download button to download your results."),
                   downloadButton('downloadData', 'Download')
                   
               ),
               mainPanel(
                 h3("Search Results: "),
                 tableOutput('table')
               )
        ),
      
      
      tabPanel(title = "Submission",
               sidebarPanel(
                 helpText("Use the text box to write information that may be missing in the database."),
                 br(),
                 helpText("Click the 'Submit' button below to submit your entry for review."),
                 actionButton("submitEntry", "Submit")
                 
               ),
               mainPanel(
                 textAreaInput("entryText", "", "Data Summary", width = "500px", height = "300px"),
                 verbatimTextOutput("value")
               )
      ),
      
      tabPanel(title = "Removal Request","Removal Request")
      

      
      )          
          
      
)
    



server <- function(input, output, session){
  
  #output$dataset <- renderText(input$dataset)
  #output$option2 <- renderText(input$option2)
  
  #--fetch from options user selected--#
  datasetInput <- reactive({
    switch(input$dataset,
           "a" = iris,
           "b" = mtcars,
           "c" = trees)
  })
  
  #---show selected data in main panel---#
  output$table <- renderTable({
    datasetInput()
  })
  
  #---fetch from options for download---#
  fileext <- reactive({
    switch(input$type,
           "Excel (CSV)" = "csv", 
           "Text(TSV)" = "txt", 
           "Text(Space Separated)" = "txt", 
           "Doc" = "doc"
           )
  })
  
  output$value <-renderText({input$caption})
  
  randomVals <- eventReactive(input$submitEntry, {
    runif(input$entryText)
  })
  
}


shinyApp(ui = ui, server = server)


