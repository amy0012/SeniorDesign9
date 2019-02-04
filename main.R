#
# This is a Shiny web application. 
# 1st Prototype for the Biology Tool Kit
# 2/3/2019
#

library(shiny)
library(shinythemes)
library(DT)

data <- df
names(data) <- c('chrom', 'start', 'stop', 'name', 'score', 'strand')

ui <- navbarPage(
  
  "Computational Biology Bio-Informatics Toolkit",
 theme = shinytheme("cosmo"),

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
                         
                         selectInput("dataset", "Select a disease type: ", c("", "type 1", "type 2", "type 3")),
                         selectInput("option2", "Select a tissue and cell line: ", c("", "cell line 1", "cell line 2", "cell line 3", "cell line 4")),
                         
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
                         div(style='height:800px; overflow-y: scroll', tableOutput('table'))
                       )
              ),
              
              
              tabPanel(title = "Submission",
                    
                       sidebarPanel(
                         helpText("Use the text box to write information that may be missing in the database."),
                         br(),
                         helpText("Click the 'Submit' button below to submit your entry for review."),
                         actionButton("submitEntry", "Submit"),
                         height = "1000px"
                      ),
                       mainPanel(
                         textAreaInput("entryText", "", "Data Summary", width = "500px", height = "300px"),
                         verbatimTextOutput("value")
                       )
                  
              ),
              
              tabPanel(title = "Removal",
                       sidebarPanel(
                         helpText("Use the text box to write information that may be missing in the database."),
                         br(),
                         helpText("Click the 'Submit' button below to submit your removal request for review."),
                         actionButton("submitRemoval", "Submit")
                         
                       ),
                       mainPanel(
                         textAreaInput("removalText", "", "Data Summary", width = "500px", height = "300px")
                       )
                       
              ),
              
              tabPanel(title = "ADMIN",
                  #use shiny js to disable the ID field
                  shinyjs::useShinyjs(),
                  #data table
                  DT::dataTableOutput("responses", width = 300), 
                  #input fields
                  tags$hr(),
                  shinyjs::disabled(textInput("id", "Id", "0")),
                  textInput("chrom", "Chromosome", ""),
                  textInput("chromStart", "ChromStart", ""),
                  textInput("chromEnd", "ChromEnd", ""),
                  textInput("name", "Name", ""),
                  textInput("score", "Score", ""),
                  textInput("blocks", "Blocks", ""),
                  #action buttons
                  actionButton("submit", "Submit"),
                  actionButton("new", "New"),
                  actionButton("delete", "Delete")
              )
              
  )          
  
  
)




server <- function(input, output, session){
  
  #output$dataset <- renderText(input$dataset)
  #output$option2 <- renderText(input$option2)
  
  #--fetch from options user selected--#
  datasetInput <- reactive({
    switch(input$dataset,
           "type 1" = data[1:100,],
           "type 2" = data[100:200,],
           "type 3" = data[200:300,])
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
  
  randomVals <- eventReactive(input$submitRemoval, {
    runif(input$removalText)
  })
  
  
  
  #____________ADMIN_____________#
  
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  
  
}




##### HELPERS #####

CastData <- function(data) {
  datar <- data.frame(chrom = data["chrom"], 
                      chromStart = as.integer(data["chromStart"]), 
                      chromEnd = as.integer(data["chromEnd"]),
                      name = data["name"],
                      score = as.integer(data["score"]),
                      blocks = data["blocks"])
  
  rownames(datar) <- data["id"]
  return (datar)
}

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", chrom = "", chromStart = 0, chromEnd = 0,
                             name = "", score = 0, blocks = ""))
  return (mydefault)
}

UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "chrom", value = unname(data["chrom"]))
  updateTextInput(session, "chromStart", value = as.integer(data["chromStart"]))
  updateTextInput(session, "chromEnd", value = as.integer(data["chromEnd"]))
  updateTextInput(session, "score", value = as.integer(data["score"]))
  updateTextInput(session, "blocks", value = as.integer(data["blocks"]))
}

GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

##### CRUD Methods #####

CreateData <- function(data) {
  
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              chrom = "Chromosome", 
              chromStart = "chromStart", 
              chromEnd = "chromEnd",
              name = "name",
              score = "score",
              blocks = "blocks")
  
  result <- list(fields = fields)
  return (result)
}


shinyApp(ui = ui, server = server)


