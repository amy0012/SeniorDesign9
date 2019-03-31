#
# This is a Shiny web application. 
# 3rd Prototype for the Biology Tool Kit
# 3/31/2019
#

library(shiny)
library(shinythemes)
library(DT)
library(gmailr)
library(dplyr)

cellLine <- wgEncodeRegTfbsClusteredWithCellsV3.bed
cellLine <- cellLine[1:10000,]
names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'score', 'strand')

traits <- RESULTS

traitOptions <- unique(c(as.character(traits$Trait)))
meshOptions <- unique(c(as.character(traits$MESH.CATEGORY)))

#------------------------- USER INTERFACE ----------------------------#

ui <- fluidPage(
  #tags$img(src = "headerwhite.png", height = 250, width = 1500),
  navbarPage(
  
  "Computational Biology Bio-Informatics Toolkit",
  theme = shinytheme("flatly"),
  ## yeti, cerulean, cosmo
  #---page tab settings---#
  tabsetPanel(type = "tab", position = "right",
              
              #SEARCH TAB
              tabPanel(title = "Search", 
                       sidebarPanel(
                         
                         

                         helpText("Welcome to the Biology Bio-Information ToolKit!
                                  Use the options below to search."),
                         br(),
                         
                         #---user input for search---#
                         # textInput("option1", "select option1"),
                         # textInput("option2", "select option2"),
                         
                         selectInput("dataset", "Select a trait: ", traitOptions),
                         selectInput("meshData", "Select a mesh trait category: ", meshOptions),
                         
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
                         div(style='height:800px; width: 570px; overflow-y: scroll', tableOutput('table'))
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
)



server <- function(input, output, session){
  
  #--fetch from trait option selected--#
  datasetInput <- reactive({
    switch(input$dataset,
           "Waist Circumference" = cellLine[cellLine$chrom == "chrX",],
           "Glucose" = cellLine[cellLine$score < 180,],
           "Forced Expiratory Volume" = cellLine[cellLine$strand == "H1-hESC",],
           "Intuition" = cellLine[cellLine$score >= 1000,])
  })
  
  #--fetch from mesh category option selected--#  
  meshInput <- reactive({
    switch(input$meshData,
           "Nutritional and Metabolic Diseases" = cellLine[cellLine$chrom == "chrX",],
           "type 2" = cellLine[cellLine$score < 180,],
           "type 3" = cellLine[cellLine$strand == "chrY",])
  })
  
  #---show selected data in main panel---#
  output$table <- renderTable({
    datasetInput()
    #meshInput()
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
  
  emails <- eventReactive(input$submitEntry
                          , {
                            test_email <- mime() %>%
                              to(c("ansleigh.yancey@gmail.com")) %>%
                              html_body(input$entryText) %>%
                              subject("subject")
                            # attach_file("file.txt")
                            
                            test_email <- sub("Content-Disposition: inline\r\n--","Content-Disposition: inline\r\n\r\n--", as.character(test_email))
                            
                            ret_val <- send_message(test_email)
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
      cellLine <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(cellLine, session)
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

CastData <- function(cellLine) {
  datar <- data.frame(chrom = cellLine["chrom"], 
                      chromStart = as.integer(cellLine["chromStart"]), 
                      chromEnd = as.integer(cellLine["chromEnd"]),
                      name = cellLine["name"],
                      score = as.integer(cellLine["score"]),
                      blocks = cellLine["blocks"])
  
  rownames(datar) <- cellLine["id"]
  return (datar)
}

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", chrom = "", chromStart = 0, chromEnd = 0,
                             name = "", score = 0, blocks = ""))
  return (mydefault)
}

UpdateInputs <- function(cellLine, session) {
  updateTextInput(session, "id", value = unname(rownames(cellLine)))
  updateTextInput(session, "chrom", value = unname(cellLine["chrom"]))
  updateTextInput(session, "chromStart", value = as.integer(cellLine["chromStart"]))
  updateTextInput(session, "chromEnd", value = as.integer(cellLine["chromEnd"]))
  updateTextInput(session, "score", value = as.integer(cellLine["score"]))
  updateTextInput(session, "blocks", value = as.integer(cellLine["blocks"]))
}

GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

##### CRUD Methods #####

CreateData <- function(cellLine) {
  
  cellLine <- CastData(cellLine)
  rownames(cellLine) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, cellLine)
  } else {
    responses <<- cellLine
  }
}

ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

UpdateData <- function(cellLine) {
  cellLine <- CastData(cellLine)
  responses[row.names(responses) == row.names(cellLine), ] <<- cellLine
}

DeleteData <- function(cellLine) {
  responses <<- responses[row.names(responses) != unname(cellLine["id"]), ]
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


