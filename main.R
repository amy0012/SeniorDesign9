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

cellLine <- select(wgEncodeRegTfbsClusteredwithCellsV3, V1, V2, V3, V4, V6)
cellLine <- cellLine[1:100000,]
names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'strand')

traits <- select(RESULTS, Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)

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
    MergeData(input$dataset)
  })
  
  #--fetch from mesh category option selected--#  
  meshInput <- reactive({
    switch(input$meshData,
           "Nutritional and Metabolic Diseases" = cellLine[cellLine$chrom == "chrX",],
           "type 2" = cellLine[cellLine$chromStart > 7000,],
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
                      blocks = cellLine["blocks"])
  
  rownames(datar) <- cellLine["id"]
  return (datar)
}

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", chrom = "", chromStart = 0, chromEnd = 0,
                             name = "", blocks = ""))
  return (mydefault)
}

UpdateInputs <- function(cellLine, session) {
  updateTextInput(session, "id", value = unname(rownames(cellLine)))
  updateTextInput(session, "chrom", value = unname(cellLine["chrom"]))
  updateTextInput(session, "chromStart", value = as.integer(cellLine["chromStart"]))
  updateTextInput(session, "chromEnd", value = as.integer(cellLine["chromEnd"]))
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
              blocks = "blocks")
  
  result <- list(fields = fields)
  return (result)
}

MergeData <- function(mergeTrait) {
  resultFrame = data.frame(matrix(ncol = 13))
  names(resultFrame) <- c('chrom', 'start', 'stop', 'name', 'strand', 'Position',
                          'Trait', 'SNP', 'p.value', 'Chr', 'Gene.Region',
                          'Context', 'MESH.CATEGORY')
  tempdf = traits[traits$Trait == mergeTrait,]
  for (row in 1:nrow(tempdf)) {
    tempdf2<-data.frame(cellLine[cellLine$start <= tempdf$Position[row],])
    tempdf2<-data.frame(tempdf2[tempdf2$stop >= tempdf$Position[row],])
    tempdf2<-data.frame(tempdf2[paste("chr", tempdf$Chr[row], sep="") == tempdf2$chrom,])
    if (nrow(tempdf2) > 0) {
      #For some reason, if you remove "+1" it will start omitting (overwriting?) entries.
      resultFrame[nrow(resultFrame) + 1,] <- list(as.character(tempdf2$chrom[1]), tempdf2$start[1], tempdf2$stop[1],
                                as.character(tempdf2$name[1]), as.character(tempdf2$strand[1]), 
                                tempdf$Position[row], as.character(tempdf$Trait[row]),
                                as.character(tempdf$SNP[row]), as.character(tempdf$p.value[row]),
                                as.character(tempdf$Chr[row]), as.character(tempdf$Gene.Region[row]),
                                as.character(tempdf$Context[row]), as.character(tempdf$MESH.CATEGORY[row]))
    }
  }
  #Remove the top row of "NA's" if there were hits. The "NA" values are created with the "+1+ above.
  if (nrow(resultFrame) > 1) {
    resultFrame <- resultFrame[-c(1),]
  }
  return (resultFrame)
}

#names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'strand')

#traits <- select(RESULTS, Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)


shinyApp(ui = ui, server = server)


