library(shiny)

ui <- fluidPage(
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

server <- function(input, output, session) {
  
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
