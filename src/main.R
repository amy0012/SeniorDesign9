#-----------------------------------------------#
#-------------- Auburn University --------------#
#---------- COMP 4710: Senior Design -----------#
#----------- Professor: Dr. Xiao Qin -----------#
#------------------- Group 9 -------------------#
#----------- Spring Semester | 2019 ------------#
#-------- Online Bioinformatics Toolkit --------#
#------------- Sponsor: Dr. Li Chen ------------#
#-----------------------------------------------#

# Project Location: https://github.com/amy0012/SeniorDesign9
# Tools Used: RStudio, Github, Shinyapps.io
# Languages Used: R

# Authors / Group Members:
#     Benjamin Williams     bmw0059@auburn.edu
#     Paul Ryu              phc0004@auburn.edu
#     Sadaira Packer        smp0043@auburn.edu
#     Ansleigh Yancey       amy0012@auburn.edu
#     Mason Monday          mam0148@auburn.edu

#------------------ Libraries ------------------#
# Shiny: Open source R package that provides a web framework for building web applications using R.
# Shiny Dashboard: Allows creations of headers, sidebar, and body for the web application.
# DT: Provides an R interface to the JavaScript library DataTables. R data objects (matrices or data
#     frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination,
#     sorting, and many other features in the tables.
# DPlyr: Data manipulation grammar package
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# "wgEncodeRegTfbsClusteredWithCellsV3" is the name of the file (of type .bed as its extension) that the
# client provided to us to use. It contains information about human genome cell signals and markers.
cellLine <- select(wgEncodeRegTfbsClusteredwithCellsV3, V1, V2, V3, V4, V6)

# As of the time that this comment was written, the search has been restricted to 100,000 entries because
# the current algorithm is slower than desired. This comment can be removed when a solution, whether it be
# mathematically or database (mySQL, DPLYR, etc) technique, is implemented to make entry size obsolete.
cellLine <- cellLine[1:100000,]

# These are the column titles.
names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'strand')

# Necessary information needed by the client when intersecting datasets is taken.
traits <- select(RESULTS, Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)
traitOptions <- unique(c(as.character(traits$Trait)))
meshOptions <- unique(c(as.character(traits$MESH.CATEGORY)))


#--------------- User Interface ----------------#
# The user interface implements the Shiny Dashboard package and interface. To successfully use it,
# a header, sidebar, and body are needed. This is implemented below.

# Header
header <- dashboardHeader(title = "Bio-Informatics Toolkit")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Search", icon = icon("search"), tabName = "Search"),
    menuItem("Removal Request", icon = icon("minus-circle"), tabName = "Removal"),
    menuItem("Submission Request", icon = icon("plus-circle"), tabName = "Submission"),
    menuItem("Admin", icon = icon("lock"), tabName = "Admin")
  )
)

# Body
body <- dashboardBody(
  tags$img(src = "headerwhite.png", height = 250, width = 1220),
  fluidRow(
    tabItems(
      tabItem(tabName = "Search",
              box(title = "Search Results", width = 8, status = "primary", solidHeader = TRUE,
                  div(style='height:777px; width: 777px; overflow-y: scroll; overflow-x: scroll', tableOutput('table'))
              ),
              box(title = "Search Options", width = 4, status = "primary", solidHeader = TRUE,
                  tabPanel(collapsible = TRUE,
                           helpText("Welcome to the Biology Bio-Information ToolKit! Use the options below to search."),
                           selectInput("dataset", "Select a trait: ", traitOptions),
                           selectInput("meshData", "Select a mesh trait category: ", meshOptions),
                           helpText("Select your download format."),
                           radioButtons("type", "Format Type: ", choices = c("Excel (CSV)", "Text(TSV)", "Text(Space Separated)", "Doc")),
                           helpText("Click the download button to download your results."),
                           downloadButton('downloadData', 'Download')
                  )
              )
      ),
      tabItem(tabName = "Removal",
              box(width = 3,
                  helpText("Use the text box to write information that may need to be removed from the database."),
                  status = "primary",
                  helpText("Click the 'submit button' below to submit your entry for review."),
                  actionButton("submitRemoval", "Submit")
              ),
              box(title = "Request Entry", width = 5, status = "primary", solidHeader = TRUE,
                  textAreaInput("removalText", "", "Data Summary", width = "470px", height = "300px")
              )
      ),
      tabItem(tabName = "Submission",
              box(width = 3,
                  helpText("Use the text box to write information that may need to be added to the database."),
                  status = "primary",
                  helpText("Click the 'submit button' below to submit your entry for review."),
                  actionButton("submitEntry", "Submit")
              ),
              box(title = "Request Entry", width = 5, status = "primary", solidHeader = TRUE,
                  textAreaInput("removalText", "", "Data Summary", width = "470px", height = "300px")
              )
      ),
      tabItem(tabName = "Admin",
              box(title = "ADMIN",
                  status = "primary",
                  solidHeader = TRUE,
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
                  actionButton("delete", "Delete"))
      )
    )
  )
)

# UI Initialization
ui <- dashboardPage(
  skin = "blue",
  header, sidebar, body)


#------------------ Server --------------------#
# Several aspects here for our web application may or may not be used, depending on
# how we implement the uploading of another RESULTS file. Currently, when downloading
# from https://www.ncbi.nlm.nih.gov/projects/gapplus/sgap_plus.htm, only a .TAB file
# extension is attached with the file. Things like Excel sheets, CSV, etc. may be
# unnecessary to try and implement.
server <- function(input, output, session){

  # Fetch from trait option selected
  datasetInput <- reactive({
    MergeData(input$dataset)
  })

  # Fetch from mesh category option selected
  meshInput <- reactive({
    switch(input$meshData,
           "Nutritional and Metabolic Diseases" = cellLine[cellLine$chrom == "chrX",],
           "type 2" = cellLine[cellLine$chromStart > 7000,],
           "type 3" = cellLine[cellLine$strand == "chrY",])
  })

  # Show selected data in main panel
  output$table <- renderTable({
    datasetInput()
  })

  # Fetch from options for download
  fileext <- reactive({
    switch(input$type,
           "Excel (CSV)" = "csv",
           "Text(TSV)" = "txt",
           "Text(Space Separated)" = "txt",
           "Doc" = "doc"
    )
  })

  # IMPORTANT: This is not relevant to our project and will soon be removed once communication
  # with all group members is established. Admin privileges and functions were never a part of
  # the original specifications and thus there is no use for it in our end goals.
  #-------------- ADMIN Server -------------#
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      cellLine <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(cellLine, session)
    }
  })
  output$responses <- DT::renderDataTable({
    input$submit
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )
}

#------------------ Helpers --------------------#
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

#------------------ CRUD Methods --------------------#
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

#------------------ MergeData --------------------#
# Algorithm to intersect datasets.
# Author: Benjamin Williams
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

shinyApp(ui, server)