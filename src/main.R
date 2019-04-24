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

# DOCUMENTATION STYLE:
# The file is documented with "Note: " and "Removed: ".
# "Note: " is normal documentation, provided to assist future in future development efforts.
# "Removed: " are line(s) of code that were not included in the final product but were kept
# in case the logic is needed in future development.


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
library(shinyalert)


# Removed:
# cellLine <- select(read.table("../data/wgEncodeRegTfbsClusteredwithCellsV3.bed",
#                              header = FALSE,
#                              sep = "\t"),
#                   V1, V2, V3, V4, V6)


# Note: "wgEncodeRegTfbsClusteredWithCellsV3" is the name of the file (of type .bed as its extension) that the	
#        client provided to us to use. It contains information about human genome cell signals and markers.
cellLine <- select(wgEncodeRegTfbsClusteredwithCellsV3, V1, V2, V3, V4, V6)


# Removed:
# cellLine <- cellLine[4000000:4300000,]


# Note: These are the column titles.
names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'strand')


# Note: Necessary information needed by the client when intersecting datasets is taken.
traits <- select(read.csv("../data/RESULTS.TAB",
                          header = TRUE,
                          sep = "\t"), 
                 Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)


# Removed: 
# traits <- select(RESULTS, Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)


traitOptions <- unique(c(as.character(traits$Trait)))
meshOptions <- unique(c(as.character(traits$MESH.CATEGORY)))


#--------------- User Interface ----------------#

# Note: Header (Required with Shiny UI)
header <- dashboardHeader(title = "Bio-Informatics Toolkit")


# Note: Sidebar (Required with Shiny UI)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Search", icon = icon("search"), tabName = "Search"),
    
    # Removed: 
    # menuItem("Removal Request", icon = icon("minus-circle"), tabName = "Removal"),
    
    menuItem("Submission", icon = icon("plus-circle"), tabName = "Submission")
    
    # Removed: 
    # ,
    # menuItem("Admin", icon = icon("lock"), tabName = "Admin")
  )
)

# Note: Body (Required with Shiny UI)
# Much of this logic was implemented from the normal Shiny tutorial found at:
# https://rstudio.github.io/shinydashboard/structure.html#body
body <- dashboardBody(
  tags$img(src = "toolkitBanner.png", height = 250, width = "100%"),
  fluidRow(
    tabItems(
      tabItem(tabName = "Search",
              box(title = "Search Results", width = 8, status = "primary", solidHeader = TRUE, 
                  div(style='height:777px; width: "100%"; overflow-y: scroll; overflow-x: scroll', tableOutput('table'))
              ),
              
              box(title = "Search Options", width = 4, status = "primary", solidHeader = TRUE,
                  h4("Welcome to the Biology Bio-Information ToolKit!"), 
                  h4("Use the options below to search."),
                  tags$hr(),
                  selectInput("dataset", "Select a trait: ", traitOptions)
              ),
              
              box(title = "Download Your Search Results", width = 4, status = "primary", solidHeader = TRUE,
                  
                  h4("To download your search results, select a file type below and click the 'Download' button."),
                  tags$hr(),
                  radioButtons("filetype", "File Type:",
                               choices = c("CSV", "TSV", "TAB")),
                  tags$hr(),
                  
                  downloadButton("downloadData", "Download")
              )
      ),
      
      # Removed: 
      # This was not needed anymore after clarifying with client specifications.
      # tabItem(tabName = "Removal",
      #         box(width = 3,
      #             helpText("Use the text box to write information that may need to be removed from the database."), 
      #             status = "primary",
      #             helpText("Click the 'submit button' below to submit your entry for review."),
      #             actionButton("submitRemoval", "Submit")
      #         ),
      #         box(title = "Request Entry", width = 5, status = "primary", solidHeader = TRUE,
      #             textAreaInput("removalText", "", "Data Summary", width = "470px", height = "300px")
      #         )
      # ),
      
      tabItem(tabName = "Submission",
              box(title = "Use Your Own Data", width = 12, solidHeader = TRUE, status = "primary",
                  
                  
                  h4("To compare your own trait dataset to the existing database, please upload a tab-delimited file."),
                  tags$hr(),
                  h4("Acceptable file types: .txt, .csv, .tab"), 
                  h4("Max file size: 30MB"),
                  tags$hr(),
                  h4("Your file must contain, at a minimum, the following eight headers for your submission to be successful."),
                  tags$hr(),
                  h4("Please ensure all eight headers are labeled exactly as shown below:"),
                  img(src = "uploadExampleFinal.PNG", height = 80, width = 800),
                  br()
              ),
              
              box(title = 'File Upload', width = 12, solidHeader = TRUE, status = "primary",
                  fileInput("file1", "Choose .TXT, .CSV, or .TAB File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  
                  h4("Click the Submit button below to add your information."),
                  actionButton("submitButton", "Submit")
              )
              
      )
      
      
      # Removed:
      # Administrator functionality was removed because they were not in original client specifications.
      # However, this may be of use in further development if administrator functionality is desired.
      # ,
      # tabItem(tabName = "Admin",
      #         box(title = "ADMIN",
      #             status = "primary",
      #             solidHeader = TRUE,
      #             #use shiny js to disable the ID field
      #             shinyjs::useShinyjs(),
      #             #data table
      #             DT::dataTableOutput("responses", width = 300), 
      #             #input fields
      #             tags$hr(),
      #             shinyjs::disabled(textInput("id", "Id", "0")),
      #             textInput("chrom", "Chromosome", ""),
      #             textInput("chromStart", "ChromStart", ""),
      #             textInput("chromEnd", "ChromEnd", ""),
      #             textInput("name", "Name", ""),
      #             textInput("blocks", "Blocks", ""),
      #             #action buttons
      #             actionButton("submit", "Submit"),
      #             actionButton("new", "New"),
      #             actionButton("delete", "Delete"))
      # )
    )
    
  ) 
)



ui <- dashboardPage(
  skin = "blue",
  header, sidebar, body,
  useShinyalert())



#------------------ Server --------------------#	
# Several aspects here for our web application may or may not be used, depending on	
# how we implement the uploading of another RESULTS file. Currently, when downloading	
# from https://www.ncbi.nlm.nih.gov/projects/gapplus/sgap_plus.htm, only a .TAB file	
# extension is attached with the file. Things like Excel sheets, CSV, etc. may be	
# unnecessary to try and implement.


server <- function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)
  
  # Note: Fetch from trait option selected
  datasetInput <- reactive({
    MergeData(input$dataset)
  })
  
  
  # Note: Fetch from mesh category option selected
  meshInput <- reactive({
    switch(input$meshData,
           "Nutritional and Metabolic Diseases" = cellLine[cellLine$chrom == "chrX",],
           "type 2" = cellLine[cellLine$chromStart > 7000,],
           "type 3" = cellLine[cellLine$strand == "chrY",])
  })
  
  
  # Note: Show selected data in main panel
  output$table <- renderTable({
    datasetInput()
    
    # Removed: 
    # meshInput()
  })
  
  
  # Note: Fetch from options for download
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$dataset, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "CSV" = ",", "TSV" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
      
      shinyalert("File Download successful")
    }
    
  )
  
  # Note: This block of code handles error conditions that may be met during
  # the uploading of different RESULTS.TAB-esque datasets. It uses standard
  # try - catch error handling.
  observeEvent(input$submitButton, {
    req(input$file1)
    tryCatch({
      traits <<- select(read.csv(input$file1$datapath,
                                 header = TRUE,
                                 sep = "\t"), 
                        Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)
      traitOptions <<- unique(c(as.character(traits$Trait)))
      meshOptions <<- unique(c(as.character(traits$MESH.CATEGORY)))
      shinyalert("File upload successful")
    },
    error=function(cond) {
      shinyalert("File upload failed; please check file format")
    })
  })
  
  
  #-------------- ADMIN Server -------------# 
  # Note: Input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  
  # Note: Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  
  # Note: Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  
  # Note: Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  
  # Note: Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      cellLine <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(cellLine, session)
    }
  })
  
  # Note: Display the table
  output$responses <- DT::renderDataTable({
    
    
    # Note: Update after submit is clicked
    input$submit
    
    
    # Note: Update after delete is clicked
    input$delete
    
    
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )
}


#-------------- Helper Methods -------------#
# Note: CastData method
CastData <- function(cellLine) {
  datar <- data.frame(chrom = cellLine["chrom"], 
                      chromStart = as.integer(cellLine["chromStart"]), 
                      chromEnd = as.integer(cellLine["chromEnd"]),
                      name = cellLine["name"],
                      blocks = cellLine["blocks"])
  rownames(datar) <- cellLine["id"]
  return (datar)
}


# Note: CreateDefaultRecord method
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", chrom = "", chromStart = 0, chromEnd = 0,
                             name = "", blocks = ""))
  return (mydefault)
}


# Note: UpdateInputs method
UpdateInputs <- function(cellLine, session) {
  updateTextInput(session, "id", value = unname(rownames(cellLine)))
  updateTextInput(session, "chrom", value = unname(cellLine["chrom"]))
  updateTextInput(session, "chromStart", value = as.integer(cellLine["chromStart"]))
  updateTextInput(session, "chromEnd", value = as.integer(cellLine["chromEnd"]))
  updateTextInput(session, "blocks", value = as.integer(cellLine["blocks"]))
}


# Note: GetNextID method
GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

#-------------- CRUD Methods -------------#
#         "Create, Update, Delete"

# Note: CreateData method
CreateData <- function(cellLine) {
  cellLine <- CastData(cellLine)
  rownames(cellLine) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, cellLine)
  } else {
    responses <<- cellLine
  }
}


# Note: ReadData method
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}


# Note: UpdateData method
UpdateData <- function(cellLine) {
  cellLine <- CastData(cellLine)
  responses[row.names(responses) == row.names(cellLine), ] <<- cellLine
}


# Note: DeleteData method
DeleteData <- function(cellLine) {
  responses <<- responses[row.names(responses) != unname(cellLine["id"]), ]
}


# Note: GetTableMetadata method
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


# Note: MergeData method
MergeData <- function(mergeTrait) {
  resultFrame = data.frame(matrix(ncol = 13))
  names(resultFrame) <- c('chrom', 'start', 'stop', 'name', 'strand', 'Position',
                          'Trait', 'SNP', 'p.value', 'Chr', 'Gene.Region',
                          'Context', 'MESH.CATEGORY')
  tempdf <- traits[traits$Trait == mergeTrait,]
  for (row in 1:nrow(tempdf)) {
    tempdf2<-cellLine[cellLine$start <= tempdf$Position[row],]
    tempdf2<-tempdf2[tempdf2$stop >= tempdf$Position[row],]
    tempdf2<-tempdf2[paste("chr", tempdf$Chr[row], sep="") == tempdf2$chrom,]
    if (nrow(tempdf2) > 0) {
      
      
      # Note: For some reason, if you remove "+1" it will start omitting (overwriting?) entries.
      resultFrame[nrow(resultFrame) + 1,] <- list(as.character(tempdf2$chrom[1]), tempdf2$start[1], tempdf2$stop[1],
                                                  as.character(tempdf2$name[1]), as.character(tempdf2$strand[1]), 
                                                  tempdf$Position[row], as.character(tempdf$Trait[row]),
                                                  as.character(tempdf$SNP[row]), as.character(tempdf$p.value[row]),
                                                  as.character(tempdf$Chr[row]), as.character(tempdf$Gene.Region[row]),
                                                  as.character(tempdf$Context[row]), as.character(tempdf$MESH.CATEGORY[row]))
    }
  }
  
  
  # Note: Remove the top row of "NA's" if there were hits. The "NA" values are created with the "+1+ above.
  if (nrow(resultFrame) > 1) {
    resultFrame <- resultFrame[-c(1),]
  }
  return (resultFrame)
}

# Removed: 
# names(cellLine) <- c('chrom', 'start', 'stop', 'name', 'strand')
# traits <- select(RESULTS, Trait, SNP, p.value, Chr, Position, Gene.Region, Context, MESH.CATEGORY)

shinyApp(ui, server)
