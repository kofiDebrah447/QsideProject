server <- (function(input, output, session){
  options(scipen = 4)
  options(shiny.maxRequestSize=30*1024^2)
  
  # Global variables ----
  globalVars <- reactiveValues()
  globalVars$sample <- FALSE

  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  # Process uploaded dataset ----
  upload_data <- reactive({
    req(input$file_upload)
    tryCatch({
      policingdata <- read_csv(input$file_upload$datapath, header = input$header)},
      error = function(e){
        stop(safeError(e))      # parsing error
      }
    )
    globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
    globalVars$dataset.original <- policingdata %>% mutate_if(is.character,as.factor)
    checkdataANDupdate()
  })  
  
  checkdataANDupdate <- function(){
    default.column.names <- c("Race", "Gender", paste0("charge",1:25),
                              "typeofarrest", "bondamount", "Patrol",
                              "Officer",  "datetimeofarrest")
    
    if(all(default.column.names %in% colnames(globalVars$dataset))){
      # fill race column select
      updateSelectInput(session, "select_race_column", choices = c(colnames(globalVars$dataset)), selected = "Race")
      # fill race selects
      updateSelectInput(session, "select_aian", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "AIAN")
      updateSelectInput(session, "select_asian", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "Asian")
      updateSelectInput(session, "select_black", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "Black")
      updateSelectInput(session, "select_hispanic", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "NA")
      updateSelectInput(session, "select_nhpi", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "NA")
      updateSelectInput(session, "select_white", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "White")
      updateSelectInput(session, "select_multi", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "NA")
      updateSelectInput(session, "select_notlisted", choices = c("NA", as.character(unique(globalVars$dataset[["Race"]]))), selected = "NA")
      
      # fill gender column select
      updateSelectInput(session, "select_gender_column", choices = c(colnames(globalVars$dataset)), selected = "Gender")
      # fill gender selects
      updateSelectInput(session, "select_man", choices = unique(globalVars$dataset[["Gender"]]), selected = "Man")
      updateSelectInput(session, "select_woman", choices = unique(globalVars$dataset[["Gender"]]), selected = "Woman")
      
      # fill charge column selectize
      updateSelectizeInput(session, "select_charges", choices = c(colnames(globalVars$dataset)), selected = paste0("charge",1:25))
        
      # fill arrest column select
      updateSelectInput(session, "select_arrest", choices = c(colnames(globalVars$dataset)), selected = "typeofarrest")
      
      # fill arrest type selectize 
      updateSelectizeInput(session, "select_arrestTypes", choices = unique(globalVars$dataset[["typeofarrest"]]), selected = c("Taken Into Custody", "Summoned/Cited"))
      
      # fill bond amount column select
      updateSelectInput(session, "select_bond", choices = c(colnames(globalVars$dataset)), selected = "bondamount")
      
      # fill patrol column select
      updateSelectInput(session, "select_patrol", choices = c(colnames(globalVars$dataset)), selected = "Patrol")
      
      # fill arresting officer column select
      updateSelectInput(session, "select_arrestingofficer", choices = c(colnames(globalVars$dataset)), selected = "Officer")

      # fill date column select
      updateSelectInput(session, "select_date", choices = c(colnames(globalVars$dataset)), selected = "datetimeofarrest")
      
      # fill timezone
      updateSelectInput(session, "select_timezone", 
                        choices = c("US/Central", "US/Eastern", "US/Mountain", 
                                    "US/Pacific", "UTC"),
                        selected = "US/Eastern")
    }else{
      # update all column inputs to have column names to select
    }
  }
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview.data <- DT::renderDataTable({
    DT::datatable(globalVars$dataset)
  })
  
  ##############################################################################################################
  # Change UI based on user 
  ##############################################################################################################
  observe({
    
  })
  
  ##############################################################################################################
  # Upload Data
  ##############################################################################################################
  observeEvent(input$file_upload,{
    inFile <<- upload_data()
  })
  
  ##############################################################################################################
  # Load Sample Data
  ##############################################################################################################
  observeEvent(input$sample, {
    if(!globalVars$sample){
      globalVars$sample <- TRUE
      shinyjs::hide("file")
      shinyjs::hide("file_upload")
      shinyjs::hide("header")
      shinyjs::show("choose_sample")
      
      if(input$sample_data_choice=="Durham NC"){
        policingdata <- read_csv("www/durhamdata.csv")
      }
      
      globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
      globalVars$dataset.original <- policingdata 
      checkdataANDupdate()
      
      updateActionButton(session, "sample", label = "<- Back")
    } else {
      globalVars$sample <- FALSE
      globalVars$dataset <- NULL
      shinyjs::show("file")
      shinyjs::show("header")
      shinyjs::show("file_upload")
      shinyjs::hide("choose_sample")
      
      updateActionButton(session, "sample", label = "Sample dataset")
      
      globalVars$dataset <- NULL
      globalVars$dataset.original <- NULL
    }
  })
  
  observeEvent(input$sample_data_choice,{
    if(globalVars$sample){
      if(input$sample_data_choice=="Durham NC"){
        policingdata <- read_csv("www/durhamdata.csv")
      }
      globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
      globalVars$dataset.original <- policingdata %>% mutate_if(is.character,as.factor)      
      checkdataANDupdate()
    }
  })
  
  
})