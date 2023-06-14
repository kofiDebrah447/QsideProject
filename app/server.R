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
      policingdata <- read_csv(input$file_upload$datapath)},
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
  
  ##############################################################################################################
  # Download Results
  ##############################################################################################################
  output$downloadresults <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste("SToPA Tookit.xlsx", sep="")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      saveWorkbook(analyzeData(), file,overwrite = TRUE)
    }
  )
  
  analyzeData <- function(){
    showModal(modalDialog("Things are happening in the background!", footer=NULL))
    
    
    ##################
    ### User input ###
    ##################
    censusapikey <- "985901667535f61f5ea97bfbf8e4fdfcd8c743c4" 
    acsyear <- 2020
    geolevel <- "tract" # Note: race x gender not available below tract level
    #       once up and running we can test different geographies
    
    # Geographic info
    municipality <- "Durham"
    county <- "Durham"
    state <- "NC"
    
    ##########################
    ### Set Census API key ###
    ##########################
    census_api_key(censusapikey)
    
    ########################################
    ### Define relevant census variables ###
    ########################################
    acs5vars <- load_variables(acsyear, "acs5")
    # See https://www.census.gov/newsroom/blogs/random-samplings/2021/08/measuring-racial-ethnic-diversity-2020-census.html
    
    totalpop_var <- "B01001_001"
    
    racegender_vars <- acs5vars %>%
      filter(geography == geolevel) %>%
      filter(str_detect(concept, "^SEX BY AGE \\(.*\\)$")) %>%
      filter(!str_detect(concept, "\\(WHITE ALONE\\)")) %>%
      filter(str_detect(label, "(Male|Female):$")) %>%
      select(-geography)
    
    income_vars <- acs5vars %>%
      filter(geography == geolevel) %>%
      filter(str_detect(concept, "^HOUSEHOLD INCOME IN THE PAST 12 MONTHS.*HOUSEHOLDER\\)$")) %>%
      filter(!str_detect(concept, "\\(WHITE ALONE HOUSEHOLDER\\)$")) %>%
      filter(label != "Estimate!!Total:") %>%
      select(-geography)
    
    ###############################
    ### Get map of municipality ###
    ###############################
    municipalitysf <- places(state = state, year = acsyear, cb = TRUE) %>%
      filter(NAME == municipality)
    
    ###################################################
    ### Get county census data including geometries ###
    ###################################################
    acsracegender <- get_acs(geography = geolevel,
                             variables = racegender_vars$name,
                             year = acsyear,
                             state = state,
                             county = county,
                             geometry = TRUE)
    
    acsincome <- get_acs(geography = geolevel,
                         variables = income_vars$name,
                         year = acsyear,
                         state = state,
                         county = county,
                         geometry = TRUE)
    
    ############################################
    ### Restrict county data to municipality ###
    ############################################
    # Note:
    # A consequential choice we make is to keep all census tracts in county that
    # touch the municipality. An alternative approach would be to intersect
    # the tracts with the municipality and weight ACS data accordingly. However,
    # this is tricky as it assumes spatially uniform distributions and ignores
    # water features.
    
    acsracegender <- st_join(municipalitysf, acsracegender, join = st_intersects) %>%
      select(variable, estimate, geometry) %>%
      st_set_geometry(NULL) %>%
      merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name")
    
    acsincome <- st_join(municipalitysf, acsincome, join = st_intersects) %>%
      select(variable, estimate, geometry) %>%
      st_set_geometry(NULL) %>%
      merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name")
    
    #########################################
    ### Calculate municipality-wide stats ###
    #########################################
    
    # Calculate the race/gender stats and recode
    acsracegender <- acsracegender %>%
      group_by(variable) %>%
      summarise(estimate = sum(estimate)) %>%
      merge(racegender_vars, all.x = TRUE, by.x = "variable", by.y = "name") %>%
      select(-variable) %>%
      rename(people = estimate) %>%
      mutate(Gender = case_when(
        str_detect(label, "Male") ~ "Man",
        str_detect(label, "Female") ~ "Woman")) %>%
      mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data"))) %>%
      mutate(Race = case_when(
        str_detect(concept, "INDIAN") ~ "AIAN",
        str_detect(concept, "ASIAN") ~ "Asian",
        str_detect(concept, "BLACK") ~ "Black",
        str_detect(concept, "\\(HISPANIC OR LATINO\\)") ~ "Hispanic/Latino",
        str_detect(concept, "HAWAIIAN") ~ "NHPI",
        str_detect(concept, "WHITE") ~ "White",
        str_detect(concept, "MORE") ~ "Multiracial",
        str_detect(concept, "OTHER") ~ "A race not listed above")) %>%
      mutate(Race = factor(Race, levels = c("AIAN",
                                            "Asian",
                                            "Black",
                                            "Hispanic/Latino",
                                            "NHPI",
                                            "White",
                                            "Multiracial",
                                            "A race not listed above",
                                            "Missing race data"))) %>%
      mutate(proportion = prop.table(people)) %>%
      mutate(datatype = "Local Population") %>%
      select(Race, Gender, proportion, datatype)
    
    # Calculate the income stats and recode
    acsincome <- acsincome %>%
      group_by(variable) %>%
      summarise(estimate = sum(estimate)) %>%
      merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name") %>%
      select(-variable) %>%
      group_by(label) %>%
      summarise(households = sum(estimate)) %>%
      rename(income = label) %>%
      mutate(income = str_replace_all(income, "Estimate!!Total:!!", "")) %>%
      mutate(proportion = prop.table(households)) %>%
      mutate(datatype = "Local Population") %>%
      select(income, proportion, datatype)
    
    #################################
    ### Tidy up policing data set ###
    #################################
    policingdata <- globalVars$dataset %>%
      # Fix Race
      mutate(Race = case_when(is.na(Race) ~ NA_character_,
                              TRUE        ~ Race)) %>%
      mutate(Race=factor(Race, levels = c("AIAN",
                                          "Asian",
                                          "Black",
                                          "Hispanic/Latino",
                                          "NHPI",
                                          "White",
                                          "Multiracial",
                                          "A race not listed above",
                                          "Missing race data"))) %>%
      # Fix Gender
      mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data")))
    
    
    # Make list of all possible charges
    chargelist <- policingdata %>%
      select(input$select_charges) %>%
      unlist %>%
      unname %>%
      unique %>%
      sort %>%
      data.frame(charge = .)
    
    # Identify routine traffic/moving violations
    # May eventually need user input to identify relevant charges
    trafficwords <- c("drive", "driving", "flashing", "headlamp", "headlight", "highway", "intersection", "lane", "license", "parking", "passing", "plate", "right of way", "speed", "stoplight", "stop light", "stopsign", "stop sign", "traffic", "vehicle", "windshield", "yield") %>%
      paste0(collapse = "|")
    nottrafficwords <- c("aggravated", "consuming", "death", "felony", "impaired", "fraud", "intoxicated", "stolen", "trafficking") %>%
      paste0(collapse = "|")
    trafficcharges <- chargelist %>%
      filter(str_detect(charge, regex(trafficwords, ignore_case = TRUE))) %>%
      filter(!str_detect(charge, regex(nottrafficwords, ignore_case = TRUE))) %>%
      pull(charge)%>%
      paste0(collapse = "|")
    tmp <- policingdata %>%
      select(all_of(input$select_charges))
    tmp[] <- tmp %>%
      lapply(function(x) str_detect(x, regex(trafficcharges, ignore_case = TRUE)))
    policingdata <- policingdata %>%
      mutate(traffic = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
    
    # Type of arrest
    policingdata <- policingdata %>%
      mutate(arrest = typeofarrest %in% input$select_arrestTypes) %>%
      select(-typeofarrest)
    
    # Identify firearms/drug possession
    # May eventually need user input to identify relevant charges
    druggunwords <- c("firearm", "possess control", "possess cs", "substance", "weapon") %>%
      paste0(collapse = "|")
    notdruggunwords <- c("alcohol", "assault", "burglar", "conspiracy", "counterfeit", "dischar", "dwelling", "embezzle", "fraudulent", "insanity", "larceny", "manufactur", "mass dest", "protect minors", "robbery", "sale", "sell", "trafficking") %>%
      paste0(collapse = "|")
    drugguncharges <- chargelist %>%
      filter(str_detect(charge, regex(druggunwords, ignore_case = TRUE))) %>%
      filter(!str_detect(charge, regex(notdruggunwords, ignore_case = TRUE))) %>%
      pull(charge) %>%
      paste0(collapse = "|")
    tmp <- policingdata %>%
      select(all_of(input$select_charges))
    tmp[] <- tmp %>%
      lapply(function(x) str_detect(x, regex(drugguncharges, ignore_case = TRUE)))
    policingdata <- policingdata %>%
      mutate(druggun = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
    
    # Parse date and time
    policingdata <- policingdata %>%
      mutate(datetimeofarrest = mdy_hm(datetimeofarrest, tz = input$select_timezone)) %>%
      mutate(Date = date(datetimeofarrest), Day = wday(datetimeofarrest, label = TRUE), Time = as_hms(datetimeofarrest)) %>%
      select(-datetimeofarrest)
    
    # Identify quality of life crimes
    # May eventually need user input to identify relevant charges
    qolwords <- c("public", "disorderly", "loiter", "noise", "urinat", "vandal") %>%
      paste0(collapse = "|")
    notqolwords <- c("assault", "embezzle", "imperson", "controlled", " cs ", "resist") %>%
      paste0(collapse = "|")
    qolcharges <- chargelist %>%
      filter(str_detect(charge, regex(qolwords, ignore_case = TRUE))) %>%
      filter(!str_detect(charge, regex(notqolwords, ignore_case = TRUE))) %>%
      pull(charge) %>%
      paste0(collapse = "|")
    tmp <- policingdata %>%
      select(all_of(input$select_charges))
    tmp[] <- tmp %>%
      lapply(function(x) str_detect(x, regex(qolcharges, ignore_case = TRUE)))
    policingdata <- policingdata %>%
      mutate(qol = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
    
    # Dump raw charges
    policingdata <- policingdata %>%
      select(-input$select_charges) 
      
      # Put in nice order
    policingdata <- policingdata %>%
      relocate(Race, Gender, traffic, arrest, bondamount, druggun, Patrol, Officer, Date, Day, Time, qol) %>%
      as.data.frame %>%
      remove_attributes("spec")
    
    ###########################
    ### Set up Excel output ###
    ###########################
    
    numq <- 10
    wb <- createWorkbook()
    for (i in 1:10) {
      addWorksheet(wb, paste0("Q",i))
    }
    centered <- createStyle(halign = "center")
    centeredrounded2 <- createStyle(halign = "center", numFmt = "0.00")
    centeredrounded3 <- createStyle(halign = "center", numFmt = "0.000")
    
    ### Below, question numbers correspond to questions as listed at:
    ### https://docs.google.com/spreadsheets/d/1A9x00WhdQccfwa3XolplvPtjBYgd32k-qEpDpFVfddo/edit#gid=0
    
    #########
    ### Q ###
    #########
    
    question <- 1
    
    observed <- policingdata %>%
      group_by(Race, Gender) %>%
      summarise(people = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(people = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    expected <- acsracegender %>%
      complete(Race, Gender, fill = list(proportion = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
    
    qdata <- observed %>%
      mutate(proportion = prop.table(people)) %>%
      select(-people) %>%
      mutate(datatype = "Policing Records") %>%
      rbind(acsracegender) %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      mutate(datatype = factor(datatype)) %>%
      mutate(datatype = relevel(datatype, ref = "Policing Records"))
    
    p <- qdata %>%
      ggplot(aes(x = Gender, y = Race, size = proportion, fill = datatype, group = datatype, alpha = datatype)) +
      geom_point(shape = 21, stroke = 0.6, color = "black") +
      scale_size(range = c(1,20)) +
      scale_fill_manual(values = c("red","gray30")) +
      scale_alpha_manual(values = c(0.7, 0.9)) +
      ylab("Race") +
      scale_x_discrete(name = "Gender", position = "top") +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(title = NULL, override.aes = list(size = 4), label.position = "bottom"), size = "none", group = "none", alpha = "none") +
      theme_bw() +
      theme(legend.text=element_text(size=8), legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", legend.key = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    deleteData(wb, sheet = question, cols = 1, rows = 2)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
    writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
    writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
    mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
    mergeCells(wb, sheet = question, rows = 1, cols = 6:7)
    writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing")
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
    if (chisq$p.value < 0.05) {
      mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
      qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
      writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
    }
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 2
    
    # Racial breakdown of traffic-related offenses
    
    observed <- policingdata %>%
      filter(traffic == TRUE) %>%
      group_by(Race, Gender) %>%
      summarise(people = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(people = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    expected <- acsracegender %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
    
    qdata <- observed %>%
      mutate(proportion = prop.table(people)) %>%
      select(-people) %>%
      mutate(datatype = "Policing Records") %>%
      rbind(acsracegender) %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      mutate(datatype = factor(datatype)) %>%
      mutate(datatype = relevel(datatype, ref = "Policing Records"))
    
    p <- qdata %>%
      ggplot(aes(x = Gender, y = Race, size = proportion, fill = datatype, group = datatype, alpha = datatype)) +
      geom_point(shape = 21, stroke = 0.6, color = "black") +
      scale_size(range = c(1,20)) +
      scale_fill_manual(values = c("red","gray30")) +
      scale_alpha_manual(values = c(0.7, 0.9)) +
      ylab("Race") +
      scale_x_discrete(name = "Gender", position = "top") +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(title = NULL, override.aes = list(size = 4), label.position = "bottom"), size = "none", group = "none", alpha = "none") +
      theme_bw() +
      theme(legend.text=element_text(size=8), legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", legend.key = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    deleteData(wb, sheet = question, cols = 1, rows = 2)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
    writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
    writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
    mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
    mergeCells(wb, sheet = question, rows = 1, cols = 6:7)
    writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing")
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
    if (chisq$p.value < 0.05) {
      mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
      qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
      writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
    }
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 3
    
    observed <- policingdata %>%
      filter(druggun == TRUE) %>%
      group_by(Race, Gender) %>%
      summarise(people = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(people = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    expected <- acsracegender %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
    
    qdata <- observed %>%
      mutate(proportion = prop.table(people)) %>%
      select(-people) %>%
      mutate(datatype = "Policing Records") %>%
      rbind(acsracegender) %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      mutate(datatype = factor(datatype)) %>%
      mutate(datatype = relevel(datatype, ref = "Policing Records"))
    
    p <- qdata %>%
      ggplot(aes(x = Gender, y = Race, size = proportion, fill = datatype, group = datatype, alpha = datatype)) +
      geom_point(shape = 21, stroke = 0.6, color = "black") +
      scale_size(range = c(1,20)) +
      scale_fill_manual(values = c("red","gray30")) +
      scale_alpha_manual(values = c(0.7, 0.9)) +
      ylab("Race") +
      scale_x_discrete(name = "Gender", position = "top") +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(title = NULL, override.aes = list(size = 4), label.position = "bottom"), size = "none", group = "none", alpha = "none") +
      theme_bw() +
      theme(legend.text=element_text(size=8), legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", legend.key = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    deleteData(wb, sheet = question, cols = 1, rows = 2)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
    writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
    writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
    mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
    mergeCells(wb, sheet = question, rows = 1, cols = 6:7)
    writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing")
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
    if (chisq$p.value < 0.05) {
      mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
      qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
      writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
    }
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 4
    
    observed <- policingdata %>%
      filter(qol == TRUE) %>%
      group_by(Race, Gender) %>%
      summarise(people = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(people = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    expected <- acsracegender %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
    
    qdata <- observed %>%
      mutate(proportion = prop.table(people)) %>%
      select(-people) %>%
      mutate(datatype = "Policing Records") %>%
      rbind(acsracegender) %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      mutate(datatype = factor(datatype)) %>%
      mutate(datatype = relevel(datatype, ref = "Policing Records"))
    
    p <- qdata %>%
      ggplot(aes(x = Gender, y = Race, size = proportion, fill = datatype, group = datatype, alpha = datatype)) +
      geom_point(shape = 21, stroke = 0.6, color = "black") +
      scale_size(range = c(1,20)) +
      scale_fill_manual(values = c("red","gray30")) +
      scale_alpha_manual(values = c(0.7, 0.9)) +
      ylab("Race") +
      scale_x_discrete(name = "Gender", position = "top") +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(title = NULL, override.aes = list(size = 4), label.position = "bottom"), size = "none", group = "none", alpha = "none") +
      theme_bw() +
      theme(legend.text=element_text(size=8), legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", legend.key = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    deleteData(wb, sheet = question, cols = 1, rows = 2)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
    writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
    writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
    mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
    mergeCells(wb, sheet = question, rows = 1, cols = 6:7)
    writeData(wb, sheet = question, startRow = 2, startCol = 2, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 4, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 6, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 3, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 5, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 7, "Population")
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
    if (chisq$p.value < 0.05) {
      mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
      qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
      writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
    }
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 5
    
    # Racial breakdown of arrests
    
    observed <- policingdata %>%
      filter(arrest == TRUE) %>%
      group_by(Race, Gender) %>%
      summarise(people = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(people = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    expected <- acsracegender %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      filter(Race != "Missing race data") %>%
      filter(Gender != "Missing gender data") %>%
      arrange(Race, Gender)
    
    chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
    
    qdata <- observed %>%
      mutate(proportion = prop.table(people)) %>%
      select(-people) %>%
      mutate(datatype = "Policing Records") %>%
      rbind(acsracegender) %>%
      complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
      mutate(datatype = factor(datatype)) %>%
      mutate(datatype = relevel(datatype, ref = "Policing Records"))
    
    p <- qdata %>%
      ggplot(aes(x = Gender, y = Race, size = proportion, fill = datatype, group = datatype, alpha = datatype)) +
      geom_point(shape = 21, stroke = 0.6, color = "black") +
      scale_size(range = c(1,20)) +
      scale_fill_manual(values = c("red","gray30")) +
      scale_alpha_manual(values = c(0.7, 0.9)) +
      ylab("Race") +
      scale_x_discrete(name = "Gender", position = "top") +
      scale_y_discrete(limits = rev) +
      guides(fill = guide_legend(title = NULL, override.aes = list(size = 4), label.position = "bottom"), size = "none", group = "none", alpha = "none") +
      theme_bw() +
      theme(legend.text=element_text(size=8), legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", legend.key = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    deleteData(wb, sheet = question, cols = 1, rows = 2)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
    writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
    writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
    mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
    mergeCells(wb, sheet = question, rows = 1, cols = 6:7)
    writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population")
    writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
    writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing")
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
    if (chisq$p.value < 0.05) {
      mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
      qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
      writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
    }
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 6
    
    # Proportion arrests for different gender/race
    
    # Note: we treat proportions as 0 when the calculation is 0/0
    qdata <- policingdata %>%
      group_by(Race, Gender) %>%
      summarise(arrests = sum(arrest), incidents = n()) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(arrests = 0, incidents = 0)) %>%
      mutate(proportion = arrests/(incidents + .Machine$double.eps)) %>%
      select(-arrests, -incidents)
    
    p <- qdata %>%
      ggplot(aes(x = Race, y = proportion, fill = Gender)) +
      geom_col(position = position_dodge()) +
      scale_y_continuous(name = "Proportion of Incidents\nResulting in Arrest", limits = c(0,1)) +
      theme(legend.position = "top", legend.direction = "horizontal", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, height = 6.5/1.618, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:4, gridExpand = TRUE)
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = question, cols = 2:4, widths = 20, ignoreMergedCells = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:4, gridExpand = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 7
    
    # Bond amount
    
    qdata <- policingdata %>%
      group_by(Race, Gender, ) %>%
      summarise(meanbond = mean(bondamount, na.rm = TRUE)) %>%
      ungroup %>%
      complete(Race, Gender, fill = list(meanbond = NA))
    
    p <- qdata %>%
      ggplot(aes(x = Race, y = meanbond, fill = Gender)) +
      geom_col(position = position_dodge()) +
      scale_y_continuous(name = "Mean Bond Amount") +
      theme(legend.position = "top", legend.direction = "horizontal", axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, height = 6.5/1.618, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Gender"), values_from = "meanbond") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:4, gridExpand = TRUE)
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = question, cols = 2:4, widths = 20, ignoreMergedCells = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded2, rows = 3:(nrow(qExcel) + 2), cols = 2:4, gridExpand = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 8
    
    qdata <- policingdata %>%
      group_by(Patrol, Race) %>%
      summarise(count = n()) %>%
      ungroup %>%
      complete(Patrol, Race, fill = list(count = 0)) %>%
      group_by(Patrol) %>%
      mutate(proportion = prop.table(count))
    
    p <- ggplot() +
      geom_col(data = qdata, aes(x = Patrol, y = proportion, fill = Race)) +
      ylab("Proportion")
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p)
    
    qExcel <- qdata %>%
      select(-count) %>%
      pivot_wider(names_from = c("Race"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Race")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:10)
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 9
    
    # Racial breakdown for different officers
    # We will look at top 25% of officers or top 10 officers, whichever is a shorter list
    
    officertally <- policingdata %>%
      group_by(Officer) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    top25percent <- officertally %>%
      filter(count >= quantile(count, 0.75)) %>%
      pull(Officer)
    
    top10 <- officertally %>%
      .[1:10, ] %>%
      pull(Officer)  
    
    officerlist <- intersect(top25percent, top10)
    
    qdata <- policingdata %>%
      filter(Officer %in% officerlist) %>%
      mutate(Officer = droplevels(Officer)) %>%
      group_by(Officer, Race) %>%
      summarise(count = n()) %>%
      ungroup %>%
      complete(Officer, Race, fill = list(count = 0)) %>%
      group_by(Officer) %>%
      mutate(proportion = prop.table(count))
    
    p <- ggplot() +
      geom_col(data = qdata, aes(x = Officer, y = proportion, fill = Race)) +
      ylab("Proportion") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 11, units = "in")
    
    qExcel <- qdata %>%
      select(-count) %>%
      pivot_wider(names_from = c("Race"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Race")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:10)
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
    setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
    
    #########
    ### Q ###
    #########
    question <- 10
    
    qdata <- policingdata %>%
      select(Day, Time) %>%
      mutate(Time = hour(Time)) %>%
      group_by(Day, Time) %>%
      summarise(count = n()) %>%
      mutate(Day = factor(Day, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
      mutate(Time = as.factor(Time)) %>%
      mutate(proportion = prop.table(count))
    
    p <- qdata %>%
      ggplot(aes(x = Time, y = count, group = Day, color = Day)) +
      geom_line() +
      scale_y_continuous(name = "Incidents") +
      scale_x_discrete(name = "Hour of Day") +
      theme(panel.grid.minor.y = element_blank())
    ggsave(filename = paste0("q", str_pad(question, 2, pad = "0"), ".png"), plot = p, width = 6.5, height = 6.5/1.618, units = "in")
    
    qExcel <- qdata %>%
      pivot_wider(names_from = c("Time"), values_from = "proportion") 
    
    writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
    writeData(wb, sheet = question, startRow = 1, startCol = 2, "Hour")
    mergeCells(wb, sheet = question, rows = 1, cols = 2:25)
    addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:25, gridExpand = TRUE)
    addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:9, cols = 2:25, gridExpand = TRUE)
    
    ########################
    ### SAVE EXCEL SHEET ###
    ########################
    removeModal()
    wb
  }
  
})