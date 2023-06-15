# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
# QSIDE app 
# @author The Data Science Collaboratory At Colgate University (datascience[at]gmail[dot]com)
# @description UI code for t-test app
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################

##########################################
# Shiny
##########################################
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinyAce)
library(shinyBS)
library(DT)
library(zip)

##########################################
# General
##########################################
library(tidyverse) #ggplot2, tibble, tidyr, reader, #purrr, dplyr, stringr, forcats

##########################################
# QSIDE
##########################################
library(tigris)
library(sf)
library(tidycensus)
library(labelled)
library(openxlsx)
library(lubridate)
library(hms)


#--------------------------------------------
#---------------     UI    ------------------
#--------------------------------------------
ui<-tagList(tags$head(tags$link(rel = "icon", type = "image/x-icon",
                                href = "https://pbs.twimg.com/profile_images/1118667730067103744/E2koq4n1_400x400.png"),
                      tags$style(HTML(".paragraph {margin:auto;max-width: 50%;font-size: 15px; text-align:justify;}
                                        h1 {text-align:center;}"))),
            useShinyjs(),
            useShinyalert(),
            
            navbarPage(title="Data Crunching App | QSIDE \U00D7 Data Science Collaboratory", id = "tabs",
                       theme = shinytheme("flatly"),
                       
                       # About page
                       tabPanel("About",
                                h1("Title", align="center"),
                                tags$div(class = "paragraph", tags$hr(),
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec rutrum augue eu lacus efficitur laoreet. Vivamus sit amet urna sed erat vestibulum faucibus ac nec mi. Praesent viverra purus ut nulla placerat, in blandit velit iaculis. Nulla facilisi. Donec ex felis, consectetur efficitur euismod a, auctor a sapien. Fusce porttitor.")), 
                                br(),
                                h3("Subtitle", align="center"), 
                                tags$div(class = "paragraph", tags$hr(),
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit."), 
                                         br(), br(), br()
                                )
                       ),
                       
                       #Dataset exploration panel
                       tabPanel("Analyze Data",
                                #visualize dataset and display dataset
                                mainPanel(
                                  tabsetPanel(id="workPanel",
                                              
                                              tabPanel("Data Input", br(), value = "datainput",
                                                       fluidPage(
                                                         fileInput("file_upload", "Upload a File",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                         actionButton("sample", "Sample dataset"),
                                                         hidden(div(id='choose_sample', 
                                                                    selectInput("sample_data_choice","Sample Data:", 
                                                                                choices = c("Durham NC"),
                                                                                selected = "Durham NC"))),
                                                         tags$hr(),
                                                         h3("Racial Variables"),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectInput("select_race_column",
                                                                              "Race Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectInput("select_aian",
                                                                              "Am. Indian/Alaska Native Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_asian",
                                                                              "Asian Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_black",
                                                                              "Black / African American Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3,
                                                                  selectInput("select_hispanic",
                                                                              "Hispanic / LatinX Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           br(),
                                                           column(width=3,
                                                                  selectInput("select_nhpi",
                                                                              "Native Hawaiin / Pacific Islander",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_white",
                                                                              "White",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_multi",
                                                                              "Multiracial Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_notlisted",
                                                                              "Another Racial Category Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         h3("Gender Variables"),
                                                         fluidRow(
                                                           column(width=3, 
                                                                  selectInput("select_gender_column",
                                                                              "Gender Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=3, 
                                                                  selectInput("select_woman",
                                                                              "Woman Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3, 
                                                                  selectInput("select_man",
                                                                              "Man Code",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  h3("Charge Variables")),
                                                           column(width=4,
                                                                  h3("Arrest")),
                                                           column(width=4,
                                                                  h3("Type of Arrest"))
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectizeInput("select_charges",
                                                                                 "Select Charge Column(s)",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = TRUE)
                                                           ),
                                                           column(width=4,
                                                                  selectInput("select_arrest",
                                                                              "Arrest Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_arrestTypes",
                                                                                 "Select Arrest Type(s)",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = TRUE)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  h3("Bond Amount")),
                                                           column(width=4,
                                                                  h3("Patrol Unit")),
                                                           column(width=4,
                                                                  h3("Arresting Officer"))
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectInput("select_bond",
                                                                              "Bond Amount Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=4,
                                                                  selectInput("select_patrol",
                                                                              "Patrol Unit Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=4,
                                                                  selectInput("select_arrestingofficer",
                                                                              "Arresting Officer Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         h3("Date and Type"),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectInput("select_date",
                                                                              "Date Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           ),
                                                           column(width=3,
                                                                  selectInput("select_timezone",
                                                                              "Time Zone Column",
                                                                              choices = NULL,
                                                                              selected = NULL)
                                                           )
                                                         ),
                                                         downloadButton("downloadresultsZip", "Download Results"), br(), br(), br()
                                                       )
                                              ),
                                              
                                              #display dataset
                                              tabPanel("Data Preview", tags$hr(), value="data",
                                                       verbatimTextOutput("warning"),
                                                       br(),
                                                       DT::dataTableOutput("preview.data"))
                                  ))
                       )
                       # ,
                       # tabPanel("References",
                       #          fluidPage(
                       #            includeHTML("www/livebib.html")
                       #          )
                       # )
            )
)


