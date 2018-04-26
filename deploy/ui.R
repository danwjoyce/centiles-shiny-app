library(shiny)
library(ggplot2)

source("centileFunctions.R")

fluidPage(
     
     titlePanel(""),
          
     fluidRow(
          column(2,                                
                    img(src = "./app_logo_final.png", height = 72, width = 150),
                    h6("Version 0.1 - 28/3/15"),
                    h4("Parameters"),
                    
                    selectInput('child_sex', 'Sex',  c("Male" = "male", "Female" = "female"),
                                                  selectize = FALSE),

                    dateInput('child_dob',
                              label = 'Date of Birth (dd/mm/yyyy)',
                              value = Sys.Date(),
                              format = "dd/mm/yyyy"),
                    
                    numericInput("child_height", 
                                 label = 'Height (cms)', 
                                 value = 40, min = 20, max = 250),
                    
                    numericInput("child_sys_bp", 
                                 label = 'Systolic Blood Pressure', 
                                 value = 100, min = 20, max = 300),
                    
                    numericInput("child_dia_bp", 
                                 label = 'Diastolic Blood Pressure', 
                                 value = 60, min = 20, max = 200)                
          ),
          column(6,
                 tabsetPanel(
                      tabPanel("Summary", htmlOutput("summary")),
                      tabPanel("Plot", plotOutput("plot")),                       
                      tabPanel("About", htmlOutput("about"))
                 )
                 
          ),
          column(2, 
                 selectInput('app_loc', 'Country Paramters',  c("USA (CDC2000)" = "US", "UK (UK90)" = "UK"),
                             selectize = FALSE),
                 selectInput("plot_type", "Plot:",
                             c("Height" = "height",
                               "Systolic BP" = "sysbp",
                               "Diastolic BP" = "diabp")),
                 actionButton("plot_now", "Plot")                
          )
     )
)
