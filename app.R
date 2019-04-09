#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyFiles)
library(dplyr)
library(stringr)
library(ggplot2)
library(pander)
library(tidyr)
library(purrr)
source("./R/wrangling_plotting_functions.R")

# Define UI for application that draws a histogram
ui <-  fluidPage(theme = shinytheme("yeti"),
                 # shinythemes::themeSelector(),
                 navbarPage("ShinyIDEA", id = "inTabset",
                            tabPanel(title = "Project setup", value = "projtab",
                                     wellPanel("Select a directory containing expert judgment files"),
                                     shinyDirButton("dir", "Input directory", "Upload"),
                                     verbatimTextOutput("dir", placeholder = TRUE),
                                     # textOutput(outputId = "tt"),
                                     br(),
                                     hr(),
                                     fluidRow(column(12, h1("Input Project Options:"))),
                                     numericInput("StConf",
                                                  label = "Select Standardised Confidence Level (0 - 100%)",value = 90,min = 0,max = 100,step = 5),
                                     numericInput("Quests",
                                                  label = "Select Number of Questions",value = 5,min = 0,max = 10,step = 1),
                                     actionButton("start", "Start")
                            ),
                            tabPanel(title = "Round1", value = "round1",
                                     wellPanel("Click on each Question Tab to view the aggregated estimates from Round 1"),
                                     br(),
                                     fluidRow(
                                         column(6, dataTableOutput("table"))
                                     )
                                     
                            )
                 )
)



server <- function(input, output) {
    # Directory Choose
    roots = c(home = '~')
    shinyDirChoose(
        input,
        'dir',
        roots = roots,
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    global <- reactiveValues(datapath = getwd())
    
    output$dir <- renderText({
        global$datapath
    })
    # Capture User Inputs into reactive variables
    dir <- reactive({input$dir})
    StConf <- reactive({input$StConf})
    Quests <- reactive({input$Quests})
    
    
    
    # Reactively update path in DirChooseWidget:
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     req(is.list(input$dir))
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    ### Take input directory, collate, wrangle data and return as table
    path1 <- reactive({
        return(print(paste0(parseDirPath(roots, input$dir),"/")))
    })
    
    dataraw <- eventReactive(input$start, {
        # Hard code user options for now
        SelectRound <- 1
        raw_dat <- clean_collate_raw_data(paste0(path1()),
                                          StConf = StConf(),
                                          Quests = Quests())

        #datpat <- paste0(path1(),"/data.csv")
        #dataruw <- read.csv(datpat, header = input$header, sep = input$sep, quote = input$quote,
        #                    stringsAsFactors = FALSE)
        #dataruw
        raw_dat
    })
    
    output$table <- renderDataTable({
        dataraw()
    })

    data_aggregated <- eventReactive(input$start, {
        # take dataraw() and send it to the aggregating functions
        plots_tables_data <- aggregate_data(dataraw(),StConf()) %>% 
            dplyr::filter(Round == 1) %>%
            dplyr::group_by(Question) %>%
            tidyr::nest(.key = "data") %>%
            dplyr::mutate(plot = map(data, plot_estimates),
                   title = map(data, ~dplyr::select(.x, Title) %>% distinct(Title))) %>%
            tidyr::unnest(title)
        
        plots_tables_data
    })

    # test that data_aggregated is correctly created
    output$tibbles <- renderPrint({
        data_aggregated()
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
