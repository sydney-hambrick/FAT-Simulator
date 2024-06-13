#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sqldf)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(stringr)
library(ggplot2)
library(tidyr)
library(summarytools)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
    dashboardHeader(title="FAT Simulated Pass Rate"),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data Table", tabName = "table", icon = icon("database")),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-pie") )
      )
    ),
    
    # Application title
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                shinyjs::useShinyjs(),
                fluidPage(
                  box(fileInput(inputId = "file", label = "Upload FAT data file", accept = ".csv")),
                  
                  box(title = "Scoring Criteria",
                      #This is how I handle where Kate asks for the option to include FAT criteria
                      checkboxInput("max_needed", label = "FAT Criteria Specifies Maximum SI", value = TRUE),
                      numericInput(inputId = "max", label = "Maximum Stain Intensity", value = 0, step = 0.25),
                      checkboxInput("min_needed", label = "FAT Criteria Specifies Minimum SI", value = TRUE),
                      numericInput(inputId = "min", label = "Minnimum Stain Intensity", value = 0),
                      checkboxInput("dif_needed", label = "FAT Criteria Specifies Maximum Difference in SI", value = TRUE),
                      numericInput(inputId = "dif", label = "Maximum Difference in Stain Intensity from Reference", value = 0.5),
                      checkboxInput("bkgd_needed", label = "FAT Criteria Specifies Maximum Background", value = TRUE),
                      numericInput(inputId = "bkgd", label = "Maximum background allowed", value = 0.5),
                      textInput(inputId = "marker", label = "What marker is being analyzed?")),
                  
                  
                  box(title = "Data Analysis",
                      checkboxGroupInput("inp",
                                         #This is a more simplified version of the analysis requested by Kate. 
                                         #This does not allow for the choice of match vs. cross, these are hard coded as one option or the other
                                         label = "What data is included in your analysis?",
                                         choiceNames = c("Slide ID", "Formulation", "Lot", "Signal Intensity", "Background"),
                                         choiceValues = c("SlideID", "Formulation", "Lot", "SignalIntensity", "Background"),
                                         selected = c("SlideID", "Formulation", "Lot", "SignalIntensity", "Background")
                                         ),
                      checkboxGroupInput("pivot",
                                         #The are the options to cross on, this analysis does not allow these to be matched.
                                         label = "What data should be kept equal in your analysis?",
                                         choiceNames = c("VTID", "Detection Kit", "Reader"),
                                         choiceValues = c("VTID", "DetectionKit", "Reader"),
                                         selected = c("VTID", "DetectionKit", "Reader")
                      ),
                      checkboxGroupInput("excludes",
                                         #This is not included in Kate's requirements but may be useful for our analysis, may be useful to follow up with her
                                           label = "What formulations to exclude?",
                                           choiceNames = c("OOS light", "N/A", "Reference", "NRC"),
                                         choiceValues = c("\"OOS LIGHT\"", "\"N/A\"", "\"REFERENCE\"", "\"NRC\""),
                                         selected = c("\"OOS LIGHT\"", "\"N/A\"", "\"REFERENCE\"", "\"NRC\"")
                      ),
                      ), 
                  div(id="placeholder"),),
            #This allows the analysis to run
            uiOutput("render"),
            actionButton("crunch", "Crunch"),
            disabled(actionButton("analyze", "Analyze"))),
        
            tabItem(tabName = "table",
                    #Allows the user to see the data before download, allowing QC before download, not requested by Kate
                    fluidRow(downloadButton("downloadData", "Download Data File"),
                      tableOutput("combined"))),
            tabItem(tabName = "analysis", 
                    #Allows the user to see the analysis before download, allowing QC before download, not requested by Kate
                    fluidPage(
                      tabBox(id = "tabOutput",
                      tabPanel(title = "All Reader Combined Data", solidHeader = TRUE,
                          tableOutput("fipAll"), tableOutput("fipCon"), plotOutput("fipAllplot", height = 400, width = 550), height = 750, width = 650),
                      tabPanel(title = "Analysis by Reader", textOutput("ier")),
                      tabPanel(title = "Combined FAT_FTPR", tableOutput("reader")),
                      tabPanel(title = "Set concentrations", uiOutput("order")),
                      tabPanel(title = "Download", downloadButton("report", "Download Final Report"))
                      
                    )))
    ))
)



server <- function(input, output) {
    observeEvent(input$crunch,{
      shinyjs::disable("analyze")
      

      #Checks if all of the values selected above are in the file, prevents errors
      checker <- read.csv(input$file$datapath, header =FALSE, nrows=1)
      match <- TRUE
      incorrect <- vector()
      for (i in c(input$inp, input$pivot, "Formulation", "Lot")){
        if (!(i %in% checker)){
          match <- FALSE
          incorrect <- c(incorrect, i)
          
        }
      }
      
      #Tells the user what values selected are not in the input file
      if (!match){
        output$incorrect <- renderTable(incorrect, colnames = F)
        insertUI(selector = "#placeholder", where = "afterBegin", tags$div(id = "test", box(title = div(h3("Values selected above that are missing from Data File"), h4("Hint: This may be the result of improperly named columns")),  tableOutput("incorrect"))))}
      if (match){shinyjs::enable("analyze")
        removeUI(selector = "#test", multiple = TRUE)}
      
      
      observeEvent(input$analyze,{
        #Creates query, in this handling it is hardcoded which options go into match vs. cross
        query <- (
          paste("SELECT a.*, ", str_sub(paste("b.", input$inp, " as ", input$inp, "_ref, ",collapse = "", sep = ""), end= -3),
                " FROM file as a, file as b WHERE ", 
                str_sub(paste("a.", input$pivot, "=b.", input$pivot, " and ", collapse = "", sep = ""), end=-5),
                paste(" and UPPER(a.Formulation) <> ", input$excludes, collapse = "", sep=""), 
                paste(" and UPPER(b.Formulation) <> ", input$excludes, collapse = "", sep=""),
                " and a.Lot \u003C b.Lot"))
        
        #Reads the file into a temp file, builds the new dataset based on the query simulateously. 
        #This could be split, this is just the easiest way to do it in R
        sqlx <- read.csv.sql(input$file$datapath, sql = query)
        
        #analyzes pass, fail, invalid, modifies the table to contain this info 
        sqly <- mutate(sqlx, SI_Delta = (SignalIntensity-SignalIntensity_ref))
        sqly <- mutate(sqly, FAT_Result = case_when(!((!input$max_needed |(SignalIntensity_ref <= input$max))&(!input$min_needed |(SignalIntensity_ref >= input$min))&(!input$bkgd_needed |(Background_ref <= input$bkgd))) ~ "Invalid",
                     !((!input$max_needed |(SignalIntensity <= input$max))&(!input$min_needed |(SignalIntensity >= input$min))&(!input$bkgd_needed |(Background <= input$bkgd))&(!input$dif_needed |(abs(SignalIntensity-SignalIntensity_ref)) <= input$dif)) ~ "Fail",
                     TRUE ~ "Pass")
        )
        sqly <- mutate(sqly, FAT_FTPR = ifelse(FAT_Result == "Pass", "Pass", "Rerurn/LEE"))
        
        #puts final data so user can view
        output$downloadData <- downloadHandler(filename = paste(input$marker, "_Simulated Pass Rate Data.csv", sep="", collapse = ""), content = function(file){write.csv(sqly, file)})
        output$combined <- renderTable({sqly})
        
        #puts analysis so user can view
        
        
        #static generates the aspects present for all reports
        output$fipAll <- renderTable({sqly %>% count(FAT_Result) %>% mutate(Percent = prop.table(n)*100) %>% rename(Frequency = n)})
        output$fipAllplot <- renderPlot({ggplot(sqly, aes(x = Formulation_ref, y = SI_Delta)) + 
            geom_point(aes(color = Formulation), size = 1, alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = .5, seed = 1))+
            geom_hline(yintercept = c(input$dif, -input$dif)) +
            theme(text = element_text(size=9), legend.title = element_text(size = 10), legend.position = "bottom")})
        output$fipCon <- renderTable({sqly %>% count(FAT_FTPR) %>% mutate(Percent = prop.table(n)*100) %>% rename(Frequency = n)})
        
        #variable number of graphs, depending on number of readers
        readName <- unique(sqly$Reader)
        lapply(readName, function(i){
          
          output[[paste0("fip", i)]] <- renderTable({sqly %>% filter(Reader == i) %>% count(FAT_Result) %>% mutate(Percent = prop.table(n)*100)%>% rename(Frequency = n)})
          output[[paste0("fipPlot", i)]] <- renderPlot({ggplot(sqly %>% filter(Reader == i), aes(x = Formulation_ref, y = SI_Delta)) + 
              geom_point(aes(color = Formulation), size = 1, alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = .5, seed = 1))+
              geom_hline(yintercept = c(input$dif, -input$dif)) +
            theme(text = element_text(size=9), legend.title = element_text(size = 10), legend.position = "bottom")})
          output[[paste0("title",i)]] <- renderText({paste("Reader ", i, " FAT_FTPR")})
          insertUI(selector = "#ier", where = "afterEnd", tabPanel(title = paste("Reader ", i, " FAT_FTPR"), solidHeader = TRUE, textOutput(paste0("title",i)),                                                                         tableOutput(paste0("fip", i)), plotOutput(paste(paste0("fipPlot", i)), height = 400, width = 550), height = 600, width = 650))
        })

        output$reader <- renderTable({sqly %>% group_by(Reader, FAT_Result) %>% summarize(n=n()) %>%spread(FAT_Result, n) %>% mutate})
        
        #file prep and final generation of static analysis
        levels <- unique(sqly$Formulation_ref)
        output$order <- renderUI({lapply(levels, function(i){numericInput(i, label = paste("Concentraction of ", i), value = 0)})})
        levelVals <- reactive(lapply(levels, function(i){i})[order(sapply(lapply(levels, function(j){input[[j]]}), "[[", 1))])
        
        #file generation
        output$report <- downloadHandler(
          filename = function(){paste(input$marker, "_Manufacturability Report.docx", sep = "", collapse = "")},
          content = function(file){
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            rmarkdown::render("report.RMD", output_format = "word_document", output_file = file,
                              params = list(sqly = sqly, max = input$max, min = input$min, dif = input$dif, bkgd = input$bkgd, marker = input$marker, max_needed = input$max_needed, min_needed = input$min_needed, dif_needed = input$dif_needed, bkgd_needed = input$bkgd_needed, order = levelVals(), readers=readName),
                              envir = new.env(parent = globalenv()), clean=F, encoding = "utf-8")})
          }
      )})
      
    }

# Run the application 
shinyApp(ui = ui, server = server)
