#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DiagrammeR) #to make diagram
library(DiagrammeRsvg) #to download diagram
library(magrittr) #to download diagram
library(rsvg) #to download diagram
library(colourpicker) #to customize diagram
library(stringr) #to catch errors in the input text

ui <- fluidPage(
    titlePanel("PRISMA Flow Diagram Generator"),
    
    sidebarLayout(
        sidebarPanel(width = 6,
            #helpText("App to create a PRISMA Flow Diagram."),
            
            #Number of records identified from main source
            numericInput("databaseSearchNum", 
                         "Number of records identified through database searching", 
                         value = 1,
                         min = 1),
            #Checkbox to add details
            checkboxInput("databaseNumDetails", label = "Add details for records identified through database?"),
            #Conditional panel if details are needed
            conditionalPanel(condition = "input.databaseNumDetails == true",
                             selectInput("databaseDet", "Select number of details", choices = seq(1,10,1)),
                             fluidRow(
                               column(width = 6, 
                                      uiOutput("input_ui_text")), #Text for detail of records
                                                                  #Automatically adjusts to the number of entries needed
                               column(width = 6, 
                                      uiOutput("input_ui")) #Number of sources
                                                            #Automatically adjusts to the number of entries needed
                             ),
                             
                             
            ),
            
            numericInput("otherNum", 
                         label = "Number of records identified through other sources", 
                         value = 1,
                         min = 1),
            checkboxInput("otherNumDetails", label = "Add details for records identified through database?"),
            conditionalPanel(condition = "input.otherNumDetails == true",
                             selectInput("otherDet", "Select number of other sources", choices = seq(1,10,1)),
                             fluidRow(
                                 column(width = 6, 
                                        uiOutput("input_ui_text_other")), 
                                 column(width = 6, 
                                        uiOutput("input_ui_other"))
                             ),
                             
                             
            ),
            
            numericInput("numAfterDupNum", 
                         label = "Number of records after duplicates removed", 
                         value = 1,
                         min = 1),
            
            numericInput("excludeNum", 
                         label = "Number of records excluded", 
                         value = 1,
                         min = 0),
            
            checkboxInput("excludeNumDetails", label = "Add reasons for records excluded?"),
            conditionalPanel(condition = "input.excludeNumDetails == true",
                             selectInput("excludeDet", "Select number of exclusion reasons", choices = seq(1,10,1)),
                             fluidRow(
                                 column(width = 6, 
                                        uiOutput("input_ui_text_exclude")), 
                                 column(width = 6, 
                                        uiOutput("input_ui_exclude"))
                             ),
                             
                             
            ),
            
            numericInput("fulltextNum", 
                         label = "Number of full text articles assessed", 
                         value = 1,
                         min = 1),
            
            numericInput("fulltextexcludeNum", 
                         label = "Number of full text articles excluded", 
                         value = 0,
                         min = 0),
            
            conditionalPanel(condition = "input.fulltextexcludeNum > 0",
                             selectInput("fulltextexcludeDet", "Select number of full text exclusion reasons", choices = seq(1,10,1)),
                             fluidRow(
                                 column(width = 6, 
                                        uiOutput("input_ui_text_fulltextexclude")), 
                                 column(width = 6, 
                                        uiOutput("input_ui_fulltextexclude"))
                             ),
                             
                             
            ),
            
            #select types of analyses done to control nodes needed
            selectInput(inputId = "typesOfAnalyses",
                        label = "Select types of analyses done",
                        choices = c("Quantitative and Qualitative" = "both",
                                    "Quantitative" = "quant",
                                    "Qualitative" = "qual"),
                        selected = "both"),
            
            #conditional panels to enter number of records for each type of analysis
            conditionalPanel(condition = "input.typesOfAnalyses == 'both' || input.typesOfAnalyses ==  'qual'",
                             numericInput("qualNum", 
                                          label = "Number of studies used for qualitative analysis", 
                                          value = 1,
                                          min = 1),
            ),
            conditionalPanel(condition = "input.typesOfAnalyses == 'both' || input.typesOfAnalyses ==  'quant'",
                             numericInput("quantNum", 
                                          label = "Number of studies used for quantitative analysis", 
                                          value = 1,
                                          min = 1),
            ),
            
            
            #Conditional panel for custom styling
            checkboxInput(inputId = "customStyle",
                          label = "Adjust colors and fonts?"),
            conditionalPanel(condition = "input.customStyle == true",
                             
                             #Nodes
                             #color
                             colourInput("nodeOutlineCol", "Color of node outline", value = "black"),
                             #fill color
                             colourInput("nodeFillCol", "Color of node fill", value = "white"),
                             #font color
                             colourInput("nodeFontColor", "Font color", value = "black"),
                             #font name
                             selectInput("font", "Font", 
                                         choices = c("Arial" = "Arial",
                                                     "Bookman" = "Bookman",
                                                     "Courier" = "Courier",
                                                     "Helvetica" = "Helvetica",
                                                     "Palatino" = "Palatino",
                                                     "Times New Roman" = "TimesNewRoman"
                                         )),
                             #border width
                             sliderInput("penwidth", "Boarder thickness",
                                         min = 0.5,
                                         max = 3, 
                                         value = 1, 
                                         step = 0.1),
                             
                             
                             #Edges
                             #color
                             colourInput("arrowCol", "Color of arrow", value = "black"),
                             #arrow width
                             sliderInput("arrowPenwidth", "Arrow thickness",
                                         min = 0.5,
                                         max = 3, 
                                         value = 1, 
                                         step = 0.1)
                                           
            ),
            
        ),
        
        mainPanel(width = 6, 
                  
                  #Download buttons for two types of files
                  fluidRow(
                      column(width = 4, 
                             downloadButton("downloadPDF", "Download as PDF")),
                      column(width = 2),
                      column(width = 4, 
                             downloadButton("downloadPNG", "Download as png"))
                  ),
                  
                  #Output the actual diagram
                  br(),
                  grVizOutput("diagram", width = "100%", height = "400px"),
                  br(),
                  
                  
                  
        )
    )
)

server <- function(input, output) {
    
    #Make actual diagram
    output$diagram <- renderDiagrammeR({
        diagram()
    })
    
    #Set up the diagram
    diagram <- reactive({
        
        #Set up all the text for DiagrammeR
        
        #We allow different types of analyses and 
        # need to set up the nodes and arrows based on user input
        
        #Add or remove nodes depending on type of analysis
        analysesSetupNodes<-ifelse(input$typesOfAnalyses == "both",
                              paste("g [label = '@@7']",
                                    "h [label = '@@8']",
                                    "i [label = '@@9']",
                                    sep = "\n"),
                              ifelse(input$typesOfAnalyses == "quant",
                                     paste("g [label = '@@7']",
                                           "h [label = '@@8']",
                                           sep = "\n"),
                                     paste0("h [label = '@@7']",
                                            "i [label = '@@8']",
                                            sep = "\n")))
        
        #Control the arrows based on type of analyses
        analysesNodes<-ifelse(input$typesOfAnalyses == "both",
                              paste("e -> {g h}",
                                    "g -> {i}",
                                    sep = "\n"),
                              ifelse(input$typesOfAnalyses == "quant",
                                     paste("e -> {g h}"),
                                     paste0("e -> {i h}")))
        
        #Control the labels based on type of analyses
        analysisNodeText<-ifelse(input$typesOfAnalyses == "both",
                                 paste(paste0("[7]: '", paste0("Studies included in\\nqualitative synthesis\\n(n = ", input$qualNum),")' "),
                                       paste0("[8]: '", fulltextexcludeRecs(),"' "),
                                       paste0("[9]: '", paste0("Studies included in\\nquantitative synthesis\\n(n = ", input$quantNum),")' "),
                                       sep = "\n"),
                                 ifelse(input$typesOfAnalyses == "quant",
                                        paste(paste0("[7]: '", paste0("Studies included in\\nquantitative synthesis\\n(n = ", input$quantNum),")' "),
                                              paste0("[8]: '", fulltextexcludeRecs(),"' "),
                                              sep = "\n"),
                                        paste(paste0("[7]: '", fulltextexcludeRecs(),"' "),
                                              paste0("[8]: '", paste0("Studies included in\\nqualitative synthesis\\n(n = ", input$qualNum),")' "),
                                              sep = "\n")))
        
        #set up the formatting for the nodes
        nodeDets<-paste0("node [color = '", input$nodeOutlineCol,
                         "', fillcolor = '", input$nodeFillCol,
                         "', penwidth = ", input$penwidth,
                         ", fontcolor = '", input$nodeFontColor,
                         "', fontname = '", input$font,
                         "']")
        
        #set up the formatting for the arrows
        edgeDets<-paste0("edge [color = '", input$arrowCol,
                         "', penwidth = ", input$arrowPenwidth,
                         "]")
        
        #Combine everything together
        d <- paste(
            "digraph dot {",
            
            "graph [layout = dot]",
            
            "node [shape = rectangle,
            style = filled]",
            
            nodeDets, #add text node formatting
            "a [label = '@@1']", #set up nodes and map to labels
            "b [label = '@@2']",
            "c [label = '@@3']",
            "d [label = '@@4']",
            "e [label = '@@5']",
            "f [label = '@@6']",
            analysesSetupNodes, #add nodes for analysis types
            
            edgeDets, #add text for arrow formatting
            "a -> {b}", #control arrows for diagram
            "c -> {b}",
            "b -> {d}",
            "d -> {e f}",
            analysesNodes, #add arrows for arrow type
            
            #add subgraphs at the same rank to control formatting
            "subgraph {",
            "rank = same; a; c;",
            "}",
            "subgraph {",
            "rank = same; d; f;",
            "}",
            "subgraph {",
            "rank = same; e; h;",
            "}",
            "}",
            
            #Add node labels
            paste0("[1]: '", numRecs(),"' "), #number of main records found
            paste0("[2]: '", paste0("Records after duplicates removed\\n(n = ", input$numAfterDupNum),")' "), 
            paste0("[3]: '", otherRecs(),"' "), #number of records from other sources
            paste0("[4]: '", paste0("Records screened\\n(n = ", input$numAfterDupNum),")' "),
            paste0("[5]: '", paste0("Full text articles\\nassessed for eligibility\\n(n = ", input$fulltextNum),")' "),
            paste0("[6]: '", excludeRecs(),"' "), #number of records excluded
            analysisNodeText, #add labels for analysi s nodes
            sep = "\n") #add line breaks between text
    
        return(grViz(d)) #return the grViz diagram
    })
    
    
    #Text for the number of records identified from the main sourc
    numRecs<- reactive({
        #total number of records found
        numRecText <- paste0("Records identified through\\ndatabase searching\\n(n = ", input$databaseSearchNum,")")
        #add more text if detailed entries selected
        if(input$databaseNumDetails){
            for(i in 1:input$databaseDet){
                validate( #check that there is not a single quote in the user entered text
                          #return error message if there is a single quote
                    need(try(!str_detect(input[[paste0("databaseSourceDet", i)]], "'")), "Please remove the apostrophe/single quote from your text.")
                )
                #Add a new line and text for the detailed entry and number of sources
                numRecText<- paste0(numRecText, 
                    "\\n", input[[paste0("databaseSourceDet", i)]], 
                    " (n = ",input[[paste0("databaseDet", i)]], ")")  
            }
        }
        return(numRecText)
    })
    
    output$input_ui <- renderUI({
        num <- as.integer(input$databaseDet)
        
        lapply(1:num, function(i) {
            #textInput(paste0("databaseSourceDet", i), label = paste0("Database source ", i), value = "")
            numericInput(paste0("databaseDet", i), label = paste0("# of records from source ", i), value = 1)
        })
    })
    
    output$input_ui_text <- renderUI({
        num <- as.integer(input$databaseDet)
        
        lapply(1:num, function(i) {
            textInput(paste0("databaseSourceDet", i), label = paste0("Database source name ", i), value = "")
            #numericInput(paste0("databaseDet", i), label = paste0("Number of records found from source ", i), value = 1)
        })
    })
    
    otherRecs<- reactive({
        numRecText <- paste0("Records identified through\\nother sources\\n(n = ", input$otherNum,")")
        if(input$otherNumDetails){
            for(i in 1:input$otherDet){
                validate(
                    need(try(!str_detect(input[[paste0("otherSourceDet", i)]], "'")), "Please remove the apostrophe/single quote from your text.")
                )
                numRecText<- paste0(numRecText, 
                                    "\\n", input[[paste0("otherSourceDet", i)]], 
                                    " (n = ",input[[paste0("otherDet", i)]], ")")  
            }
        }
        return(numRecText)
    })
    
    output$input_ui_other <- renderUI({
        num <- as.integer(input$otherDet)
        
        lapply(1:num, function(i) {
            #textInput(paste0("databaseSourceDet", i), label = paste0("Database source ", i), value = "")
            numericInput(paste0("otherDet", i), label = paste0("# of records from other source ", i), value = 1)
        })
    })
    
    output$input_ui_text_other <- renderUI({
        num <- as.integer(input$otherDet)
        
        lapply(1:num, function(i) {
            textInput(paste0("otherSourceDet", i), label = paste0("Other source name ", i), value = "")
            #numericInput(paste0("databaseDet", i), label = paste0("Number of records found from source ", i), value = 1)
        })
    })
    
    
    excludeRecs <- reactive({
        numRecText <- paste0("Records excluded\\nother sources\\n(n = ", input$excludeNum,")")
        if(input$excludeNumDetails){
            for(i in 1:input$excludeDet){
                validate(
                    need(try(!str_detect(input[[paste0("excludeSourceDet", i)]], "'")), "Please remove the apostrophe/single quote from your text.")
                )
                numRecText<- paste0(numRecText, 
                                    "\\n", input[[paste0("excludeSourceDet", i)]], 
                                    " (n = ",input[[paste0("excludeDet", i)]], ")")  
            }
        }
        return(numRecText)
    })
    
    output$input_ui_exclude <- renderUI({
        num <- as.integer(input$excludeDet)
        
        lapply(1:num, function(i) {
            #textInput(paste0("databaseSourceDet", i), label = paste0("Database source ", i), value = "")
            numericInput(paste0("excludeDet", i), label = paste0("# excluded due to reason ", i), value = 1)
        })
    })
    
    output$input_ui_text_exclude <- renderUI({
        num <- as.integer(input$excludeDet)
        
        lapply(1:num, function(i) {
            textInput(paste0("excludeSourceDet", i), label = paste0("Record exclusion reason ", i), value = "")
            #numericInput(paste0("databaseDet", i), label = paste0("Number of records found from source ", i), value = 1)
        })
    })
    
    
    fulltextexcludeRecs <- reactive({
        numRecText <- paste0("Full-text articles excluded\\n(n = ", input$fulltextexcludeNum,")")
        if(input$fulltextexcludeNum > 0){
            for(i in 1:input$fulltextexcludeDet){
                numRecText<- paste0(numRecText, 
                                    "\\n", input[[paste0("fulltextexcludeSourceDet", i)]], 
                                    " (n = ",input[[paste0("fulltextexcludeDet", i)]], ")")  
            }
        }
        return(numRecText)
    })
    
    output$input_ui_fulltextexclude <- renderUI({
        num <- as.integer(input$fulltextexcludeDet)
        
        lapply(1:num, function(i) {
            #textInput(paste0("databaseSourceDet", i), label = paste0("Database source ", i), value = "")
            numericInput(paste0("fulltextexcludeDet", i), label = paste0("# excluded due to reason ", i), value = 1)
        })
    })
    
    output$input_ui_text_fulltextexclude <- renderUI({
        num <- as.integer(input$fulltextexcludeDet)
        
        lapply(1:num, function(i) {
            textInput(paste0("fulltextexcludeSourceDet", i), label = paste0("Full text exclusion reason ", i), value = "")
            #numericInput(paste0("databaseDet", i), label = paste0("Number of records found from source ", i), value = 1)
        })
    })
    
    output$downloadPDF <- downloadHandler(
        filename = function() {
            paste("diagram.pdf", sep = "")
        },
        content = function(file) {
            diagram() %>%
                export_svg %>% charToRaw %>% rsvg_pdf(file)
            
        }
    )
    
    output$downloadPNG <- downloadHandler(
        filename = function() {
            paste("diagram.png", sep = "")
        },
        content = function(file) {
            diagram() %>%
                export_svg %>% charToRaw %>% rsvg_png(file)
            
        }
    )
    # 
    # output$table <- renderTable({
    #     num <- as.integer(input$num)
    #     
    #     data.frame(lapply(1:num, function(i) {
    #         input[[paste0("databaseDet", i)]]
    #     }))
    # })
}

shinyApp(ui = ui, server = server)
