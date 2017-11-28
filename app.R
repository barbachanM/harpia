
library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(hash)
library(stringr)
library(lmerTest)
require(ggplot2)
library(markovchain)
library(igraph)
library(mixOmics)
library(Boruta)
library(gtools)
library(stringi)
library(readr)
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = 'Harpia'),
                    
                    
                    dashboardSidebar(
                      tags$p(), tags$hr(),
                      
                      shinyDirButton('folder_G1', 'Group 1', 'Please select a folder'),tags$p(),
                      shinyDirButton('folder_G2', 'Group 2', 'Please select a folder'),
                      helpText("Please upload folders containg tab delimited .csv files. "),
                      tags$p(), #tags$hr(),
                      fileInput("fileAlphabet", "Choose Alphabet File", accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")),
                      tags$p(), tags$hr(),
                      selectInput("selectH", label = h4("Select Entropy Level for Analysis"),
                                  choices = list("-" = 0, "H2" = 2, "H3" = 3, "H4" = 4)),
                      
                      tags$hr(),
                      
                      actionButton("run", "Run!")
                      
                      
                    ) ,
                    
                    dashboardBody(
                      useShinyjs(),
                      
                      conditionalPanel(
                        condition = ("input.run%2 == 0"),
                        h3("Upload Step"),
                        htmlOutput("directorypath"),tags$p(),
                        htmlOutput("directorypath2")
                        #img(src='myImage.png', align = "right")
                      ),
                      conditionalPanel(
                        condition = "input.run%2 == 1",
                        
                        fluidRow(
                          headerPanel(
                            'Analysis Output'),
                          #textOutput("x"),
                          tabsetPanel( id = "tabset",
                            tabPanel("Entropy Analysis"
                                     ,plotOutput("plot1"), tags$hr(),downloadButton('downloadPlot1', 'Download Plot')
                            ),
                            tabPanel("Markov Model Graphs", tags$div(class="header", checked=NA, tags$em(bsButton("help2","Info", icon = NULL, style = "inverse",size = "small", type = "action", block = FALSE, disabled = FALSE, value = FALSE))),plotOutput("plot5"),downloadButton('downloadPlot5', 'Download Plot')
                                     ,tags$hr(),plotOutput("plot6"),downloadButton('downloadPlot6', 'Download Plot'))
                            ,
                            tabPanel("Linear Model Analysis", tags$div(class="header", checked=NA,
                                                                       
                                                                       tags$em(bsButton("help1","Info", icon = NULL, style = "inverse",
                                                                                        size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                        value = FALSE)
                                                                       )
                            ),verbatimTextOutput("summaryMLE"),downloadButton('downloadData', 'Download Summary'),tags$hr(),plotOutput("plot3"),downloadButton('downloadPlot3', 'Download Plot'),tags$hr(),plotOutput("plot4"),downloadButton('downloadPlot4', 'Download Plot'), plotOutput("plot2"),downloadButton('downloadPlo2', 'Download Plot')
                            ,tags$hr()
                            
                            ), 
                            
                            
                            
                            
                            ### Boruta
                            tabPanel("Boruta - Random forest", tags$div(class="header", checked=NA,
                                                                        
                                                                        tags$em(bsButton("help4","Info", icon = NULL, style = "inverse",
                                                                                         size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                         value = FALSE)
                                                                        )
                            ),selectInput("selectB", label = h4("Select Entropy Level for Classification", bsTooltip("selectB", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                          choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3, "H4" = 4)),plotOutput("borutaplot"),downloadButton('downloadborutaplot', 'Download Plot')
                            ,tags$hr(),verbatimTextOutput("boruta"),downloadButton('borutaOutcome', 'Download Boruta Outcome')
                            ,tags$hr(),verbatimTextOutput("bStats"),downloadButton('borutaStats', 'Download Boruta Outcome'))
                            
                            
                           
                          )
                          
                        )
                      )
                    ))

server <- shinyServer(function(input, output, session) {
  start_time <- Sys.time()
  observeEvent(input$run,addClass(selector = "body", class = "sidebar-collapse"))

  
  volumes = getVolumes()
  
  
  folderInput1 = NULL
  folderInput1 <- reactive({
    shinyDirChoose(input, 'folder_G1', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$folder_G1))
  })
  folderInput2 = NULL
  folderInput2 <- reactive({
    shinyDirChoose(input, 'folder_G2', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$folder_G2))
  })
  
  
  output$directorypath <- renderUI({
    HTML(folderInput1())
  })
  output$directorypath2 = renderUI({
    HTML(folderInput2())
  })
  
  
  observeEvent(input$run,{
    
  
    withProgress(
      {
        
        
        alphabetFromText <- reactive({
          alphabet = input$fileAlphabet
          dataH1 = read_file(alphabet$datapath)
          
          alphabetFromText = unlist(str_split(dataH1, ","))
          
          return(alphabetFromText)
        })
        
        alphabetH1 = isolate(alphabetFromText())
        print(alphabetH1)
        for (i in 2:4){
          assign(paste("Alphabet.", i, sep = ""), permutations(n=length(isolate(alphabetFromText())),r=i,v=isolate(alphabetFromText()),repeats.allowed=T))
          
        }
        alphabetH2 = c()
        for(rowN in 1:nrow(Alphabet.2)){
          alphabetH2 = c(alphabetH2, paste(Alphabet.2[rowN,], collapse="\t"))
          
        }
        
        alphabetH3 = c()
        for(rowN in 1:nrow(Alphabet.3)){
          alphabetH3 = c(alphabetH3, paste(Alphabet.3[rowN,], collapse="\t"))
          
        }
        
        alphabetH4 = c()
        for(rowN in 1:nrow(Alphabet.4)){
          alphabetH4 = c(alphabetH4, paste(Alphabet.4[rowN,], collapse="\t"))
          
        }
        
        nH1 = length(alphabetH1)
        nH2 = length(alphabetH2)
        nH3 = length(alphabetH3)
        nH4 = length(alphabetH4)}
      ,
      message = 'Creating Alphabets...',
      detail = ''
    )
    
    
    files1 <- reactive({
      list.files(path = folderInput1(), pattern = "*.csv", full.names = T)
    })
    nFiles1 <-reactive({length(files1())})
    
    files2 <- reactive({
      list.files(path = folderInput2(), pattern = "*.csv", full.names = T)
    })
    nFiles2 <- reactive({ length(files2())})
    
    f1 = isolate(files1())
    print(f1)
    f2 = isolate(files2())
    print(f2)
    
    updateSelectInput(session, 'selectB', choices =c(1:input$selectH))
    
    if(input$selectH == 2){
      
      EntropyDataGroup1 = reactive({if(!is.null(f1)){
        lof1 =  isolate(files1())  
        nf1 = isolate(nFiles1())
        
        df = data.frame(matrix(0,ncol = 3, nrow = nf1))
        rownames(df) = lof1
        colnames(df) = c("H0", "H1", "H2")
        
        C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(C_2) = alphabetH2
        colnames(C_2) = lof1
        
        F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
        rownames(F_1) = alphabetH1
        colnames(F_1) = lof1
        
        F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(F_2) = alphabetH2
        colnames(F_2) = lof1
        
        
        combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2)
        return(combo)}
        else(NULL)
        
        
      })
      EntropyAnalysisGroup1 = reactive({ if(!is.null(EntropyDataGroup1())){
        data = EntropyDataGroup1()
        
        f1 = isolate(row.names(data$df))
        Entropy = isolate(data$df)
        Counts2 = isolate(data$C_2)
        F1 = isolate(data$F_1)
        F2 = isolate(data$F_2)
        ### Progress Message ###
        withProgress(message = 'Uploading Group 1...',value = 0, { 
          
          for(f in f1){
            incProgress(1/length(f1), detail = "")
            
            fileIN = readLines(f)
            
            EntropyHash = hash(keys = c('H1','H2'))
            EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            
            CountHash = hash(keys = c('H1','H2'))
            CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          
            
            ProbabilityHash = hash(keys = c('H1','H2'))
            ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            
            
            for (call in alphabetH1){
              values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            }
            for (call in alphabetH2){
              values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              Counts2[f][call,] = values(CountHash$H2, keys= call)
            }
            
            
            totalH1 = sum(values(CountHash$H1))
            
            y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
            totalH2 = c()
            for(g in y){
              totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
            }
            
            names(totalH2) = alphabetH1

            
            h11 = 0
            for (call in alphabetH1){
              values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
              F1[f][call,] = values(ProbabilityHash$H1, keys= call)
              #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
              values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
              if (!is.nan(values(EntropyHash$H1, keys= call))){
                h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
              else{
                values(EntropyHash$H1, keys= call) = 0
              }
            }
            h22 = 0
            for (call in alphabetH2){
              first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
              values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
              F2[f][call,] = values(ProbabilityHash$H2, keys= call)
              # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
              values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
              if (!is.nan(values(EntropyHash$H2, keys= call))){
                h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
              else{
                values(EntropyHash$H2, keys= call) = 0
              }
            }
            
            Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
            Entropy[f,"H1"] = h11
            Entropy[f,"H2"] = h22
            
          }    }
        )
        outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2)
        return(outputData)}
      })
      
      EntropyDataGroup2 = reactive({if(!is.null(f2)){
        #print("meuCacique 2")
        lof1 =  isolate(files2())  
        nf1 = isolate(nFiles2())
        
        df = data.frame(matrix(0,ncol = 3, nrow = nf1))
        rownames(df) = lof1
        colnames(df) = c("H0", "H1", "H2")
        
        C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(C_2) = alphabetH2
        colnames(C_2) = lof1
        
        F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
        rownames(F_1) = alphabetH1
        colnames(F_1) = lof1
        
        F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(F_2) = alphabetH2
        colnames(F_2) = lof1
        
        
        combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2)
        return(combo)}
        else(NULL)
        
        
      })
      EntropyAnalysisGroup2 = reactive({ if(!is.null(EntropyDataGroup2())){
        data = EntropyDataGroup2()
        
        f1 = isolate(row.names(data$df))
        Entropy = isolate(data$df)
        Counts2 = isolate(data$C_2)
        F1 = isolate(data$F_1)
        F2 = isolate(data$F_2)
        
        ### Progress Message ###
        withProgress(message = 'Uploading Group 2...', value = 0, { 
          
          for(f in f1){
            incProgress(1/length(f1), detail = "")
            
            fileIN = readLines(f)
            
            EntropyHash = hash(keys = c('H1','H2'))
            EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            
            CountHash = hash(keys = c('H1','H2'))
            CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            
            ProbabilityHash = hash(keys = c('H1','H2'))
            ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            
            
            for (call in alphabetH1){
              values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            }
            for (call in alphabetH2){
              values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              Counts2[f][call,] = values(CountHash$H2, keys= call)
            }
            
            totalH1 = sum(values(CountHash$H1))
            
            y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
            totalH2 = c()
            for(g in y){
              totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
            }
            
            names(totalH2) = alphabetH1
        
            h11 = 0
            for (call in alphabetH1){
              values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
              F1[f][call,] = values(ProbabilityHash$H1, keys= call)
              #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
              values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
              if (!is.nan(values(EntropyHash$H1, keys= call))){
                h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
              else{
                values(EntropyHash$H1, keys= call) = 0
              }
            }
            h22 = 0
            for (call in alphabetH2){
              first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
              values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
              F2[f][call,] = values(ProbabilityHash$H2, keys= call)
              # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
              values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
              if (!is.nan(values(EntropyHash$H2, keys= call))){
                h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
              else{
                values(EntropyHash$H2, keys= call) = 0
              }
            }

            
            Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
            Entropy[f,"H1"] = h11
            Entropy[f,"H2"] = h22
            
          }    }
        )
        outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2)
        return(outputData)}
      })
    }  
    
    
    
    if(input$selectH == 3){
      
      EntropyDataGroup1 = reactive({if(!is.null(f1)){
        print("entrouaqui")
        lof1 =  isolate(files1())  
        nf1 = isolate(nFiles1())
        
        df = data.frame(matrix(0,ncol = 4, nrow = nf1))
        rownames(df) = lof1
        colnames(df) = c("H0", "H1", "H2", "H3")
        
        C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(C_2) = alphabetH2
        colnames(C_2) = lof1
        
        F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
        rownames(F_1) = alphabetH1
        colnames(F_1) = lof1
        
        F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(F_2) = alphabetH2
        colnames(F_2) = lof1
        
        F_3 = data.frame(matrix(0,ncol = nf1, nrow = nH3))
        rownames(F_3) = alphabetH3
        colnames(F_3) = lof1
      
        
        combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3)
        return(combo)}
        else(NULL)
        
        
      })
      EntropyAnalysisGroup1 = reactive({ if(!is.null(EntropyDataGroup1())){
        data = EntropyDataGroup1()
        
        f1 = isolate(row.names(data$df))
        Entropy = isolate(data$df)
        Counts2 = isolate(data$C_2)
        F1 = isolate(data$F_1)
        F2 = isolate(data$F_2)
        F3 = isolate(data$F_3)
        ### Progress Message ###
        withProgress(message = 'Uploading Group 1...',value = 0, { 
          
          for(f in f1){
            incProgress(1/length(f1), detail = "")
            
            fileIN = readLines(f)
            
            EntropyHash = hash(keys = c('H1','H2','H3'))
            EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
            
            CountHash = hash(keys = c('H1','H2','H3'))
            CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
            
            ProbabilityHash = hash(keys = c('H1','H2','H3'))
            ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
            
            
            for (call in alphabetH1){
              values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            }
            for (call in alphabetH2){
              values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              Counts2[f][call,] = values(CountHash$H2, keys= call)
            }
            for (call in alphabetH3){
              values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              
            }
            
            
            totalH1 = sum(values(CountHash$H1))
            
            y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
            totalH2 = c()
            for(g in y){
              totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
            }
            
            names(totalH2) = alphabetH1
            
            z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
            totalH3 = c()
            for(g in z){
              totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
            }
            names(totalH3) = alphabetH2

            h11 = 0
            for (call in alphabetH1){
              values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
              F1[f][call,] = values(ProbabilityHash$H1, keys= call)
              #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
              values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
              if (!is.nan(values(EntropyHash$H1, keys= call))){
                h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
              else{
                values(EntropyHash$H1, keys= call) = 0
              }
            }
            h22 = 0
            for (call in alphabetH2){
              first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
              values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
              F2[f][call,] = values(ProbabilityHash$H2, keys= call)
              # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
              values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
              if (!is.nan(values(EntropyHash$H2, keys= call))){
                h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
              else{
                values(EntropyHash$H2, keys= call) = 0
              }
            }
            h33 = 0
            for (call in alphabetH3){
              firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
              first = firstTwo[1]
              firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
              values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
              F3[f][call,] = values(ProbabilityHash$H3, keys= call)
              # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
              values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
              if (!is.nan(values(EntropyHash$H3, keys= call))){
                h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
              else{
                values(EntropyHash$H3, keys= call) = 0
              }
            }
            
            
            Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
            Entropy[f,"H1"] = h11
            Entropy[f,"H2"] = h22
            Entropy[f,"H3"] = h33
            
          }    }
        )
        outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3)
        return(outputData)}
      })
      
      EntropyDataGroup2 = reactive({if(!is.null(f2)){
        #print("meuCacique 2")
        lof1 =  isolate(files2())  
        nf1 = isolate(nFiles2())
        
        df = data.frame(matrix(0,ncol = 4, nrow = nf1))
        rownames(df) = lof1
        colnames(df) = c("H0", "H1", "H2", "H3")
        
        C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(C_2) = alphabetH2
        colnames(C_2) = lof1
        
        F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
        rownames(F_1) = alphabetH1
        colnames(F_1) = lof1
        
        F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
        rownames(F_2) = alphabetH2
        colnames(F_2) = lof1
        
        F_3 = data.frame(matrix(0,ncol = nf1, nrow = nH3))
        rownames(F_3) = alphabetH3
        colnames(F_3) = lof1

        
        combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3)
        return(combo)}
        else(NULL)
        
        
      })
      EntropyAnalysisGroup2 = reactive({ if(!is.null(EntropyDataGroup2())){
        data = EntropyDataGroup2()
        
        f1 = isolate(row.names(data$df))
        Entropy = isolate(data$df)
        Counts2 = isolate(data$C_2)
        F1 = isolate(data$F_1)
        F2 = isolate(data$F_2)
        F3 = isolate(data$F_3)
        
        ### Progress Message ###
        withProgress(message = 'Uploading Group 2...', value = 0, { 
          
          for(f in f1){
            incProgress(1/length(f1), detail = "")
            
            fileIN = readLines(f)
            
            EntropyHash = hash(keys = c('H1','H2','H3'))
            EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
            
            CountHash = hash(keys = c('H1','H2','H3'))
            CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
            
            ProbabilityHash = hash(keys = c('H1','H2','H3'))
            ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
            ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
            ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
            
            
            for (call in alphabetH1){
              values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            }
            for (call in alphabetH2){
              values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              Counts2[f][call,] = values(CountHash$H2, keys= call)
            }
            for (call in alphabetH3){
              values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
              
            }

            
            totalH1 = sum(values(CountHash$H1))
            
            y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
            totalH2 = c()
            for(g in y){
              totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
            }
            
            names(totalH2) = alphabetH1
            
            z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
            totalH3 = c()
            for(g in z){
              totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
            }
            names(totalH3) = alphabetH2

            h11 = 0
            for (call in alphabetH1){
              values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
              F1[f][call,] = values(ProbabilityHash$H1, keys= call)
              #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
              values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
              if (!is.nan(values(EntropyHash$H1, keys= call))){
                h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
              else{
                values(EntropyHash$H1, keys= call) = 0
              }
            }
            h22 = 0
            for (call in alphabetH2){
              first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
              values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
              F2[f][call,] = values(ProbabilityHash$H2, keys= call)
              # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
              values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
              if (!is.nan(values(EntropyHash$H2, keys= call))){
                h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
              else{
                values(EntropyHash$H2, keys= call) = 0
              }
            }
            h33 = 0
            for (call in alphabetH3){
              firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
              first = firstTwo[1]
              firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
              values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
              F3[f][call,] = values(ProbabilityHash$H3, keys= call)
              # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
              values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
              if (!is.nan(values(EntropyHash$H3, keys= call))){
                h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
              else{
                values(EntropyHash$H3, keys= call) = 0
              }
            }
            
            
            Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
            Entropy[f,"H1"] = h11
            Entropy[f,"H2"] = h22
            Entropy[f,"H3"] = h33
            
          }    }
        )
        outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3)
        return(outputData)}
      })
    }
    
    
    if(input$selectH == 4){
    
    EntropyDataGroup1 = reactive({if(!is.null(f1)){
      print("entrouaqui")
      lof1 =  isolate(files1())  
      nf1 = isolate(nFiles1())
      
      df = data.frame(matrix(0,ncol = 5, nrow = nf1))
      rownames(df) = lof1
      colnames(df) = c("H0", "H1", "H2", "H3","H4")
      
      C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
      rownames(C_2) = alphabetH2
      colnames(C_2) = lof1
      
      F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
      rownames(F_1) = alphabetH1
      colnames(F_1) = lof1
      
      F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
      rownames(F_2) = alphabetH2
      colnames(F_2) = lof1
      
      F_3 = data.frame(matrix(0,ncol = nf1, nrow = nH3))
      rownames(F_3) = alphabetH3
      colnames(F_3) = lof1
      
      F_4 = data.frame(matrix(0,ncol = nf1, nrow = nH4))
      rownames(F_4) = alphabetH4
      colnames(F_4) = lof1
      
      combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3, F_4 = F_4)
      return(combo)}
      else(NULL)
      
      
    })
    EntropyAnalysisGroup1 = reactive({ if(!is.null(EntropyDataGroup1())){
      data = EntropyDataGroup1()
      
      f1 = isolate(row.names(data$df))
      Entropy = isolate(data$df)
      Counts2 = isolate(data$C_2)
      F1 = isolate(data$F_1)
      F2 = isolate(data$F_2)
      F3 = isolate(data$F_3)
      F4 = isolate(data$F_4)
      
      ### Progress Message ###
      withProgress(message = 'Uploading Group 1...',value = 0, { 
        
        for(f in f1){
          incProgress(1/length(f1), detail = "")
          
          fileIN = readLines(f)
          
          EntropyHash = hash(keys = c('H1','H2','H3', 'H4'))
          EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
          EntropyHash$H4 = hash(keys = alphabetH4, values = rep(0,nH4) )
          
          CountHash = hash(keys = c('H1','H2','H3', 'H4'))
          CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
          CountHash$H4 = hash(keys = alphabetH4,values = rep(0,nH4) )
          
          ProbabilityHash = hash(keys = c('H1','H2','H3','H4'))
          ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
          ProbabilityHash$H4 = hash(keys = alphabetH4, values = rep(0,nH4) )
          
          
          for (call in alphabetH1){
            values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          }
          for (call in alphabetH2){
            values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            Counts2[f][call,] = values(CountHash$H2, keys= call)
          }
          for (call in alphabetH3){
            values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            
          }
          for (call in alphabetH4){
            values(CountHash$H4, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            
          }
          
          
          
          totalH1 = sum(values(CountHash$H1))
          
          y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
          totalH2 = c()
          for(g in y){
            totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
          }
          
          names(totalH2) = alphabetH1
          
          z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
          totalH3 = c()
          for(g in z){
            totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
          }
          names(totalH3) = alphabetH2
          
          z = split(seq(1:nH4), ceiling(seq_along(seq(1:nH4))/10))
          totalH4 = c()
          for(g in z){
            totalH4 = c(totalH4, sum(values(CountHash$H4,keys = alphabetH4[g])))
          }
          names(totalH4) = alphabetH3
          
          
          h11 = 0
          for (call in alphabetH1){
            values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
            F1[f][call,] = values(ProbabilityHash$H1, keys= call)
            #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
            values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
            if (!is.nan(values(EntropyHash$H1, keys= call))){
              h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
            else{
              values(EntropyHash$H1, keys= call) = 0
            }
          }
          h22 = 0
          for (call in alphabetH2){
            first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
            values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
            F2[f][call,] = values(ProbabilityHash$H2, keys= call)
            # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
            values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
            if (!is.nan(values(EntropyHash$H2, keys= call))){
              h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
            else{
              values(EntropyHash$H2, keys= call) = 0
            }
          }
          h33 = 0
          for (call in alphabetH3){
            firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
            first = firstTwo[1]
            firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
            values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
            F3[f][call,] = values(ProbabilityHash$H3, keys= call)
            # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
            values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
            if (!is.nan(values(EntropyHash$H3, keys= call))){
              h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
            else{
              values(EntropyHash$H3, keys= call) = 0
            }
          }
          
          h44 = 0
          for (call in alphabetH4){
            separate = unlist(strsplit(call,'\t', fixed=FALSE))
            first = separate[1]
            firstTwo = paste(separate[1],'\t',separate[2],sep='')
            three =  paste(separate[1],'\t',separate[2],'\t',separate[3],sep='')
            values(ProbabilityHash$H4, keys= call) = values(CountHash$H4, keys= call)/totalH4[three]
            F4[f][call,] = values(ProbabilityHash$H4, keys= call)
            # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
            values(EntropyHash$H4, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= three)*values(ProbabilityHash$H4, keys= call)*log2(values(ProbabilityHash$H4, keys= call))
            if (!is.nan(values(EntropyHash$H4, keys= call))){
              h44 = h44 + (as.double(values(EntropyHash$H4, keys= call)))}
            else{
              values(EntropyHash$H4, keys= call) = 0
            }
          }
          
          
          Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
          Entropy[f,"H1"] = h11
          Entropy[f,"H2"] = h22
          Entropy[f,"H3"] = h33
          Entropy[f,"H4"] = h44
          
        }    }
      )
      outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3, F4=F4)
      return(outputData)}
    })

    EntropyDataGroup2 = reactive({if(!is.null(f2)){
      #print("meuCacique 2")
      lof1 =  isolate(files2())  
      nf1 = isolate(nFiles2())
      
      df = data.frame(matrix(0,ncol = 5, nrow = nf1))
      rownames(df) = lof1
      colnames(df) = c("H0", "H1", "H2", "H3","H4")
      
      C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
      rownames(C_2) = alphabetH2
      colnames(C_2) = lof1
      
      F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
      rownames(F_1) = alphabetH1
      colnames(F_1) = lof1
      
      F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
      rownames(F_2) = alphabetH2
      colnames(F_2) = lof1
      
      F_3 = data.frame(matrix(0,ncol = nf1, nrow = nH3))
      rownames(F_3) = alphabetH3
      colnames(F_3) = lof1
      
      F_4 = data.frame(matrix(0,ncol = nf1, nrow = nH4))
      rownames(F_4) = alphabetH4
      colnames(F_4) = lof1
      
      combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3, F_4 = F_4)
      return(combo)}
      else(NULL)
      
      
    })
    EntropyAnalysisGroup2 = reactive({ if(!is.null(EntropyDataGroup2())){
      data = EntropyDataGroup2()
      
      f1 = isolate(row.names(data$df))
      Entropy = isolate(data$df)
      Counts2 = isolate(data$C_2)
      F1 = isolate(data$F_1)
      F2 = isolate(data$F_2)
      F3 = isolate(data$F_3)
      F4 = isolate(data$F_4)
      
      ### Progress Message ###
      withProgress(message = 'Uploading Group 2...', value = 0, { 
        
        for(f in f1){
          incProgress(1/length(f1), detail = "")
          
          fileIN = readLines(f)
          
          EntropyHash = hash(keys = c('H1','H2','H3', 'H4'))
          EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
          EntropyHash$H4 = hash(keys = alphabetH4, values = rep(0,nH4) )
          
          CountHash = hash(keys = c('H1','H2','H3', 'H4'))
          CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
          CountHash$H4 = hash(keys = alphabetH4,values = rep(0,nH4) )
          
          ProbabilityHash = hash(keys = c('H1','H2','H3','H4'))
          ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
          ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
          ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
          ProbabilityHash$H4 = hash(keys = alphabetH4, values = rep(0,nH4) )
          
          
          for (call in alphabetH1){
            values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          }
          for (call in alphabetH2){
            values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            Counts2[f][call,] = values(CountHash$H2, keys= call)
          }
          for (call in alphabetH3){
            values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            
          }
          for (call in alphabetH4){
            values(CountHash$H4, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
            
          }
          
          
          
          totalH1 = sum(values(CountHash$H1))
          
          y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
          totalH2 = c()
          for(g in y){
            totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
          }
          
          names(totalH2) = alphabetH1
          
          z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
          totalH3 = c()
          for(g in z){
            totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
          }
          names(totalH3) = alphabetH2
          
          z = split(seq(1:nH4), ceiling(seq_along(seq(1:nH4))/10))
          totalH4 = c()
          for(g in z){
            totalH4 = c(totalH4, sum(values(CountHash$H4,keys = alphabetH4[g])))
          }
          names(totalH4) = alphabetH3
          
          
          h11 = 0
          for (call in alphabetH1){
            values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
            F1[f][call,] = values(ProbabilityHash$H1, keys= call)
            #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
            values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
            if (!is.nan(values(EntropyHash$H1, keys= call))){
              h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
            else{
              values(EntropyHash$H1, keys= call) = 0
            }
          }
          h22 = 0
          for (call in alphabetH2){
            first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
            values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
            F2[f][call,] = values(ProbabilityHash$H2, keys= call)
            # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
            values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
            if (!is.nan(values(EntropyHash$H2, keys= call))){
              h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
            else{
              values(EntropyHash$H2, keys= call) = 0
            }
          }
          h33 = 0
          for (call in alphabetH3){
            firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
            first = firstTwo[1]
            firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
            values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
            F3[f][call,] = values(ProbabilityHash$H3, keys= call)
            # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
            values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
            if (!is.nan(values(EntropyHash$H3, keys= call))){
              h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
            else{
              values(EntropyHash$H3, keys= call) = 0
            }
          }
          
          h44 = 0
          for (call in alphabetH4){
            separate = unlist(strsplit(call,'\t', fixed=FALSE))
            first = separate[1]
            firstTwo = paste(separate[1],'\t',separate[2],sep='')
            three =  paste(separate[1],'\t',separate[2],'\t',separate[3],sep='')
            values(ProbabilityHash$H4, keys= call) = values(CountHash$H4, keys= call)/totalH4[three]
            F4[f][call,] = values(ProbabilityHash$H4, keys= call)
            # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
            values(EntropyHash$H4, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= three)*values(ProbabilityHash$H4, keys= call)*log2(values(ProbabilityHash$H4, keys= call))
            if (!is.nan(values(EntropyHash$H4, keys= call))){
              h44 = h44 + (as.double(values(EntropyHash$H4, keys= call)))}
            else{
              values(EntropyHash$H4, keys= call) = 0
            }
          }
          
          
          Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
          Entropy[f,"H1"] = h11
          Entropy[f,"H2"] = h22
          Entropy[f,"H3"] = h33
          Entropy[f,"H4"] = h44
          
        }    }
      )
      outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3, F4=F4)
      return(outputData)}
    })
    }
    
    
    end_time <- Sys.time()
    
    Group1Data = EntropyAnalysisGroup1()
    Group2Data = EntropyAnalysisGroup2()
    
    print(end_time - start_time)
    #####$ Analysis: 
    
    plot1 = reactive({
      if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1())){
        Group2_Data = EntropyAnalysisGroup2()
        Group1_Data = EntropyAnalysisGroup1()
        EntropyGroup2LM = Group2_Data$Entropy
        EntropyGroup1LM = Group1_Data$Entropy
        
        Group2 = colMeans(EntropyGroup2LM)
        Group1 = colMeans(EntropyGroup1LM)
        
        stats = c()
        for (level in colnames(EntropyGroup2LM)){
          print(level)
          
          #print(mean(EntropyGroup2LM[[level]]))
          print(as.numeric(sd(EntropyGroup2LM[[level]])))
          #print(as.numeric(quantile(EntropyGroup2LM[[level]],0.75)))
          stats = c(stats,as.numeric(sd(EntropyGroup2LM[[level]])))
        }
        G2Quantiles = t(matrix(stats, ncol=length(colnames(EntropyGroup2LM)), byrow=TRUE))
        G2Quantiles = as.data.frame(G2Quantiles, stringsAsFactors=FALSE)
        colnames(G2Quantiles) = c("SD")
        row.names(G2Quantiles) = colnames(EntropyGroup2LM)
        
        stats = c()
        for (level in colnames(EntropyGroup1LM)){
          print(level)
          
          #print(mean(EntropyGroup2LM[[level]]))
          print(as.numeric(sd(EntropyGroup1LM[[level]])))
          #print(as.numeric(quantile(EntropyGroup2LM[[level]],0.75)))
          stats = c(stats,as.numeric(sd(EntropyGroup1LM[[level]])))
        }
        G1Quantiles = t(matrix(stats, ncol=length(colnames(EntropyGroup1LM)), byrow=TRUE))
        G1Quantiles = as.data.frame(G1Quantiles, stringsAsFactors=FALSE)
        colnames(G1Quantiles) = c("SD")
        row.names(G1Quantiles) = colnames(EntropyGroup1LM)
        
        
        dataEntropy = data.frame(
          Group = factor(c(rep("Group2",length(colnames(EntropyGroup2LM))),c(rep("Group1",length(colnames(EntropyGroup2LM)))))),
          Level = factor(c(rep(colnames(EntropyGroup2LM),2)), levels=c(colnames(EntropyGroup2LM))),
          Entropy = c(Group2,Group1),
          SD = c(G2Quantiles$SD,G1Quantiles$SD)
          # q2 = c(G2Quantiles$Q2,G1Quantiles$Q2)
        )
        
        
        # dataEntropy = data.frame(
        #   Group = factor(c(rep("Group2",5),c(rep("Group1",5)))),
        #   Level = factor(c("H0","H1","H2","H3","H4","H0","H1","H2","H3","H4"), levels=c("H0","H1","H2","H3","H4")),
        #   Entropy = c(Group2,Group1))
        
        pd <- position_dodge(0.05) 
        # 
        return({ggplot(data=dataEntropy, aes(x=Level, y=Entropy, group=Group, colour= Group)) +
            geom_line(aes(linetype=Group)) +
            geom_errorbar(aes(ymin=Entropy-SD, ymax=Entropy+SD), width=.1,position=pd) +
            scale_color_manual(values=c('gray0','gray44'))+
            geom_point(position=pd, size=3)
          
          # ggplot(data, aes(x=factor(X), y=Y, colour = factor(dep_C1)))  +
          #   geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5)  +
          #   stat_summary(fun.y=median, geom="line", aes(group=factor(dep_C1)), size=2) 
          
          # return({ggplot(dataEntropy, aes(x=Level, y=Entropy,  group=Group, colour=Group)) + 
          #     geom_boxplot(outlier.size=0, fill = "white", position="identity", alpha=.5)  +
          #     stat_summary(fun.y=median, geom="line", aes(group=Group), size=2) 
          
        })
      }
      
    })
    output$plot1 = renderPlot({ 
      print(plot1())
    })
    
    output$downloadPlot1 <- downloadHandler(
      filename = function() { "EntropyAnalysis.png" },
      content = function(file) {
        ggsave(file, plot = plot1(), device = "png")
      })
    
    createMLEData = reactive({
      if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1())){
        Group2_Data = EntropyAnalysisGroup2()
        Group1_Data = EntropyAnalysisGroup1()
        EntropyGroup2LM = Group2_Data$Entropy
        EntropyGroup1LM = Group1_Data$Entropy
        
        Group2 = colMeans(EntropyGroup2LM)
        Group1 = colMeans(EntropyGroup1LM)
        
        
        EntropyData = rbind(EntropyGroup1LM, EntropyGroup2LM)
        Genotype = c(rep("Group1",length(row.names(EntropyGroup1LM))),rep("Group2",length(row.names(EntropyGroup2LM))))
        EntropyData = cbind(EntropyData, Genotype)
        Mouse = c()
        for (m in rownames(EntropyData)){
          #print(m)
          #m = substr(m,9,12)
          Mouse = c(Mouse,m)
          
        }
        MLEData = data.frame(matrix(vector(), 0, 4,
                                    dimnames=list(c(), c("Mouse", "Entropy", "Level","Genotype" ))),
                             stringsAsFactors=T)
        for (n in rownames(EntropyData)){
          # m2 = substr(n,9,12)
          
          e = c()
          for(level in colnames(EntropyGroup2LM)){
            e = c(e, EntropyData[n,level])
          }
          mouseData = data.frame(Mouse = c(rep(n,length(colnames(EntropyGroup2LM)))),
                                 Entropy = e,
                                 Level = factor(colnames(EntropyGroup2LM)),
                                 Genotype = {
                                   if(EntropyData[n,"Genotype"] == "Group1"){Genotype = c(rep("Group1",length(colnames(EntropyGroup2LM))))}
                                   else{Genotype = c(rep("Group2",length(colnames(EntropyGroup2LM))))}}
          )
          
          MLEData = rbind.data.frame(MLEData,mouseData)
        }
        # 
        # for (n in rownames(EntropyData)){
        #   #m2 = substr(n,9,12)
        #   mouseData = data.frame(Mouse = c(rep(n,5)),
        #                          Entropy = c(EntropyData[n,"H0"],EntropyData[n,"H1"],
        #                                      EntropyData[n,"H2"],EntropyData[n,"H3"],EntropyData[n,"H4"]),
        #                          Level = factor(c("H0","H1","H2","H3","H4")),
        #                          Genotype = c(rep(EntropyData[n,"Genotype"],5)))
        #   
        #   MLEData = rbind.data.frame(MLEData,mouseData)
        #   
        # }
        return(MLEData)
      } 
      else(return(NULL))
      
      
    })
    
    lmerAnalysis = reactive({
      if(!is.null(createMLEData())){
        MLEData = isolate(createMLEData())
        # options(lmerControl=list(check.nobs.vs.rankZ = "warning", check.nobs.vs.nlev = "warning",
        #                         check.nobs.vs.nRE = "warning", check.nlev.gtreq.5 = "warning", check.nlev.gtr.1 = "warning"))
        mod1 = lmer(Entropy ~ Genotype*Level +  (1|Mouse),MLEData)
        summary(mod1)
        return(mod1)
        
      }
      else(return(NULL))
      
    })
    
    plot2 = reactive({    
      if(!is.null(createMLEData())){
        outputOptions(output,"plot2",suspendWhenHidden=FALSE)
        MLEData = isolate(createMLEData())
        return({boxplot(Entropy ~ Genotype*Level,
                        col=c("white","lightgray"),
                        las = 2,ylab ="Entropy", 
                        xlab ="Group and Level",cex.lab=1.3, cex.axis=0.6, cex.main=1.5,MLEData)
          #means <- tapply(MLEData$Genotype,MLEData$Level,mean)
          #points(means,col="red",pch=18)
          # abline(means)
        })
      }
      
      else(return(NULL))})
    
    output$plot2 = renderPlot({
      if(!is.null(createMLEData())){
        
        print(plot2())}
    })
    
    output$downloadPlot2 <- downloadHandler(
      filename = function() { "EntropyAnalysis.png" },
      content = function(file) {
        ggsave(file, plot = isolate(plot2()), device = "png")
      })
    plot3 = reactive({if(!is.null(lmerAnalysis())){
      outputOptions(output,"plot3",suspendWhenHidden=FALSE)
      mle = isolate(lmerAnalysis())
      return({plot(mle)
      })}})
    output$plot3 = renderPlot({
      if(!is.null(createMLEData())){
        p = isolate(plot3())
        print(p)}
    })
    
    
    output$downloadPlot3 <- downloadHandler(
      filename = function() { "EntropyAnalysis.png" },
      content = function(file) {
        ggsave(file, plot = plot3(), device = "png")
      })
    
    plot4 = reactive({if(!is.null(lmerAnalysis())){
      outputOptions(output,"plot4",suspendWhenHidden=FALSE)
      mle = isolate(lmerAnalysis())
      return({qqnorm(resid(mle))
        qqline(resid(mle))
      })}})
    output$plot4 = renderPlot({ 
      print(plot4())
      
    })
    
    output$downloadPlot4 <- downloadHandler(
      filename = function() { "LMER_Plot2.png" },
      content = function(file) {
        ggsave(file, plot = plot3(), device = "png")
      })
    
    
    summaryMLE = reactive({if(!is.null(lmerAnalysis())){
      mle = isolate(lmerAnalysis())
      return({summary(mle)})
    }})
    
    output$summaryMLE = renderPrint({ 
      outputOptions(output,"summaryMLE",suspendWhenHidden=FALSE)
      if(!is.null(lmerAnalysis())){
        # mle = isolate(lmerAnalysis())
        return({summaryMLE()})}
    })
    
    # output$downloadData <- downloadHandler(
    #   filename = function(){
    #     paste("LMER_Summary-", Sys.Date(), ".txt", sep = "")
    #   },
    #   content = function(file) {
    #     writeLines(paste(summaryMLE(), sep = "\n"))
    #   })
    # 
    
    output$downloadData = downloadHandler(
      filename = 'test.pdf',
      content = function(file) {
        print(summaryMLE())
        dev.copy2pdf(file = file, width=12, height=8, out.type="pdf")
      })
    
    ## Markov Graphf
    
    plot5 = reactive({if(!is.null(EntropyAnalysisGroup1())){
      Group1_Data = EntropyAnalysisGroup1()
      countGroup1 = rowMeans(Group1_Data$Counts2)
      observations = c()
      for (n in alphabetH2){
        aux = c()
        call = unlist(strsplit(n,'\t', fixed=FALSE))
        aux = rep(call,countGroup1[n])
        observations = c(observations,aux)
      }
      names(countGroup1) = alphabetH2
      markovModelH2 = markovchainFit(data=observations)
      tpmH2 = as.matrix(markovModelH2$estimate@transitionMatrix)
      
      g <- graph.adjacency(tpmH2, weighted=TRUE)
      # complicatedCalls = intersect(unique(observations),c("Cx", "Ts", "Fs", "Ha", "C"))
      # otherCalls = setdiff(unique(observations),complicatedCalls)
      callOrder = unique(observations)
      V(g)$color =  "gray40"
      # for (calls in complicatedCalls){
      #   V(g)[calls]$color = "firebrick3"
      # }
      
      E(g)$weight = edge.betweenness(g)
      deg <- degree(g, mode="all")
      V(g)$size <- deg*2
      E(g)$arrow.size <- .1
      E(g)$edge.color <- "gray80"
      E(g)$width <- edge.betweenness(g)*.06
      #E(g)$width <- E(g)$weight*.06
      V(g)$label.cex = .7
      return(plot(g, main = "Transition Graph for Group 1", layout=layout_in_circle(g, order = callOrder), vertex.label.color= "white",
                  vertex.label.family = "Helvetica", edge.label.font = 2))
    }
      else(stop("Upload folder") )
    })
    output$plot5 = renderPlot({ 
      if(!is.null(EntropyAnalysisGroup1())){
        print(plot5())}
    })
    output$downloadPlot5 <- downloadHandler(
      filename = function() { "TransitionGraphforGroup1Group.png" },
      content = function(file) {
        ggsave(file, plot = plot5(), device = "png")
      })
    
    plot6= reactive({ 
      if(!is.null(EntropyAnalysisGroup2())){
        Group2_Data = EntropyAnalysisGroup2()
        countGroup2 = rowMeans(Group2_Data$Counts2)
        observations = c()
        for (n in alphabetH2){
          aux = c()
          call = unlist(strsplit(n,'\t', fixed=FALSE))
          aux = rep(call,countGroup2[n])
          observations = c(observations,aux)
        }
        names(countGroup2) = alphabetH2
        markovModelH2 = markovchainFit(data=observations)
        tpmH2 = as.matrix(markovModelH2$estimate@transitionMatrix)
        
        g <- graph.adjacency(tpmH2, weighted=TRUE)
        # complicatedCalls = intersect(unique(observations),c("Cx", "Ts", "Fs", "Ha", "C"))
        # otherCalls = setdiff(unique(observations),complicatedCalls)
        callOrder = unique(observations)
        V(g)$color =  "gray51"
        # for (calls in complicatedCalls){
        #   V(g)[calls]$color = "firebrick3"
        # }
        
        E(g)$weight = edge.betweenness(g)
        deg <- degree(g, mode="all")
        V(g)$size <- deg*2
        E(g)$arrow.size <- .1
        E(g)$edge.color <- "gray80"
        E(g)$width <- edge.betweenness(g)*.06
        #E(g)$width <- E(g)$weight*.06
        V(g)$label.cex = .7
        return(plot(g, main = "Transition Graph for Group 2 ", layout=layout_in_circle(g, order = callOrder), vertex.label.color= "white",
                    vertex.label.family = "Helvetica", edge.label.font = 2))
      }
      else(stop("Upload folder") )
      
    })
    output$plot6 = renderPlot({ 
      if(!is.null(EntropyAnalysisGroup2())){
        print(plot6())}
    })
    output$downloadPlot6 <- downloadHandler(
      filename = function() { "TransitionGraphforGroup2Group.png" },
      content = function(file) {
        ggsave(file, plot = plot5(), device = "png")
      })
    
    
    boruta = reactive({
      if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1()) & input$selectB != 0) {    
        Group2_Data = EntropyAnalysisGroup2()
        Group1_Data = EntropyAnalysisGroup1()
        
        if (input$selectB == 1){
          groupDataGroup1 = t(Group1_Data$F1)
          groupDataGroup1 = cbind(groupDataGroup1,rep("Group1",ncol(Group1_Data$F1)))
          groupDataGroup2 = t(Group2_Data$F1)
          groupDataGroup2 = cbind(groupDataGroup2,rep("Group2",ncol(Group2_Data$F1)))
          
        }
        if (input$selectB == 2){
          groupDataGroup1 = t(Group1_Data$F2)
          groupDataGroup1 = cbind(groupDataGroup1,rep("Group1",ncol(Group1_Data$F2)))
          groupDataGroup2 = t(Group2_Data$F2)
          groupDataGroup2 = cbind(groupDataGroup2,rep("Group2",ncol(Group2_Data$F2)))
          
        }
        
        if (input$selectB == 3){
          groupDataGroup1 = t(Group1_Data$F3)
          groupDataGroup1 = cbind(groupDataGroup1,rep("Group1",ncol(Group1_Data$F3)))
          groupDataGroup2 = t(Group2_Data$F3)
          groupDataGroup2 = cbind(groupDataGroup2,rep("Group2",ncol(Group2_Data$F3)))
          
        }
        
        if (input$selectB == 4 & input$selectH == 4){
          groupDataGroup1 = t(Group1_Data$F4)
          groupDataGroup1 = cbind(groupDataGroup1,rep("Group1",ncol(Group1_Data$F4)))
          groupDataGroup2 = t(Group2_Data$F4)
          groupDataGroup2 = cbind(groupDataGroup2,rep("Group2",ncol(Group2_Data$F4)))
          
        }else(return)
        
        
        set.seed(7777)
        
        #print(groupDataHT)
        
        borutaDF = rbind.data.frame(groupDataGroup2,groupDataGroup1)
        colnames(borutaDF)[ncol(borutaDF)] = "Group"
        #print(borutaTest)
        
        b = Boruta(Group~.,data=borutaDF,pValue = 0.001)
        
        return(b)}
      else(return(NULL))
      
    })
    
    
    borutaplot = reactive({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        return(
          plot(calls.boruta, colCode = c("darkseagreen4", "goldenrod1", "firebrick", "dodgerblue3")))
        plotImpHistory(b, xlab = "Classifier run",
                       ylab = "Importance")
      }
      else(stop("Upload folder") )
      
    })
    
    
    output$borutaplot = renderPlot({ 
      if(!is.null(boruta())){
        print(borutaplot())}
    })
    output$downloadborutaplot <- downloadHandler(
      filename = function() { "borutaPlot.png" },
      content = function(file) {
        #ggsave(file, plot = borutaplot(), device = "png")
        ggsave(plot = plot(borutaplot()), "borutaPlot.png" , device = "png")
      })
    
    borutaOutcome = reactive({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        return(calls.boruta)
      }
      else(stop("Upload folder") )
      
    })
    
    output$boruta = renderPrint({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        print(calls.boruta$call)
        print(borutaOutcome())}
    })
    
    output$borutaOutcome <- downloadHandler(
      filename = function(){
        paste("Boruta-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write.table(paste(print(borutaOutcome()),collapse=", "), file,col.names=FALSE)
      })
    
    
    
    borutaStats = reactive({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        stats = attStats(calls.boruta)
        stats = subset(stats, decision == "Confirmed")
        return(stats)
      }
      else(stop("Upload folder") )
      
    })
    
    output$bStats = renderPrint({ 
      if(!is.null(boruta())){
        
        print(borutaStats())}
    })
    
    output$borutaStats <- downloadHandler(
      filename = function(){
        paste("Boruta-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write.table(paste(print(borutaStats()),collapse=", "), file,col.names=FALSE)
      })
    
    
    
    
    ### PopOvers:
    
    addPopover(session=session, id="help1", title="", 
               content="Mixed-effects linear model where genotype is a fixed effect and entropy level is a random effect. Because is expected a different baseline entropy for each mouse and for the change in entropy between each entropy level to vary between mice, a random intercept, random slope model was applied.", placement = "bottom",
               trigger = "click", options = NULL)
    addPopover(session=session, id="help2", title="", 
               content="Markov Model graph for the transitions between two calls. Thickness of edges and size of nodes represent the relative proportion of a transition and call numbers, respectively. Complex calls are represented by red nodes and simple calls are represented by blue nodes.", placement = "bottom",
               trigger = "click", options = NULL)
    
    addPopover(session=session, id="help3", title="", 
               content="Sparse Partial Least Squares Determination Analysis is used to perform variable selection and classification in a one step procedure.", placement = "bottom",
               trigger = "click", options = NULL)
    
    addPopover(session=session, id="help4", title="", 
               content="Feature selection algorithm using Random Forest classification. It iteratively removes features proved to be less relevant than random probes", placement = "bottom",
               trigger = "click", options = NULL)
    
    
    
    
    
    
    
  })
  
})

runApp(list(
  ui=ui,
  server=server
))
