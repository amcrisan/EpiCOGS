#load the clinical data from the file input
clinicalData<-reactiveValues()

clinicalData<<-eventReactive(input$file1,{
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)
  
  if(input$inType=="New"){
   dat<-read.csv(inFile$datapath)
  }else if(input$inType =="Formatted"){
    dat<-readRDS(file=inFile$datapath)
  }

  dat
  
})


#create a variable the stores the column names
# automatically generate the side bar options based upon the input data
output$inputChoices <- renderUI({
  
    if(input$inType == "New"){
      colvals<-colnames(clinicalData())
      
      lapply(colvals, function(i) {
        selectInput(sprintf("InputChoice_%s",i),label=i,choices=as.character(supportedInfo$varLabel),selected="Ignore",multiple = FALSE)
      })
    }else{
      h1("Check Mapping Panel")
      colvals<-colnames(clinicalData())
      
      
      lapply(colvals, function(i) {
        idxVal<-which(supportedInfo$varName==i)
        
        if(length(idxVal)==0){
          idxVal<-which(supportedInfo$varName=="Ignore")
        }
   
        fluidRow(
          column(width=6,
                    selectInput(sprintf("InputChoice_%s",i),
                    label=sprintf("%s data type",i),
                    choices=as.character(supportedInfo$varLabel),
                    selected=supportedInfo[idxVal,]$varLabel,
                    multiple = FALSE))#,
                 # column(width=6,
                 # selectInput(sprintf("InputChoice_stdType%s",i),
                 #             label=sprintf("%s standard type",i),
                 #             choices=as.character(supportedInfo$stdVarType),
                 #             selected=supportedInfo[idxVal,]$stdVarType,
                 #             multiple = FALSE))
        )
      })
      
    }

})


mapDat<-eventReactive(input$done, {
  
    colvals<-colnames(clinicalData())
    
    dat<-sapply(colvals, function(i) {
      idxVal<-which(supportedInfo$varLabel == input[[sprintf("InputChoice_%s",i)]])
      c(i,input[[sprintf("InputChoice_%s",i)]],as.character(supportedInfo[idxVal,]$varType),as.character(supportedInfo[idxVal,]$stdVarType))
    }) %>% t()
    
    dat<-unname(dat)
    colnames(dat)<-c("varName","varLabel","varType","stdVarType")
    dat<-as.data.frame(dat)
    
})

output$mapOverView<-renderTable({
  mapDat()[,1:3]
})

output$someVals<-renderText({
  sprintf("Rows in data: %d", nrow(clinicalData()))
})