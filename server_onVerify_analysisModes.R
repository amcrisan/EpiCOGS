sourceName <- reactiveValues()
sourceName$df<-NULL

output$availableAnalysis<-renderUI({
  isolate({
    files<-list.files(path = "./analysisModes/",full.names=T)
    for(file in files){
      source(file)
      #figure out what kinds of variable exist in current data
      #only load those analyses that have these datatypes
      varTypeVec<-supportedInfo %>%
        filter(varName %in% colnames(clinicalData())) %>%
        select(varType) %>%
        unique()
      
      print(setdiff(requiredData,varTypeVec$varType))
      
      if(length(setdiff(requiredData,varTypeVec$varType)) == 0){
        sourceName$df<-rbind(sourceName$df,c(file,provideName()))
      }
    }
  })
  selectizeInput(inputId="analysisChoices",label="Please choose one of the following analysis to run",
                 width='50%',
                 choices=sourceName$df[,2],
                 selected=NULL,
                 multiple=FALSE)
})


#create a UI module to add
observeEvent(input$analysisChoices,{
  idxSource<-which(sourceName$df[,2] == input$analysisChoices)
  source(sourceName$df[idxSource,1],local=TRUE)
  
  callModule(moduleServer, "analysisSelected",data=clinical())
  
})
