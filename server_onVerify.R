
output$allPatientMenu<-renderMenu({
  totalAll<-ifelse(is.null(clinical()),"0",toString(nrow(clinical())))
  menuItem("All Cases",tabName = "all_patients",badgeLabel = totalAll, badgeColor = "purple",selected=FALSE)
})


output$selectedPatientMenu<-renderMenu({
  totalSel<-ifelse(is.null(selectedPTs$df),"0",toString(nrow(selectedPTs$df)))
  menuItem("Selected Cases", tabName = "selected_patients",badgeLabel = totalSel, badgeColor = "green",selected=FALSE)
})



#constructs the filtration menu based upon the kinds of the data the user has
output$filterMenu<-renderUI({
  #userInterfaceIsGo<<-TRUE
  
 # supportedInfo<-mapDat()
  
  varTypeVec<-supportedInfo %>%
    filter(varName %in% colnames(clinicalData2())) %>%
    select(varType) %>%
    unique()
   
  lapply(as.character(varTypeVec$varType),function(varTypeVal){
    
    if(varTypeVal %in% levels(mapIcons$varType)){
      
      #set up some variable to feed to the UI elements
      varNameVec<-supportedInfo %>%
        filter(varName %in% colnames(clinicalData2())) %>%
        filter(varType  == varTypeVal) %>%
        select(varName)
      
      iconVals <- mapIcons %>%
        filter(varType ==varTypeVal)
      
      box(
        title = tagList(shiny::icon(as.character(iconVals$iconType)), as.character(iconVals$iconLabel)),
        solidHeader=TRUE,
        background = "black",
        width=12,
        collapsible = TRUE,
        collapsed = TRUE,
        
        lapply(as.character(varNameVec$varName), function(i) {
          labelVal<-filter(supportedInfo,varName ==i)
          #Based on the different std var Type, the user will have the option to filter on different variables
          switch(as.character(labelVal$stdVarType),
            "category" = checkboxGroupInput(sprintf("filter_%s",i),sprintf("Filter By: %s",as.character(labelVal$varLabel)),
                          choices=unique(as.character(clinicalData2()[,i])),
                          selected=unique(as.character(clinicalData2()[,i])),
                          inline = TRUE),
            "category-many"=selectizeInput(sprintf("filter_%s",i),sprintf("Filter By: %s",as.character(labelVal$varLabel)),
                                               choices=unique(as.character(clinicalData2()[,i])),
                                               #selected=unique(as.character(clinicalData()[,i])),
                                               multiple=TRUE),
            "string"= selectizeInput(sprintf("filter_%s",i),sprintf("Filter By: %s",as.character(labelVal$varLabel)),
                                         choices=unique(as.character(clinicalData2()[,i])),
                                         multiple=TRUE),
            "date" = dateRangeInput(sprintf("filter_%s",i),sprintf("Filter By: %s",as.character(labelVal$varLabel)),
                                    start = min(clinicalData2()[,i]),
                                    end = max(clinicalData2()[,i]),
                                    min = min(clinicalData2()[,i]),
                                    max = max(clinicalData2()[,i]))
          )
        })
      )
    }
  })
})


