
#Variable : reactive data based upon the filtration options
clinical<-reactive({
  dat<-clinicalData2()
  
  varTypeVec<-supportedInfo %>%
    filter(varName %in% colnames(dat)) %>%
    select(varType) %>%
    unique()
  

  #now, perform the filtration based upon the data elements that are supported
  lapply(as.character(varTypeVec$varType),function(varTypeVal){
    #set up some variable to feed to the UI elements
    varNameVec<-supportedInfo %>%
      filter(varName %in% colnames(dat)) %>%
      filter(varType  == varTypeVal) %>%
      select(varName)
    
    lapply(as.character(varNameVec$varName), function(i) {
        labelVal<-filter(supportedInfo,varName ==i)

        if(as.character(labelVal$stdVarType) %in% c("category","category-many","string")){
          
          #don't filter anything if the value is null
          if(!is.null(input[[sprintf("filter_%s",i)]])){
            filterString<-paste(i, "%in%",
                        sprintf("c('%s',NA)", #I don't wants NAs filtered out for all vars
                        paste(input[[sprintf("filter_%s",i)]], collapse="','")))
  
              dat<<-dat %>%
                filter_(filterString)
          }
        }else if(as.character(labelVal$stdVarType) == "date"){
          if(!is.null(input[[sprintf("filter_%s",i)]][1])){
            
            #note, an assumption here is ymd format that could break everything
            filterString<-paste(sprintf("(%s > \'%s\')",i,input[[sprintf("filter_%s",i)]][1]),
                                sprintf("(%s <\'%s\')",i,input[[sprintf("filter_%s",i)]][2]),sep = " & ")
            
            #mutate doens't work consistently, so this is a work around.
            if(class(dat[,sprintf("%s",i)]) == "character"){
              dat[,sprintf("%s",i)]<-lubridate::ymd(dat[,sprintf("%s",i)])
            }
            
            #now filter
            dat<<-dat %>%
              filter_(filterString)
          }
        }
    })
  })

  dat
})


# a reactive value that will contain subset of paitients user 
# wants to store for analysis
selectedPTs <- reactiveValues()
selectedPTs$df<-NULL


observe({
  s = input$linelist_rows_selected
  
  #use isolate to avoid triggering a bunch of dependencies
  isolate({
    currData<-selectedPTs$df
    if (length(s)){
      if(is.null(selectedPTs$df)){
        currData <-clinical()[s,]
      }else{
        #remove duplicated row (based upon PHD and Lat, Long co-ordinates)
        currData<-rbind(currData,clinical()[s,]) %>% unique()
      }
    }
    selectedPTs$df <- currData
  })
})


#Viz : Base Leaflet map
output$map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("Stamen.TonerLite",
                     options = providerTileOptions(noWrap = TRUE)
    )%>%
    addCircles(.,lng=~Long,lat=~Lat,data = clinical())
})


# Viz: case numbers over time
output$caseOverTime<- renderPlot({
  datVar<-clinical()
  
  datVar$year <- year(datVar$diagnosisDate)
  
  temp<-datVar %>% 
    group_by(year,State) %>% 
    summarise(countVal = n())
  
  p<-ggplot(data=temp,aes(x=year,y=countVal,group=State)) +
    geom_bar(stat="identity",aes(fill = State)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90),legend.position = "bottom")
  
  p
})

# table summarizing cases

output$summaryTable<-renderTable({
  datVar<-clinical()
  sumVar<-c()
  
  t<-table(datVar$gender)
  t2<-t/sum(t)
  
  sumVar<-c("Gender",sprintf())
  
})

#Viz:  output all patients the line list
output$linelist <- DT::renderDataTable({
  DT::datatable(clinical(), options = list(scrollX = TRUE))
})

#Viz: output selected patients line list
output$selectedPatients <- DT::renderDataTable({
  DT::datatable(selectedPTs$df, options = list(scrollX = TRUE))
})

#OverviewDashBoard: output selected patients line list
output$varsToShow<-renderUI({
  vars<-colnames(clinical())
  
  selectizeInput("whatVars",label="Show overview for which variables?",
                 choices=vars,
                 selected=NULL,
                 multiple=TRUE)
})


#Function: highlight the selected patients
observe({
  s = input$linelist_rows_selected #not sure why I need this, but it makes it work
  
  testVal<-isolate(is.null(selectedPTs$df))
  if(!testVal){
    
    leafletProxy("map", data = selectedPTs$df) %>%
      addCircles(.,color="red",lng=~Long,lat=~Lat,radius=20)
  }
})

#Action : Get google driving directions

directionURL<<-eventReactive(input$drivingDirections,{
  temp<-selectedPTs$df[,c("Long","Lat")]
  
  #ok weird fix here
  locationVal<-c()
  for(i in 1:nrow(temp)){
    locationVal<-c(locationVal,
                   revgeocode(as.numeric(temp[i,c("Long","Lat")])))
  }
  
  temp<-cbind(locationVal,temp)
  colnames(temp)<-c("Location","Long","Lat")
  
  #get travel order and selected number of patients
  startPlace<-as.character(temp[sample(1:nrow(temp),1),]$Location)
  
  travelOrder<-getTravelOrder(datVal=temp,start=startPlace)
  
  print(travelOrder)
  
  # this will output the HTML directions directly into the SHINY
  #APP. but this wasn't useful because no one reads maps anymore
  #driveDirectionMap<-getTravelDirects(travelOrderDist)
  
  
  #construct HTML link to google maps
  constructLink<-c("https://www.google.com/maps/dir")
  for(loca in travelOrder){
    constructLink<-c(constructLink,
                     strsplit(gsub(",","",loca)," ") %>% 
                       unlist() %>% 
                       paste0(.,collapse="+"))
  }
  
  
  paste0(constructLink,collapse="/")

})

output$routeURL<-renderUI({
  if(!is.null(directionURL)){
    HTMLString<-c(sprintf("<a href='%s'> CLICK HERE FOR GOOGLE MAPS </a><br>",directionURL()))
  }else{
    HTMLString<-c(sprintf(("<h3>Please click on the action button to get directions</h3>")))
  }
  
  HTML(HTMLString)
})



