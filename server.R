library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(ggmap)
library(DT)
library(highcharter)
library(lubridate)

#load data wrangling 
#source("./supportingScripts/dataWrangling.R",local = T)

clinicalData<-NULL
selectedPTs<-NULL

source("./supportingScripts/utilFUN.R",local=T)
#source("./analysisModes/compareItems.R")

# Define server logic required to draw a histogram
function(input, output,session) {
  
  #reads in the user's data and figures out what kinds of variables the user has
  source("server_onLoad.R",local=TRUE)
  
  
  #so if the data is verified, then dynamically generate the UI
  observeEvent(input$verified,{
    isolate({
      keepCols <- mapDat() %>%
        filter(varType != "Ignore") %>%
        select(varName)

      #this is kind of a weird work around, but it works
      clinicalData2<<-reactive(clinicalData()[,as.character(keepCols$varName)])
      
      #also reassign supported info so that it can take care of reclassifications
     supportedInfo<<-mapDat() %>% 
       filter(varName %in% as.character(keepCols$varName))
    })

    #builds the filteration navigation side bar
    source("server_onVerify.R",local=TRUE)
    
    #changes the tab from data input to al patients
    #builds the default visual encodings
    source("server_onVerify_encodings.R",local=TRUE)
    source("server_onVerify_analysisModes.R",local=TRUE)
    
   
    updateTabItems(session, "tabs", selected = "all_patients")
    
  })

}