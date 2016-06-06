library(lubridate)
library(stringdist)
library(ggdendro)

provideName<-function(){
  return("Surveillance")
}

requiredData<-c("Location","Demographic")

############ INTERACTS WITH TB COGS ############

# main out is a list the contains all the UI elements of an analysis mode
# it will be passed to the renderUI function in the "analysisMode.R" serer function
moduleServer<-function(input, output, session,data){
  #moduleUI
  output$moduleUI<-renderUI({
    ns <- session$ns
    list(
      h3("Disease Burden"),
      box(collapsible=T,
          collapsed=T,
          width=12,
          title = "Cases over Time by Statate/Province",
          plotOutput(ns("caseoverTime"))
      ),
    box(collapsible = T,
        collapsed =T,
        width=12,
        title = "Total Disease Burden by State/Province",
        plotOutput(ns("totalBurden"))
      ),
    h3("By Covariates"),
    box(collapsible = T,
        collapsed =T,
        width=12,
        title = "Active TB by Gender",
        plotOutput(ns("tbByGenderoverTime"))
    ),
    h3("Miru Genotypes"),
    box(collapsible = T,
        collapsed =T,
        width=12,
        title = "MIRU Patterns by State/Province",
        plotOutput(ns("miruPlot_all"))
    ),
    box(collapsible = T,
        collapsed =T,
        width=12,
        title = "MIRU Patterns by State/Province and year",
        plotOutput(ns("miruPlot_year"))
    ),
    box(collapsible = T,
        collapsed =T,
        width=12,
        title = "MIRU Pattern Dendrogram",
        plotOutput(ns("miruPlot_tree"))
    )
    )
  })
  
  #---------------- MIRU PATTERNS OVER TIME -----------------
  
  output$miruPlot_all<-renderPlot({
    data$year <- year(data$diagnosisDate)
    
    temp<-data %>% 
      group_by(MIRU_Pattern, State) %>% 
      summarise(countVal = n())
    
   p<- ggplot(data=temp,aes(x=State,y=countVal,group=MIRU_Pattern)) +
      geom_bar(stat="identity",aes(fill = MIRU_Pattern)) +
      theme_bw() +
      theme(axis.text.x=element_text(angle=90),legend.position = "bottom")
   
   p<- p + guides(fill=guide_legend(ncol=2,bycol=TRUE))
   p
  })
  
  #---------------- MIRU PATTERNS OVER TIME  BY REGION -----------------
  output$miruPlot_year<-renderPlot({
    data$year <- year(data$diagnosisDate)
    
    temp<-data %>% 
      group_by(MIRU_Pattern, State, year) %>% 
      summarise(countVal = n())
    
    p<- ggplot(data=temp,aes(x=year,y=countVal,group=MIRU_Pattern)) +
      geom_bar(stat="identity",aes(fill = MIRU_Pattern)) +
      scale_x_continuous(breaks = min(data$year):max(data$year))+
      facet_grid(.~State)+
      ylab("Total Number of Cases")+
      theme_bw() +
      theme(axis.text.x=element_text(angle=90,vjust=0.5), legend.position="bottom")
    
    p<- p + guides(fill=guide_legend(ncol=2,bycol=TRUE))
    p
  })
  
  output$miruPlot_tree<-renderPlot({
    MIRU_dat<-data$MIRU_Pattern
    names(MIRU_dat)<-data$caseID
    distVal<-stringdistmatrix(MIRU_dat, method = "hamming")
    hc_clust<-hclust(distVal,method="average")
    dhc <- as.dendrogram(hc_clust)
    # Rectangular lines
    ddata <- dendro_data(dhc, type = "rectangle")
    
    #provide leaf labels
    datLab<-ddata$labels
    datLab<-cbind(datLab,data[datLab$label,])
    
  p<-ggplot(segment(ddata)) + 
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
      #geom_point(data=datLab,aes(x = x, y = y, colour=originOfBirth),pch=15)+
      geom_point(data=datLab,aes(x = x, y = y - 0.25, colour=State),pch=15)+
      coord_flip() +
      theme_bw()+
      xlab("")+
      ylab("Distance")+
      scale_y_reverse(expand = c(0.2, 0))+
      theme(axis.text.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor.y= element_blank(),
            panel.border=element_blank(),
            axis.ticks.y = element_blank())
    p
  })
  
  #---------------- CASES OVER TIME -----------------
  output$caseoverTime<-renderPlot({
    
    data$year <- year(data$diagnosisDate)
    
    temp<-data %>% 
      group_by(tbType, State,year) %>% 
      summarise(countVal = n())
    
    p<-ggplot(data=temp,aes(x=year,y=countVal,group=State)) +
      geom_line(aes(colour=State)) +
      geom_point(pch=21,aes(colour=State),fill="white")+
      theme_bw() +
      ylab("Total Number of Cases")+
      xlab("Year of Diagnosis")+
      theme(axis.text.x=element_text(angle=90,vjust=0.5))
      
    p
  })
  
  
  #---------------- TOTAL DISEASE BURDEN -----------------
  output$totalBurden<-renderPlot({
    
    p<-ggplot(data=data,aes(x=State,group=State)) +
      geom_bar(aes(fill=State))+
      theme_bw() +
      ylab("Total Number of Cases")+
      xlab("State")+
      theme(axis.text.x=element_text(angle=90,vjust=0.5))
    
    p
  })
  
  
  #---------------- CASES BY GENDER OVER TIME -----------------
  output$tbByGenderoverTime<-renderPlot({
    
    data$year <- year(data$diagnosisDate)
    
    temp<-data %>% 
      group_by(tbType, gender,year) %>% 
      summarise(countVal = n())
    
    p<-ggplot(data=temp,aes(x=year,y=countVal,group=gender)) +
      geom_bar(stat="identity",aes(fill=gender),position="dodge")+
      scale_x_continuous(breaks = min(data$year):max(data$year))+
      theme_bw() +
      ylab("Total Number of Cases")+
      xlab("Year of Diagnosis")+
      theme(axis.text.x=element_text(angle=90,vjust=0.5))
    
    p
  })

  
  #---------------- AGE OVER TIME -----------------
  output$tbByAgeoverTime<-renderPlot({
    
    data$year <- year(data$diagnosisDate)
    #age groups
    #<1, 1-5,5-9,10-14, 15-19,20-24,25-29,30-39,40-59,>60
    
    p<-ggplot(data=data,aes(x=year,y=age,group=gender)) +
      geom_bar(stat="identity",aes(fill=gender),position="dodge")+
      scale_x_continuous(breaks = min(data$year):max(data$year))+
      theme_bw() +
      ylab("Total Number of Cases")+
      xlab("Year of Diagnosis")+
      theme(axis.text.x=element_text(angle=90,vjust=0.5))
    
    p
  })
}

  
    
