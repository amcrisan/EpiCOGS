
############ CUSTOM FUNCTIONS ############
#need to provide a function that gives a generic names
provideName<-function(){
  return("Compare Data")
}

requiredData<-c("Location","Demographic")


bestPlotChoice<-function(var1,var2,dat){
  p<-ggplot(data=dat,aes_string(x=var1,fill=var2))+
    geom_bar(stat="count",colour="black")+
    theme_bw()+
    theme(axis.text.x=element_text(angle=90))
  
  return(p)
}

############ INTERACTS WITH TB COGS ############

# main out is a list the contains all the UI elements of an analysis mode
# it will be passed to the renderUI function in the "analysisMode.R" serer function
moduleServer<-function(input, output, session,data){
  #moduleUI
  output$moduleUI<-renderUI({
    ns <- session$ns
      list(
        fluidRow(
          column(width=5,
          selectizeInput(ns("choiceX"),label="Covaraite 1",
                         choices=colnames(data),
                         selected=NULL,
                         multiple=FALSE)),
          column(width=5,
          selectizeInput(ns("choiceY"),label="Covariate 2",
                         choices=colnames(data),
                         selected=NULL,
                         multiple=FALSE)
          )
          ),
        plotOutput(ns("bestPlot"))
      )
  })
  
  output$bestPlot<-renderPlot(
    bestPlotChoice(input$choiceX,input$choiceY,data)
  )
}
