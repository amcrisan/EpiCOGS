#loading the csv spread sheet default values
mapIcons<-read.csv(file="./uiControl/iconMapping.csv",header=T)
supportedInfo<-read.csv(file="./uiControl/supportedInfo.csv",header=T)

#some global variables
userInterfaceIsGo<-FALSE

# This is for the analysis mode file UI and server control. I wanted to use modules
# in order to handle the the dynamic analysis selection mode, so I needed to out
# the UI function here in the gloab file, and each seperate analysis file will have
# define the appearence of module UI.

analysisModeUI<-function(id, label = "") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("moduleUI"))
  )
}
