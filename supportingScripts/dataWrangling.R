#loading additional functions that will help process the data
source("supportingScripts/utilFUN.R")

#load data
clinicalData<-read.csv(file = "../TB_DataVizPrototypes/data/DataUPDATED_v2.0.csv",header=T,na.strings=c(" ","NO DATA","NONE","NA","N/A"),stringsAsFactors = FALSE)

#replace NA values with UNKNOWN
clinicalData[is.na(clinicalData)]<-"UNKNOWN"

#now make clinical data a data frame so I can manipulate the levels
clinicalData<-data.frame(clinicalData)

#------------------  V ONE TIME ACTION V --------------------------------------------------------------
#Add a little bit of jitter to the lat and long, as there are some cases the sit on top of eachother
# LAT LONG DATA ARE THE GEOGRAPHIC CENTRIODS FOR CANADIAN CENSUS DATA, WHICH I HAVE THEN SLIGHTLY PERTURBED
# clinicalData$Lat<-sapply(clinicalData$Lat,function(x){ifelse(runif(1)>0.5,x+0.0005,x-0.0005)})
# clinicalData$Long<-sapply(clinicalData$Long,function(x){ifelse(runif(1)>0.5,x+0.0005,x-0.0005)})
# 
# addresses<-c()
# for(i in 1:nrow(clinicalData)){
#   addresses<-c(addresses,
#                revgeocode(as.numeric(clinicalData[i,c("Long","Lat")]),override_limit = TRUE))
# }
# 
# clinicalData$fakeAddress<-unlist(addresses)
# 
# 
# newVar<-c()
# for(address in clinicalData$fakeAddress){
#   if(!is.na(address)){
#     addVal<-unlist(str_split(address,","))
#     splitVal<-unlist(strsplit(addVal[3]," "))[3:4]
#     
#     newVar<-rbind(newVar,
#                   c(addVal[1],sub("^\\s+", "", addVal[2]),paste0(splitVal,collapse="")))
#   }else{
#     newVar<-rbind(newVar,rep(NA,3))
#   }
# }
# 
# colnames(newVar)<-c("fakeStreet_add","fakeCity","fakePostal")
# clinicalData<-cbind(clinicalData,newVar)
# write.csv(file="../TB_DataVizPrototypes/data/DataUPDATED_v2.0.csv", clinicalData)

#now reverse geocode so that there are actuall addresses - AGAIN THESE ARE NOT REAL ADDRESSES

#------------------  ^ ONE TIME ACTION ^ --------------------------------------------------------------

#Cleaning up the clinical data
#this is quite silly but I need to add a date (first of the month) for R to use the
#date variable properly
#clinicalData$Diagnosis.Date<-paste(clinicalData$Diagnosis.Date,"01",sep="-")
#clinicalData$Diagnosis.Date<-as.Date(as.character(clinicalData$Diagnosis.Date),format="%B-%y-%d")

clinicalData$Diagnosis.Date<-convertDate(clinicalData$Diagnosis.Date)
clinicalData$Treatment.Start.Date<-convertDate(clinicalData$Treatment.Start.Date)
clinicalData$Treatment.End.Date<-convertDate(clinicalData$Treatment.End.Date)

#Emphasize the levels of the treatment data
clinicalData$Treatment.Compliance.Code.Description<-factor(clinicalData$Treatment.Compliance.Code.Description,levels=c("<50%","50-79%","80-99%","100%","UNKNOWN"))


#Assign NA values in Unknown in mode of treatment
clinicalData$Major.Mode.of.Treatment.Description[is.na(clinicalData$Major.Mode.of.Treatment.Descriptio)]<-"UNKNOWN"

#Randomly assign the "follow-up" variable
clinicalData$followUP<-sample(c("No","Yes"),nrow(clinicalData),replace = T,prob=c(0.8,0.2))

#filterout any patients that have an unknow TB status
clinicalData<- clinicalData %>% filter(!is.na(TB.Type))

colorChoices<-sample(colors(),13,replace=F)

#BCCentre<-geocode("British Columbia")

personOfInterest<-NULL

selectedColumns<-c("clientID",
                   "TB.Type",
                   "followUP",
                   "Gender",
                   "Origin.of.Birth",
                   "HA",
                   "HSDA",
                   "Diagnosis.Date",
                   "Method.of.Detection",
                   "Treatment.Start.Date",
                   "Treatment.End.Date",
                   "Major.Mode.of.Treatment.Description",
                   "Treatment.Compliance.Code.Description", 
                   "Treatment.Outcome.Status.Code.Description")


#OK - standardize the columns headers so that I can work with different datasets



