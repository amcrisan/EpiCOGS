# This is a total false data set that is being used to demonstrate some of the capacities of this
# tool. 

set.seed(40)

library(dplyr)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(ggdendro)


# -------------------------- STEP 1 -------------------------- 
# going to use the super zip data (http://shiny.rstudio.com/gallery/superzip-example.html) to
# put people in logic places (i.e. not in the middle of the ocean). Also, by distributing
# synthetic patients according to population means that there will be some clustering that can
# be seen.


#load super zip data
allzips <- readRDS("fakeData/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )


#to improve clustering of cases, limit the selection to super zips!
#focus on states where the TB rate is greater than 3.0 (national average)
# in the continental US (also the hone in search)
#data from : 

abvAverage <- c("CA","TX","DC","GA","NY")

caseList<-filter(cleantable,Superzip==1) %>%
  filter(State %in% abvAverage)%>%
  sample_n(size=1000,replace=TRUE,weight=Population) %>%
  select(City,State,Lat,Long)

#Sanity check : yeah they look pretty clustered.
#Real data might be more sparse.
leaflet(caseList) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(lng = ~Long, lat = ~Lat)


#now give these people some characteristics
caseList$caseID<-sprintf("%04d", 1:nrow(caseList))

#demographic variables
caseList$originOfBirth<-sample(c("US","Non-US"),nrow(caseList),replace=TRUE,prob=c(0.3,0.7))
caseList$gender<-sample(c("M","F"),nrow(caseList),replace=TRUE,prob = c(0.3,0.7))

#diagnosis variables
#mode of detection - solicited from TB experts
detectMethod<-c("Conact investigation",
               "Post-mortem",
               "Incidental findings",
               "Other",
               "Unknown",
               "Pre-landing Surveillance",
               "Post-landing Surveillance",
               "Screening Program")


caseList$MethodOfDetection<-rep("Unknown",nrow(caseList))

caseList[caseList$originOfBirth=="Non-US",]$MethodOfDetection<- sample(detectMethod,
                                             size=sum(caseList$originOfBirth=="Non-US"),
                                             replace=TRUE)

caseList[caseList$MethodOfDetection=="Unknown",]$MethodOfDetection<-sample(setdiff(detectMethod,c("Pre-landing Surveillance","Post-landing Surveillance")),
                                             size=sum(caseList$MethodOfDetection=="Unknown"),
                                             replace=TRUE)

caseList$Age<-rnorm(n = nrow(caseList),mean = 50,sd=10)

caseList$tbType <-rep("Active",nrow(caseList))

#assign diagnosis date by region, so it looks like people are plausibly connected
diagMin<-Sys.Date() -(365*5)
diagMax<-Sys.Date() - 60

caseList$diagnosisDate <- rep(NA,nrow(caseList))
  
for(regionVal in unique(caseList$State)){
    regionStartDate<-diagMin + sample(1:((diagMax - 365) - diagMin),1)
    
    idx<-which(caseList$State == regionVal)
    
    caseList[idx,]$diagnosisDate<-(regionStartDate + sample(1:(diagMax - regionStartDate),size=length(idx),replace=FALSE)) %>% as.character()

}


#add treatment start and end dates
#usually treatment is 6 to 9 months.
caseList<- caseList %>%
  mutate(treatmentStartDate = (as.Date(diagnosisDate) + sample(1:30,1))) %>%
  mutate(treatmentEndDate = treatmentStartDate %m+% months(sample(6:9,1))) %>%
  mutate(treatmentModality = sample(c("Unknown","DOT","Daily-self administered","Other"),replace=T,size=nrow(caseList))) %>%
  mutate(treatmentCompletion = ifelse(treatmentEndDate > diagMax,
                                       "<50%",
                                       sample(c("50-79%","80-99%","100%"),replace=TRUE,nrow(caseList),prob=c(0.2,0.4,0.4)))) %>%
  mutate(treatmentStartLocation = sample(c("Outpatient","General Hospital","Unknown"),size=nrow(caseList),
                                           replace=TRUE,prob=c(0.5,0.4,0.1)))



caseList$treatmentOutcome<-rep("Ongoing",nrow(caseList))
caseList[caseList$treatmentCompletion =="100%",]$treatmentOutcome<-"Treatment Completed"
caseList[caseList$treatmentOutcome != "100%",]$treatmentOutcome<-sample(c("Unknown","Death","Ongoing"), prob=c(0.2,0.1,0.7),
                                                       replace=TRUE,
                                                       size=sum(caseList$treatmentOutcome != "100%"))

caseList$followUp<-rep("Yes",nrow(caseList))
caseList[caseList$treatmentCompletion =="100%",]$followUp<-"Yes"



#for those that died, record a reason that they died
caseList$causeOfDeath<-rep(NA,nrow(caseList))
caseList[caseList$treatmentOutcome=="Death",]$causeOfDeath<-sample(c("TB was cause of death","Other cause","Unknown"),
                                                                      replace=TRUE,
                                                                      size=sum(caseList$treatmentOutcome=="Death"))

            

#write.csv(file="fakeData/FakeData.csv",quote=F,caseList)
saveRDS(file="fakeData/FakeData_obsOnly.RDS",caseList)

temp<-caseList %>%
  select(-contains("treatment"))

saveRDS(file="fakeData/FakeData_obsOnly_nRx.RDS",temp)


# -------------------------- STEP 2 -------------------------- 
# I've grabbed some MIRU-VNTR patterns and the drug susceptibility from
# MIRU-VNTRPlus data base. I've taken lineages EAI, LAM, Beijing, Haarlem, and H37Rv
# I've randomly distributed these straing among the "cases"

#stored as seperate dataset

strains<-read.csv(file="fakeData/LOCI_24_MIRU_VNTR_LINEAGES.csv",header=T)

MIRU_VNTR_String<-apply(strains,1,function(x){
 paste0(x[5:28],collapse="")
})

strains<-select(strains,-contains("MIRU.")) %>%
  mutate(MIRU_Pattern=MIRU_VNTR_String)


#add MIRU pattern in a somewhat clustered manner

#function to assign MIRUS
assignMIRU<-function(n=NA,dat=NULL,n_strains=3){
  strainVals<-sample_n(strains,n_strains) %>%
    sample_n(n,replace=TRUE)
  
  return(strainVals)
}


#randomly assign MIRUs with some clustering
miru24<-data.frame(matrix(NA, nrow = nrow(caseList), ncol = ncol(strains)))

for(state in unique(caseList$State)){
  idx<-which(caseList$State==state)
  
  vals<-assignMIRU(length(idx),strains,2)
  miru24[idx,]<-vals
}

#I know they line up, so put them together
temp <-cbind(caseList,miru24)
colnames(temp)<-c(colnames(caseList),colnames(strains))

#now assign to caselist

caseList<-temp
saveRDS(file="fakeData/FakeData_obsandGenotype.RDS",caseList)

#Sanity check : Yes there are only a small number of strains per state
pal<-colorFactor(brewer.pal(n=10,name="Set3"), temp$MIRU_Pattern)

leaflet(temp) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~Long, lat = ~Lat, fillColor = ~pal(MIRU_Pattern),popup=~ID, opacity=0.7,stroke=FALSE) %>%
  addLegend(pal = pal, values = ~MIRU_Pattern, opacity = 1)

ggplot(temp,aes(x=State,fill=MIRU_Pattern)) +
  geom_bar(stat="count",colour="black")+
  theme_bw()


#another sanity check

temp2<-temp %>%
  filter(City == "New York")

distVals<- dist(temp2$MIRU_Pattern,method="euclidean")

dd.row<-as.dendrogram(hclust(distVals))
ddata_x <- dendro_data(dd.row)



# ggplot(data=segment(ddata_x))+
#   geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
#   geom_point(aes(x=xend,y=0,colour=gender))
# 
# #show this with some metadata
# 
# ggdendrogram(hc, rotate = FALSE, size = 2)

# -------------------------- STEP 3 -------------------------- 
# Make a contact spreadsheet, this would link each patient to some other 
# Patients, do so based upon "patient ID".

#stored as seperate dataset   


#stick with contacts in the same state, and roughly the same city



# -------------------------- STEP 4 -------------------------- 
#Make a date frame of supported variable types that can be used on Shiny App start up.
# 
# supportedInfo<-data.frame(varName = c(colnames(caseList),"healthJurLvl1", "healthJurLvl2","Ignore"),
#                           varLabel = c("City","State/Province","Latitude","Longtitude",
#                                        "Case ID","Origin of Birth","Gender", "Method of Detection", "Type of TB",
#                                        "Diagnosis Date", "Treatment Start Date", "Treatment End Date", "Main Mode of Treatment",
#                                        "Treatment Completion","Treatement Start Location","Treatment Outcome", "Follow Required?",
#                                        "Cause of Death","Health Jurisdiction Level 1","Health Jurisdiction Level 2","Ignore"),
#                           varType = c("Location","Location","Coordinate","Coordinate","ID","Demographic","Demogrphic","Diagnosis",
#                                       "Diagnosis","Diagnosis","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment",
#                                       "Treatment","Treatment","Location","Location","Ignore")
#                           )
# 
# save(supportedInfo,file="supportedDataTypes.Rda")

