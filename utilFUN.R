#converts a exisiting date formate to one that is more workable
convertDate<-function(dateVar=NULL){
  dateVar<-paste(dateVar,"01",sep="-")
  dateVar<-as.Date(dateVar,format="%B-%y-%d")
  return(dateVar)
}




getTravelOrder<-function(datVal=NULL,start="655 West 12th Ave, Vancouver",geoStart=TRUE,shortestBy="dist"){
  if(!(shortestBy %in% c("dist","time"))){
    stop("the variable shortestBy must be one of either dist[default], or time")
  }
  
  #recursion stopping criteria
  if(nrow(datVal)==0){
    return()
  }
  
  #remove any other address that match to the current start location
  datVal<-subset(datVal, Location != start)
  
  #start point is the BC Centre for Disease Control
  if(geoStart){
    start<-geocode(start)
  }
  
  #do a greedy search, select the shortest possible distance from your starting point
  #then the next shortest possible distance from each point onwards and so on..
  store<-c()
  #where to start
  for(i in 1:nrow(datVal)){
    from<- revgeocode(as.numeric(start))
    to<- revgeocode(as.numeric(datVal[i,c("long","lat")]))
    
    if(shortestBy=="dist"){
    #next location by distance
    store<-c(store,mapdist(from=from,to=to)["km"])
    }else{
    #next location by time
      store<-c(store,mapdist(from=from,to=to)["minutes"])
    }
  }
  
  store<-unlist(store)

  nextPlace<-as.character(datVal[which(store==min(store)),"Location"])
  nextPlacedf<-subset(datVal,Location !=nextPlace)
  
  routeVals<-c(revgeocode(as.numeric(start)),
               getTravelOrder(datVal=nextPlacedf,start=nextPlace))

  return(routeVals)
}

#Extract the HTML driving instructions from the route information
getHTMLInstructions<-function(val=NULL){
  nSteps<-length(val$routes[[1]]$legs[[1]]$steps)
 
  driveDirections<-c()
  #in what I am doing here, legs and route should always be one, if that's not the case
  #this will break
  for(i in 1:nSteps){
    driveDirections<-c(driveDirections,val$routes[[1]]$legs[[1]]$steps[[i]]$html_instructions)
  }
  return(driveDirections)
}


getTravelDirects<-function(locationOrder=NULL){
  totalRoute<-c()
  for(i in 1:(length(locationOrder)-1)){
    print(i)
    #get detailed route
    allRoute<-route(from=locationOrder[i],to=locationOrder[i+1],output = "all")
    navDirections<-getHTMLInstructions(allRoute)

    simpleRoute<-route(from=locationOrder[i],to=locationOrder[i+1])
    simpleRoute<-mutate(simpleRoute,routeID=i)
    simpleRoute<-mutate(simpleRoute,instruction=navDirections)
   
    totalRoute<-rbind(totalRoute,simpleRoute)
    # Basically, this sleep statement is so that I don't max out the query per second limit
    # on the google maps API (otherwise, this function sometimes sliently dies)
    Sys.sleep(1)
  }
  
  return(totalRoute)
}

#------ FOR TESTING ONLY---------
#test data : Just using the main hospitals public addressess, mostly cause I have sense of what order they shoul be travelled in
# datVal<-c("655 West 12th Ave, Vancouver","899 W 12th Ave, Vancouver","2329 West Mall, Vancouver","1081 Burrard St, Vancouver","4480 Oak St, Vancouver")
# datVal<-cbind(datVal,geocode(datVal))
# 
# colnames(datVal)<-c("Location","long","lat")
# 
# travelOrderDist<-getTravelOrder(datVal)
# travelOrderTime<-getTravelOrder(datVal,shortestBy="time")
# 
# 
# driveDirectionMap<-getTravelDirects(travelOrderDist)
# 
# #testing using the fake clinical data
#  test<-clinicalData[1:10,c("fakeAddress","Long","Lat","ATOC.HA")]
#  colnames(test)<-c("Location","long","lat","HA")
# 
#   # #HOLY SHIT IT CAN DO WATER CROSSINGS!
#   # # But I will simplify life an fouc on land destination
#   test<-test %>% filter(HA != "4. VANCOUVER ISLAND")
#   travelOrderDist2<-getTravelOrder(test[1:4,])
# 
#   #i think this might time out (too many queries per second)
#   driveDirectionMap<-getTravelDirects(travelOrderDist)
# 
#   constructLink<-c("https://www.google.com/maps/dir/",
#                    strsplit(gsub(",","",travelOrderDist2[1])," ") %>% unlist() %>% paste0(.,collapse="+"),
#                    "/",
#                    strsplit(gsub(",","",travelOrderDist2[2])," ") %>% unlist() %>% paste0(.,collapse="+"))
#https://gearside.com/easily-link-to-locations-and-directions-using-the-new-google-maps/
#https://www.google.com/maps/dir/760+W+Genesee+St+Syracuse+NY+13204/314+Avery+Ave+Syracuse+NY+13204/9090+Destiny+USA+Dr+Syracuse+NY+13204

#------ FOR TESTING ONLY---------