###----------------------------------------------------------------###
# Project Name : Workload Centers
# Code Name: R - Project_1
# Date: 02/22/2016
# Team Name : R-Wranglers
# Team Members : Mangapathi Shashank Navuduri, Srinivas Godavarthi, 
#                Amit Madhup, Sai Sarath Talamudupula, Saleem M
###----------------------------------------------------------------###
setwd('C:/Users/Shashank Navuduri/Desktop/R Project1')
##Installing Libraries
install.packages("ggmap")
library(ggmap)
library(stringr)


## Importing Dataset
zipdata <- read.csv("C:/Users/Shashank Navuduri/Desktop/R Project1/RI_ZipCodes.csv")
View(zipdata)

## Creating Analytical Dataset
zipdata1<- zipdata
colnames(zipdata1)<-c("ZIP2", "X2", "Y2", "STATECODE2", "STATENAME", "Frequency2")
mergeddata<- merge(zipdata,zipdata1, by="STATENAME", all=TRUE)
#attach(mergeddata)

## Calculating Driving Distance
## Please note that mapdist (Google API) function has the limitation of doing only 2500 distance mappings per day per user
## To generate driving distance values for all 7655 distance mappings, we had to run this function adjusting the count in for loop and then run it over multiple machines 
## The driving distances have then been collated reread into the R system below which is same 
## as the mergedata with an additional column of driving distances.
for (i in 1:7655) 
{
  x<-mapdist(str_replace_all(paste("0",as.character(mergeddata[i,2])),fixed(" "),""), str_replace_all(paste("0",as.character(mergeddata[i,7])),fixed(" "),""), mode = 'driving')
  mergeddata[i,12]<-x[1,5]
}


## To filter out distances between the same zipcodes
mergeddata = mergeddata[mergeddata$ZIP!=mergeddata$ZIP2,]
write.csv(mergeddata, file="Outputdata.csv")

## Haversine distance calculation

Outputdata_Final_ggmap <- read.csv("D:/Sem-2/R/Project-1/New folder/Outputdata.csv")
attach(Outputdata_Final_ggmap)

Outputdata_Final_ggmap$haversine<- acos( sin(X*pi/180)*sin(X2*pi/180) + cos(X*pi/180)*cos(X2*pi/180)*cos(Y2*pi/180-Y*pi/180) ) * 6371000
Outputdata_Final_ggmap$haversine<-Outputdata_Final_ggmap$haversine*0.621371/1000
View(Outputdata_Final_ggmap)
final_data<-Outputdata_Final_ggmap[is.na(Outputdata_Final_ggmap$haversine)==FALSE | ZIP!=ZIP2, ]
detach(Outputdata_Final_ggmap)

## Workload Moments
## Moments are calculated by multiplying the distance with the frequency of the visiting site
attach(final_data)
final_data$drivingmoment<-V12*Frequency2 # V12 is the driving distance between two zipcodes
final_data$haversinemoment<-final_data$haversine*Frequency2 # haversine is the haversine distance between two zipcodes

## Summarizing
install.packages("dplyr")
library(dplyr)

# Grouping the zip codes by taking total of all moments
sum_havmom <- aggregate(final_data$haversinemoment, by=list(final_data$ZIP), FUN=sum)
sum_drimom <- aggregate(final_data$drivingmoment, by=list(final_data$ZIP),  FUN=sum)
colnames(sum_havmom)<-c('ZIP', 'haversinemoments')
colnames(sum_drimom)<-c('ZIP', 'drivingmoments')

# Merging the above two aggregated data frames, each for haversine moments and driving moments
summary_data<- merge(sum_havmom,sum_drimom, by="ZIP", all=TRUE)



# Zipcode with minimum aggregated distance will be the workload center 

#Workload center based on haversine distance
Haver_workload = summary_data[which.min(summary_data$haversinemoments),1]
#Workload center based on driving distance
Driving_workload = summary_data[which.min(summary_data$drivingmoments),1]


#Visualing the problem

# Subsetting the customer site zipcodes (those having frequencies associated)
Outputdata_Final = Outputdata_Final_ggmap[Outputdata_Final_ggmap$Frequency>0,]

# subsetting the identified Haversine distance work load center details
# subsetting the identified Driving distance work load center details
dum1 = Outputdata_Final_ggmap[Outputdata_Final_ggmap$ZIP == Haver_workload,]
dum2 = Outputdata_Final_ggmap[Outputdata_Final_ggmap$ZIP == Driving_workload,]


# getting the map
workload_pos <- get_map(location = "Rhode Island", zoom = 9 , maptype = "roadmap", scale = 1)

# plotting the map with points as the customer sites and highlighted work load center sites
# Blue represents the Haversine distance based work load center
# Cyan represents the Driving distance based work load center
ggmap(workload_pos) +  geom_point(data = dum1,aes(x=X,y=Y),size=8,shape =17,col="blue",show.legend = F) +  geom_point(data = dum2,aes(x=X,y=Y),size=8,col="CYAN",shape = 15,show.legend = F)  + geom_point(data = Outputdata_Final, aes(x = X, y = Y, fill = "Customer sites",size = Frequency),alpha =.5,shape=21, show.legend = T) + scale_size_continuous(range = c(5,15))         
        

 