#GLMs for deer (drivers of deer activity)
#Laura Nicholson

#loading necessary files 
library(dplyr)
library(camtrapR)
library(exiftoolr)
library(rgdal)
library(plotKML)

#here going to try installing the latest development version of camtrapR from 
#GitHub... hopefully it doesn't break anything!
#library(remotes)
#install_github("jniedballa/camtrapR")
#hmm doesn't want to install...
#had to quit out of everything (it didn't like that I was currently using github)
#and install it in a brand new script, but then worked! 

#setting working directory 
setwd("F:/Gamecams")

#reading in record table 
recordtable <- read.csv("recordtable3.csv", fileEncoding = 'UTF-8-BOM')

#reading in station info 
stations <- read.csv("Station Info3.csv", header=T)

#strcuture of recordtable
str(recordtable)

#subsetting for just deer sightings
deer <- recordtable[recordtable$Species == 'Deer',]

#checking to see if we have all the sightings 
#for the record table
unique(recordtable[c("Station")])
#28 sites 

#for the deer 
unique(deer[c("Station")])
#23 sites

#filtering for deer sightings spaced more than 10 min apart <-- poo
ddeer <- deer %>%
 filter(delta.time.mins>10)

#how many sites left
unique(ddeer[c("Station")])
#21 (2 sites were lost... does this make sense.. still not really..!! 
#they still had deer sightings that were independent.. it is still getting 
#rid of sites that are just nearby other photos... )

?recordTable
deer_filter10 <- camtrapR:::assessTemporalIndependence(deer,
                                      deltaTimeComparedTo = "lastIndependentRecord",
                                      columnOfInterest = "Species",
                                      stationCol = "Station",
                                      cameraCol = "Station",
                                      camerasIndependent = TRUE,
                                      minDeltaTime = 10
)

?camtrapR

##woo ok trying Jurgen's function himself!
deer_filter10_min <- assessTemporalIndependence(intable = deer,
                                                  deltaTimeComparedTo = "lastIndependentRecord",
                                                  columnOfInterest = "Species",
                                                  stationCol = "Station",
                                                  cameraCol = "Station",
                                                  camerasIndependent = TRUE,
                                                  minDeltaTime = 10
)

View(deer_filter10_min)

#had to run whole other r code first.. but now getting error
# --> Error in Ops.data.frame(intable[, cameraCol], intable[xy, cameraCol]) : 
# '==' only defined for equally-sized data frames 
###UGHH ok so got it to run at least - I think it didn't like that I didn't have a 
#cameraCol argument in there, but now that I do it just returns with everything as 
#NAs, so that is also definitely not it!

#for now going to just move forward without the independence in pictures, and 
#revisit that when I have more help 

#here going to at least divide deer presences into % detected per day
#I do wonder if the number of days is accurate... hmm could try to check
#via R to count the number of days... but oof that is a whole thing..
#attempt to do it here at some point 


#then dividing total counts of rows by site and day by the number of days

library(dplyr)
#so here summarizing number of rows by day 
by_date <- deer %>%
  group_by(Station, Date) %>%
  tally()
#ok so this gives us a total number of sightings per day and site
#so since we want to get a detection rate basically, we would want to sum 
#all the days for each site and divide by the number of days that it was out!
# woo! ("Detection rate : the total count of detections of each species divided by the 
#number of camera nights (ie detections/day) at each study area) 
by_site <- deer %>%
  group_by(Station) %>%
  tally()
#nice 
write.csv(by_site, "by_site.csv")


#datasets I have
#wet versus dry
#vegetation communities
veg <- read.csv("veg_complete_sub40.csv", fileEncoding = 'UTF-8-BOM')


#all veg stuff
veg1 <- read.csv("habitat_info.csv", fileEncoding = 'UTF-8-BOM')









