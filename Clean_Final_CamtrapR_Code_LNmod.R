#Drivers of Species diversity in a hydro gradient in Picayune#

#working directory
#setwd("E:/R analysis")

#installing the packages
#install.packages("camtrapR")
#install.packages("exiftoolr")
#install.packages("rgdal")
#install.packages("plotKML")
#install.packages("dplyr")

#loading necessary packages
library(camtrapR)
library(exiftoolr)
library(rgdal)
library(plotKML)
library(dplyr)

#here loading in our (partially filled out cam trap station table)
camtraps <- read.csv("Station Info2.csv", fileEncoding = 'UTF-8-BOM')

#Here creating a file pathway to the external harddrive so 
#R knows where to find the processed photos
image_file_path <- file.path("F:/Gamecams")

#I think this is applicable if we were running photos straight from directory
#in exiftools, but as we are pulling data in remotely, I don't think this applies 
wd_images_ID <- system.file("F:/Gamecams", package = "camtrapR") #not sure about this one#

##### recordtable#####
#this is the basis for everything we will need moving forward
#aka the base dataset!

#Before running the CameraOperation function make sure all cells in Station Folder are filled in on the csv#

##Creating camera operation matrix 
# no problems/malfunction (assuming that there are no problems)
?cameraOperation
camop_no_problem <- cameraOperation(CTtable = camtraps,
                                    stationCol = "Station",
                                    setupCol = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    writecsv = FALSE,
                                    hasProblems = FALSE,
                                    dateFormat = "%m/%d/%Y"
)

#Creating camera operation matrix 
# with problems/malfunction *assuming that there are problems
camop_problem <- cameraOperation(CTtable = camtraps,
                                 stationCol = "Station",
                                 setupCol = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv = FALSE,
                                 hasProblems = TRUE,
                                 dateFormat = "%m/%d/%Y"
)

#looking at them both (in this case, we assumed there were no problems
#so only the no problem funciton worked)
camop_no_problem
camop_problem

# load station information
data(camtraps)

#Developing the recordtable using info from the harddrive 
recordtable_allsites <- recordTable(inDir = image_file_path,
                                    IDfrom = "metadata",
                                    metadataSpeciesTag = "Species",) #species ID tag name)
#works!!

#trying again with a more specific version
recordtable_allsites <- recordTable(inDir = image_file_path,
                                    IDfrom = "metadata",
                                    metadataSpeciesTag = "Species",
                                    minDeltaTime = 10,
                                    deltaTimeComparedTo = "lastRecord",
                                    exclude = "Empty", "Person", "Unidentified",
                                    removeDuplicateRecords = TRUE,
                                    returnFileNamesMissingTags = FALSE,) 
??minDeltaTime

###THIS PART IS NOT WORKING!!
#limited record table (trying to take out empty, etc. and limit delta from the beginning)
if (Sys.which("exiftool") != ""){ # only run this function if ExifTool is available
recordtable_allsites_mod <- recordTable(inDir = wd_images_ID,
                                    IDfrom = "directory",
                                    metadataSpeciesTag = "Species", #species ID tag name)
                                    minDeltaTime = 60,
                                    deltaTimeComparedTo = "lastRecord",
                                    exclude = "Empty", "Person", "Unidentified", 
)
}
#ugh still not working...

#shows cumulative breakdown of photos
?view
View(recordtable_allsites)
## WOO It worked!!
#has all sites except FP69 (because this one was being funky!)

library(dplyr)
#to get all the files that are not empty (with stuff in it)
sightings  <- recordtable_allsites %>%
  filter(Species!="Empty", Species!="Person", Species!="Unidentified",)

#Taking into account independent detections (delta.time.secs) <- amount of time in between photos 
#can set to be ie 15 min to make sure sightings are independent
#here setting to a separation of 10 min (to only include sightings with more than 10 min since the last sighting)
##at least I hope thats what I did! (doesn't include argument of deltaTimeComparedTo that would be relevant 
#for recordTable function, but currently can't get that one to work (on line 77))
#deltaindiv <- sightings %>%
 # filter(delta.time.mins>10)
#seems like only BM29, BM50 and FS142 showing up with actual sightnings....
#modifying the deltaT in this way takes out all sightings even if its the only one... so not the best way to do it 


# load record database
data(recordtable_allsites)

#Detection Maps#

#for all species
Maps_all <- detectionMaps(CTtable = camtraps,
                              recordTable = recordtable_allsites,
                              Xcol = "utm_x",
                              Ycol = "utm_y",
                              stationCol = "Station",
                              speciesCol = "Species",
                              writePNG = FALSE,
                              plotR = TRUE,
                              printLabels = TRUE,
                              richnessPlot = TRUE,
                              addLegend = TRUE
)

#this is for delta >10
Mapsall_10 <- detectionMaps(CTtable = camtraps,
                              recordTable = deltaindiv,
                              Xcol = "utm_x",
                              Ycol = "utm_y",
                              stationCol = "Station",
                              speciesCol = "Species",
                              writePNG = FALSE,
                              plotR = TRUE,
                              printLabels = TRUE,
                              richnessPlot = TRUE,
                              addLegend = TRUE
)

str(Maps_all)

#creating diversity dataframe using data
Diversity <- Maps_all[-c(2:3,21)]
Diversity1 <- Diversity[-18,]

summary(Diversity1$Anole)

deer <- Diversity1$Deer
hist(deer)

# create maps - this is just for deer
Mapsdeer <- detectionMaps(CTtable = camtraps,
                          recordTable = recordtable_allsites,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Deer"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)

#map including detla of 10 min to separate out individuals 
Maps_indivs <- detectionMaps(CTtable = camtraps,
                          recordTable = deltaindiv,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Deer"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)
#woo they work, just like not enough data I think...

#map for bears
Mapbears <- detectionMaps(CTtable = camtraps,
                          recordTable = recordtable_allsites,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Bear"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)

#map for hog
Maphog <- detectionMaps(CTtable = camtraps,
                          recordTable = recordtable_allsites,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Hog"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)

#map for turkey
Mapturkey <- detectionMaps(CTtable = camtraps,
                          recordTable = recordtable_allsites,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Turkey"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)

#map for coon
Mapcoon <- detectionMaps(CTtable = camtraps,
                          recordTable = recordtable_allsites,
                          Xcol = "utm_x",
                          Ycol = "utm_y",
                          stationCol = "Station",
                          speciesToShow = c("Racoon"),
                          speciesCol = "Species",
                          writePNG = FALSE,
                          plotR = TRUE,
                          printLabels = TRUE,
                          richnessPlot = TRUE,
                          addLegend = TRUE
)
#found mostly in reference sites for all species maps above
#can't figure out why it isn't putting in values for the number of species found at each site... --> ahhh you literally just have to zoom (plot isn't big enough to see numbers right away)

#####activity Density####
activityDensity(recordTable = sightings,
                allSpecies = TRUE,
                writePNG = FALSE,
                plotR = TRUE,
                add.rug = TRUE)
#throwing error --> datetime column must be a factor or characters 
str(sightings)

#for just deer
activityDensity(recordTable = sightings,
                species = "Deer")


####DetectionHistory####
#for occupancy analysis output
#although is occupancy analysis applicable in this case as observations are not necessarily 
#independent..?

#the example - commented detectionhistory
# detectionHistory(deltaindiv, #the recordTable (in this case using the delta modified)
#                              Deer, #the species for which to compute the detection history
#                              camop_no_problem, #camOp (camera operation matrix) 
#                              #output = c("binary", "count"),
#                              stationCol = "Station",
#                              speciesCol = "Species",
#                              recordDateTimeCol = "DateTimeOriginal",
#                              #recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
#                              occasionLength,#from my research I think this depends on the detectability of specific species
#                              #but not sure how many days that should be..? 
#                              minActiveDaysPerOccasion,
#                              maxNumberDays,
#                              day1,
#                              buffer,
#                              includeEffort = FALSE, #controls whether an additional effort matrix is computer.. 
#                              #if FALSE --> will have NAs for sites when not set up/operational, 
#                              #if TRUE, it will only have 0/1s and no NAs.. 
#                              #"If TRUE, then could be included in occupancy models as a continuous observation
#                              #covariate to estimate the effect of effort on detection probability 
#                              ##don't totally understand that part --> if included wouldn't it be skewing data 
#                              #if sites non operational were regarded as 0s even though it was just non operational..?
#                              scaleEffort = FALSE,
#                              #occasionStartTime = 0, --> can be used to make occasions start another hour other than midnight (ie for nocturnal mammals) 
#                              datesAsOccasionNames = FALSE,
#                              timeZone,
#                              writecsv = FALSE,
#                              outDir,
#                              #unmarkedMultFrameInput #I think this is just for multi-season 
# )
##--> To comment out a bunch of code at once Crtl + Shift + C  YAY!

#now to actually do it! 

#without effort 
DetHist1 <- detectionHistory(recordTable = deltaindiv,
                             camOp = camop_no_problem,
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             species = "Deer",
                             occasionLength = 1, #not sure what to use for this but just using what they had for the 
                             #example...basically I think it is separating each site down into 7 day intervals 
                             #with which to either say whether there was a deer present there or not.
                             #also changed this to 2, 1 to see how it affected the results...
                             day1 = "station",
                             datesAsOccasionNames = FALSE,
                             includeEffort = FALSE,
)
                             
?timeZone

#this is a list with 1 element
DetHist1 

#this is the contained detection/non-detection matrix 
DetHist1$detection_history

#with effort included
DetHist2 <- detectionHistory(recordTable = deltaindiv,
                             camOp = camop_no_problem,
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             species = "Deer",
                             occasionLength = 1,
                             day1 = "station",
                             datesAsOccasionNames = FALSE,
                             includeEffort = TRUE,
                             scaleEffort = FALSE,
)

#detection history
DetHist2$detection_history

#effot
DetHist2$effort
#woo ok worked, but not totally sure how to interpret...



