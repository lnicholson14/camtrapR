#Establishing workflow for extracting hydrodata
#from hydrostations (to then synthesize with DBHydro well data)
#Laura Nicholson

#start by setting the working directory to the external harddrive
#often need to do this by browsing (by clicking on the ... under the refresh symbol)
#but here now that I found it I can copy/paste it from the console
#setwd("F:/Gamecams")

#loading necessary packages
#loading necessary packages
library(camtrapR)
library(exiftoolr)
library(rgdal)
library(plotKML)
library(dplyr)


#Here creating a file pathway to the external harddrive so 
#R knows where to find the processed photos
image_file_path <- file.path("F:/Gamecams")

#here loading in our (partially filled out cam trap station table)
hydros <- read.csv("Hydro_camops.csv", fileEncoding = 'UTF-8-BOM')

#Creating camera operation matrix 
# with problems/malfunction *assuming that there are problems
camop_problem <- cameraOperation(CTtable = hydros,
                                 stationCol = "Station",
                                 setupCol = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv = FALSE,
                                 hasProblems = TRUE,
                                 dateFormat = "%m/%d/%Y"
)

recordtable_allsites <- recordTable(inDir = image_file_path,
                                    IDfrom = "metadata",
                                    metadataSpeciesTag = "Hydrology",)
#maybe need to add in -->  "wet", "<H", "H1", "H2", "H3", "H4", "H5", "H6" ? 
#nope, looks like it is in there because it is hierarchically organized! woo! 
#yay it worked! (although some had records removed because of missing "species metadata tag:)
## especially for the GC sites  --> "metadataSpeciesTag 'Hydrology' not found in image metadata tag 'HierarchicalSubject'."


library(dplyr)
#maybe if we want to limit it to one photo per day?
#could set the delta to something.. but I think that might just get rid of other sightings where is was
#taking photos over a succession of a day... will have to think on this part more 
deltaindiv <- sightings %>%
 filter(delta.time.mins>10)



