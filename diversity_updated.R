#preliminary analysis of gamecam data 

#reading in CSVs
#diversity 
sbs <- read.csv("Diversity3.csv", header=TRUE)
sbs1 <- read.csv("Diversity4.csv", header=TRUE)

#percentage by day
#nope sbs2 <- sbs1[(4:18)/19]
sbs_percents <- read.csv("Diversity_percents.csv", header=TRUE)

#average sightings by day between restoration categories to then compare between categories?

####species accumulation curve####



#Great guide -->> http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

#reading in CSV
diversity <- read.csv("Diversity2.csv")

#reading in gamecam site data
habitat <- read.csv("gamecam_sites1.csv")
#was reading in as an excel format first, needed to convert to CSV

#loading ggplot2 
library(ggplot2)

#to summarize deer by rest category
tapply(sbs_percents$Deer, sbs_percents$rest_cat, FUN=sum)
deer <- tapply(sbs_percents$Deer, sbs_percents$rest_cat, FUN=sum)

#turning deer into a dataframe
deer1 <- as.data.frame(deer)

#adding column name back in -=> not working (subscript out of bounds..?0)
colnames(deer)[] <- "rest_cat"

#going to remake this tiny dataframe just bc
deer2 <- data.frame(
  rest_cat = rep(c("unrestored", "partial_rest", "restored", "reference"), each = 1),
  count = c(2.166667,2.079545,4.469441,12.595956)
)

deerplot <- ggplot(deer2, aes(x=rest_cat, y=count)) +
  geom_bar(stat="identity", width = .5, color= "blue", fill="steelblue")
deerplot

#if we were to do stacked plots (for all animal species!)
#diversity <- ggplot(diversity2?, aes(x= species, y = count))+
# geom_col(aes(fill=rest_cat)m width = 0.7)

#to summarize all by rest category...
library(dplyr)
#tapply(sbs_percents, sbs_percents$rest_cat, FUN=sum)

#another try
#diversity %>%
#  group_by(rest_cat) %>%
#  summarise(n = n())
#argg no this is all for long-form data!! D:

#hmm going to start by filtering for the restoration
#categories 
unrest <- filter(sbs_percents, rest_cat == "unrestored")
partial <- filter(sbs_percents, rest_cat == "partial_rest")
restored <- filter(sbs_percents, rest_cat == "restored")
reference <- filter(sbs_percents, rest_cat == "reference")


#summarizing restoration categories 
unrest1 <- colSums(unrest[4:17])
unrest2 <- as.data.frame(unrest1)

partial1 <- colSums(partial[4:17])
partial2 <- as.data.frame(partial1)


restored1 <- colSums(restored[4:17])
restored2 <- as.data.frame(restored1)

reference1 <- colSums(reference[4:17])
reference2 <- as.data.frame(reference1)

#binding together all categories 
all_cats <- cbind(unrest2, partial2, restored2, reference2)
str(all_cats)
all_cats1 <- as.matrix(all_cats)
#woo it worked

#now could try to do a stacked bar
#trying not ggplot...
barplot(all_cats1, main = "Counts of wildlife by restoration category",
        xlab = "number of visits",
        col=c("darkblue", "red", "purple", "green", legend = colnames(all_cats1)))

#and in ggplot --- arg really meant for longform also..? 
#ggplot(all_cats1, aes(fill=))




###### DIVERSITY STUFF!! #####
#basically the idea with diversity indices is to do 
#a number of different ones as they all tell slightly 
#different things about the sites, and ideally they would
#all support the same story about the relationship between sites

##vegan stuff 
library(vegan)
data(BCI)

#Shannon Index example
H <- diversity(BCI)
H

#our stuff for shannon index 
#transforming code (swapping rows and columns)
all_cats2 <- t(all_cats)
#jk we also want by site so using the original diversity dataset,
#need to take out non-numeric columns (site names and rest categories)
#diversity1 <- (diversity[,-(1:3 )])
#diversity1 <- (diversity,(-c("X", "Station", "rest_cat"))
#also taking out people, unidentified and empty
diversity1 <- diversity[, -which(names(diversity) %in% c("X", "Station", "rest_cat", "Empty", "Person", "Unidentified"))]      

#our shannon index (which is finding the diversity indices for all sites)
H1 <- diversity(sbs_percents[4:17])
H1
h2 <- as.matrix(H1)

H3 <- diversity(all_cats2)
h3 <- as.matrix(H3)
#nice so this compares the average diversity between restoration categories!
#(could also maybe look at across habitat differences...!)

#Pielou's evenness (between sites) 
J <- H1/log(specnumber(),)
J
j2 <- as.matrix(J)

#to find number of species (species richness of all sites)
alpha_d <- specnumber(sbs_percents)
alpha_d
alpha_d1 <- as.matrix(alpha_d)

#now to compare species richness between restoration categories
alpha_r <- specnumber(all_cats2)
alpha_r
alpha_r1 <- as.matrix(alpha_r)

barchart(alpha_r1, xlab = "Total species richness")
#nice :)

#species accumulation curve (can help you determine how many sites you would need to have all the 
#species)... (maybe just a side thing...)
# but here you need to enter counts of individuals (since the entries for sbs1 is total number of pictures
#that really throws this number off....) 
specaccum(sbs1[4:17],"rarefaction", permutations = 100, conditioned = TRUE, gamma = "jack1", w = NULL,)


#to find Renyi's diversities?

#alpha parameter of Fisher's log-series
#alpha <- fisher.alpha(diversity1) hmm did not work..

#next we could put all of these into a table to findbaverages
#for each restoration category and compare across them?
diversity2 <- cbind(diversity$rest_cat, h2, j2, alpha_d1)
diversity3 <- as.data.frame(diversity2)

#filtering for only the unrestored sites to average their diversity indices
unrest_div <- filter(diversity3, V1 == "unrestored")

#filtering for only the partially restored sites to average their diversity indices
partial_div <- filter(diversity3, V1 == "partial_rest")

#filtering for only the restored sites to average their diversity indices
rest_div <- filter(diversity3, V1 == "restored")

#filtering for only the reference sites to average their diversity indices
reference_div <- filter(diversity3, V1 == "reference")

#now can take averages of these diversity indices for each of the restoration
#categories 
#oof this is doing it manually... but here goes
#need to convert values to be numeric

###for UNRESTORED
#converting shannon's index to be numeric and taking the mean
#for unrestored
unrest_div$V2 <- as.numeric(unrest_div$V2)
avg_shannon_unrest <- mean(unrest_div$V2) #V2 = Shannon's Index 
avg_shannon_unrest 
# 0.4626993 
#without people, etc. --> 0.182284

#converting Pielou's evenness to be numeric and taking the mean
#for unrestored
unrest_div$V3 <- as.numeric(unrest_div$V3)
avg_even_unrest <- mean(unrest_div$V3) #V3 Pielou's evenness (between sites) 
avg_even_unrest
# 0.3476282
#without people, etc --> NaN

#converting species richness to be numeric and taking the mean
#for unrestored
unrest_div$V4 <- as.numeric(unrest_div$V4)
avg_unrest_sp_richness <- mean(unrest_div$V4) 
avg_unrest_sp_richness 
# 3.333333
#without people, etc. --> 1

###for PARTIALLY RESTORED
#converting shannon's index to be numeric and taking the mean
#for  partially restored
partial_div$V2 <- as.numeric(partial_div$V2)
avg_shannon_partial <- mean(partial_div$V2) #V2 = Shannon's Index 
avg_shannon_partial
# 0.5181876 
#without people etc --> 0.09483478

#converting Pielou's evenness to be numeric and taking the mean
#for  partially restored
partial_div$V3 <- as.numeric(partial_div$V3)
avg_evenness_partial <- mean(partial_div$V3) #V3 Pielou's evenness (between sites) 
avg_evenness_partial
# 0.3061199
#without people, etc, --> NaN  :()

#converting species richness to be numeric and taking the mean
#for partially restored
partial_div$V4 <- as.numeric(partial_div$V4)
avg_partial_sp_richness <- mean(partial_div$V4) 
avg_partial_sp_richness
# 5.5

###for  RESTORED
#converting shannon's index to be numeric and taking the mean
#for restored
rest_div$V2 <- as.numeric(rest_div$V2)
avg_shannon_restored <- mean(rest_div$V2) #V2 = Shannon's Index 
avg_shannon_restored
# 1.16492 
#without people --> 0.4763486

#converting Pielou's evenness to be numeric and taking the mean
#for restored
rest_div$V3 <- as.numeric(rest_div$V3)
avg_even_restored <- mean(rest_div$V3) #V3 Pielou's evenness (between sites) 
avg_even_restored
# 0.6761512
# without people --> NaN

#converting species richness to be numeric and taking the mean
#for restored
rest_div$V4 <- as.numeric(rest_div$V4)
avg_rest_sp_richness <- mean(rest_div$V4) 
avg_rest_sp_richness
# 6
#without people, etc. --> 2.4


###for REFERENCE
#converting shannon's index to be numeric and taking the mean
#for reference
reference_div$V2 <- as.numeric(reference_div$V2)
avg_shannon_ref <- mean(reference_div$V2) #V2 = Shannon's Index 
avg_shannon_ref
# 0.766422
#without people, etc. --> 0.4578748

#converting Pielou's evenness to be numeric and taking the mean
#for reference
reference_div$V3 <- as.numeric(reference_div$V3)
avg_even_ref <- mean(reference_div$V3) #V3 Pielou's evenness (between sites) 
avg_even_ref
# 0.3860935
#without people, etc. --> NaN

#converting species richness to be numeric and taking the mean
#for reference
reference_div$V4 <- as.numeric(reference_div$V4)
avg_ref_sp_richness <- mean(reference_div$V4) 
avg_ref_sp_richness
# 6.428571
#without people, etc. --> 3

#now could do a one way ANOVA to see if these differences in diversity 
#are significant?
#so I think it takes the averages for you with anova..?!!
#YES you need to input all of the data because it is 
#testing for variance!


#also put them in a nice table :)
