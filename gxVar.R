# GR6 Variables
rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
library("ggplot2")
library("gridExtra")
source("./functions/gxFunctions.R")

##### NVisitsNUVFUVtwoArtifacts ##### 401,408 #################################################
# classes = c("numeric","character","numeric","numeric","numeric","numeric")
# NVisData <- read.table("~/Dropbox/R/NVisitsNUVFUVtwoArtifacts.csv", header=T, sep=",", colClasses=classes)
# names(NVisData)

NVisData <- loadit("NVisitsNUVFUV.csv")

##################################################################################
# Load the data from visits: 2,106,816 rows for 401,408 coadds.
var <- loadit("VisitsNUVFUV.csv")

# Load the data from objects with 30 visits more more (7264?)
IDs30 <- NVisData[,2][NVisData$nVisits>30]

# Load FUV & NUV periods
# Selected only object with BOTH periods where periods are within 10%
goodPeriods <- loadit("Period10Per.csv")
# Extract IDS
goodIDs <- goodPeriods[,1]

#################################################################################
### FUNCTION: WRITETOFILE ALL FILES ############################################# 
#################################################################################
# Write single file

#Extra 807
#IDs <- NVisData[,1]

# IDs <- NVisData[,2][NVisData$nVisits>30]
# # Write data for all IDs
#for (id in IDs)
# {
#  writeToFile(id,var)  
# }

#writeToFile("2534578948691481315",var)

#################################################################################
### FUNCTION: SHOWRAW FOR INDIVIDUAL COADDOBJID ################################# 
#################################################################################
showRaw("3050100368537043532",var)

#################################################################################
### FUNCTION: SHOWPERIOD FOR INDIVIDUAL COADDOBJID ############################## 
#################################################################################
ID = "3050100368537043532"
showPeriod(ID,var)
Period = 1.157450
showPeriod(ID,var,Period)

ID = "3060620495791611952"
source("./functions/gxFunctions.R")
showPeriods(ID,var)

# Write data for all IDs
for (id in goodIDs)
  {
    showPeriods(id,var)
  }

#################################################################################
### FUNCTION: SCOREPERIOD FOR INDIVIDUAL COADDOBJID ############################# 
#################################################################################
ID = "3060620495791616669"

source("./functions/gxFunctions.R")
ScorePeriod(ID)

############################################
### PROJECTIONS ############################ 
############################################
xb <- seq(0,360,60)
yb <- seq(-90,90,30)
mol <- data.frame(lat=NVisData$glat,lon=NVisData$glon,Survey=NVisData$coaddSurvey)
ggplot() + 
  geom_point(aes(mol$lon,mol$lat,colour=mol$Survey)) + 
  opts(axis.title.x="Galactic Longitude",
       axis.title.y="Galactic Latitude",
       title="GALEX GR6 Sky Coverage") + 
  scale_y_continuous(limits=c(-90,90),breaks=yb) + 
  scale_x_continuous(limits=c(0,360),breaks=xb) + 
  coord_map(project="mollweide") + 
  guides(colour = guide_legend(override.aes = list(alpha=1,size = 1))) + 
  theme_bw()




