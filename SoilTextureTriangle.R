##Author:   Mutua John
##Created:  June 3rd 2015

###Create soil texture triangle based on clay, silt and sand soil rasters

##set the working directory
setwd("D:/ToBackup/Data/Projects/SAGCOT/Soil/ISRIC_250m_Sagcot")

##required packages
library("raster")
library("soiltexture")

##create a stack of raster files
s <- stack('af_CLYPPT_T__M_sd1_250m.tif', 'af_SNDPPT_T__M_sd1_250m.tif','af_SLTPPT_T__M_sd1_250m.tif')
names(s) <- c("CLAY","SAND","SILT")
my.data <- as.data.frame(s, na.rm=TRUE)

##change % to 100
my.data <- 100 * my.data / rowSums(my.data)

##Soil texture based on classes of the USDA system / triangle

TT.plot(class.sys = "USDA.TT", tri.data=my.data, class.lab.col=NULL, pch=1, col=12, main="SAGCOT soil texture triangle")

##Return a table of classes of a texture classification system.
TT.classes.tbl(class.sys = "USDA.TT", collapse = NULL)
