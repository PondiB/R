library("knitr")
setwd("D:/ToBackup/Scripts/R")
file.exists("MODIS_TimeSeriesProcessor.Rmd")
purl("MODIS_TimeSeriesProcessor.Rmd")

setwd("D:/ToBackup/Scripts/R")
file.exists("LULCClassificationProcessor.Rmd")
purl("LULCClassificationProcessor.Rmd")

setwd("D:/ToBackup/Scripts/R")
file.exists("BushDensityProcessor.Rmd")
purl("BushDensityProcessor.Rmd")
