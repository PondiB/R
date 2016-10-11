#########################################
# This script conducts a land-cover classification of a 6-band Landsat 8 image
# author John Mutua; j.y.mutua@cgiar.org
########################################

# workspace clearance
rm(list = ls(all = TRUE))

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# load packages
library(rgdal)
library(raster)
library(caret)

#to enable reproducing of results
set.seed(500)

## set working directory
setwd("C:/Users/jymutua/Documents/LDN_Workshop/Sample_dataset/Land_Use_Land_Cover")

#import the Landsat image into R as a RasterBrick
#replace the original band names (e.g., ‘X485.0.Nanometers’) with shorter ones (‘B1’ to ‘B7’)
img <- brick("TOA/Otji_TOA.tif")
names(img) <- c(paste0("B", 2:7, coll = "")) 

#make a natural colour visualization of the Landsat image in R using the plotRGB command, 
#for example, a natural colour composite 4:3:2  (Red - Green - Blue)
plotRGB(img * (img >= 0), r = 4, g = 3, b = 2, scale = 10000)

#import the training data into R as an object of class SpatialPolygonsDataFrame and create a variable to store the name of the ‘class’ column:
#codes used in the training data, forest and Woodland classes later combined to Forest/woodland: 
#1-Forest, 2-Woodland, 31-Bushland, 32-Grassland, 42-Cultivated area, 51-Wetland, 52-Water body, 61-Artificial Surface, 71-Bareland
trainData <- shapefile("Training_data/Otji_trainingData.shp")
responseCol <- "LC_Code"

#extract the pixel values in the training areas for every band in the Landsat image and store them in a data frame
#along with the corresponding land cover class id
#extract training data using both point and polygon shapefiles
trainSet = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(category))
    trainSet <- rbind(trainSet, dataSet)
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    trainSet <- rbind(trainSet, df)
  }
}

#trainSet data.frame contains the values for each of six Landsat TOA bands plus the class attribute
#parttion the data into training and testing
inData <- createDataPartition(y = trainSet$class, p = 0.7, list = FALSE)
training <- trainSet[inData,]
testing <- trainSet[-inData,]

#some background checking to ensure everything is ok
table(training$class)
table(testing$class)

#for training the model we will use 1000 observations which are randomly sample from the training data.frame.
train_sample <- training[sample(1:nrow(training), 1000), ]
table(train_sample$class)

#train the RF model
rf_Otji <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data =  train_sample)

#predict the model
beginCluster()
prediction <- clusterR(img, raster::predict, args = list(model = rf_Otji))
endCluster()

#let’s better work with the testing set as it is an independent set of data 
#and let’s examine the producer’s accuracy for the model
prediction_2 <- predict(rf_Otji, testing)
confusionMatrix(prediction_2, testing$class)$overall[1]

#save your output
writeRaster(prediction, "Otji_classified.tif", overwrite=TRUE)

#plot the results
#recall: 1-Forest, 2-Woodland, 31-Bushland, 32-Grassland, 42-Cultivated area, 51-Wetland, 52-Water body, 61-Artificial Surface, 71-Bareland
cols <- c("dark green", "forestgreen", "yellow", "magenta", "blue", "red")
plot(prediction, col=cols, legend=FALSE)
legend("bottomright", 
       legend=c("Forest", "Woodland", "Bushland", "Grassland", "Water body", "Bare land"), 
       fill=cols, bg="white")

# Calculate processing time
timeDiff <- Sys.time() - startTime
cat("\nProcessing time", format(timeDiff), "\n")
