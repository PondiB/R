#########################################
# script extracts NDVI values from MODIS rasters and plots a time series by season and month
# script reads preprocessed MODIS rasters
# set path to the working directory
# set path to the NDVI images
# define seasons by month
# author John Mutua; j.y.mutua@cgiar.org
########################################

# workspace clearance
rm(list = ls(all = TRUE))

## ----load-libraries----
# load packages
require(raster) #working with rasters#
require(rgdal) #working with geospatial data#
require(ggplot2) #creating graphs#
require(dplyr) #subsetting by season#
require(lubridate) #working with dates#
require(grid) #arranging plots#
require(gridExtra) #arranging plots#

## set working directory
setwd("D:/ToBackup/Data/Regional/LDN_Namibia")

## ----import-NDVI-rasters----
rasList <- list.files("NDVI_Modis/AOI_NDVI", full.names = TRUE, pattern = ".tif$")

# create a raster stack
NDVI_Otjo_stack <- stack(rasList) 

# insert a Temporal id
oldnames<-names(NDVI_Otjo_stack)
head(oldnames)
# characters corresponding to the year are located between the 20 and 23 position
year<-substr(oldnames,20,23)
table(year)

# characters corresponding to the date are located between the 24 and 26 position
julianDay<-substr(oldnames,24,26)

# calculate mean NDVI for each raster and convert output array to a data frame
avg_NDVI_Otjo <- as.data.frame(cellStats(NDVI_Otjo_stack,mean))

names(avg_NDVI_Otjo)

# view only the value in row 1, column 1 of the data frame
avg_NDVI_Otjo[1,1]

# view column name slot
names(avg_NDVI_Otjo)

# rename the NDVI column
names(avg_NDVI_Otjo) <- "meanNDVI"

# view clean column name 
names(avg_NDVI_Otjo)

# add julianDay values as a column in the data frame
avg_NDVI_Otjo$julianDay <- julianDay

# what class is the new column
class(avg_NDVI_Otjo$julianDay)

# create a time vector with the date
tVector<-paste(year,julianDay,sep="-")

# convert the vector to a date
timeNDVI<-as.Date(tVector,format = "%Y-%j")
head(timeNDVI)

# add date values as a column in the data frame
avg_NDVI_Otjo$date <- timeNDVI

# Classes of the two columns now? 
class(avg_NDVI_Otjo$date)
class(avg_NDVI_Otjo$julianDay)

# add a site column to our data
avg_NDVI_Otjo$site <- "Otjozondjupa Region"

# view data
head(avg_NDVI_Otjo)

# add a "year" column to our data
avg_NDVI_Otjo$year <- year

# view data
avg_NDVI_Otjo

## ----plot-time-series----
# plot NDVI by time although this doesn't make sense

ggplot(avg_NDVI_Otjo, aes(year, meanNDVI), na.rm=TRUE) +
  geom_point(size=4,colour = "PeachPuff4") + 
  ggtitle("MODIS Derived NDVI - Otjozondjupa Region") +
  xlab("Time") + ylab("Mean NDVI") +
  theme(text = element_text(size=20))

## write NDVI data to a .csv File
# create new df to prevent changes to avg_NDVI_Otjo
Otjo_NDVI_Values<-avg_NDVI_Otjo

# drop the row.names column 
row.names(Otjo_NDVI_Values)<-NULL

# check data frame
head(Otjo_NDVI_Values)

# create a .csv of mean NDVI values 
write.csv(Otjo_NDVI_Values, file="meanNDVI_Otjo_2005-2015.csv")

# add month to data frame
avg_NDVI_Otjo$month  <- month(avg_NDVI_Otjo$date)

# view head and tail of column
head(avg_NDVI_Otjo$month)
tail(avg_NDVI_Otjo$month)

# subset by data by Season
# create a new categorical variable called season by grouping months together.
avg_NDVI_Otjo_Seasons <- avg_NDVI_Otjo %>% 
  mutate(season = 
           ifelse(month %in% c(12, 1, 2, 3, 4, 5), "Hot-Wet",
                  ifelse(month %in% c(6, 7, 8), "Cool-Dry",
                         ifelse(month %in% c(9, 10, 11), "Hot-Dry", "Error"))))

# check to see if this worked
head(avg_NDVI_Otjo_Seasons$month)
head(avg_NDVI_Otjo_Seasons$season)
tail(avg_NDVI_Otjo_Seasons$month)
tail(avg_NDVI_Otjo_Seasons$season)

######################################
###-----------MONTHLY PLOTS--------###
######################################

# aggregate data by month
monthNDVI<-avg_NDVI_Otjo_Seasons %>%
  group_by(month) %>%
  summarise(monthNDVI=mean(meanNDVI, na.rm=TRUE))

# convert month numeric to month abbrevaition
monthNDVI$month_name <- month.abb[monthNDVI$month]

# reassign the month_name field to a factor
monthNDVI$month_name = factor(monthNDVI$month_name,
                              levels=c('Jan','Feb','Mar',
                                       'Apr','May','Jun','Jul',
                                       'Aug','Sep','Oct',
                                       'Nov','Dec'))

head(monthNDVI)

# plot data by month
Monthly_NDVI_Plot<-ggplot(monthNDVI, aes(month_name, monthNDVI, group=4)) +
  geom_line(colour="red") + 
  ggtitle("Average NDVI (2005-2015) - Monthly") +
  xlab("Month") + ylab("Mean NDVI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18)) + geom_point()
Monthly_NDVI_Plot

# plot the same plot as before but with one plot per season, save as .pdf
ggsave(file="Otjozondjupa_Monthly_NDVI.pdf", width = 297, height = 210, units = "mm")

###----------INDIVINDUAL SEASONAL PLOTS------------###
### HOT-WET Season
Hot_Wet <- subset(monthNDVI, month >= 12 | month <= 5)
target <- c("12", "5", "4", "3", "2", "1")
Hot_Wet<-Hot_Wet[match(target, Hot_Wet$month),]

Hot_Wet$month_name <- factor(Hot_Wet$month_name, c("Dec", "Jan", "Feb", "Mar", "Apr", "May"))

Hot_Wet_NDVI_Plot<-ggplot(Hot_Wet, aes(month_name, monthNDVI, group=1)) +
  geom_line(colour="red") +
  ggtitle("Average NDVI (2005-2015) - Hot Wet Season ") +
  xlab("Month") + ylab("Mean NDVI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18)) + geom_point()
Hot_Wet_NDVI_Plot

# plot the same plot as before but with one plot per season, save as .pdf
ggsave(file="Otjozondjupa_Hot_Wet_NDVI.pdf", width = 297, height = 210, units = "mm")

### COOL-DRY Season
Cool_Dry <- subset(monthNDVI, month <= 8 & month >= 6)

Cool_Dry_NDVI_Plot<-ggplot(Cool_Dry, aes(month_name, monthNDVI, group=1)) +
  geom_line(colour="red") +
  ggtitle("Average NDVI (2005-2015) - Cool Dry Season") +
  xlab("Month") + ylab("Mean NDVI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18)) + geom_point()
Cool_Dry_NDVI_Plot

# plot the same plot as before but with one plot per season, save as .pdf
ggsave(file="Otjozondjupa_Cool_Dry_NDVI.pdf", width = 297, height = 210, units = "mm")

### HOT DRY Season
Hot_Dry <- subset(monthNDVI, month <= 11 & month >= 9)

Hot_Dry_NDVI_Plot<-ggplot(Hot_Dry, aes(month_name, monthNDVI, group=1)) +
  geom_line(colour="red") +
  ggtitle("Average NDVI (2005-2015) - Hot Dry Season") +
  xlab("Month") + ylab("Mean NDVI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18)) + geom_point()
Hot_Dry_NDVI_Plot

# plot the same plot as before but with one plot per season, save as .pdf
ggsave(file="Otjozondjupa_Hot_Dry_NDVI.pdf", width = 297, height = 210, units = "mm")

##-----------END OF MONTHLY PLOTS--------

######################################
##-----------SEASONAL PLOTS--------
######################################

# aggregate data by season
seasonNDVI<-avg_NDVI_Otjo_Seasons %>%
  group_by(year, season) %>%
  summarise(seasonNDVI=mean(meanNDVI, na.rm=TRUE))

# create a .csv of seasonal NDVI values 
write.csv(seasonNDVI, file="Seasonal_NDVI_2005-2015.csv")

# plot data by season
Seasonal_NDVI_Plot<-ggplot(seasonNDVI, aes(year, seasonNDVI, group=3)) +
  geom_line(colour="red") +
  ggtitle("Average NDVI (2005-2015) - Seasonal") +
  xlab("Year") + ylab("Mean NDVI") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18)) + geom_point()

# plot the same plot as before but with one plot per season, save as .pdf
Seasonal_NDVI_Plot + facet_grid(. ~ season)
ggsave(file="Otjozondjupa_Seasonal_NDVI.pdf", width = 297, height = 210, units = "mm")

# plot the same plot in a landscape orientation, save as .pdf
Seasonal_NDVI_Plot + facet_grid(season ~ .)
ggsave(file="Otjozondjupa_Seasonal_NDVI.pdf", width = 297, height = 210, units = "mm")
