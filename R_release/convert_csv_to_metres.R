##########################################################################################################
#This file generates a csv containing x_metres, y_metres columns according to a defined projection
#projections can be found at http://spatialreference.org/
#
#input_file: can contain many columns. must have lat/long columns in decimal degrees
#output_file: the full path to the output file .csv
#long_lat_columns: specify longitude and latitude columns to use 
#newproj: converts to metres according to "newproj" projection
#
#Nunzio.Knerr@csiro.au
#Date: 26/05/2014
# ###### EDIT THESE ###########################
input_csv_file <- paste0("./pipeline_test/test_hornworts.csv") # input file, csv with columns longitude, latitude in decimal dgrees
output_csv_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.csv") # output filename .csv
long_lat_columns <- c("longitude", "latitude")# the names of the longitude and latitude columns go here
###############################################
# other projections can be used 
oldproj <- paste0(" +init=epsg:4326") #this is WGS84 most common used for google earth etc. in decimal degrees
newproj <- paste0(" +init=epsg:3577") #albers equal area projection
#####################################################################################################
library(rgdal) 

locs<-read.csv(input_csv_file,header=T,sep=",")# load in the csv with lat longs
locs <- locs[!is.na(locs[long_lat_columns[[1]]]),]# need to find a better way of removing NA's
locs_sub <- locs[long_lat_columns]#subset - just take the columns defined as Longitude and Latitude at the top 
#View(locs_sub)
coordinates(locs_sub) <- locs_sub[long_lat_columns]#convert to coordinates
proj4string(locs_sub) <- CRS(oldproj)#asign the old projection
convert_to_metres <- spTransform(locs_sub, CRS=CRS(newproj))  # spTransform makes the projection
projection_suffix <- substr(newproj,(nchar(newproj)-8),nchar(newproj))#
projection_suffix <- gsub(":", "_", projection_suffix)
x_name <- paste("x_",projection_suffix, sep="")
y_name <- paste("y_",projection_suffix, sep="")
convert_to_metres<-as.data.frame(convert_to_metres)
#View(convert_to_metres)
convert_to_metres <- convert_to_metres[,3:4] #just get the columns that have converted values
colnames(convert_to_metres) <- c(x_name, y_name)#set the column names
#View(convert_to_metres)
points_in_metres <- cbind(locs, convert_to_metres)# merge the 2 datasets for writing csv
#View(points_in_metres)
write.csv(points_in_metres, file=output_csv_file, row.names = FALSE)#write the file out

print("output file should be at:")
print(output_csv_file)
###################################################################################################
#https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
###################################################################################################