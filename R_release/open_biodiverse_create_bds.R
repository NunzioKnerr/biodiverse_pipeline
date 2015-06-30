##########################################################################################################
# RUN THIS SCRIPT 3rd
#
#This script uses a perl script to open biodiverse, load in a data file and save out the biodiverse file so analyses can be run.
#Inputs:
#csv_file: the file conating the spatial data to load in
#out_file: the full path to the biodiverse basedata file (.bds) that should be generated.
#label_column_number: the column number for the labels, typically the taxon name
#group_column_number_x: the x co-ordinate column, could be latitude or metres
#group_column_number_y: the y co-ordinate column, could be longitude or metres
#cell_size_x: the x size of the group cell (in the same unit as the x co-ordinates)
#cell_size_y: the y size of the group cell (in the same unit as the y co-ordinates)
#
#
#Nunzio.Knerr@csiro.au
#updated Date:24/06/2015
#
##########################################################################################################
#
csv_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.csv")
out_file <-  paste(substr(csv_file,1,nchar(csv_file)-4), ".bds",sep="")# auto replace last 4 characters with new extension
#out_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.bds") #if you want to specify name manually, uncomment this
label_column_number <- 1 #column number that the label is in
group_column_number_x <- 9 #column number that the x group is in
group_column_number_y <- 10 #column number that the y group is in
cell_size_x <- "100000" #cell size x axis, in units of the data, 100000 metres in this example
cell_size_y <- "100000" #cell size y axis, in units of the data, 100000 metres in this example
#
###### do not edit below #########
#  Should be set on system, but useful for portabilty to other systems
#Sys.setenv(PATH="C:/strawberry/perl/bin;c:/strawberry/c/bin;%PATH%") #set path to srawberryperl for 64 bit version
#Sys.setenv(PERL5LIB="C:/biodiverse/lib")
source("./R_release/biodiverse_path_reference.R")

label_column_number <- as.character(eval(label_column_number-1)) #perl is zero based so minus one from number
group_column_number_x <- as.character(eval(group_column_number_x-1)) #perl is zero based so minus one from number
group_column_number_y <- as.character(eval(group_column_number_y-1)) #perl is zero based so minus one from number
cell_size_x = as.character(cell_size_x)
cell_size_y = as.character(cell_size_y)

cmd <- paste("perl ", biodiverse_pipeline_install_folder, "perl/create_bds.pl", sep="")
cmd <- paste (cmd, "--csv_file", shQuote(csv_file),  "--out_file", shQuote(out_file), "--label_column_number", shQuote(label_column_number), "--group_column_number_x", shQuote(group_column_number_x), "--group_column_number_y", shQuote(group_column_number_y), "--cell_size_x", shQuote(cell_size_x), "--cell_size_y", shQuote(cell_size_y))

system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 

