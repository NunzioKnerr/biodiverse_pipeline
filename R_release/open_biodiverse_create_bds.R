##########################################################################################################
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
#Date:26/05/2014
#
##########################################################################################################
#
csv_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.csv")
out_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.bds")
label_column_number <- paste0(1) 
group_column_number_x <- paste0(9) 
group_column_number_y <- paste0(10)
cell_size_x <- paste0(100000)
cell_size_y <- paste0(100000)
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

