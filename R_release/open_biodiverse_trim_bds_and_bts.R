##########################################################################################################
## RUN THIS SCRIPT 5th
#
#This script uses a perl script to open biodiverse file and tree file and trime each to match eachother.
#Inputs:
#input_bds_file = "C:/GIS-Datasets/pipeline_test/test_hornworts.bds"
#input_bts_file = "C:/GIS-Datasets/pipeline_test/tree.bts"
#output_bds_file = "C:/GIS-Datasets/pipeline_test/test_hornworts_trimmed.bds"
#output_bts_file = "C:/GIS-Datasets/pipeline_test/tree_trimmed.bts"
#
#Nunzio.Knerr@csiro.au
#Date:30/05/2015
#
##########################################################################################################
#

source("./R_release/biodiverse_path_reference.R")

cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/trim_bds_and_bts.pl", sep="")
input_bds_file <- paste0("./pipeline_test/test_hornworts_epsg_3577.bds")
input_bts_file <- paste0("./pipeline_test/tree.bts")
output_bds_file <-  paste(substr(input_bds_file,1,nchar(input_bds_file)-4), "_trimmed.bds",sep="")
output_bts_file <- paste(substr(input_bts_file,1,nchar(input_bts_file)-4), "_trimmed.bts",sep="")  
###### do not edit below #########
cmd = paste (cmd, "--input_bds_file", shQuote(input_bds_file ), "--input_bts_file", shQuote(input_bts_file), "--output_bds_file", shQuote(output_bds_file), "--output_bts_file", shQuote(output_bts_file))

system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
