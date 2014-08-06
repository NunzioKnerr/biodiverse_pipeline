##########################################################################################################
#
#This script uses a perl script to open biodiverse file and tree file and trime each to match eachother.
#Inputs:
#input_bds_file = "C:/GIS-Datasets/pipeline_test/test_hornworts.bds"
#input_bts_file = "C:/GIS-Datasets/pipeline_test/tree.bts"
#output_bds_file = "C:/GIS-Datasets/pipeline_test/test_hornworts_trimmed.bds"
#output_bts_file = "C:/GIS-Datasets/pipeline_test/tree_trimmed.bts"
#
#Nunzio.Knerr@csiro.au
#Date:26/05/2014
#
##########################################################################################################
#
source("./R_release/biodiverse_path_reference.R")

cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/trim_bds_and_bts.pl", sep="")
input_bds_file <- "./pipeline_test/test_hornworts_epsg_3577.bds"
input_bts_file <- "./pipeline_test/tree.bts"
output_bds_file <- "./pipeline_test/test_hornworts_trimmed.bds"
output_bts_file <- "./pipeline_test/tree_trimmed.bts"

###### do not edit below #########
cmd = paste (cmd, "--input_bds_file", shQuote(input_bds_file ), "--input_bts_file", shQuote(input_bts_file), "--output_bds_file", shQuote(output_bds_file), "--output_bts_file", shQuote(output_bts_file))

system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
