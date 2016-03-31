##########################################################################################################
# RUN THIS SCRIPT 8th
#
#
#This script uses a perl script to open biodiverse file and output results as csv.
#Inputs:
#input_bds_file: The biodiverse file (.bds) to load and export results from
#output_csv_prefix: the text to add to the begingin of the csv file name
#
#
#Nunzio.Knerr@csiro.au
#Date:1/07/2015
#
##########################################################################################################
#
source("./R_release/biodiverse_path_reference.R")

cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/load_bds_and_export_results.pl", sep="")
input_bds_file <-  paste0("./pipeline_test/test_hornworts_epsg_3577_trimmed_analysed.bds")
output_csv_prefix <-  paste0("./pipeline_test/test_hornworts_epsg_3577_trimmed_analysed_output")

###### do not edit below #########
cmd <- paste(cmd, "--input_bds_file", shQuote(input_bds_file), "--output_csv_prefix", shQuote(output_csv_prefix), sep =" ")
              
print(cmd)
system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
