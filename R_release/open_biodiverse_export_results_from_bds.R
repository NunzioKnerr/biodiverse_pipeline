##########################################################################################################
#
#This script uses a perl script to open biodiverse file and output results as csv.
#Inputs:
#input_bds_file: The biodiverse file (.bds) to load and export results from
#output_csv_prefix: the text to add to the begingin of the csv file name
#
#
#Nunzio.Knerr@csiro.au
#Date:26/05/2014
#
##########################################################################################################
#
source("./R_release/biodiverse_path_reference.R")

cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/load_bds_and_export_results.pl", sep="")
input_bds_file <- "C:/GIS-Datasets/Asteraceae_Sep_2014/biodiverse_daisies_species_maxent/tree_small_branches/asteraceae_species_maxent_predicted_albers_trimmed_analysed.bds"
output_csv_prefix <-"C:/GIS-Datasets/Asteraceae_Sep_2014/biodiverse_daisies_species_maxent/tree_small_branches/asteraceae_species_maxent_predicted_albers_trimmed_analysed_output"

###### do not edit below #########
cmd <- paste(cmd, "--input_bds_file", shQuote(input_bds_file), "--output_csv_prefix", shQuote(output_csv_prefix), sep =" ")
              
print(cmd)
system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
