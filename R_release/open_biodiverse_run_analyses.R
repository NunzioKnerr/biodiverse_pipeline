##########################################################################################################
#
#This script uses a perl script to open biodiverse file and run the analyses you want.
#Inputs:
#input_bds_file: The full path to the biodiverse file to use (.bds)
#input_bds_file: The full path to the tree file to use (.bts)
#calcs: A comma delimited list of the calculations/analyses you want to compute.
#       List of calc names can be found on the biodiverse indicies page
#       https://code.google.com/p/biodiverse/wiki/Indices
#       listed as Subroutine: 
#       For example "Subroutine: calc_endemism_central" 
#
#Nunzio.Knerr@csiro.au
#Date:26/05/2014
#
##########################################################################################################
#
source("./R_release/biodiverse_path_reference.R")

input_bds_file <- "./pipeline_test/test_hornworts_trimmed.bds"
input_bts_file <- "./pipeline_test/tree_trimmed.bts"
calcs <- paste("calc_endemism_whole,calc_pd,calc_pe,calc_phylo_rpd1,calc_phylo_rpd2,calc_phylo_rpe1,calc_phylo_rpe2")
#calcs = paste("calc_numeric_label_stats")
cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/run_analyses.pl", sep="")
#
#
###### do not edit below #########
cmd <- paste(cmd, "--input_bds_file", shQuote(input_bds_file), "--input_bts_file", shQuote(input_bts_file), "--calcs", shQuote(calcs), sep =" ")
            
print(cmd)

system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
