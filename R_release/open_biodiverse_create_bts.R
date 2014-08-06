##########################################################################################################
#
#This script uses a perl script to open biodiverse, load in a tree file and save out the biodiverse tree file for use in other scripts.
#Inputs:
#input_tree_file: the tree file to use
#out_file: the full path and name of the tree file to save including (.bts)
#
#
#Nunzio.Knerr@csiro.au
#Date:26/05/2014
#
##########################################################################################################
input_tree_file <- paste0("./pipeline_test/tree.nex")
out_file <- paste0("./pipeline_test/tree.bts")
#
source("./R_release/biodiverse_path_reference.R")

cmd <- paste ("perl ", biodiverse_pipeline_install_folder, "perl/create_bts.pl", sep="")
cmd <- paste (cmd, "--input_tree_file", input_tree_file, "--out_file", out_file)

system(cmd, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 
