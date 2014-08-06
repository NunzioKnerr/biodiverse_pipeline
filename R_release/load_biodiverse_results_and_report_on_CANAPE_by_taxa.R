library(tidyr)
library(dplyr) 

#getwd()
source("./R_release/biodiverse_path_reference.R")
biodiverse_group_data <- paste0("./pipeline_test/biodiverse_analysed_output_groups.csv")
observed_data <-  paste0("./pipeline_test/biodiverse_analysed_output_SPATIAL_RESULTS.csv")
randomisation_results <-  paste0("./pipeline_test/biodiverse_analysed_output_rand--SPATIAL_RESULTS.csv")
output_file <- paste0("./pipeline_test/CANAPE_summary_by_taxa.csv")

biodiverse_group_data <- read.table(biodiverse_group_data, header=T,sep=",")
#View(biodiverse_group_data)
biodiverse_group_data_df <- tbl_df(biodiverse_group_data)
#biodiverse_group_data_df

biodiverse_group_data_trans <- biodiverse_group_data_df %>%
  gather(Taxa, record_count, -ELEMENT,-Axis_0, -Axis_1, convert = TRUE) %>% #transform the group export
  mutate(Element = ELEMENT) %>% #transform the group export
  filter(!(is.na(record_count))) %>% #remove the nas
  group_by(ELEMENT) %>% #group by cell element
  arrange(record_count,Taxa)
#biodiverse_group_data_trans

observed_data <- read.table(observed_data, header=T,sep=",")
#View(observed_data)
#observed_data <- subset(observed_data, select=c(-Element, -Axis_0, -Axis_1))
randomisation_results <- read.table(randomisation_results, header=T,sep=",")
#View(randomisation_results)

biodiverse_results_concatenated <- merge(observed_data, randomisation_results, by.x="Element", by.y="ELEMENT")
#biodiverse_results_concatenated <- biodiverse_results_concatenated[!is.na(biodiverse_results_concatenated[,4]),]#trim NA.s
#View(biodiverse_results_concatenated)

###################################

#Standard 2 tailed test for RPD
significance_fun <- function(x){
  if (x >= 0.99) {
    return("Very Highly Sig")
  } else if (x >= 0.975){
    return ("Highly Sig")
  } else if (x <= 0.01){
    return ("Very Sig Low")
  } else if (x <= 0.025){
    return ("Sig Low")
  } else {
    return("Not Sig")
  }
}
#two pass test for RPE
# x=P_PE_WE_P, y=P_PHYLO_RPE_NULL2, z=P_PHYLO_RPE2
significance_super_fun <- function(x, y, z){
  if (x < 0.95 & y < 0.95) {
    return("Not Sig")
  } else if (z <= 0.025){
    return ("Neo")
  } else if (z >= 0.975){
    return ("Palaeo")
  } else if (x >= 0.99 & y >= 0.99){
    return ("Super")
  } else {
    return("Mixed")
  }
}

###################################################################################
#
#Create new columns in dataframe and populate them using the functions above
#
###################################################################################
trait_index <- grep("^P_PHYLO_RPD1$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PHYLO_RPD1_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply the function to every row of column with index "trait_index" and generate a column in the dataframe showing significant cells

trait_index <- grep("^P_PHYLO_RPD2$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PHYLO_RPD2_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply the function to every row of column with index "trait_index" and generate a column in the dataframe showing significant cells

trait_index <- grep("^P_PD_P$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PD_P_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

trait_index <- grep("^P_PE_WE_P$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PE_WE_P_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

trait_index <- grep("^P_PD_P_per_taxon$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PD_P_per_taxon_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

trait_index <- grep("^P_PHYLO_RPE2$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PHYLO_RPE2_ONE_STEP_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply the function to every row of column with index "trait_index" and generate a column in the dataframe showing significant cells

#This uses the 2 pass test to pull out palaeo, neo and super for RPE
biodiverse_results_concatenated$P_PHYLO_RPE1_SIG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL1[x], biodiverse_results_concatenated$P_PHYLO_RPE1[x])) 

biodiverse_results_concatenated$P_PHYLO_RPE2_SIG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL2[x], biodiverse_results_concatenated$P_PHYLO_RPE2[x])) 

#test <- subset(biodiverse_results_concatenated, ,select=c("Element", "P_PE_WE_P", "P_PD_P_SIG", "P_PHYLO_RPD_NULL1", "P_PHYLO_RPD1","P_PHYLO_RPE2_ONE_STEP_SIG", "P_PHYLO_RPE2_SIG"))
# 
#View(test)
#sig_only <- subset(biodiverse_results_concatenated, !(P_PHYLO_RPE2_SIG=="Not Sig"),select=c("Element", "P_PE_WE_P", "P_PD_P_SIG", "P_PHYLO_RPD_NULL1", "P_PHYLO_RPD1","P_PHYLO_RPE2_ONE_STEP_SIG", "P_PHYLO_RPE2_SIG"))
#View(sig_only)


#########################

biodiverse_results_df <- tbl_df(biodiverse_results_concatenated)
#biodiverse_results_df

all_results <- left_join(biodiverse_group_data_trans, biodiverse_results_concatenated)
#all_results
#View(all_results)

neos <- all_results %>%
  filter(P_PHYLO_RPE2_SIG=="Neo") %>%
  #filter(ELEMENT=="1375000:-4275000") %>%
  #arrange(Taxa, desc(record_count)) %>%
  arrange(Element, desc(record_count)) %>%
  group_by(Taxa, Element)
#View(neos)

taxa_by_element <- summarise(neos)#split the taxa by element  
#View(taxa_by_element)  
total_elements <- length(unique(neos$ELEMENT)) #get the total neo cell number
total_elements
in_neos <- summarise(taxa_by_element, neo_count = n(), in_neo_pcnt = ((n() / total_elements)*100)) %>%
arrange(desc(in_neo_pcnt))  
#View(in_neo)

palaeos <- all_results %>%
  filter(P_PHYLO_RPE2_SIG=="Palaeo") %>%
  arrange(Element, desc(record_count)) %>%
  group_by(Taxa, Element)
palaeos

taxa_by_element <- summarise(palaeos)#split the taxa by element  
#View(taxa_by_element)  
total_elements <- length(unique(palaeos$ELEMENT)) #get the total neo cell number
total_elements
in_palaeos <- summarise(taxa_by_element, palaeo_count = n(), in_palaeo_pcnt = ((n() / total_elements)*100)) %>%
arrange(desc(in_palaeo_pcnt))
#View(in_palaeos)

supers <- all_results %>%
  filter(P_PHYLO_RPE2_SIG=="Super") %>%
  arrange(Element, desc(record_count)) %>%
  group_by(Taxa, Element)
supers

taxa_by_element <- summarise(supers)#split the taxa by element  
#View(taxa_by_element)  
total_elements <- length(unique(supers$ELEMENT)) #get the total neo cell number
total_elements
in_supers <- summarise(taxa_by_element, super_count = n(), in_supers_pcnt = ((n() / total_elements)*100)) %>%
arrange(desc(in_supers_pcnt))
#View(in_supers)

mixed <- all_results %>%
  filter(P_PHYLO_RPE2_SIG=="Mixed") %>%
  arrange(Element, desc(record_count)) %>%
  group_by(Taxa, Element)
mixed

taxa_by_element <- summarise(mixed)#split the taxa by element  
#View(taxa_by_element)  
total_elements <- length(unique(mixed$ELEMENT)) #get the total neo cell number
total_elements
in_mixed <- summarise(taxa_by_element, mixed_count = n(), in_mixed_pcnt = ((n() / total_elements)*100)) %>%
  arrange(desc(in_mixed_pcnt))
#View(in_mixed)

all_sig <- all_results %>%
  filter(P_PHYLO_RPE2_SIG %in% c("Mixed", "Super", "Neo", "Palaeo")) %>%
  arrange(Element, desc(record_count)) %>%
  group_by(Taxa, Element)
all_sig
#View(all_sig)

taxa_by_element <- summarise(all_sig)#split the taxa by element  
#View(taxa_by_element)  
total_elements <- length(unique(all_sig$ELEMENT)) #get the total neo cell number
total_elements
in_all_sig <- summarise(taxa_by_element, all_sig_count = n(), in_all_sig_pcnt = ((n() / total_elements)*100)) %>%
  #mutate(boolean_all = ifelse(in_all_sig_pcnt > 40, "gt 40", "lt 40")) %>%
  arrange(desc(in_all_sig_pcnt))
#View(in_all_sig)

all_taxa <- all_results %>%
  group_by(Taxa) 
#all_taxa

by_taxa <- summarise(all_taxa, total_sample_count = n())

CANAPE_summary <- merge(by_taxa, in_all_sig ,by.x="Taxa",by.y="Taxa",all.x=T)
CANAPE_summary <- merge(CANAPE_summary, in_neos ,by.x="Taxa",by.y="Taxa",all.x=T)
CANAPE_summary <- merge(CANAPE_summary, in_mixed ,by.x="Taxa",by.y="Taxa",all.x=T)
CANAPE_summary <- merge(CANAPE_summary, in_supers ,by.x="Taxa",by.y="Taxa",all.x=T)
CANAPE_summary <- merge(CANAPE_summary, in_palaeos ,by.x="Taxa",by.y="Taxa",all.x=T)
#View(CANAPE_summary)

write.csv(CANAPE_summary, file=output_file,  row.names = FALSE)
