library(sp)    
library(maptools) 
library(raster)
#library(RColorBrewer)
library(grid)
library(ggplot2)
library(gridExtra)
library(Cairo)
library(extrafont)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

myFont <- choose_font(c("HelvLight", "Arial", "sans"), quiet = TRUE)#load a font if available

data_dir <- "C:/GIS-Datasets/Australian_Genera_Angiosperms_October_2014/"
output_folder <- data_dir
observed_data <- paste0(data_dir, "Australian_genera_angiosperms_biodiverse_analysed_output_SPATIAL_RESULTS.csv")
randomisation_results  <- paste0(data_dir, "Australian_genera_angiosperms_biodiverse_analysed_output_rand--SPATIAL_RESULTS.csv")
map_shape_file <- paste0("C:/biodiverse_pipeline/shape_files/coastline_albers.shp")


observed_data <- read.table(observed_data, header=T,sep=",")
randomisation_results <- read.table(randomisation_results, header=T,sep=",")
biodiverse_results_concatenated <- cbind(observed_data, randomisation_results)
#biodiverse_results_concatenated<- biodiverse_results_concatenated[!is.na(biodiverse_results_concatenated[,4]),]#trim NA.s
#View(biodiverse_results_concatenated)

map_shape_file <- paste0("C:/GIS-Datasets/acacia_nature_paper_feb_2014/maps_plot/coastline_albers.shp")
map_data <- readShapePoly(map_shape_file)
map_extent <- extent(map_data)

#############################################################################
# Functions for populating new columns with text of significance
#############################################################################
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
#Create new columns in dataframe and populate them using the functions above
###################################################################################

#  SWL:  not sure why RPE2 is in this list when it is also handled below
targets <- c("PHYLO_RPD1", "PHYLO_RPD2", "PD_P", "PE_WE_P", "PD_P_per_taxon")

for (name in targets) {
  colname <- paste0("P_", name)  #  prepend the P_ since we want the proportions, saves some typing above
  #print(colname)
  new_colname = paste0(colname, "_SIG")
  trait_index <- match (colname, colnames(biodiverse_results_concatenated))
  # Apply the function to every row of column with index "trait_index" 
  #  and generate a column in the dataframe showing significant cells
  biodiverse_results_concatenated[[new_colname]] <- apply (biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) 
}

write.csv(biodiverse_results_concatenated, file="test_fern_CANAPE.csv", row.names=FALSE)
#This uses the 2 pass test to pull out palaeo, neo and super for RPE
#  SWL - could be refactored into a function
biodiverse_results_concatenated$P_PHYLO_RPE1_SIG <- sapply(
  1:nrow(biodiverse_results_concatenated), 
  function(x) significance_super_fun(
    biodiverse_results_concatenated$P_PE_WE_P[x], 
    biodiverse_results_concatenated$P_PHYLO_RPE_NULL1[x], 
    biodiverse_results_concatenated$P_PHYLO_RPE1[x]
  )
)

biodiverse_results_concatenated$P_PHYLO_RPE2_SIG <- sapply(
  1:nrow(biodiverse_results_concatenated), 
  function(x) significance_super_fun(
    biodiverse_results_concatenated$P_PE_WE_P[x], 
    biodiverse_results_concatenated$P_PHYLO_RPE_NULL2[x], 
    biodiverse_results_concatenated$P_PHYLO_RPE2[x]
  )
)


# 
# trait_index <- grep("^P_PHYLO_RPE2$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
# #This uses the 2 pass test to pull out palaeo, neo and super for RPE
# biodiverse_results_concatenated$P_PHYLO_RPE1_SIGGG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL1[x], biodiverse_results_concatenated$P_PHYLO_RPE1[x])) 
# 
# 
# biodiverse_results_concatenated$P_PHYLO_RPE2_SIGGG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL2[x], biodiverse_results_concatenated$P_PHYLO_RPE2[x])) 
########################################################
#filter out only the significant canape cells
########################################################
biodiverse_results_concatenated_dt <- tbl_dt(biodiverse_results_concatenated)
#View(biodiverse_results_concatenated_dt$P_PHYLO_RPE2_SIG)
#biodiverse_results_concatenated_dt
#write.csv(biodiverse_results_concatenated_dt, file="test_fern_CANAPE.csv", row.names=FALSE)
CANAPE <- biodiverse_results_concatenated_dt %>%
  filter(P_PHYLO_RPE2_SIG != "Not Sig")
#View(CANAPE$P_PHYLO_RPE2_SIG)
########################################################

rwbld_list_observed <- paste0(data_dir, "ferns_09_14_biodiverse_analysed_output_PHYLO_RPE2_RWBLD_LIST.csv")
rwbld_stats_observed <- paste0(data_dir, "ferns_09_14_biodiverse_analysed_output_PHYLO_RPE2_RWBLD_STATS.csv")
rwbld_stats_rand <- paste0(data_dir, "ferns_09_14_biodiverse_analysed_output_rand--PHYLO_RPE2_RWBLD_STATS.csv")

rwbld_list_observed <- read.table(rwbld_list_observed, header=T,sep=",")
rwbld_stats_observed <- read.table(rwbld_stats_observed, header=T,sep=",")
rwbld_stats_rand <- read.table(rwbld_stats_rand, header=T,sep=",")

rwbld_list_observed_dt <- tbl_dt(rwbld_list_observed)
rwbld_list_observed_dt
rwbld_stats_observed_dt <- tbl_dt(rwbld_stats_observed)

rwbld_stats_rand_dt <- tbl_dt(rwbld_stats_rand)
#rwbld_stats_rand_dt
rwbld_stats_all <- inner_join(rwbld_stats_rand_dt, rwbld_stats_observed_dt)#
#View(rwbld_stats_all)
rwbld_sig <- inner_join(rwbld_stats_all, CANAPE)# pull out only CANAPE cells

rwbald_merged <- inner_join(rwbld_stats_all, biodiverse_results_concatenated_dt)

##################
#RWDLB function
###################
# x = P_RPE2_RWBLD_SD, y = P_RPE2_RWBLD_SKEWNESS, P_PHYLO_RPE2_SIG != "Not Sig"
rwbld_fun <- function(x, y, z){
  if (z != "Not Sig"){
  if (x < 0.95) {
    return("Meso")
  } else if (y >= 0.95){
    return ("Palaeo")
  } else if (y <= 0.05){
    return ("Neo")
  } else if (y >= 0.05 & y <= 0.95){
    return ("Minestrone")
  } else {
    return("Not Sig")
  }
} else{
  return("Not Sig") 
}
}


rwbald_merged$RWBLD_SIG_ALL_IN_ONE <- sapply(
  1:nrow(rwbld_stats_all), 
  function(x) rwbld_fun(
    rwbald_merged$P_RPE2_RWBLD_SD[x], 
    rwbald_merged$P_RPE2_RWBLD_SKEWNESS[x],
    rwbald_merged$P_PHYLO_RPE2_SIG[x]
  )
)

#  mutate(meso  = P_RPE2_RWBLD_SD < 0.95) %>%
#  mutate(paleo = P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS > 0.95) %>%
#  mutate(neo   = P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS < 0.05) %>%
#  mutate(mixed = P_RPE2_RWBLD_SD > 0.95 & (P_RPE2_RWBLD_SKEWNESS >= 0.05 & P_RPE2_RWBLD_SKEWNESS <= 0.95))  

write.csv(rwbald_merged, file=paste(output_folder, "rwbald_merged.csv", sep=""), row.names=FALSE)
#########################


#View(rwbld_sig)
limits <- rwbld_list_observed_dt %>%
  select(-ELEMENT, -Axis_0, -Axis_1)

limits
xmax <- max(limits, na.rm=TRUE)
xmax
#xmin <- min(limits, na.rm=TRUE)
#xmin
xmin <- -(xmax)
xmin
#################################
#add columns for each rwbld
############################
options(scipen=999)# disable scientific notation

for_hist <- rwbld_sig %>%
  mutate(meso  = P_RPE2_RWBLD_SD < 0.95) %>%
  mutate(paleo = P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS > 0.95) %>%
  mutate(neo   = P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS < 0.05) %>%
  mutate(mixed = P_RPE2_RWBLD_SD > 0.95 & (P_RPE2_RWBLD_SKEWNESS >= 0.05 & P_RPE2_RWBLD_SKEWNESS <= 0.95))  
for_hist
#View(for_hist$mixed)
# tester <- rwbld_list_observed_dt %>%
#   select(-ELEMENT, -Axis_0, -Axis_1) %>%
#   range(tester)
# tester
#View(for_hist$mixed)

write.csv(for_hist, file=paste(output_folder, "rwbld_results.csv", sep=""), row.names=FALSE)

rwbald_test_all <- inner_join(for_hist, rwbald_merged)
write.csv(rwbald_test_all, file=paste(output_folder, "rwbald_test_all.csv", sep=""), row.names=FALSE)
##############################
#meso
###############################
meso <- for_hist %>%
  select(ELEMENT, Axis_0, Axis_1, meso, RPE2_RWBLD_SD) %>%
  filter(meso == TRUE) %>%
  left_join(rwbld_list_observed_dt)
View(meso)

#cols <- ncol(meso)
#names(meso)
print("meso n:")
nrow(meso)

for(n in 1:nrow(meso)){
  branches <- select(meso[n],-ELEMENT, -Axis_0, -Axis_1, -meso, -RPE2_RWBLD_SD)
  branches[is.na(branches)] <- 0 #replace blanks w 0
  ab_SD <- rwbld_stats_observed_dt
  filename <- paste("meso_", str_replace(meso$ELEMENT[n], ":", "_"), ".png", sep="")
  full_filename <- paste(output_folder, filename, sep="")
  CairoPNG(width = 800, height = 400, file =full_filename, canvas="white", bg = "white", units="px", dpi=72, title = "") 
  hist(as.numeric(branches), col="darkolivegreen", border="black", main=paste0("meso ", meso$ELEMENT[n]), xlab="P_RPE2_RWBLD_SD < 0.95")#, xlim=c(xmin, xmax))
  abline(v=meso$RPE2_RWBLD_SD[n]*2)
  abline(v=meso$RPE2_RWBLD_SD[n]*-2)
  dev.off()
}



##############################
#paleo
###############################
paleo<- for_hist %>%
  select(ELEMENT, Axis_0, Axis_1, paleo, RPE2_RWBLD_SD) %>%
  filter(paleo == TRUE) %>%
  left_join(rwbld_list_observed_dt)
#View(paleo)

#cols <- ncol(paleo)
#names(paleo)
print("paleo n:")
nrow(paleo)

for(n in 1:nrow(paleo)){
  branches <- select(paleo[n],-ELEMENT, -Axis_0, -Axis_1, -paleo, -RPE2_RWBLD_SD)
  branches[is.na(branches)] <- 0 #replace blanks w 0 
  filename <- paste("paleo_", str_replace(paleo$ELEMENT[n], ":", "_"), ".png", sep="")
  full_filename <- paste(output_folder, filename, sep="")
  CairoPNG(width = 800, height = 400, file = full_filename, canvas="white", bg = "white", units="px", dpi=72, title = "") 
  hist(as.numeric(branches), col="royalblue4", border="black", main=paste0("paleo ",paleo$ELEMENT[n]), xlab="P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS > 0.95")#, xlim=c(xmin, xmax))
  abline(v=paleo$RPE2_RWBLD_SD[n]*2)
  abline(v=paleo$RPE2_RWBLD_SD[n]*-2)
  dev.off()
}

##############################
#neo
###############################
neo<- for_hist %>%
  select(ELEMENT, Axis_0, Axis_1, neo, RPE2_RWBLD_SD) %>%
  filter(neo == TRUE) %>%
  left_join(rwbld_list_observed_dt)
#View(neo)

#cols <- ncol(neo)
#names(neo)
print("neo n:")
nrow(neo)

for(n in 1:nrow(neo)){
  branches <- select(neo[n],-ELEMENT, -Axis_0, -Axis_1, -neo, -RPE2_RWBLD_SD)
  branches[is.na(branches)] <- 0 #replace blanks w 0 
  filename <- paste("neo_", str_replace(neo$ELEMENT[n], ":", "_"), ".png", sep="")
  full_filename <- paste(output_folder, filename, sep="")
  CairoPNG(width = 800, height = 400, file = full_filename, canvas="white", bg = "white", units="px", dpi=72, title = "") 
  hist(as.numeric(branches), col="red4", border="black", main=paste0("neo ",mneo$ELEMENT[n]), xlab="P_RPE2_RWBLD_SD > 0.95 & P_RPE2_RWBLD_SKEWNESS < 0.05")#, xlim=c(xmin, xmax))
  abline(v=neo$RPE2_RWBLD_SD[n]*2)
  abline(v=neo$RPE2_RWBLD_SD[n]*-2)
  dev.off()
}

##############################
#mixed
###############################
mixed <- for_hist %>%
  select(ELEMENT, Axis_0, Axis_1, mixed, RPE2_RWBLD_SD) %>%
  filter(mixed == TRUE) %>%
  left_join(rwbld_list_observed_dt)
View(mixed)

#cols <- ncol(mixed)
#names(mixed)
print("mixed n:")
nrow(mixed)

for(n in 1:nrow(mixed)){
  branches <- select(mixed[n],-ELEMENT, -Axis_0, -Axis_1, -mixed, -RPE2_RWBLD_SD)
  branches[is.na(branches)] <- 0 #replace blanks w 0 
  filename <- paste("mixed_", str_replace(mixed$ELEMENT[n], ":", "_"), ".png", sep="")
  full_filename <- paste(output_folder, filename, sep="")
  CairoPNG(width = 800, height = 400, file = full_filename, canvas="white", bg = "white", units="px", dpi=72, title = "") 
  hist(as.numeric(branches), col="#CB7FFF", border="black", main=paste0("mixed ",mixed$ELEMENT[n]), xlab="P_RPE2_RWBLD_SD > 0.95 & (P_RPE2_RWBLD_SKEWNESS >= 0.05 & P_RPE2_RWBLD_SKEWNESS <= 0.95)")#, xlim=c(xmin, xmax))
  abline(v=mixed$RPE2_RWBLD_SD[n]*2)
  abline(v=mixed$RPE2_RWBLD_SD[n]*-2)
  dev.off()
}


#ferns_09_14_biodiverse_analysed_output_PHYLO_RPE2_RWBLD_STATS$RPE2_RWBLD_SD column abline 


#######################################################

#Only interested in CANAPE significant cells values
# histogram is of *PHYLO_RPE2_RWBLD_LIST.csv from analysed data
# SD and Skewness is P_RPE2_RWBLD_SD  P_RPE2_RWBLD_SKEWNESS
# from randomised stat file *PHYLO_RPE2_RWBLD_STATS.csv


max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_text <- "RWBLD_SIG_ALL_IN_ONE"
sigplot <- "RWBLD_SIG_ALL_IN_ONE"
col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Neo" = "red", "Meso" = "darkolivegreen", "Minestrone"= "#CB7FFF")
legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Meso"="Meso", "Minestrone"="Minestrone")
#legend_order <-c("Neo", "Paleo", "Not Sig", "Meso", "Minestrone")

#rwbald_merged[, sigplot] <- factor(rwbald_merged[, sigplot], levels=legend_order)
#View(rwbald_merged$RWBLD_SIG_ALL_IN_ONE)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

map_plot_rwbld <- ggplot(data=rwbald_merged) + 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + xlim(min_x, max_x) +  ylim(min_y, max_y) + # xlim(-1900000, 2180000) + ylim(-4850000,-500000)+#xlim(-1900000, 2200000) + ylim(-4900000,-700000)+ # using aes_string allows variables to be passed to ggplot
  #scale_fill_manual(values = col_scheme) +  
  scale_fill_manual(values = col_scheme,  labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom",  title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90)
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") + 
  coord_fixed() +
   #annotate("text", label = "b", x = -1700000, y = -4750000, size =rel(20), face = 'plain', family = "HelvLight") +
   annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
   annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
   annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = "HelvLight") +
   annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = "HelvLight") +
   annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = "HelvLight") +
   annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = "HelvLight") + 
   
   theme(
     text = element_text(family = "HelvLight"),
     strip.background = element_blank(),
     axis.line=element_blank(),axis.text.x=element_blank(),
     axis.text.y=element_blank(),axis.ticks=element_blank(),
     axis.title.x=element_blank(),
     axis.title.y=element_blank(),
     axis.line = element_blank(),
     axis.text = element_blank(), 
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank(),
     axis.text = element_blank(),
     #legend.background = element_rect(colour = "black", fill="transparent", size=1),
     legend.key =element_rect(colour = "black", fill="transparent", size=1),
     legend.key.height = unit(1.1, "cm"),
     legend.key.width = unit(6.2, "cm"),
     legend.position=c(.5, 0.1),
     legend.direction='horizontal',
     legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
     legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
     panel.grid = element_blank(),
     panel.background=element_blank(),#element_rect(colour = "black", fill="white", size = 1),
     #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
     #panel.margin=unit(c(0,0,0,0),"line"),
     plot.background=element_rect(colour = "black", fill="white", size = 1),
     plot.margin=unit(c(0,0,-0.61,-0.61),"line"))


print(map_plot_rwbld)


  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "map_plot_rwbld.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 a") #
  print(map_plot_rwbld)
  dev.off()

