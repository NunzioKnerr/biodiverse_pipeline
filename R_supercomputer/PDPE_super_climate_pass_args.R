require(sp)
#require(rgdal)
require(raster)
require(maptools)    
require(RColorBrewer)
require(grid)
require(ggplot2)
require(gtable)
require(gridExtra)
require(Cairo)

######################################################################################
############
#  Should be set on system, but useful for portabilty to other systems
#Sys.setenv(PATH="C:\\strawberry\\perl\\bin;c:\\strawberry\\c\\bin;%PATH%") #set path to srawberryperl for 64 bit version
Sys.setenv(PERL5LIB=path.expand("~/biodiverse/lib"))
Sys.setenv(BIODIVERSE_EXTENSIONS=path.expand("~/biodiverse/Biodiverse_extensions.txt"))
TopFolder <-  path.expand("~/output/") #folder to recurse through
### path to tree file (and trimmed output)
input_bts_file = path.expand("~/R_eucs/Eucs_national.bts")
output_bts_file = path.expand("~/R_eucs/Eucs_national_trimmed.bts")
outline_map <- path.expand("~/R_eucs/AUS_adm1_cleaned_albers.shp")

#for super computer
args <- commandArgs(TRUE)
in.scenario <- args[1]
in.res <-  args[2]
in.iter <- args[3]
in.seed <- args[4]
#out_file <- paste("BOS_predict_", in.file, sep="")
#####
######################################################################################
#set the scenario you want to run
scenario <- c(in.scenario)  #, "2025", "2055", "2085") #ARG
######################################################################################
#shell commands, set path to perl and biodiverse
cmdcb = "perl  ~/biodiverse_pipeline/perl/create_bds_climate.pl"
cmdtrim = "perl ~/biodiverse_pipeline/perl/trim_bds_and_bts.pl"
cmdpdpe = "perl ~/biodiverse_pipeline/perl/run_analyses.pl"
cmdrand = "perl ~/biodiverse/bin/run_randomisation.pl"
cmdexp = "perl ~/biodiverse_pipeline/perl/load_bds_and_export_results.pl"

#threshold <- 0.01
resolution <- in.res #ARG
#iterations in the randomisation 
iterations <- in.iter #ARG
seed <- in.seed #ARG
###################################################################################################
####switch for pdfs
pdfs=FALSE  #TRUE

#inp <-paste(TopFolder,"/input/", scenario,sep="")
out <-paste(TopFolder, scenario,sep="") 

#SubF <- list.files(inp,recursive=T, pattern=paste("*.asc",sep=""))
#setwd(inp)
#f <- file.path(TopFolder, c(SubF))

#only 10 species
# #SubF <- SubF[1:10]
# 
# 
# for (i in 1:length(SubF))
# {
#   fn <- SubF[i]
#   
# 
# #  if (length(grep("2055",fn))==1) folder="2055" 
# #  if (length(grep("2085",fn))==1) folder="2085" 
# #  if (is.na(folder)) folder="present"
#   
#   r <-raster(fn)
#   r1<-r
#   values(r1) <- ifelse(values(r)<threshold,NA,values(r))
#   #plot(r1)
#   
#   generaspec <- strsplit(fn,"\\.")[[1]][1]
#   sp <- strsplit(generaspec," ")[[1]]
#   
#   speciesname <- paste(sp[1], sp[2],sep="_")
#   
#   
#   wgs84proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #WGS84 proj4 projection
#   
#   r1@crs <- CRS(wgs84proj)
#   
#   albersGDA94proj <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #GDA94 / Australian Albers proj4 projection
#   r2 <- projectRaster(r1, crs=albersGDA94proj, res=resolution)
# #  plot(r2)
#   #writeRaster(r2, paste(speciesname,".asc",sep=""), format="ascii", overwrite=T)
#   
#   xyz<-as.data.frame(r2, na.rm=FALSE, xy=TRUE)
#   xyz <- xyz[complete.cases(xyz),]
#   colnames(xyz)[1:3] <- c("albers_gda_x", "albers_gda_y", "maxent")
#   xyz$taxon <- speciesname
#   
# #multiply by 1000
# #xyz$maxent <- round(xyz$maxent *1000)
#   
#   write.csv(xyz, file= paste(out,"/",speciesname,"_",scenario,".csv",sep=""), row.names=F)
#   cat(paste("Layer:", i,"=", paste(out,"/",speciesname,"_",scenario,".csv",sep=""),"created. \n"))
# }
# ###################################################################################################
# 
# ###################################################################################################
# 

#### a single csv for each scenario

#TopFolder <-  "C:/Carlos/UC/Climate_change_MDB/Sample5kmPredsForC/test" #folder to recurse through
#newFolder <- paste(TopFolder,"/",folder,sep="")
setwd(out)
# files <- list.files(out,recursive=T, pattern=paste("*.csv",sep=""))
# 
# 
# csvs <- read.csv(files[1])
# 
# for (i in 2:length(files)) 
# {
#   dummy <- read.csv(paste(out,"/",files[i],sep=""))
#   csvs <- rbind(csvs, dummy)
# }
# write.csv(csvs, file=paste(out,"/combined_",scenario,".csv",sep=""), row.names=F)
# 



##### create biodiverse file bds



csv_file = paste(out, "/eucs_maxent_had_", scenario,".csv", sep="")
out_file = paste(out, "/eucs_maxent_had_", scenario,".bds", sep="")


label_column_number = "5"
group_column_number_x = "3"
group_column_number_y = "4"
sample_count_column_number = "2"
cell_size_x = as.character(resolution)
cell_size_y = as.character(resolution)

cmdcb = paste (cmdcb, 
               "--csv_file", shQuote(csv_file),  
               "--out_file", shQuote(out_file), 
               "--label_column_number", shQuote(label_column_number), 
               "--group_column_number_x", shQuote(group_column_number_x), 
               "--group_column_number_y", shQuote(group_column_number_y), 
               "--cell_size_x", shQuote(cell_size_x), 
               "--cell_size_y", shQuote(cell_size_y), 
               "--sample_count_column_number", shQuote(sample_count_column_number)
)


system(cmdcb, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 




#### trim the tree and biodiverse file
input_bds_file = paste(out, "/eucs_maxent_had_", scenario,".bds", sep="")
output_bds_file = paste(out, "/eucs_maxent_had_trimmed_",scenario,".bds", sep="")
cmdtrim = paste (cmdtrim, "--input_bds_file", shQuote(input_bds_file ), "--input_bts_file", shQuote(input_bts_file), "--output_bds_file", shQuote(output_bds_file), "--output_bts_file", shQuote(output_bts_file))
system(cmdtrim, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 




##### run the PD and PE
input_bds_file =  paste(out, "/eucs_maxent_had_trimmed_", scenario,".bds", sep="")

#calcs = paste(" calc_phylo_rpe2")
calcs = paste ("calc_endemism_whole,calc_pd,calc_pe,calc_phylo_rpd1,calc_phylo_rpd2,calc_phylo_rpe1,calc_phylo_rpe2,calc_local_sample_count_stats ")
#calcs = paste("calc_endemism_whole,calc_pd,calc_pe,calc_phylo_rpd2,calc_phylo_rpe2")

cmdpdpe <- paste(cmdpdpe, "--input_bds_file", shQuote(input_bds_file), "--input_bts_file", shQuote(input_bts_file), "--calcs", shQuote(calcs), sep =" ")
system(cmdpdpe, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 

################# randomisation test

basedata = paste(out, "/eucs_maxent_had_trimmed_", scenario,"_analysed.bds", sep="")
rand_name = paste("rand_", seed, sep="")


cmdrand <- paste(cmdrand, "--basedata", shQuote(basedata), "--rand_name", shQuote(rand_name), "--iterations", shQuote(iterations),"--seed", shQuote(seed), " --args function=rand_structured max_iters=999")
system(cmdrand, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 

###

#run load_bds and export results.pl

#  Should be set on system, but useful for portabilty to other systems



input_bds_file =  paste(out, "/eucs_maxent_had_trimmed_", scenario,"_analysed.bds", sep="") #trimmed_analysed file
output_csv_prefix = paste(out,"/comb_",scenario,"" ,sep="") 

cmdexp <- paste(cmdexp, "--input_bds_file", shQuote(input_bds_file), "--output_csv_prefix", shQuote(output_csv_prefix), sep =" ")
system(cmdexp, intern=FALSE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=TRUE) 

### add quotes around each file name/directory for safety and user proofing
## system(paste(shQuote("echo"), shQuote("HelloWorld"))) 

#?shQuote






#memory.limit(16000)


observed_data <- paste(output_csv_prefix, "_SPATIAL_RESULTS.csv", sep="")
#observed_data <- choose.files(caption="Select the .csv with the observed results")
#randomisation_results <- choose.files(caption="Select the .csv with the randomisation results", default=observed_data)
randomisation_results  <- paste(output_csv_prefix, "_rand--SPATIAL_RESULTS.csv", sep="")
#random_data <-"C:\\GIS-Datasets\\acacia_again_june_16_2013\\randomised_raw\\concatenated.csv"
#random_data <- choose.files(caption="Select the concatenated random values .csv file", default=observed_data)
#output_dir <-  choose.dir(caption="select the output directory", default=observed_data)


observed_data <- read.table(observed_data, header=T,sep=",")
randomisation_results <- read.table(randomisation_results, header=T,sep=",")
biodiverse_results_concatenated <- cbind(observed_data, randomisation_results)
#system.time(random_data <- fread(random_data))   


biodiverse_results_concatenated<- biodiverse_results_concatenated[!is.na(biodiverse_results_concatenated[,4]),]#trim NA.s

#View(randomisation_results)
#View(random_data)
#View(observed_data)
#View(randomisation_results)


#############################################################################
#
# functions for populating new columns with text of significance
#
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

significance_point_size_fun <- function(x, y, z){
  if (x < 0.95 & y < 0.95) {
    return(1)
  } else if (z <= 0.025){
    return (1.5)#neo
  } else if (z >= 0.975){
    return (1.5)#paleo
  } else if (x >= 0.99 & y >= 0.99){
    return (1.5)#super
  } else {
    return(1.5)#mixed
  }
}

###################################################################################
#
#Create new columns in dataframe and populate them using the funtcions above
#
##################################################################################
trait_index <- grep("^P_PHYLO_RPD1$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PHYLO_RPD1_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply the function to every row of column with index "trait_index" and generate a column in the dataframe showing significant cells

trait_index <- grep("^P_PHYLO_RPD2$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PHYLO_RPD2_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply the function to every row of column with index "trait_index" and generate a column in the dataframe showing significant cells

trait_index <- grep("^P_PD_P$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PD_P_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

trait_index <- grep("^P_PE_WE_P$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
biodiverse_results_concatenated$P_PE_WE_P_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

#trait_index <- grep("^P_PD_P_per_taxon$", colnames(biodiverse_results_concatenated)) #get the index of the column with the trait 
#biodiverse_results_concatenated$P_PD_P_per_taxon_SIG <- apply(biodiverse_results_concatenated[trait_index],  MARGIN=c(1), significance_fun) # apply

#This uses the 2 pass test to pull out palaeo, neo and super for RPE
biodiverse_results_concatenated$P_PHYLO_RPE1_SIG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL1[x], biodiverse_results_concatenated$P_PHYLO_RPE1[x])) 


biodiverse_results_concatenated$P_PHYLO_RPE2_SIG <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_super_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL2[x], biodiverse_results_concatenated$P_PHYLO_RPE2[x])) 

biodiverse_results_concatenated$P_PHYLO_RPE2_SIG_POINT_SIZE <- sapply(1:nrow(biodiverse_results_concatenated), function(x) significance_point_size_fun(biodiverse_results_concatenated$P_PE_WE_P[x], biodiverse_results_concatenated$P_PHYLO_RPE_NULL2[x], biodiverse_results_concatenated$P_PHYLO_RPE2[x])) 


map_data <- readShapePoly(outline_map)
#wgs84proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #WGS84 proj4 projection
#projection(map_data) <- CRS(wgs84proj)

#map_data <- spTransform(map_data, CRS(albersGDA94proj))



#writePolyShape(map_data,"C:/Carlos/UC/Australia_outile/AUS_adm1_cleaned_albers.shp")


###############################################################################################



##########################################################
#
#
# Figure 1
#
#
############################################################



Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "ABC3_SUM_SET1"

df <- biodiverse_results_concatenated


yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours
legend_text <- paste("Species richness", sep="")
legend_position <- paste("top", sep="")
rounding_digits <- 0
#outfile <- paste("acacia_observed_ENDC_RICHNESS_final.pdf", sep="") 

#   Create vectors for legend text using the max and min vaules in the raster and a number in between
ENDW_RICHNESS.hs <- seq(0,max(biodiverse_results_concatenated$ABC3_SUM_SET1),length.out=5)
ENDW_RICHNESS.hs.round <- round(ENDW_RICHNESS.hs, digits=rounding_digits)
#ENDW_RICHNESS.hs.round <- c(0, 10, 25, 35, max(biodiverse_results_concatenated$ENDW_RICHNESS))
#ENDW_RICHNESS.hs.round <- c(0, 12, 24, 34, max(biodiverse_results_concatenated$ENDW_RICHNESS))
#View(b.hs.round)
#ENDW_RICHNESS.st_dev <- sd(biodiverse_results_concatenated$ENDW_RICHNESS)
#View(b.st_dev)
#b.hs.ceiling <- ceiling(b.hs)
ENDW_RICHNESS.max_val <- round(max(biodiverse_results_concatenated$ABC3_SUM_SET1), digits=0)
#View(max_val)
ENDW_RICHNESS.limits_set <- c(0,250)   #ENDW_RICHNESS.max_val+0.1
#View(limits_set)

#(max(rast_p$Values))
#scale_fill_gradientn(name="Legend Value",colours = colours, breaks= round(b.hs), values=c(0,0.1,0.6,0.7,0.75,0.8,0.9,0.95,0.99))
#breaks=b.dem
p1 <- ggplot(data=df)+ xlim(-1880000, 2180000) + ylim(-4850000,-500000)+ 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
  #scale_fill_continuous(limits = c(min(rast_p$Values),max(rast_p$Values)), breaks=b.hs.round, guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
  #scale_fill_continuous(limits = limits_set, breaks=b.hs.round, guide = guide_colourbar(nbin=100, raster= TRUE, ticks = TRUE, draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_fill_gradientn(name = legend_text, limits = ENDW_RICHNESS.limits_set, colours = colours, breaks= ENDW_RICHNESS.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust=0.5, title.vjust=0.9, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + #
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "a", x = 2000000, y = -4750000, size=rel(20),  face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        #legend.key = element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.margin = unit(2, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

#plot.margin = unit(c(1,0.5,0,0.5), "lines"))

# # png(filename=outfile, width=1500, height=1500)# for writing to file
# pdf(outfile, height=30, width=30, paper="a4r")
print(p1)
# dev.off()# for writing to file
# 

##########################################################
#
#
# Figure 1b
#
#
############################################################


rounding_digits <- 2
yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours



# 
# 
# PE_WE.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5)
# conv <- floor(log10((PE_WE.hs)[2]))
# 
# legend_text <- paste("Phylogenetic endemism"," x 10",conv, sep="")
# 
# 
# #PE_WE.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=rounding_digits)
# #View(max_val)
# PE_WE.limits_set <- c(0,max(biodiverse_results_concatenated[,sigplot])*10^(-conv)*1.1)
# df$convPE_WE_P <- df[,sigplot]*10^(-conv)
# 
# 
# PE_WE.hs.round <- seq(0, round(PE_WE.limits_set)[2], length=5)
# 
# 
# sigplot="convPE_WE_P"
sigplot <- "ENDW_WE"
df <- biodiverse_results_concatenated

#   Create vectors for legend text using the max and min vaules in the raster and a number in between
ENDW_WE.hs <- seq(0,max(biodiverse_results_concatenated$ENDW_WE),length.out=5)
conv <- floor(log10((ENDW_WE.hs)[2]))

legend_text <- bquote('Weighted endemism x ' ~ 10^{.(conv)})

ENDW_WE.limits_set <- c(0,max(biodiverse_results_concatenated$ENDW_WE)*10^(-conv)*1.1)

df$convENDW_WE <- df[,sigplot] * 10^(-conv)

ENDW_WE.hs.round <- seq(0, round(ENDW_WE.limits_set)[2], length=5)

##### change scale i

#ENDW_WE.hs.round <- c(0, 0.5, 1, 1.5, 2, max(biodiverse_results_concatenated$ENDW_WE))
#ENDW_WE.hs.round <- round(ENDW_WE.hs.round, digits=rounding_digits)

##############################################################################################

#View(b.hs.round)
#ENDW_WE.st_dev <- sd(biodiverse_results_concatenated$ENDW_WE)
#View(b.st_dev)
# ENDW_WE.hs.ceiling <- ceiling(ENDW_WE.hs)
# ENDW_WE.max_val <- round(max(biodiverse_results_concatenated$ENDW_WE), digits=2)
# #View(max_val)
# ENDW_WE.limits_set <- c(0,ENDW_WE.max_val+0.0009)
# 
# 
# ENDW_WE.hs.round <- round(seq(0,max(biodiverse_results_concatenated$ENDW_WE)*1.01,length=5),4)

#View(limits_set)


sigplot="convENDW_WE"
legend_position <- paste("top", sep="")


Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

#change boundary
p2 <- ggplot(data=df)+ xlim(-1880000, 2180000) + ylim(-4850000,-500000)+ 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
  #scale_fill_continuous(limits = c(min(rast_p$Values),max(rast_p$Values)), breaks=b.hs.round, guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
  #scale_fill_continuous(limits = limits_set, breaks=b.hs.round, guide = guide_colourbar(nbin=100, raster= TRUE, ticks = TRUE, draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_fill_gradientn(name = legend_text, limits = ENDW_WE.limits_set, colours = colours, breaks= ENDW_WE.hs.round,  guide = guide_colourbar(direction = "horizontal", title.position = "top",  title.hjust=0.5, title.vjust=0.99, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + #
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "b", x = -1700000, y = -4750000, size =rel(20), face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        #legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p2)

##########################################################
#
#
# Figure 1c
#
#
############################################################


Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "PD_P"
df <- biodiverse_results_concatenated

rounding_digits <- 3
yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours
#   Create vectors for legend text using the max and min vaules in the raster and a number in between
PD.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5) #PD_P version
PD.hs.round <- round(PD.hs, digits=rounding_digits)
#PD.hs.round <- c(0, 0.5, 1, max(biodiverse_results_concatenated[,sigplot])) #PD version
#PD.hs.round <- round(PD.hs.round, digits=rounding_digits)
#View(PD.hs.round)
#PD.st_dev <- sd(biodiverse_results_concatenated$PD)
#View(b.st_dev)
#b.hs.ceiling <- ceiling(b.hs)
PD.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=3)
#View(max_val)
PD.limits_set <- c(0,PD.max_val+0.001)
#View(limits_set)

legend_text <- paste("Phylogenetic diversity", sep="")
legend_position <- paste("bottom", sep="")


p3 <- ggplot(data=df)+xlim(-1880000, 2180000) + ylim(-5200000,-850000)+  #coord_equal()+scale_x_continuous("")+scale_y_continuous("")+
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
  #scale_fill_continuous(limits = c(min(rast_p$Values),max(rast_p$Values)), breaks=b.hs.round, guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
  #scale_fill_continuous(limits = limits_set, breaks=b.hs.round, guide = guide_colourbar(nbin=100, raster= TRUE, ticks = TRUE, draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_fill_gradientn(name = legend_text, limits = PD.limits_set, colours = colours, breaks= PD.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "c", x =  2000000, y = -1000000, size=rel(20), face = 'bold') +
  #guide_legend(direction = "horizontal", title.position = "top", label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, label.theme = element_text(angle = 90))+
  theme(text = element_text(family = "sans"),
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
        #legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p3)

##########################################################
#
#
# Figure 1d
#
#
############################################################

Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "PE_WE_P"
df <- biodiverse_results_concatenated


rounding_digits <- 1
yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours
#   Create vectors for legend text using the max and min vaules in the raster and a number in between
#PE_WE.hs.round <- c(0,  0.02, 0.04, max(biodiverse_results_concatenated[,sigplot]))#PE_WE version
#PE_WE.hs.round <- c(0,  0.002, 0.004, max(biodiverse_results_concatenated[,sigplot]))#PE_WE_P Proportional_verion
#PE_WE.hs.round <- round(PE_WE.hs.round, digits=rounding_digits)
#View(PE_WE.hs.round)
#PE_WE.st_dev <- sd(biodiverse_results_concatenated$PE_WE)
#View(b.st_dev)
#b.hs.ceiling <- ceiling(b.hs)

#View(limits_set)


PE_WE.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5)
conv <- floor(log10((PE_WE.hs)[2]))

legend_text <- bquote('Phylogenetic endemism x ' ~ 10^{.(conv)})




#PE_WE.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=rounding_digits)
#View(max_val)
PE_WE.limits_set <- c(0,max(biodiverse_results_concatenated[,sigplot])*10^(-conv)*1.1)
df$convPE_WE_P <- df[,sigplot]*10^(-conv)


PE_WE.hs.round <- seq(0, round(PE_WE.limits_set)[2], length=5)


sigplot="convPE_WE_P"

p4 <- ggplot(data=df)+xlim(-1880000, 2180000) + ylim(-5200000,-850000)+
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
  #scale_fill_continuous(limits = c(min(rast_p$Values),max(rast_p$Values)), breaks=b.hs.round, guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
  #scale_fill_continuous(limits = limits_set, breaks=b.hs.round, guide = guide_colourbar(nbin=100, raster= TRUE, ticks = TRUE, draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_fill_gradientn(name = legend_text, limits = PE_WE.limits_set, colours = colours, breaks= PE_WE.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "d", x = -1700000, y = -1000000, size=rel(20), face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        #legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p4)



# full_png <- paste("figure_1.png", sep="")
# #png(filename=full_png, width=1970, height=2037)# for writing to file)
# png(filename=full_png, width=2501, height=2554)# for writing to file)
# #pdf("myplot.pdf", height=25, width=25, paper="a4r")
# multiplot(p1, p3, p2, p4, cols=2)
# dev.off()
# 

# Get the widths
gA <- ggplot_gtable(ggplot_build(p1))
gB <- ggplot_gtable(ggplot_build(p2))
gC <- ggplot_gtable(ggplot_build(p3))
gD <- ggplot_gtable(ggplot_build(p4))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth

Cairo(width = 2323, height = 2486, file = paste(out,"/","figure_1_",scenario,".png", sep=""), type="png", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=FALSE) #
grid.arrange(gA, gB, gC, gD, nrow=2) # Arrange the four charts
dev.off()

if (pdfs) 
{
  Cairo(width = 2323, height = 2486, file = paste(out,"/","figure_1_",scenario,".pdf", sep=""), type="pdf", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=TRUE) #
grid.arrange(gA, gB, gC, gD, nrow=2)  # Arrange the four charts
dev.off()
}



#########################################################################
#
#Figure 2 seperate versions
#
#
########################################################################

map_text <- "Phylogenetic diversity"
sigplot <- "P_PD_P_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

#PD_map <- plot_map_and_scatter(map_text, biodiverse_results_concatenated, sigplot, col_scheme, legend_order)

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

map_plot_1 <- ggplot(data=biodiverse_results_concatenated) + 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ xlim(-1880000, 2180000) + ylim(-4850000,-500000)+# using aes_string allows variables to be passed to ggplot
  #scale_fill_manual(values = col_scheme) +  
  scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "top", title.hjust=0.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90), label.hjust = 0.5, label.vjust = 0.5
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  #labs(x=NULL, y=NULL) +
  annotate("text", label = "a", x = 2000000, y = -4750000, size=rel(20),  face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))
print(map_plot_1)


map_text <- "Relative phylogenetic diversity"
sigplot <- "P_PHYLO_RPD2_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = " > 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")


biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

map_plot_2 <- ggplot(data=biodiverse_results_concatenated) + 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+  xlim(-1880000, 2180000) + ylim(-4850000,-500000)+#xlim(-1900000, 2200000) + ylim(-4900000,-700000)+ # using aes_string allows variables to be passed to ggplot
  #scale_fill_manual(values = col_scheme) +  
  scale_fill_manual(values = col_scheme,  labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "top",  title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90)
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "b", x = -1700000, y = -4750000, size =rel(20), face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))


print(map_plot_2)


map_text <- "Phylogenetic endemism"
sigplot <- "P_PE_WE_P_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
#labels_order <- c("Very Low","just Low","Not Sig","Highly Sig","Very Highly Significant")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"


map_plot_3 <- ggplot(data=biodiverse_results_concatenated) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ xlim(-1880000, 2180000) + ylim(-5200000,-850000)+# using aes_string allows variables to be passed to ggplot
  scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1))+ #label.theme = element_text(angle = 90)
  #labs(title = map_text) +
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "c", x =  2000000, y = -1000000, size=rel(20), face = 'bold') +
  #guide_legend(direction = "horizontal", title.position = "top", label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, label.theme = element_text(angle = 90))+
  theme(text = element_text(family = "sans"),
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
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(map_plot_3)


map_text <- "Relative phylogenetic endemism"
#sigplot <- "Relative Phylogenetic Endemism"
sigplot <- "P_PHYLO_RPE2_SIG"
col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
legend_order <-c("Neo","Palaeo", "Not Sig", "Mixed", "Super")
legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

map_plot_4 <- ggplot(data=biodiverse_results_concatenated) + 
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+xlim(-1880000, 2180000) + ylim(-5200000,-850000)+  #xlim(-1900000, 2200000) + ylim(-5100000,-900000)+ # using aes_string allows variables to be passed to ggplot
  scale_fill_manual(values = col_scheme,  labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90)
  #theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
  coord_fixed() +
  annotate("text", label = "d", x = -1700000, y = -1000000, size=rel(20), face = 'bold') +
  theme(text = element_text(family = "sans"),
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
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain'),
        panel.grid = element_blank(),
        panel.background=element_rect(colour = "black", fill="white", size = 1),
        #panel.border = element_rect(colour = "black", fill=NA, size = 1),#element_blank(),
        #panel.margin=unit(c(0,0,0,0),"line"),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))
print(map_plot_4)

#ggsave(filename="test.pdf", plot=map_plot_4, width=18, height=18, units="cm")
#?ggsave
# library(Cairo)
#  Cairo(width = 1160, height = 1243, file = "test.png", type="png", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=FALSE) 
#  print(map_plot_4)
#  dev.off()

# Get the widths
gA <- ggplot_gtable(ggplot_build(map_plot_1))
gB <- ggplot_gtable(ggplot_build(map_plot_2))
gC <- ggplot_gtable(ggplot_build(map_plot_3))
gD <- ggplot_gtable(ggplot_build(map_plot_4))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3])

View(gA$widths)

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth

if (pdfs)
{
  Cairo(width = 2323, height = 2486, file = paste(out,"/","figure_2_",scenario,".pdf", sep=""), type="pdf", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=FALSE) #
# Arrange the four charts
grid.arrange(gA, gB, gC, gD, nrow=2)
dev.off()
}
Cairo(width = 2323, height = 2486, file = paste(out,"/","figure_2_",scenario,".png", sep=""), type="png", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=FALSE) #
# Arrange the four charts
grid.arrange(gA, gB, gC, gD, nrow=2)
dev.off()



###################################################
#Figure 3 PE_P vs PE_NULL2
####################################################

options("scipen"=100, "digits"=5)
sigplot <- "P_PHYLO_RPE2_SIG"
col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "transparent", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
legend_order <-c("Neo","Palaeo", "Mixed", "Super",  "Not Sig")
legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)

fig_3_plot <- ggplot() +# xlim(0, 0.00415) +  ylim(0, .0065) +
  #geom_point(data=random_data, aes(PHYLO_RPE_NULL2, PE_WE_P), alpha=1/100, colour = "black", size=3) + # alpha=1/100,
  geom_point(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P), colour="lightgoldenrodyellow", size=5) +
  geom_point(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P,  colour=P_PHYLO_RPE2_SIG), size=9) +#P_PHYLO_RPE2_SIG_POINT_SIZE
  scale_colour_manual(values = col_scheme, labels=legend_labels, name="") +
  stat_smooth(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P), colour = "grey", method="lm", se=FALSE) +
  labs(x = "\nPhylogenetic endemism on comparison tree", y = "Phylogenetic endemism on actual tree\n") +
  #geom_point(data=biodiverse_results_concatenated, aes(RICHNESS_ALL, PHYLO_RPE_NULL1, colour="NULL1")) +
  #stat_smooth(data=biodiverse_results_concatenated,aes(RICHNESS_ALL, PHYLO_RPE_NULL1,  colour="NULL1"), method="lm", se=FALSE)+
  #geom_point(data=biodiverse_results_concatenated, aes(RICHNESS_ALL, PHYLO_RPE_NULL2, colour="NULL2")) +
  #stat_smooth(data=biodiverse_results_concatenated, aes(RICHNESS_ALL, PHYLO_RPE_NULL2,  colour="NULL2"), method="lm", se=FALSE) +
  theme(
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.key.size=unit(1,"cm"),
    legend.text=element_text(size=rel(3.8)),
    axis.text = element_text(size=rel(3.8), colour="black"),
    axis.title = element_text(size=rel(3.8), colour="black"),
    #legend.text=element_text(size = 40),
    #axis.text = element_text(size = 40, colour="black"),
    #axis.title = element_text(size = 40, colour="black"),
    #axis.ticks.margin = unit(10, "mm"),
    legend.key = element_rect(colour="black", fill="grey", size=0.5),
    legend.key.height = unit(2.3, "cm"),
    legend.position = c(0.9,0.3),
    legend.background = element_rect(colour="transparent", fill="transparent"),
    panel.grid.major= element_line(colour = "grey"),
    panel.border = element_rect( fill="transparent", colour = "black"))

Cairo(width = 2323, height = 1050, file = paste(out,"/","figure_3_",scenario,".png", sep=""), type="png", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=TRUE) 
print(fig_3_plot)
dev.off()

options("scipen"=10, "digits"=4)
#library(Cairo)
if (pdfs)
{
Cairo(width = 2323, height = 1050, file = paste(out,"/","figure_3_",scenario,".pdf", sep=""), type="pdf", pointsize=40, canvas="white", bg = "white", units="px", dpi=72, family = "Arial", title = "R Graphics Output", fonts = "Sans", version = "1.4", paper = "special", pagecentre=TRUE) 
print(fig_3_plot)
dev.off()
}


#################################################################################################################################






#######################################
#test <- subset(biodiverse_results_concatenated, ,select=c("Element", "P_PE_WE_P", "P_PD_P_SIG", "P_PHYLO_RPD_NULL1", "P_PHYLO_RPD1", "P_PHYLO_RPE2_SIG", "PHYLO_RPE_NULL2", "PE_WE_P"))
# 
#View(test)


#test <- subset(biodiverse_results_concatenated, ,select=c("Element", "P_PE_WE_P", "P_PD_P_SIG", "P_PHYLO_RPD_NULL1", "P_PHYLO_RPD1", "P_PHYLO_RPE2_SIG"))
# 
#View(biodiverse_results_concatenated)

##################################################






