library(sp)    
library(maptools) 
library(raster)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(gridExtra)
library(Cairo)
library(extrafont)
#loadfonts()

myFont <- choose_font(c("HelvLight", "Arial", "sans"), quiet = TRUE)#load a font if available

source("./R_release/biodiverse_path_reference.R")
observed_data <- paste0("C:/biodiverse_pipeline/pipeline_test/biodiverse_analysed_output_SPATIAL_RESULTS.csv")
randomisation_results  <- paste0("C:/biodiverse_pipeline/pipeline_test/biodiverse_analysed_output_rand--SPATIAL_RESULTS.csv")
map_shape_file <- paste(biodiverse_pipeline_install_folder, "/shape_files/coastline_albers.shp", sep="")
output_folder <-  paste0("C:/biodiverse_pipeline/pipeline_test/")

print_seperate_images <- TRUE
output_PNG <- TRUE
output_PDF <- TRUE

observed_data <- read.table(observed_data, header=T,sep=",")
randomisation_results <- read.table(randomisation_results, header=T,sep=",")
biodiverse_results_concatenated <- cbind(observed_data, randomisation_results)
#biodiverse_results_concatenated<- biodiverse_results_concatenated[!is.na(biodiverse_results_concatenated[,4]),]#trim NA.s
#View(biodiverse_results_concatenated)

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

##########################################################
# Figure 1 a
############################################################

df <- biodiverse_results_concatenated # dataframe to use
Axis_0 <- "Axis_0" #x axis column
Axis_1 <- "Axis_1" #y axis column
sigplot <- "ENDW_RICHNESS" # grid to plot column

yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr") # use colourbrewer to make colours
colours <- yellowOrangeBrown_colours
legend_text <- paste("Taxon Richness", sep="") # text for the legend
legend_position <- paste("top", sep="") # position of the legend
rounding_digits <- 0 # rounding digits to use

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

#Create vectors for legend text using the max and min vaules in the raster and a number in between
ENDW_RICHNESS.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5)
ENDW_RICHNESS.hs.round <- round(ENDW_RICHNESS.hs, digits=rounding_digits)
ENDW_RICHNESS.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=0)
ENDW_RICHNESS.limits_set <- c(0,ENDW_RICHNESS.max_val+0.1)

p1 <- ggplot(data=df) + xlim(min_x, max_x) +  ylim(min_y, max_y) +  
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + 
  scale_fill_gradientn(name = legend_text, limits = ENDW_RICHNESS.limits_set, colours = colours, breaks= ENDW_RICHNESS.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust=0.5, title.vjust=0.9, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + #
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #annotate("text", label = "a", x = 2000000, y = -4750000, size=rel(20),  face = 'plain', family = myFont) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
 theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.key.height = unit(1.1, "cm"),legend.margin = unit(2, "cm"),
        legend.key.width = unit(6.2, "cm"),legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        panel.grid = element_blank(),panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p1)

if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_1_a.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 a") #
  print(p1)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36, height = 34, file = paste(output_folder, "figure_1_a.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1 a", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(p1)
  dev.off()
}
}
########################################################
# Figure 1 b
########################################################
df <- biodiverse_results_concatenated
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "ENDW_WE"
legend_text <- paste("Weighted Endemism", sep="")
legend_position <- paste("top", sep="")
rounding_digits <- 2
yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

#   Create vectors for legend text using the max and min vaules in the raster and a number in between
ENDW_WE.hs <- seq(0,max(biodiverse_results_concatenated$ENDW_WE),length.out=5)
ENDW_WE.hs.round <- round(ENDW_WE.hs, digits=rounding_digits)
ENDW_WE.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=rounding_digits)
ENDW_WE.limits_set <- c(0,ENDW_WE.max_val+0.01)

p2 <- ggplot(data=df)+ xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) +
  scale_fill_gradientn(name = legend_text, limits = ENDW_WE.limits_set, colours = colours, breaks= ENDW_WE.hs.round,  guide = guide_colourbar(direction = "horizontal", title.position = "top",  title.hjust=0.5, title.vjust=0.99, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + #
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #annotate("text", label = "b", x = -1700000, y = -4750000, size =rel(20), face = 'plain', family = myFont, ) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +   
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.key.height = unit(1.1, "cm"),legend.margin = unit(2, "cm"),
        legend.key.width = unit(6.2, "cm"),legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        panel.grid = element_blank(),panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p2)

if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_1_b.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 b") #
  print(p2)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_1_b.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1 b", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(p2)
  dev.off()
}
}
#########################################
# Figure 1 c
#########################################
df <- biodiverse_results_concatenated
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "PD_P"
rounding_digits <- 3
legend_text <- paste("Phylogenetic diversity", sep="")
legend_position <- paste("bottom", sep="")

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours
#   Create vectors for legend text using the max and min vaules in the raster and a number in between
PD.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5) #PD_P version
PD.hs.round <- round(PD.hs, digits=rounding_digits)
PD.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=rounding_digits)
PD.limits_set <- c(0,PD.max_val+0.1)

p3 <- ggplot(data=df) + xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + 
  scale_fill_gradientn(name = legend_text, limits = PD.limits_set, colours = colours, breaks= PD.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #annotate("text", label = "c", x =  2000000, y = -1000000, size=rel(20), face = 'plain', family = myFont) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))
        
print(p3)

if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_1_c.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 c") #
  print(p3)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_1_c.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1 c", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(p3)
  dev.off()
}
}
#########################################
# Figure 1 d
#########################################
df <- biodiverse_results_concatenated
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"
sigplot <- "PE_WE_P"
rounding_digits <- 3

yellowOrangeBrown_colours <- brewer.pal(9, "YlOrBr")
colours <- yellowOrangeBrown_colours

PE_WE.hs <- seq(0,max(biodiverse_results_concatenated[,sigplot]),length.out=5) #PD_P version
PE_WE.hs.round <- round(PE_WE.hs, digits=rounding_digits)
PE_WE.max_val <- round(max(biodiverse_results_concatenated[,sigplot]), digits=rounding_digits)
PE_WE.limits_set <- c(0,PE_WE.max_val+.001)
legend_text <- paste("Phylogenetic endemism", sep="")

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

p4 <- ggplot(data=df)+ xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + 
  scale_fill_gradientn(name = legend_text, limits = PE_WE.limits_set, colours = colours, breaks= PE_WE.hs.round, guide = guide_colourbar(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, raster=FALSE)) + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #annotate("text", label = "d", x = -1700000, y = -1000000, size=rel(20), face = 'plain', family = myFont) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(4), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(p4)

if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_1_d.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 d") #
  print(p4)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_1_d.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(p4)
  dev.off()
}
}
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

if (output_PNG == TRUE){
CairoPNG(width = 3323, height = 3200, file = paste(output_folder, "figure_1.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1") #
grid.arrange(gA, gB, gC, gD, nrow=2) # Arrange the four charts
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_1.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1", version = "1.7", paper = "special", pagecentre=TRUE) #
   grid.arrange(gA, gB, gC, gD, nrow=2)  # Arrange the four charts
dev.off()
}
getwd()
#Sys.setenv(R_GSCMD ="C:/Program Files/gs/gs9.10/bin/gswin64c.exe")
#Sys.getenv("R_GSCMD")
#names(pdfFonts())
#embed_fonts("figure_1.pdf", outfile = "figure_1_scale_embed.pdf")

#########################################################################
#Figure 2 a
########################################################################
map_text <- "Phylogenetic Diversity"
sigplot <- "P_PD_P_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

#PD_map <- plot_map_and_scatter(map_text, biodiverse_results_concatenated, sigplot, col_scheme, legend_order)

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_plot_1 <- ggplot(data=biodiverse_results_concatenated) +  xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ # using aes_string allows variables to be passed to ggplot
  #scale_fill_manual(values = col_scheme) +  
  scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "top", title.hjust=0.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90), label.hjust = 0.5, label.vjust = 0.5
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #labs(x=NULL, y=NULL) +
  #annotate("text", label = "a", x = 2000000, y = -4750000, size=rel(20),  face = 'plain', family = myFont) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(), axis.text = element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(map_plot_1)

if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_2_a.png",sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 2 a") #
  print(map_plot_1)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_2_a.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 2 a", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(map_plot_1)
  dev.off()
}
}
#########################################################################
#Figure 2 b
########################################################################
map_text <- "Relative Phylogenetic Diversity"
sigplot <- "P_PHYLO_RPD2_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = " > 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_plot_2 <- ggplot(data=biodiverse_results_concatenated) +  xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+  
  scale_fill_manual(values = col_scheme,  labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "top",  title.hjust=0.5, title.vjust=0.1, label.position="bottom", label.hjust = 0.5, label.vjust = 0.5))+ #label.theme = element_text(angle = 90)
  theme_minimal() + 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  #annotate("text", label = "b", x = -1700000, y = -4750000, size =rel(20), face = 'plain', family = myFont) +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.93),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(map_plot_2)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_2_b.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 2 a") #
  print(map_plot_2)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_2_b.pdf", sep=""), pointsize=40, bg = "white", title = "Figure 2 b", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(map_plot_2)
  dev.off()
}
}
#########################################################################
#Figure 2 c
########################################################################
map_text <- "Phylogenetic Endemism"
sigplot <- "P_PE_WE_P_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
#labels_order <- c("Very Low","just Low","Not Sig","Highly Sig","Very Highly Significant")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_plot_3 <- ggplot(data=biodiverse_results_concatenated) +  xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ 
  scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1))+
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(map_plot_3)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_2_c.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 2 c") #
  print(map_plot_3)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_2_c.pdf", sep=""), pointsize=40, bg = "white", title = "Figure 2 c", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(map_plot_3)
  dev.off()
}
}
#########################################################################
#Figure 2 d
########################################################################
map_text <- "Relative Phylogenetic Endemism"
sigplot <- "P_PHYLO_RPE2_ONE_STEP_SIG"
col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_plot_4 <- ggplot(data=biodiverse_results_concatenated) +  xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+
  scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1))+ 
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
  coord_fixed() +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(6.2, "cm"),
        legend.position=c(.5, 0.06),
        legend.direction='horizontal',
        legend.title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,-0.61,-0.61),"line"))

print(map_plot_4)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_2_d.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 2 d") #
  print(map_plot_4)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_2_d.pdf", sep=""), pointsize=40, bg = "white", title = "Figure 2 d", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(map_plot_4)
  dev.off()
}
}
#################################################

# Get the widths
gA <- ggplot_gtable(ggplot_build(map_plot_1))
gB <- ggplot_gtable(ggplot_build(map_plot_2))
gC <- ggplot_gtable(ggplot_build(map_plot_3))
gD <- ggplot_gtable(ggplot_build(map_plot_4))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth

if (output_PNG == TRUE){
CairoPNG(width = 3323, height = 3200, file = paste(output_folder, "figure_2.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
grid.arrange(gA, gB, gC, gD, nrow=2) # Arrange the four charts
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_2.pdf", sep=""), pointsize=40, bg = "white", family =  "HelvLight", title = "Figure 2", version = "1.7", paper = "special", pagecentre=TRUE) #
grid.arrange(gA, gB, gC, gD, nrow=2)  # Arrange the four charts
dev.off()
}
#Sys.setenv(R_GSCMD ="C:/Program Files/gs/gs9.10/bin/gswin64c.exe")
#embed_fonts("figure_2.pdf", outfile = "figure_2_embed_scale.pdf")

########################################################

map_text <- "Categorical Analysis of Neo- And Paleo- Endemism"
sigplot <- "P_PHYLO_RPE2_SIG"
col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
legend_order <-c("Neo","Palaeo", "Not Sig", "Mixed", "Super")
legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
Axis_0 <- "Axis_0"
Axis_1 <- "Axis_1"

max_x <- map_extent@xmax+700000 # extent of map + space for legend
min_x <- map_extent@xmin-700000 # other extent of map
max_y <- map_extent@ymax+700000 # extent of map + space for legend
min_y <- map_extent@ymin-700000 # other extent of map

map_plot_5 <- ggplot(data=biodiverse_results_concatenated) + xlim(min_x, max_x) +  ylim(min_y, max_y) +
  geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ 
  scale_fill_manual(values = col_scheme,  labels=legend_labels, name="", guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1, lineheight=2))+  
  labs(title=map_text, aes(vjust = 0.1))+
  geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent") +
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
  annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  coord_fixed() +
  theme(text = element_text(family = myFont),
        strip.background = element_blank(),
        title = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key =element_rect(colour = "black", fill="transparent", size=1),
        legend.key.height = unit(1.1, "cm"),
        legend.key.width = unit(7, "cm"),
        legend.position=c(.5, 0.001),
        legend.direction='horizontal',
        legend.text = element_text(colour = 'black', angle = 0, size=rel(3.8), face = 'plain', family = myFont),
        panel.grid = element_blank(),
        panel.background=element_blank(),#element_rect(colour = "black", fill="white", size = 1),
        panel.border = element_blank(),
        plot.background=element_blank(),#element_rect(colour = "black", fill="white", size = 1),
        plot.margin=unit(c(0,0,0,0),"line"))

print(map_plot_5)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
 CairoPNG(width = 2000, height = 2000, file = paste(output_folder, "figure_3a.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
 print(map_plot_5)
 dev.off()
}
if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_3a.pdf", sep=""), pointsize=40, bg = "white", family =  "HelvLight", title = "Figure 3", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(map_plot_5)
  dev.off()
}
}
###################################################
#Figure 3 PE vs PE_NULL2
####################################################
sigplot <- "P_PHYLO_RPE2_SIG"
col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "transparent", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
legend_order <-c("Neo","Palaeo", "Mixed", "Super",  "Not Sig")
legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")

biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)

PHYLO_RPE_NULL2_PE_WE_P.lm <- lm(PHYLO_RPE_NULL2 ~ PE_WE_P, data=biodiverse_results_concatenated)
r_sq <- summary(PD_PE.lm)$r.squared
r_sq <- round(r_sq, digits=4)
r_on_plot <- paste("R^2 ==", r_sq, sep="")
r_sq_position <- max(biodiverse_results_concatenated$PHYLO_RPE_NULL2)*0.9

fig_3_plot <- ggplot() +
   geom_point(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P), colour="#FAFAD2", size=6) +
  geom_point(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P,  colour=P_PHYLO_RPE2_SIG), size=9) +  scale_colour_manual(values = col_scheme, labels=legend_labels, name="", guide=FALSE) +
  stat_smooth(data=biodiverse_results_concatenated, aes(PHYLO_RPE_NULL2, PE_WE_P), colour = "grey69", size=1.8, method="lm", se=FALSE) +
  labs(x = "\nPhylogenetic Endemism on Comparison Tree", y = "Phylogenetic Endemism on Actual Tree\n") +
  annotate("text", label = r_on_plot, x = r_sq_position, y = 0, size =rel(20), face = 'bold', parse=TRUE) +
  theme(text = element_text(family = myFont),
        panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.text = element_text(size=rel(3.8), colour="black"),
    axis.title = element_text(size=rel(3.8), colour="black"),
    legend.key=element_blank(),
    panel.grid.major= element_line(colour = "grey"),
    panel.border = element_rect( fill="transparent", colour = "black"))

print(fig_3_plot)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
  CairoPNG(width = 2000, height = 2000, file = paste(output_folder, "figure_3b.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
  print(fig_3_plot)
  dev.off()
}
if (output_PDF == TRUE){
  CairoPDF(width = 36.74, height = 39.19, file = paste(output_folder, "figure_3b.pdf", sep=""), pointsize=40, bg = "white", family =  "HelvLight", title = "Figure 3", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(fig_3_plot)
  dev.off()
}
}
# Get the widths
gA <- ggplot_gtable(ggplot_build(map_plot_5))
gB <- ggplot_gtable(ggplot_build(fig_3_plot))
maxWidth <- unit.pmax(gA$widths[2:3], gB$widths[2:3])

if (output_PNG == TRUE){
 CairoPNG(width = 2000, height = 4000, file = paste(output_folder, "figure_3.png", sep=""), canvas="transparent", bg = "transparent", units="px", dpi=72, title = "R Graphics Output") #
 grid.arrange(gA, gB, nrow=2, widths=c(1.1,1), heights=c(1.1,1))
 dev.off()
}
if (output_PDF == TRUE){
CairoPDF(width = 20, height = 40, file = paste(output_folder, "figure_3.pdf", sep=""), pointsize=40, bg = "white", title = "R Graphics Output", version = "1.7", paper = "special", pagecentre=TRUE) #
  grid.arrange(gA, gB, nrow=2, widths=c(1.1,1), heights=c(1.1,1)) 
dev.off()
}

#loadfonts()
#Sys.setenv(R_GSCMD ="C:/Program Files/gs/gs9.10/bin/gswin64c.exe")
#Sys.getenv("R_GSCMD")
#embed_fonts("figure_3_scale.pdf", outfile = "figure_3_scale_embed.pdf")

# loadfonts(device= "postscript")
# postscript("figure_3.eps", width = 20, height = 40, pointsize=40, family="HelvLight", paper="special", onefile=TRUE) #
# grid.arrange(gA, gB, nrow=2, widths=c(1.1,1), heights=c(1.1,1)) 
# dev.off()
# 
# embed_fonts("figure_3.eps", outfile = "figure_3_embed.eps", options = "-dEPSCrop")
####################

#########################################################################################
# Figure scatterplot PD_P vs SR 
#########################################################################################

SR_PD.lm <- lm(ENDW_RICHNESS ~ PD_P, data=biodiverse_results_concatenated)
r_sq <- summary(SR_PD.lm)$r.squared
r_sq <- round(r_sq, digits=4)
r_on_plot <- paste("R^2 ==", r_sq, sep="")
r_sq_position <- max(biodiverse_results_concatenated$ENDW_RICHNESS)*0.9

fig_s1_plot <- ggplot() +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PD_P), alpha=1, colour = "black", size=3) + 
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PD_P), colour = "grey69", size=1.8, method="lm", se=FALSE) +
  labs(x = "\nTaxon Richness", y = "Phylogenetic Diversity\n") +
  annotate("text", label = r_on_plot, x = r_sq_position, y = 0, size =rel(20), face = 'bold', parse=TRUE) +
  theme(text = element_text(family = myFont),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size=rel(5.5), colour="black"),
        axis.title = element_text(size=rel(5.5), colour="black"),
        legend.key=element_blank(),
        panel.grid.major= element_line(colour = "grey"),
        panel.border = element_rect( fill="transparent", colour = "black")
        )

print(fig_s1_plot)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
CairoPNG(width = 2323, height = 2486, file = paste(output_folder, "figure_SR_vs_PD.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
print(fig_s1_plot)
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 20, height = 20, file = paste(output_folder, "figure_SR_vs_PD.pdf", sep=""), pointsize=40, bg = "white", title = "R Graphics Output", version = "1.7", paper = "special", pagecentre=TRUE) #
print(fig_s1_plot)
dev.off()
}
}

#########################################################################################
# Figure scatterplot PE_P vs SR
#########################################################################################

SR_PE.lm <- lm(ENDW_RICHNESS ~ PE_WE_P, data=biodiverse_results_concatenated)
#summary(SR_PD.lm)
r_sq <- summary(SR_PE.lm)$r.squared
r_sq <- round(r_sq, digits=4)
r_on_plot <- paste("R^2 ==", r_sq, sep="")
r_sq_position <- max(biodiverse_results_concatenated$ENDW_RICHNESS)*0.9

fig_s2_plot <- ggplot() +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PE_WE_P), alpha=1, colour = "black", size=3) + 
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PE_WE_P), colour = "grey69", size=1.8, method="lm", se=FALSE) +
  labs(x = "\nTaxon Richness", y = "Phylogenetic Endemism\n") +
  annotate("text", label = r_on_plot, x = r_sq_position, y = 0, size =rel(20), face = 'bold', parse=TRUE) +
  theme(text = element_text(family = myFont),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size=rel(5.5), colour="black"),
        axis.title = element_text(size=rel(5.5), colour="black"),
        legend.key=element_blank(),
        panel.grid.major= element_line(colour = "grey"),
        panel.border = element_rect( fill="transparent", colour = "black")
        )

print(fig_s2_plot)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
CairoPNG(width = 2323, height = 2486, file = paste(output_folder, "figure_SR_vs_PE.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
print(fig_s2_plot)
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 20, height = 20, file = paste(output_folder, "figure_SR_vs_PE.pdf", sep=""), pointsize=40, bg = "white", title = "R Graphics Output", version = "1.7", paper = "special", pagecentre=TRUE) #
print(fig_s2_plot)
dev.off()
}
}

#########################################################################################
# Figure scatterplot PD_P vs PE
#########################################################################################

PD_PE.lm <- lm(PD_P ~ PE_WE_P, data=biodiverse_results_concatenated)
r_sq <- summary(PD_PE.lm)$r.squared
r_sq <- round(r_sq, digits=4)
r_on_plot <- paste("R^2 ==", r_sq, sep="")
r_sq_position <- max(biodiverse_results_concatenated$PD_P)*0.9

fig_s3_plot <- ggplot() +
  geom_point(data=biodiverse_results_concatenated, aes(PD_P, PE_WE_P), alpha=1, colour = "black", size=3) + 
  stat_smooth(data=biodiverse_results_concatenated, aes(PD_P, PE_WE_P), colour = "grey69", size=1.8, method="lm", se=FALSE) +
  labs(x = "\nPhylogenetic Diversity", y = "Phylogenetic Endemism\n") +
  annotate("text", label = r_on_plot, x =r_sq_position, y = 0, size =rel(20), face = 'bold', parse=TRUE) +
  theme(text = element_text(family = myFont),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size=rel(5.5), colour="black"),
        axis.title = element_text(size=rel(5.5), colour="black"),
        legend.key=element_blank(),
        panel.grid.major= element_line(colour = "grey"),
        panel.border = element_rect( fill="transparent", colour = "black")
        )

print(fig_s3_plot)
if (print_seperate_images == TRUE){
if (output_PNG == TRUE){
CairoPNG(width = 2323, height = 2486, file = paste(output_folder, "figure_PD_vs_PE.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
print(fig_s3_plot)
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 20, height = 20, file = paste(output_folder, "figure_PD_vs_PE.pdf", sep=""), pointsize=40, bg = "white", title = "R Graphics Output", version = "1.7", paper = "special", pagecentre=TRUE) #
print(fig_s3_plot)
dev.off()
}
}
# Get the widths
gA <- ggplot_gtable(ggplot_build(fig_s1_plot))
gB <- ggplot_gtable(ggplot_build(fig_s2_plot))
gC <- ggplot_gtable(ggplot_build(fig_s3_plot))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth

if (output_PNG == TRUE){
CairoPNG(width = 2323, height = 7458, file = paste(output_folder, "figure_4.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
grid.arrange(gA, gB, gC, nrow=3, widths=c(1,1,1), heights=c(1,1,1)) 
dev.off()
}

if (output_PDF == TRUE){
CairoPDF(width = 20, height = 60, file = paste(output_folder, "figure_4.pdf", sep=""), pointsize=40, bg = "white", title = "R Graphics Output", version = "1.7", paper = "special", pagecentre=TRUE) #
grid.arrange(gA, gB, gC, nrow=3, widths=c(1,1,1), heights=c(1,1,1)) 
dev.off()
}
