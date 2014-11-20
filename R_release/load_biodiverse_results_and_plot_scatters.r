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

#################################################################################################
#
#plot values on scatter against richness Actual, NULL1 and NULL2
#
#################################################################################################
# 
p_pe_plot <- ggplot(data=biodiverse_results_concatenated) + 
  #geom_point(data=random_data, aes(ENDW_RICHNESS,PE_WE_P, colour="RANDOMISED")) +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PE_WE_P, colour="NULL0")) +
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PE_WE_P,  colour="NULL0"), method="lm", se=FALSE) +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPE_NULL1, colour="NULL1")) +
  stat_smooth(data=biodiverse_results_concatenated,aes(ENDW_RICHNESS, PHYLO_RPE_NULL1,  colour="NULL1"), method="lm", se=FALSE)+
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPE_NULL2, colour="NULL2")) +
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPE_NULL2,  colour="NULL2"), method="lm", se=FALSE) +
  #labs(title = taxon)+
  #annotate("text", x = 4, y = -0.001, label = taxon) +
  theme(
    text = element_text(family = "HelvLight"),
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
  )+
  theme(
    panel.grid.major= element_line(colour = "grey"),
    panel.border = element_rect( fill="transparent", colour = "black"))

p_pd_plot <- ggplot(data=biodiverse_results_concatenated) + 
  #geom_point(data=random_data, aes(ENDW_RICHNESS,PE_WE_P, colour="RANDOMISED")) +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PD_P, colour="NULL0")) +
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PD_P,  colour="NULL0"), method="lm", se=FALSE) +
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPD_NULL1, colour="NULL1")) +
  stat_smooth(data=biodiverse_results_concatenated,aes(ENDW_RICHNESS, PHYLO_RPD_NULL1,  colour="NULL1"), method="lm", se=FALSE)+
  geom_point(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPD_NULL2, colour="NULL2")) +
  stat_smooth(data=biodiverse_results_concatenated, aes(ENDW_RICHNESS, PHYLO_RPD_NULL2,  colour="NULL2"), method="lm", se=FALSE)+
 # labs(title = taxon)+
  #annotate("text", x = 4, y = -0.001, label = taxon) +
  theme(
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
  )



if (output_PNG == TRUE){
    CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_pd_plot.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 a") #
    print(p_pd_plot)
    dev.off()
  }
  
if (output_PDF == TRUE){
    CairoPDF(width = 36, height = 34, file = paste(output_folder, "figure_pd_plot.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1 a", version = "1.7", paper = "special", pagecentre=TRUE) #
    print(p_pd_plot)
    dev.off()
  }


if (output_PNG == TRUE){
  CairoPNG(width = 2325, height = 2246, file = paste(output_folder, "figure_pe_plot.png", sep=""), canvas="white", bg = "white", units="px", dpi=72, title = "Figure 1 a") #
  print(p_pe_plot)
  dev.off()
}

if (output_PDF == TRUE){
  CairoPDF(width = 36, height = 34, file = paste(output_folder, "figure_pe_plot.pdf",sep=""), pointsize=40, bg = "white", title = "Figure 1 a", version = "1.7", paper = "special", pagecentre=TRUE) #
  print(p_pe_plot)
  dev.off()
}





############################################



############################################################################################################
#
#PLOT AND MAP RPD
#
############################################################################################################

# 
# sigplot <- "P_PHYLO_RPD1_SIG"
# Axis_0 <- "Axis_0"
# Axis_1 <- "Axis_1"
# trait_to_plot_x <- paste("PHYLO_RPD_NULL1")
# trait_to_plot_y <- paste("PE_WE_P")
# 
# #RPD
# biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=c("Very Highly Sig","Highly Sig","Sig Low", "Very Sig Low", "Not Sig"))
# #png(file = paste("Acacia_", sigplot, ".png", sep=""), width = 1024, height=1024)
# map_plot <- ggplot(data=biodiverse_results_concatenated) + 
#   #geom_tile(aes(Axis_0, Axis_1, fill=super)) +  
#   geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
#   scale_fill_manual(values = c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")) +  
#   annotate("text", x = 4, y = 25, label = sigplot) +
#   theme_minimal() + 
#   map + 
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         #legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())
# #dev.off()
# 
# scatter <-  ggplot(data=biodiverse_results_concatenated) + 
#   # geom_point(data=random_data, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, alpha=0.0001, show_guide = FALSE)) +
#   geom_point(data=biodiverse_results_concatenated, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, colour=sigplot, show_guide = TRUE))+
#   scale_colour_manual(values = c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "transparent", "Very Sig Low" = "red4", "Sig Low" = "red"))+
#   theme(
#     #axis.line=element_blank(),axis.text.x=element_blank(),
#     #axis.text.y=element_blank(),axis.ticks=element_blank(),
#     #axis.title.x=element_blank(),
#     #axis.title.y=element_blank(),
#     #legend.position="none",
#     panel.background=element_blank(),
#     #panel.border=element_blank(),
#     panel.grid.major=element_blank(),
#     panel.grid.minor=element_blank()
#   )+
#   theme(
#     #panel.grid.minor= element_line(colour = "grey"),
#     panel.grid.major= element_line(colour = "grey"),
#     panel.border = element_rect( fill="transparent", colour = "black"))
# 
# 
# full_png <- paste(output_dir,"/scatterplot_random_and_observed_", trait_to_plot_x, "_vs_", trait_to_plot_y, "_and_map", ".png", sep="")
# 
# png(filename=full_png, width=2048, height=1024)# for writing to file)
# 
# multiplot(map_plot, scatter, cols=2)
# 
# dev.off()

###########################################################################################################################################  RPE ##############
#
#PLOT AND MAP RPE AND SCATTER PLOT
#
###########################################################################################################################################
# 
# sigplot <- "P_PHYLO_RPE2_SIG"
# Axis_0 <- "Axis_0"
# Axis_1 <- "Axis_1"
# trait_to_plot_x <- paste("PHYLO_RPE_NULL1")
# trait_to_plot_y <- paste("PE_WE_P")
# 
# #RPE
# biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=c("Neo","Palaeo", "Both", "Super", "Not Sig")) # this factor reorders the values so that they appear in the order we want on the legend
# #View(biodiverse_results_concatenated$P_PHYLO_RPE2_SIG)
# 
# #png(file = paste("Acacia_", sigplot, ".png", sep=""), width = 1024, height=1024)
# map_plot <- ggplot(data=biodiverse_results_concatenated) + 
#   #geom_tile(aes(Axis_0, Axis_1, fill=super)) +  
#   geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot)) + # using aes_string allows variables to be passed to ggplot
#   scale_fill_manual(values = c("Palaeo" = "royalblue4","Not Sig" = "lightgoldenrodyellow", "Neo" = "red4", "Super" = "#9D00FF", "Both"= "#CB7FFF")) +  
#   annotate("text", x = 4, y = 25, label = sigplot) +
#   geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent", alpha=1/100) +
#   coord_fixed() +
#   theme_minimal() + 
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())
# 
# 
# scatter <-  ggplot(data=biodiverse_results_concatenated) + 
#   # geom_point(data=random_data, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, alpha=0.0001, show_guide = FALSE)) +
#   geom_point(data=biodiverse_results_concatenated, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, colour=sigplot, show_guide = TRUE), size=3)+
#   scale_colour_manual(values = c("Palaeo" = "royalblue4","Not Sig" = "transparent", "Neo" = "red4", "Super" = "#9D00FF", "Both"= "#CB7FFF"))+
#   geom_abline(size = 0.9, colour="grey") +
#   theme(
#     #axis.line=element_blank(),axis.text.x=element_blank(),
#     #axis.text.y=element_blank(),axis.ticks=element_blank(),
#     #axis.title.x=element_blank(),
#     #axis.title.y=element_blank(),
#     #legend.position="none",
#     panel.background=element_blank(),
#     #panel.border=element_blank(),
#     panel.grid.major=element_blank(),
#     panel.grid.minor=element_blank()
#   )+
#   theme(
#     #panel.grid.minor= element_line(colour = "grey"),
#     panel.grid.major= element_line(colour = "grey"),
#     panel.border = element_rect( fill="transparent", colour = "black"))
# 
# plot(map_plot)
# plot(scatter)
# 
# 
# full_png <- paste(output_dir,"/scatterplot_random_and_observed_", trait_to_plot_x, "_vs_", trait_to_plot_y, "_and_map", ".png", sep="")
#     png(filename=full_png, width=2048, height=1024)# for writing to file)
#     multiplot(map_plot, scatter, cols=2)
# dev.off()
# 
# ggplot(data=biodiverse_results_concatenated) + 
#     # geom_point(data=random_data, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, alpha=0.0001, show_guide = FALSE)) +
#     geom_point(data=biodiverse_results_concatenated, aes_string(x=trait_to_plot_x, y=trait_to_plot_y, colour=sigplot, show_guide = TRUE))+
#     scale_colour_manual(values = c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "transparent", "Very Sig Low" = "red4", "Sig Low" = "red"))+
#     theme(panel.background=element_blank(),
#           panel.grid.major=element_blank(),
#           panel.grid.minor=element_blank()
#   )
