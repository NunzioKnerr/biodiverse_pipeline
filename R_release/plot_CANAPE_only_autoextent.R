#two pass test for RPE using new significance classes from Biodiverse
# x=SIG_1TAIL_PE_WE_P, y=SIG_1TAIL_PHYLO_RPE_NULL2, z=SIG_2TAIL_PHYLO_RPE2
significance_super_fun <- function(x, y, z){
  #  simplify the logic below
  if (is.na(x)) {x = 0}
  if (is.na(y)) {y = 0}
  if (is.na(z)) {z = 0.5}

  if (x <= 0.95 & y <= 0.95) {
    return ("Not Sig")
  } else if (z < 0.025) {
    return ("Neo")
  } else if (z > 0.975) {
    return ("Palaeo")
  } else if (x > 0.99 & y > 0.99) {
    return ("Super")
  } else {
    return ("Mixed")
  }
}


#  Plot the categorised randomisation outputs from Biodiverse
#  Function is in desperate need of a refactor
plot_CANAPE = function (rand_cats_df, plot_file_pfx, map_shape_file, return_data=F) {

  library(sp)    
  library(maptools) 
  library(raster)
  library(RColorBrewer)
  library(grid)
  library(ggplot2)
  library(gridExtra)
  library(Cairo)
  library(extrafont)
  library(rgdal)
  
  
  map_text <- "Categorical Analysis of Neo- And Paleo- Endemism"
  sigplot <- "P_PHYLO_RPE2_CANAPE_SIG"
  col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
  legend_order <-c("Neo","Palaeo", "Not Sig", "Mixed", "Super")
  legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")
  myFont <- choose_font(c("HelvLight", "Arial", "sans"), quiet = TRUE) #load a font if available
  
  if (missing (map_shape_file)) {
    base_dir = "C:/shawn/git/biodiverse_pipeline"
    map_shape_file <- paste(base_dir, "shape_files/coastline_albers.shp", sep="/")
  }
  
  if (missing (plot_file_pfx)) {
    plot_file_pfx = paste(getwd(), "CANAPE") 
  }
  

  biodiverse_results_concatenated = rand_cats_df  
  biodiverse_results_concatenated[[sigplot]] = sapply(
    1:nrow(rand_cats_df), 
    function(x) significance_super_fun(
      rand_cats_df$PE_WE_P[x], 
      rand_cats_df$PHYLO_RPE_NULL2[x], 
      rand_cats_df$PHYLO_RPE2[x]
      # rand_cats_df$SIG_2TAIL_PHYLO_RPE_DIFFC[x]
    )
  )

  print_seperate_images <- TRUE
  output_PNG <- TRUE
  output_PDF <- FALSE
  
  
  map_data   <- readShapePoly(map_shape_file)
  map_extent <- extent(map_data)
  
  ########################################################
  
  
  biodiverse_results_concatenated[, sigplot] <- factor(biodiverse_results_concatenated[, sigplot], levels=legend_order)
  #sig_cats = factor(sig_cats, levels=legend_order)
  Axis_0 <- "Axis_0"
  Axis_1 <- "Axis_1"
  
  
  max_x <- map_extent@xmax+7 # extent of map + space for legend
  min_x <- map_extent@xmin-7 # other extent of map
  max_y <- map_extent@ymax+7 # extent of map + space for legend
  min_y <- map_extent@ymin-7 # other extent of map
  
  map_plot_5 <- ggplot(data=biodiverse_results_concatenated) + xlim(min_x, max_x) +  ylim(min_y, max_y) +
    geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sigplot))+ 
    scale_fill_manual(values = col_scheme,  labels=legend_labels, name="", guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=0.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1, lineheight=2))+  
    labs(title=map_text, aes(vjust = 0.1))+
    geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray55", fill="transparent") +
    annotate("text", label = sigplot, x = 1000, y = -4850000, size=rel(10),  fontface = 'plain', family = myFont) +
    annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1)+
    annotate("rect", xmin = -250000, xmax = 250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1)+
    annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  fontface = 'plain', family = myFont) +
    annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  fontface = 'plain', family = myFont) +
    annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  fontface = 'plain', family = myFont) +
    annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  fontface = 'plain', family = myFont) +
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
      fname = paste0(plot_file_pfx, ".png")
      CairoPNG(width = 2000, height = 2000, file = fname, canvas="white", bg = "white", units="px", dpi=72, title = "R Graphics Output") #
      print(map_plot_5)
      dev.off()
    }
    if (output_PDF == TRUE){
      fname = paste0(plot_file_pfx, ".pdf")
      CairoPDF(width = 36.74, height = 39.19, file = fname, pointsize=40, bg = "white", family =  "HelvLight", title = "Figure 3", version = "1.7", paper = "special", pagecentre=TRUE) #
      print(map_plot_5)
      dev.off()
    }
  }
  
  if (return_data) {
    return (biodiverse_results_concatenated[[sigplot]])
  }
}

plot_CANAPE_all_files = function (wd, recursive=FALSE, return_data=FALSE, ...) {
  if (missing(wd)) {
    wd = getwd()
  }

  files = list.files(
    path=wd, 
    pattern='--sig--SPATIAL_RESULTS.csv$', 
    full.names=TRUE, 
    recursive=recursive
  )
  
  results = data.frame()

  for (file in files) {
    message (file)
    df = read.csv (file)
    if (!return_data) {
      plot_CANAPE (df, plot_file_pfx=file, return_data=return_data, ...)
    }
    else {
      x = plot_CANAPE (df, plot_file_pfx=file, return_data=return_data, ...)
      d = data.frame(file = x)
      colnames(d) = file
      if (ncol(results)) {
        results = cbind (results, d)
      }
      else {
        results = d
      }
    }
  }
  
  if (return_data) {
    return (results)
  }
}
