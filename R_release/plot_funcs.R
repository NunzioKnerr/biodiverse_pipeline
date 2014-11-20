
get_legend_rpe = function () {
  col_scheme <- c("Palaeo" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Neo" = "red", "Super" = "#9D00FF", "Mixed"= "#CB7FFF")
  legend_order <-c("Neo","Palaeo", "Not Sig", "Mixed", "Super")
  legend_labels <- c("Neo"="Neo","Palaeo"="Paleo", "Not Sig"="Not significant", "Mixed"="Mixed", "Super"="Super")
  
  results = list(col_scheme=col_scheme, legend_order=legend_order, legend_labels=legend_labels)
  return(results)
}

get_legend_two_tailed_sig = function () {
  col_scheme <- c("Very Highly Sig" = "royalblue4","Highly Sig" = "royalblue1","Not Sig" = "lightgoldenrodyellow", "Very Sig Low" = "red4", "Sig Low" = "red")
  legend_order <- c("Very Sig Low","Sig Low","Not Sig","Highly Sig","Very Highly Sig")
  legend_labels <- c("Very Highly Sig" = "> 0.99","Highly Sig" = "> 0.975","Not Sig" = "Not significant", "Sig Low" = "< 0.025", "Very Sig Low" = "< 0.01")

  results = list(col_scheme=col_scheme, legend_order=legend_order, legend_labels=legend_labels)
  return(results)
}


#  plot categorical maps such as significance thresholded values or CANAPE
plot_categorical_map <- function (df, map_text="", sig_col_name, output_folder, map_margin = 700000, legend_list) {

  col_scheme    = legend_list$col_scheme
  legend_order  = legend_list$legend_order
  legend_labels = legend_list$legend_labels

  df[[sig_col_name]] <- factor(df[[sig_col_name]], levels=legend_order)
  Axis_0 <- "Axis_0"
  Axis_1 <- "Axis_1"
  
  max_x <- map_extent@xmax + map_margin # extent of map + space for legend
  min_x <- map_extent@xmin - map_margin # other extent of map
  max_y <- map_extent@ymax + map_margin # extent of map + space for legend
  min_y <- map_extent@ymin - map_margin # other extent of map

  map_plot <- ggplot(data=df) +  xlim(min_x, max_x) +  ylim(min_y, max_y) +
    geom_tile(aes_string(x=Axis_0, y=Axis_1, fill=sig_col_name)) + 
    scale_fill_manual(values = col_scheme, labels=legend_labels, name=map_text, guide = guide_legend(direction = "horizontal", title.position = "bottom", title.hjust=0.5, title.vjust=.5, label.position="bottom", label.hjust = 0.5, label.vjust = 0.1))+
    geom_polygon(data=map_data, aes(x=long, y=lat, group = group),colour="gray74", fill="transparent") +
    coord_fixed() +

    annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1) +
    annotate("rect", xmin = -250000, xmax =  250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1) +
    annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
    annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
    annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
    annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont)
  
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
  
  print(map_plot)

  if (print_seperate_images == TRUE){
    if (output_PNG == TRUE){
      fname = paste0(output_folder, "/figure_2_c.png")
      print (paste("Plotting to file", fname))
      CairoPNG(width = 2325, height = 2246, file = fname, canvas="white", bg = "white", units="px", dpi=72, title = "Figure 2 c") #
      print(map_plot)
      dev.off()
    }
    
    if (output_PDF == TRUE){
      fname = paste0(output_folder, "/figure_2_c.pdf")
      print (paste("Plotting to file", fname))
      CairoPDF(width = 36.74, height = 39.19, file = fname, pointsize=40, bg = "white", title = "Figure 2 c", version = "1.7", paper = "special", pagecentre=TRUE) #
      print(map_plot)
      dev.off()
    }
  }

}


#  we should be able to factor all this out, but it does not work at the moment
get_scale_bar_components = function () {
  #  scale bar stuff
  components = 
  annotate("rect", xmin = -750000, xmax = -250000, ymin = -4500000, ymax = -4550000, fill = "black", colour = "black", alpha = 1) +
  annotate("rect", xmin = -250000, xmax =  250000, ymin = -4500000, ymax = -4550000, fill = "white", colour = "black", alpha = 1) +
  annotate("text", label = "0", x = -750000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "500", x = -250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "1000", x = 250000, y = -4650000, size=rel(18),  face = 'plain', family = myFont) +
  annotate("text", label = "km", x = 620000, y = -4650000, size=rel(18),  face = 'plain', family = myFont)
  
  return (components)
}

