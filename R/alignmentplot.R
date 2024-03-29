## Generating Alignment Threshold Plots
#' @title alignmentout
#' @description Generating Alignment Threshold Plots having True/False invariant with estimates of group items and invariant average
#' @details In order to produce the plots, 'alignmentthresholdplot' requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export alignmentthresholdplot
#' @import ggplot2 data.table
#' @param labelfile entering the label file for groups
#' @param directory entering the directory folder name to store the output
#' @param color.blind entering TRUE (default is TRUE) applying for a color-blinded user
#' @return Alignment Threshold Plot(s) files in a specific folder


alignmentthresholdplot <- function(labelfile=NULL, directory=NULL, color.blind=FALSE){

  # File path ---------------------------------------------------
  if (is.null(directory)) {
    filepath <- paste0("Output","_", Sys.Date())
    filepath.misc <- paste0("Output","_", Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  } else {
    filepath <- directory
    filepath.misc <- paste0(directory,"/Misc")
  }


  if (!file.exists(paste0(filepath.misc, "/ext2_summary of analysis.txt"))) {
    stop("\nMust run `alignmentout()` to obtain the threshold information for the graphs\n")
  }

  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))

  ## Factors: number of continuous latent variables=================================
  #ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))
  m<-grep("Continuous latent variables",ext2)
  Factor<-unlist(str_extract_all(ext2[m+1],"\\w+"))

  ## Items: number of dependent variables============================================
  n<-grep("Binary and ordered categorical", ext2, ignore.case = T)
  Item.name<-str_extract_all(ext2[n+1],"\\w+")[[1]]
  for (i in (n+2):(m-1)){
    Item.name<-c(Item.name,str_extract_all(ext2[i],"\\w+")[[1]])
    if (i==(m-1)) Item.name<-Item.name[!is.na(Item.name)]
  }
  Item.n <-  length(Item.name)
  Item.name<-sapply(Item.name, FUN = function(x)str_sub(x,1,8)) #limit to 8 character long for each name

  ## Categories of each Item (Threshold: number of categories - 1)===================
  s <- k <-grep("UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES", ext2, ignore.case = T) + 2 # s: stands for start
  while (!grepl("^[ \t\n]*$", ext2[k])) {
    k<-k+1
    e<-k #e: stands for end
  }

  Category<-vector(mode = "numeric", length=length(Item.name))
  for (i in 2:length(Item.name)){
    while ((s<e)&(!str_detect(ext2[s], Item.name[i]))){
      s<-s+1
    }
    Category[i-1]<-as.numeric(str_sub(ext2[s-1],str_locate(ext2[s-1],"y")+2, str_locate(ext2[s-1],"y")+3))
  }

  Category[length(Item.name)]<-as.numeric(str_sub(ext2[e-1],str_locate(ext2[e-1],"y")+2, str_locate(ext2[e-1],"y")+3))
  Threshold <- Category - 1

  Threshold.max<-max(Threshold)
  # Provide the number of thresholds -------------------------------------------
  #Threshold.max <- as.numeric(readline(prompt="Please enter the number of threshold files (number of thresholds): "))

  #condit.input <- readline(prompt="Input the label file for groups (y/n)?")
  if (!is.null(labelfile)){
    #labelfile <- readline(prompt="Input path and legend's label file name (use /): ")


    my_label <- readxl::read_excel(labelfile)
    GroupLabel<-append(my_label[,2, drop=TRUE],"Invariant Average")


    for (i in 1:Threshold.max){

      # Read data ----------------------------------------------------------
      if (!file.exists(paste0(filepath,"/threshold",i,".csv"))) {
        stop("\nMust run `alignmentout()` to obtain the threshold information for the graphs\n")
      }
      eg2.w <- read.csv(file = paste0(filepath,"/threshold",i,".csv"), stringsAsFactors = TRUE)

      # Transform from wide to long -----------------------------------------
      Thresgrp <- names(eg2.w[,grep("Threshold._[GW]", names(eg2.w), value = TRUE)])
      Thresgrp.se <- names(eg2.w[,grep("SE", names(eg2.w), value = TRUE)])
      Thresgrp.inv <- names(eg2.w[,grep("Invariant", names(eg2.w), value = TRUE)])

      Group <- as.numeric(length(Thresgrp.inv))

      eg2.l<-melt(setDT(eg2.w),
        measure.vars = list(Thresgrp, Thresgrp.se, Thresgrp.inv),
        variable.name = 'lclass', value.name = c(paste0('Threshold',i), 'SE', 'Invariant'))

      eg2.l <- eg2.l[,-3] # delete the Threshold_Rsquare
      eg2.l<-as.data.frame(eg2.l) # ggplot2 works with data.frame rather than data.table
      eg2.l$Invariant <- ifelse((eg2.l$lclass == (Group+1) & !is.na(paste0(eg2.l$Threshold,"i"))), "True", eg2.l$Invariant)


      write.csv(eg2.l, paste0(filepath.misc,"/thresholds",i,"_longform.csv"), row.names=FALSE)

      # step to order the graph on weighted_average value
      eg2.l$weighted.avg <- rep(eg2.l[,4][which(eg2.l$lclass==as.numeric(Group+1))], Group+1)


      # Plot ggplot ---------------------------------------------------------

      ## set colors
      if (color.blind == TRUE){
        group.colors <- c(scales::dichromat_pal("BluetoOrangeRed.14")(Group),"#000000")
      }  else {
        group.colors <- c(scales::hue_pal()(Group),"#000000")
      }

      names(group.colors) <- levels(eg2.l$lclass);

      ## set shape
      if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
        group.shape <- c(17,16)
      } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
        group.shape <- 16
      } else {group.shape <- 17}

      ## Plotting ggplot
      gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,7],na.rm = T,.desc=T), y = eg2.l[,4])) +
        geom_point(aes(shape = Invariant, colour = lclass), size = 3) +
        scale_shape_manual(values = group.shape, na.translate = F) +
        scale_colour_manual(name = "Group\n(Latent Class&\nInvariant)", values = group.colors, labels = GroupLabel) +
        coord_flip() +
        facet_grid(Factor.by ~ ., scales = "free", switch = "y") +
        xlab("Item") +
        ylab(paste("Threshold", i)) +
        labs(title=paste("Alignment model - Threshold", i, "Plot"), subtitle="Groups") +
        theme_bw()

      ## Save to TIF/TIFF
      cat("Exporting", paste0("\"alignment model - Threshold", i, " - ", Group, " groups.tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
      ggsave(filename = paste0(filepath,"/alignment model - Threshold", i, " - ", Group, " groups.tiff"), gg, width = 10, height = 8, dpi = 300, units = "in", device = "tiff")

    }

    } else {


      for(i in 1:Threshold.max){

        # Read data ----------------------------------------------------------
        if (!file.exists(paste0(filepath,"/threshold",i,".csv"))) {
          stop("\nMust run `alignmentout()` to obtain the threshold information for the graphs\n")
        }
        eg2.w <- read.csv(file = paste0(filepath,"/threshold",i,".csv"), stringsAsFactors = TRUE)

        # Transform from wide to long -----------------------------------------
        Thresgrp <- names(eg2.w[,grep("Threshold._[GW]", names(eg2.w), value = TRUE)])
        Thresgrp.se <- names(eg2.w[,grep("SE", names(eg2.w), value = TRUE)])
        Thresgrp.inv <- names(eg2.w[,grep("Invariant", names(eg2.w), value = TRUE)])

        Group <- as.numeric(length(Thresgrp.inv))

        eg2.l<-melt(setDT(eg2.w),
          measure.vars = list(Thresgrp, Thresgrp.se, Thresgrp.inv),
          variable.name = 'lclass', value.name = c(paste0('Threshold',i), 'SE', 'Invariant'))

        eg2.l <- eg2.l[,-3] # delete the Threshold_Rsquare
        eg2.l<-as.data.frame(eg2.l) # ggplot2 works with data.frame rather than data.table
        eg2.l$Invariant <- ifelse((eg2.l$lclass == (Group+1) & !is.na(paste0(eg2.l$Threshold,"i"))), "True", eg2.l$Invariant)


        write.csv(eg2.l, paste0(filepath.misc,"/thresholds",i,"_longform.csv"), row.names=FALSE)

        # step to order the graph on weighted_average value
        eg2.l$weighted.avg <- rep(eg2.l[,4][which(eg2.l$lclass==as.numeric(Group+1))], Group+1)

        # Plot ggplot ---------------------------------------------------------

        ## set colors
        if (color.blind == TRUE){
          group.colors <- c(scales::dichromat_pal("BluetoOrangeRed.14")(Group),"#000000")
        }  else {
          group.colors <- c(scales::hue_pal()(Group),"#000000")
        }

        names(group.colors) <- levels(eg2.l$lclass);

        ## set shape
        if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
          group.shape <- c(17,16)
        } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
          group.shape <- 16
        } else {group.shape <- 17}

        ## Plotting ggplot
        gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,7],na.rm = T,.desc=T), y = eg2.l[,4])) +
          geom_point(aes(shape = Invariant, colour = lclass), size = 3) +
          scale_shape_manual(values = group.shape, na.translate = F) +
          scale_colour_manual(name = "Group\n(Latent Class&\nInvariant)", values = group.colors) +
          coord_flip() +
          facet_grid(Factor.by ~ ., scales = "free", switch = "y") +
          xlab("Item") +
          ylab(paste("Threshold", i)) +
          labs(title=paste("Alignment model - Threshold", i, "Plot"), subtitle="Groups") +
          theme_bw()

        ## Save to TIF/TIFF
        cat("Exporting", paste0("\"alignment model - Threshold", i, " - ", Group, " groups.tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
        ggsave(filename = paste0(filepath,"/alignment model - Threshold", i, " - ", Group, " groups.tiff"), gg, width = 10, height = 8, dpi = 300, units = "in", device = "tiff")
      }
  }
}





## Generating Alignment Loading Plot
#' @title alignmentloadingplot
#' @description Generating Alignment Loading Plot having True/False invariant with estimates of group items and invariant average
#' @details In order to produce the plots, 'alignmentloadingplot' requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export alignmentloadingplot
#' @import ggplot2 data.table scales
#' @param labelfile entering the label file for groups
#' @param directory entering the directory folder name to store the output
#' @param color.blind entering TRUE (default is TRUE) applying for a color-blinded user
#' @return An Alignment Loading Plot file in a specific folder


alignmentloadingplot <- function(labelfile=NULL, directory=NULL, color.blind=FALSE){

  # File path ---------------------------------------------------
  if (is.null(directory)) {
    filepath <- paste0("Output","_", Sys.Date())
    filepath.misc <- paste0("Output","_", Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  } else {
    filepath <- directory
    filepath.misc <- paste0(directory,"/Misc")
  }

  if (!file.exists(paste0(filepath,"/loadings.csv"))) {
    stop("\nMust run `alignmentout()` to obtain the loadings information for the graph\n")
  }

  #condit.input <- readline(prompt="Input the label file for groups (y/n)?")
  if (!is.null(labelfile)){

    #labelfile <- readline(prompt="Input path and legend's label file name (use /): ")


    my_label <- readxl::read_excel(labelfile)
    GroupLabel<-append(my_label[,2, drop=TRUE],"Invariant Average")


    # Read data ---------------------------------------------------
    if (!file.exists(paste0(filepath,"/loadings.csv"))) {
      stop("\nMust run `alignmentout()` to obtain the loadings information for the graph\n")
    }
    eg2.w <- read.csv(file = paste0(filepath,"/loadings.csv"), stringsAsFactors = TRUE)


    # Transform from wide to long -----------------------------------------
    Loadinggrp <- names(eg2.w[,grep("Loadings_[GW]", names(eg2.w), value=TRUE)])
    Loadinggrp.se <- names(eg2.w[,grep("SE", names(eg2.w), value=TRUE)])
    Loadinggrp.inv <- names(eg2.w[,grep("Invariant", names(eg2.w), value=TRUE)])

    Group <- as.numeric(length(Loadinggrp.inv))

    eg2.l <- melt(setDT(eg2.w),
      measure.vars = list(Loadinggrp, Loadinggrp.se, Loadinggrp.inv),
      variable.name = 'lclass', value.name = c('Loading', 'SE', 'Invariant'))

    eg2.l <- eg2.l[,-3]
    eg2.l <- as.data.frame(eg2.l)

    eg2.l$Invariant <- ifelse((eg2.l$lclass == (Group+1) & !is.na(eg2.l$Loading)), "True", eg2.l$Invariant)


    write.csv(eg2.l, paste0(filepath.misc,"/loadings_longform.csv"), row.names=FALSE)

    # step to order the graph on weighted_average value
    eg2.l$weighted.avg <- rep(eg2.l[,4][which(eg2.l$lclass==as.numeric(Group+1))], Group+1)



    # Plot ggplot ---------------------------------------------------------

    ## set colors
    if (color.blind == TRUE){
      group.colors <- c(scales::dichromat_pal("BluetoOrangeRed.14")(Group),"#000000")
    }  else {
      group.colors <- c(scales::hue_pal()(Group),"#000000")
    }

    names(group.colors) <- levels(eg2.l$lclass);

    ## set shape
    if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
      group.shape <- c(17,16)
    } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
      group.shape <- 16
    } else {group.shape <- 17}

    ## Plotting gg
    gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,7],na.rm = T,.desc=T), y = eg2.l[,4])) +
      geom_point(aes(shape = Invariant, colour = lclass), size = 3) +
      scale_shape_manual(values = group.shape, na.translate = F) +
      scale_colour_manual(name = "Group\n(Latent Class&\nInvariant)", values = group.colors, labels = GroupLabel) +
      coord_flip() +
      facet_grid(Factor.by ~ ., scales = "free", switch = "y") +
      xlab("Item") +
      ylab("Loadings") +
      labs(title = "Alignment model - Loadings Plot", subtitle = "Groups") +
      theme_bw()

    ## Save to TIF/TIFF
    cat("Exporting", paste0("\"alignment model - Loadings - ", Group, " groups.tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
    ggsave(filename = paste0(filepath,"/alignment model - Loadings - ", Group, " groups.tiff"), gg, width = 10, height = 8, dpi = 300, units = "in", device = "tiff")


  } else {


    # Read data ---------------------------------------------------
    if (!file.exists(paste0(filepath,"/loadings.csv"))) {
      stop("\nMust run `alignmentout()` to obtain the loadings information for the graph\n")
    }
    eg2.w <- read.csv(file = paste0(filepath,"/loadings.csv"), stringsAsFactors = TRUE)


    # Transform from wide to long -----------------------------------------
    Loadinggrp <- names(eg2.w[,grep("Loadings_[GW]", names(eg2.w), value=TRUE)])
    Loadinggrp.se <- names(eg2.w[,grep("SE", names(eg2.w), value=TRUE)])
    Loadinggrp.inv <- names(eg2.w[,grep("Invariant", names(eg2.w), value=TRUE)])

    Group <- as.numeric(length(Loadinggrp.inv))

    eg2.l<-melt(setDT(eg2.w),
      measure.vars = list(Loadinggrp, Loadinggrp.se, Loadinggrp.inv),
      variable.name = 'lclass', value.name = c('Loading', 'SE', 'Invariant'))

    eg2.l <- eg2.l[,-3]
    eg2.l<-as.data.frame(eg2.l)

    eg2.l$Invariant <- ifelse((eg2.l$lclass == (Group+1) & !is.na(eg2.l$Loading)), "True", eg2.l$Invariant)

    write.csv(eg2.l, paste0(filepath.misc,"/loadings_longform.csv"), row.names=FALSE)

    # step to order the graph on weighted_average value
    eg2.l$weighted.avg <- rep(eg2.l[,4][which(eg2.l$lclass==as.numeric(Group+1))], Group+1)


    # Plot ggplot ---------------------------------------------------------

    ## set colors
    if (color.blind == TRUE){
      group.colors <- c(scales::dichromat_pal("BluetoOrangeRed.14")(Group),"#000000")
    }  else {
      group.colors <- c(scales::hue_pal()(Group),"#000000")
    }

    names(group.colors) <- levels(eg2.l$lclass);

    ## set shape
    if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
      group.shape <- c(17,16)
    } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
      group.shape <- 16
    } else {group.shape <- 17}

    ## Plotting gg
    gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,7],na.rm = T,.desc=T), y = eg2.l[,4])) +
      geom_point(aes(shape = Invariant, colour = lclass), size = 3) +
      scale_shape_manual(values = group.shape, na.translate = F) +
      scale_colour_manual(name = "Group\n(Latent Class&\nInvariant)", values = group.colors) +
      coord_flip() +
      facet_grid(Factor.by ~ ., scales = "free", switch = "y") +
      xlab("Item") +
      ylab("Loadings") +
      labs(title = "Alignment model - Loadings Plot", subtitle = "Groups") +
      theme_bw()

    ## Save to TIF/TIFF
    cat("Exporting", paste0("\"alignment model - Loadings - ", Group, " groups.tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
    ggsave(filename = paste0(filepath,"/alignment model - Loadings - ", Group, " groups.tiff"), gg, width = 10, height = 8, dpi = 300, units = "in", device = "tiff")
  }
}

