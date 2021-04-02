## Generating Alignment Threshold Plots
#' @title alignmentout
#' @description Generating Alignment Threshold Plots having True/False invariant with estimates of group items and invariant average
#' @details In order to procuce the plots, 'alignmentthresholdplot' requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export alignmentthresholdplot
#' @import ggplot2 data.table
#' @return Alignment Threshold Plot(s) files in a specific folder


alignmentthresholdplot<-function(){

  # Provide the number of thresholds -------------------------------------------
  Threshold.max <- as.numeric(readline(prompt="Please enter the number of threshold files (number of thresholds): "))

  condit.input <- readline(prompt="Input the label file for groups (y/n)?")
  if (tolower(condit.input)=="y"){
    labelfile <- readline(prompt="Input path and legend's label file name (use /): ")


    my_label <- readxl::read_excel(labelfile)
    GroupLabel<-append(my_label$GroupLabel,"Invariant Average")

    # File path ---------------------------------------------------
    filepath <- paste0("Output","_",Sys.Date())
    filepath.misc <- paste0("Output","_",Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc

    for (i in 1:Threshold.max){

      # Read data ----------------------------------------------------------
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
      # Plot ggplot ---------------------------------------------------------

      ## set colors
      group.colors <- c(scales::hue_pal()(Group),"#000000")
      names(group.colors) <- levels(eg2.l$lclass);

      ## set shape
      if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
        group.shape <- c(17,16)
      } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
        group.shape <- 16
      } else {group.shape <- 17}

      ## Plotting ggplot
      gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,4],na.rm = T,.desc=T), y = eg2.l[,4])) +
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

      # File path ---------------------------------------------------
      filepath <- paste0("Output","_",Sys.Date())

      for(i in 1:Threshold.max){

        # Read data ----------------------------------------------------------
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

        # Plot ggplot ---------------------------------------------------------

        ## set colors
        group.colors <- c(scales::hue_pal()(Group),"#000000")
        names(group.colors) <- levels(eg2.l$lclass);

        ## set shape
        if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
          group.shape <- c(17,16)
        } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
          group.shape <- 16
        } else {group.shape <- 17}

        ## Plotting ggplot
        gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,4],na.rm = T,.desc=T), y = eg2.l[,4])) +
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
#' @details In order to procuce the plots, 'alignmentloadingplot' requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export alignmentloadingplot
#' @import ggplot2 data.table
#' @return An Alignment Loading Plot file in a specific folder


alignmentloadingplot<-function(){

  condit.input <- readline(prompt="Input the label file for groups (y/n)?")
  if (tolower(condit.input)=="y"){

    labelfile <- readline(prompt="Input path and legend's label file name (use /): ")


    my_label <- readxl::read_excel(labelfile)
    GroupLabel<-append(my_label$GroupLabel,"Invariant Average")

    # File path ---------------------------------------------------
    filepath <- paste0("Output","_",Sys.Date())
    filepath.misc <- paste0("Output","_",Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc

    # Read data ---------------------------------------------------
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


    # Plot ggplot ---------------------------------------------------------

    ## set colors
    group.colors <- c(scales::hue_pal()(Group),"#000000")
    names(group.colors) <- levels(eg2.l$lclass);

    ## set shape
    if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
      group.shape <- c(17,16)
    } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
      group.shape <- 16
    } else {group.shape <- 17}

    ## Plotting gg
    gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,4],na.rm = T,.desc=T), y = eg2.l[,4])) +
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

    # File path ---------------------------------------------------
    filepath <- paste0("Output","_",Sys.Date())

    # Read data ---------------------------------------------------
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


    # Plot ggplot ---------------------------------------------------------

    ## set colors
    group.colors <- c(scales::hue_pal()(Group),"#000000")
    names(group.colors) <- levels(eg2.l$lclass);

    ## set shape
    if (length(unique(stats::na.omit(eg2.l$Invariant))) == 2){
      group.shape <- c(17,16)
    } else if (unique(stats::na.omit(eg2.l$Invariant)) == "True"){
      group.shape <- 16
    } else {group.shape <- 17}

    ## Plotting gg
    gg <- ggplot(eg2.l, aes(x = forcats::fct_reorder(Item, eg2.l[,4],na.rm = T,.desc=T), y = eg2.l[,4])) +
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

