## Convert IFA to IRT
#' @title convert2irt
#' @description Convert IFA estimates (threshold and loading) to IRT estimates (difficulty and discrimination)
#' @details convert2irt requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export convert2irt
#' @import stringr
#' @return A list of CSV files (with difficulty and discrimination of file name) in the specific folder.

convert2irt <- function(){

  # Set file path --------------------------------------------------------------------
  filepath <- paste0("Output","_",Sys.Date())
  if (!file.exists(paste0(filepath,"/loadings.csv"))) {
    stop("\nMust run the `alignmentout` to obtain the threshold and loading parameters first\n")
  }

  # Set the empty lists having 'Group' length ----------------------------------------
  lclass.file <- vector(mode = "list", length = Group) # empty_list
  f.mean <- vector(mode = "list", length = Group)
  f.var <- vector(mode = "list", length = Group)

  # Obtain mean and variance of factors----------------------------------------------
  for (i in 1:Group){
    ## Read latent class files =======================================================
    lclass.file[[i]] <- readLines(paste0(filepath,"/LatentClass ",i,".txt"))

    ## Set the emply vectors for mean and variance having 'Factor' length
    f.mean[[i]] <- vector(mode = "numeric", length = length(Factor))
    f.var[[i]] <- vector(mode = "numeric", length = length(Factor))

    ## Capture lines having information of mean and variance of factors ===============
    var.line <- grep("^.*Variances.*", lclass.file[[i]], ignore.case = T)
    means.line<-grep("^.*Means.*", lclass.file[[i]], ignore.case = T)

    ## Capture the mean and variance of factors =======================================
    for (j in 1:length(Factor)){
      f.var[[i]][j]  <- unlist(str_extract(lclass.file[[i]][var.line + j], "[-+]?\\d+.\\d+"))
      f.mean[[i]][j] <- unlist(str_extract(lclass.file[[i]][means.line[1]+j], "[-+]?\\d+.\\d+"))
    }
  }

  # Create the discimination parameters from loadings table----------------------------
  loadings.file <- utils::read.csv(file = paste0(filepath,"/loadings.csv"))

  drop.names <- c(names(loadings.file[,grep("SE", names(loadings.file), value=TRUE)]), names(loadings.file[,grep("Invariant", names(loadings.file), value=TRUE)]), "Loadings_Weighted_Average", "Loadings_R_square")
  loadings.file <- dplyr::select(loadings.file, -which(names(loadings.file) %in% drop.names))

  for (i in 1:Group){
    dns <- vector()
    for (j in 1:length(Factor)){
      loadings.file <- loadings.file %>%
        dplyr::mutate(
          dns = ifelse(Factor.by == Factor[j], .[,i+2]*sqrt(as.numeric(f.var[[i]][j])),dns)
          )
    }
    names(loadings.file)[Group+2+i] <- paste0("Discriminations_G",i)
  }

  utils::write.csv(loadings.file, paste0(filepath,"/discriminations.csv"), row.names=FALSE)



  # Create the difficulty parameters from thresholds table------------------------------
  thresholds.file <- vector(mode = "list", length = Threshold.max)


  for (h in 1:Threshold.max){
    thresholds.file[[h]] <- utils::read.csv(file = paste0(filepath,"/threshold",h,".csv"))
    drop.names <- c(names(thresholds.file[[h]][,grep("SE", names(thresholds.file[[h]]), value=TRUE)]), names(thresholds.file[[h]][,grep("Invariant", names(thresholds.file[[h]]), value=TRUE)]), paste0("Threshold",h,"_Weighted_Average"), paste0("Threshold",h,"_R.square"))
    thresholds.file[[h]] <- dplyr::select(thresholds.file[[h]], -which(names(thresholds.file[[h]]) %in% drop.names))

    for (i in 1:Group){
      dts <- vector()
      for (j in 1:length(Factor)){
        thresholds.file[[h]] <- thresholds.file[[h]] %>%
          dplyr::mutate(dts = ifelse(Factor.by == Factor[j], (.[,i+2] - as.numeric(f.mean[[i]][j])*loadings.file[,i+2])/loadings.file[,Group+2+i], dts)
          )
      }
      names(thresholds.file[[h]])[Group+2+i] <- paste0("Difficulty",h,"_G",i)
    }

    utils::write.csv(thresholds.file[[h]], paste0(filepath,"/difficulty",h,".csv"), row.names=FALSE)
  }
}


