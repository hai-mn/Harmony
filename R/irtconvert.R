## Convert IFA to IRT
#' @title convert2irt
#' @description Convert IFA estimates (threshold and loading) to IRT estimates (difficulty and discrimination)
#' @details convert2irt requires the user to firstly run the 'alignmentout' to obtain the threshold and loading parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export convert2irt
#' @import stringr
#' @param directory entering the directory folder name to store the output
#' @return A list of CSV files (with difficulty and discrimination of file name) in the specific folder.

convert2irt <- function(directory = NULL){

  # File path ---------------------------------------------------
  if (is.null(directory)) {
    filepath <- paste0("Output","_", Sys.Date())
    filepath.misc <- paste0("Output","_", Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  } else {
    filepath <- directory
    filepath.misc <- paste0(directory,"/Misc")
  }
  
  
  if (!file.exists(paste0(filepath,"/loadings.csv"))) {
    stop("\nMust run the `alignmentout` to obtain the threshold and loading parameters first\n")
  }

  ## Number of Groups: latent classes===============================================
  ext1<-readLines(paste0(filepath.misc, "/ext1_input instructions.txt"))
  g<-grep("^.*classes =.*", ext1, ignore.case = T, value=T)
  g.line<-grep("^.*KNOWNCLASS.*", ext1, ignore.case = T, value=T)
  Group <- as.numeric(str_extract_all(g,"\\d+"))
  
  ## Factors: number of continuous latent variables=================================
  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))
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
  
  
  # Set the empty lists having 'Group' length ----------------------------------------
  lclass.file <- vector(mode = "list", length = Group) # empty_list
  f.mean <- vector(mode = "list", length = Group)
  f.var <- vector(mode = "list", length = Group)

  # Obtain mean and variance of factors----------------------------------------------
  for (i in 1:Group){
    ## Read latent class files =======================================================
    lclass.file[[i]] <- readLines(paste0(filepath.misc,"/LatentClass ",i,".txt"))

    ## Set the emply vectors for mean and variance having 'Factor' length
    f.mean[[i]] <- vector(mode = "numeric", length = length(Factor))
    f.var[[i]] <- vector(mode = "numeric", length = length(Factor))

    ## Capture lines having information of mean and variance of factors ===============
    var.line <- grep("^.*Variances.*", lclass.file[[i]], ignore.case = T)
    means.line<-grep("^.*Means.*", lclass.file[[i]], ignore.case = T)

    ## Capture the mean and variance of factors =======================================
    for (j in 1:length(Factor)){
      #f.var[[i]][j]  <- str_extract(lclass.file[[i]][var.line + j], "[-+]?\\d+.\\d+")
      #f.mean[[i]][j] <- str_extract(lclass.file[[i]][means.line[1]+j], "[-+]?\\d+.\\d+")
      f.var[[i]][j]  <- unlist(str_extract(lclass.file[[i]][var.line + j], "[-+]?\\d+.\\d+"))
      f.mean[[i]][j] <- unlist(str_extract(lclass.file[[i]][means.line[1]+j], "[-+]?\\d+.\\d+"))
    }
  }

  ## Build up the mean and variance table, save to csv
  f.mean.dta <- as.data.frame(f.mean); f.var.dta <- as.data.frame(f.var);
  for (i in 1:dim(f.mean.dta)[2]){
    names(f.mean.dta)[i] <- paste0("Mean_G",i)
    names(f.var.dta)[i] <- paste0("Var_G",i)
  }
  ### Extract factor name from ext2_summary of analysis.txt
  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))
  m <- grep("Continuous latent variables",ext2)
  Factor <- str_extract_all(ext2[m+1],"\\w+")[[1]]

  cat("Exporting \"group_factor means and variances.csv\"", paste0("in \"../",filepath, "\""), "folder\n")

  utils::write.csv(cbind(Factor, f.mean.dta, f.var.dta), paste0(filepath,"/group_factor means and variances.csv"), row.names=FALSE)

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

  drop.loading.names <- names(loadings.file[,grep("Loadings", names(loadings.file), value=TRUE)])
  loadings.file.reduced <- dplyr::select(loadings.file, -which(names(loadings.file) %in% drop.loading.names))

  cat("Exporting \"discriminations.csv\"", paste0("in \"../",filepath, "\""), "folder\n")

  utils::write.csv(loadings.file.reduced, paste0(filepath,"/discriminations.csv"), row.names=FALSE)



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
          dplyr::mutate(
            dts = ifelse(Factor.by == Factor[j], (.[,i+2] - as.numeric(f.mean[[i]][j])*loadings.file[,i+2])/loadings.file[,Group+2+i], dts)
          )
      }
      names(thresholds.file[[h]])[Group+2+i] <- paste0("Difficulty",h,"_G",i)
    }

    drop.threshold.names <- names(thresholds.file[[h]][,grep("Threshold", names(thresholds.file[[h]]), value=TRUE)])
    thresholds.file[[h]] <- dplyr::select(thresholds.file[[h]], -which(names(thresholds.file[[h]]) %in% drop.threshold.names))

    cat("Exporting", paste0("\"difficulty",h,".csv\""), paste0("in \"../",filepath, "\""), "folder\n")

    utils::write.csv(thresholds.file[[h]], paste0(filepath,"/difficulty",h,".csv"), row.names=FALSE)
  }
}


