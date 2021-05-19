## Detect the crosstab' cell size less than or equal to a specified number
#' @title cellsizedetect
#' @description Check the 2 x 2 crosstabs of items having the cell size and below specified by the user
#' @details 'cellsizedetect' requires Mplus output having CROSSTABS FOR CATEGORICAL VARIABLES part
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export cellsizedetect
#' @import stringr
#' @param infile entering the Mplus output file name and path
#' @param n.detect entering the number of cell size to detect, the default is zero
#' @param silent default is TRUE; if specifying FALSE, crosstabs are printed out 
#' @return A list of which crosstab(s) satisfied the input condition.


cellsizedetect <- function(infile="", n.detect="0", silent = TRUE){
  
  ext<-readLines(infile)
  
  # ---------------------------------------------------------------------------------
  if (length(grep("CROSSTABS FOR CATEGORICAL VARIABLES", ext)) == 0) {
    stop("There is no crosstabs to look at the cell size in the Mplus output")
  }
  
  # Check user input --------------------------------------------------------
  readinteger <- function(){
    if(!grepl("^[0-9]+$", n.detect)) {return(readinteger())}
  }
  readinteger()
  
  # Step 1: get information of analysis group and items
  # Basic extraction function  
  pextract <- function(begp, endp, ext){
    ## extract paragraph(s) defined from beginning phrase and ending phrase
    lns <- data.frame(beg=which(grepl(begp,ext)),
                      end=which(grepl(endp,ext)))
    ext.2 <- lapply(seq_along(lns$beg),function(l){
      paste(ext[seq(from=lns$beg[l], to=lns$end[l])])})
    return(ext.2[[1]])
  }
  
  ext1 <- pextract("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS", ext)
  ext2 <- pextract("SUMMARY OF ANALYSIS", "SUMMARY OF DATA|CROSSTABS FOR CATEGORICAL VARIABLES", ext)  
  
  ext1<-gsub("!.*","",ext1)
  l <- grep("CATEGORICAL =", ext1)
  Item.string<-str_extract_all(ext1[l],"\\w+")[[1]]
  i<-l+1
  while (!grepl(";$", ext1[i])){
    Item.string<-c(Item.string,str_extract_all(ext1[i],"\\w+")[[1]])
    i<-i+1
  }
  Item.string<-toupper(Item.string[-1])

  n<-grep("Binary and ordered categorical", ext2, ignore.case = T)
  Item.name<-str_extract_all(ext2[n+1],"\\w+")[[1]]
  i <- n+2
  while (!grepl("^\\s*$",ext2[i])){
    Item.name<-c(Item.name,str_extract_all(ext2[i],"\\w+")[[1]])
    i<-i+1
  }
  
  #### Set up the Item string served for pattern later on
  Itemstring<-gsub(", ","|",toString(union(Item.name,Item.string)))
  
  # Step 2: extract the only crosstabs of analysis group and items
  crosstabs.file <- pextract("CROSSTABS FOR CATEGORICAL VARIABLES", 
                             "RESULTS FOR BASIC ANALYSIS|MODEL FIT INFORMATION", ext)
  
  st<-grep(paste0(Itemstring,"|Count"), crosstabs.file, value=T, ignore.case=T)

  
  k <- grep(paste0(Item.name[1], " AND"), st) #choose the compared group

  # Read string data and split into data frame in list
  output.l <- list()
  r<-vector()
  for (i in 1:length(k)){
    j<-k[i]
    h<-vector()
    while(!grepl("^Total",st[j])){
      h<-append(h,j+1)
      j<-j+1
    }
    output.l[[i]]<-append(r,c(k[i],h))

  }
  # now we got the all string tables of compared group and items
  st.temp <- list()
  for (i in 1:length(output.l)){
    st.temp[[i]] <- st[output.l[[i]]]
  }
  
  
  
  # Step 3: check the existed cell size number

  
  indicator <- vector()
  tabs <- list()
  for (i in 1:length(st.temp)){
    dat <- as.data.frame(do.call(rbind, 
                                strsplit(st.temp[[i]][-c(1:2,length(st.temp[[i]]))], 
                                         split=" {2,10}")), 
                        stringsAsFactors=FALSE)
    dat <- dat[,-c(1:3,ncol(dat))] #crosstab number itself
    dat[] <- lapply(dat, function(x) as.numeric(as.character(x))) # convert to numeric
    tabs[[i]] <- dat
    indicator[i] <- any(apply(apply(tabs[[i]], 2, function(x) x<=as.numeric(n.detect)), 2, any))
  }
  
  if (all(indicator == FALSE)) {
    cat("No cell size equal (or below)", n.detect,"\n")
  } else {
    cat("Crosstab(s) had cell size equal (or below)", n.detect,": \n")
    for (i in 1:length(indicator)) {
      if (indicator[i]==TRUE) {cat(st.temp[[i]][[1]], sep = "\n ")}
    }
    cat("\n")
  }
    

  if(!silent) {
    cat("Crosstab(s): \n")
    for (i in 1:length(indicator)) {
      if (indicator[i]==TRUE) cat(st.temp[[i]], sep = "\n")
      #cat("\n")
    } 
  }


}

