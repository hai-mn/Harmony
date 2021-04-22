## Detect the cell having a specified size and below
#' @title cellsizedetect
#' @description Check the 2 x 2 crosstabs of items having the cell size and below specified by the user
#' @details 'cellsizedetect' requires Mplus output having CROSSTABS FOR CATEGORICAL VARIABLES part
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export cellsizedetect
#' @import stringr
#' @param infile entering the Mplus output file name and path
#' @param n.detect entering the number of cell size to detect, the default is zero
#' @return Crosstabs between items having the specified cell size and below

cellsizedetect <- function(infile="", n.detect="0"){

  #infile <- readline(prompt="Enter the path & Mplus output file (use / to separate the path file): ")

  ext<-readLines(infile)

  # ---------------------------------------------------------------------------------
  if (length(grep("CROSSTABS FOR CATEGORICAL VARIABLES", ext)) == 0) {
    stop("There is no crosstabs to look at the cell size in the Mplus output")
  }

  #filepath <- paste0("Output","_",Sys.Date())
  #ifelse(!dir.exists(file.path(filepath)), dir.create(file.path(filepath)), FALSE)

  filepath <<- paste0("Output","_",Sys.Date())
  filepath.misc <<- paste0("Output","_",Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  ifelse(!dir.exists(file.path(filepath)), dir.create(file.path(filepath)), FALSE)
  ifelse(!dir.exists(file.path(filepath.misc)), dir.create(file.path(filepath.misc)), FALSE)

  mplussplit(outpath = filepath.misc, inputfile = infile)

  ext1<-readLines(paste0(filepath.misc, "/ext1_input instructions.txt"))
  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))
  m<-grep("Continuous latent variables",ext2)

  Factor<-unlist(str_extract_all(ext2[m+1],"\\w+"))
  Factor.n <- length(Factor)

  n<-grep("Binary and ordered categorical", ext2, ignore.case = T)
  Item.name<-str_extract_all(ext2[n+1],"\\w+")[[1]]
  for (i in (n+2):(m-1)){
    Item.name<-c(Item.name,str_extract_all(ext2[i],"\\w+")[[1]])
    if (i==(m-1)) Item.name<-Item.name[!is.na(Item.name)]
  }
  Item.n <-  length(Item.name)

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
  Threshold.max <- max(Threshold)

  f.stri<-NA; f.stri.c<-NA; f<-NA
  by.items <- vector(mode = "list", length = length(Factor)) # empty_list

  for (i in 1:length(Factor)) {
    f.stri[i]<-paste(Factor[i],"BY ")
    f.stri.c[i]<-paste("^\\s+",Factor[i],"BY\\s+|\\s+;")
    f<-grep(f.stri[i],ext1, ignore.case = T)
    by.items[[i]]<-toupper(gsub(f.stri.c[i], "", ext1[f], ignore.case = T))
  }
  Item<-unlist(by.items) # matches with Item.name

  ## Limit to 8 character long for each name
  Item.orig<-Item # reserve the original Items
  Item<-sapply(Item, FUN = function(x)str_sub(x,1,8))

  #### Set up the Item string served for pattern later on
  Itemstring<-gsub(", ","|",toString(union(Item.name,Item.orig)))


  paraextract(paste0(filepath.misc, "/ext2_summary of analysis.txt"),"CROSSTABS FOR CATEGORICAL VARIABLES","MODEL FIT INFORMATION",paste0(filepath.misc, "/crosstabs.txt"))
  crosstabs.file <- readLines(paste0(filepath.misc, "/crosstabs.txt"))

  st<-grep(paste0(Itemstring,"|Count"), crosstabs.file, value=T, ignore.case=T)

  # Reading user input --------------------------------------------------------
  readinteger <- function(){
    #n.detect <<- readline(prompt="Enter the Number of Cell Size to Detect: ")
    if(!grepl("^[0-9]+$",n.detect))
    {
      return(readinteger())
    }

  }

  readinteger()

  cat("The Number of Cell Size to Detect from:", n.detect,"and below \n")
  cat("\n")

  for (i in n.detect:0){
    pas <- sprintf("Count(.*) %s ", i)
    h <- grep(pas, st)
    if (!length(h)){
      cat("Check the crosstab having the cell size of", i, "from: no cell size of", i,"\n")
    } else {
      k <- Threshold.max + 1 + 3 # k = 6
      j <- vector(mode = "numeric",length = length(h))
      for (g in 1:length(h)){
        j<-h[g]
        for (j in (h[g]-k+2):h[g]){
          if (grepl(" AND ", st[j])) {
            l <- j
          } else {
            j <- j - 1
          }
        }
        cat("Check the crosstab having the cell size of", i, "from:", st[l],"\n")
      }
    }
  }
}
