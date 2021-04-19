## Generating table(s) of Threshold and Loading Parameters
#' @title alignmentout
#' @description Generating table(s) with estimates, alignment values and R-square of Thresholds and Loadings
#' @details
#' Split a Mplus output into multiple parts.
#' Produce table(s) with estimates, alignment values and R-square of Thresholds and Loadings.
#' All would be stored in folder 'Output_current date' at the working directory.
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Tianxiu Wang, Ariel Aloe, Rachel Gordon
#' @export alignmentout
#' @import tidyr stringr openxlsx
#' @return A list of text, CSV files and one Excel file with multiple tabs in the specific folder.


alignmentout<-function(infile=""){

  # Inform users the parameters ----------------------------------------------------
  ## 1. Enter a Mplus ouput file
  ## 2. Number of Groups: latent classes
  ## 3. Factors: number of continuous latent variables
  ## 4. Items: number of dependent variables
  ## 5. Threshold: number of categories - 1
  cat("\nThe function provides information from Mplus alignment output, including: \n - Groups (latent classes),\n - Factors (continuous latent variables),\n - Items (dependent variables) and\n - Categories of each Item (equal to Thresholds + 1).\n")
  cat("\nIn addition, you may find from the folder 'Output_current date' in the working directory:\n - the multiple text files which split from the origin Mplus output\n - the thresholds, loadings tables (CSV format) and\n - especially, a combined Excel file with all separated spreadsheets")

  ## 1. Enter a Mplus ouput file======================================================
  # Directly input the filepath as an argument in the function
  #infile <- readline(prompt="Enter path and Mplus output file (separated by /):\n")

  ### Create a folder to store the output
  filepath <<- paste0("Output","_",Sys.Date())
  filepath.misc <<- paste0("Output","_",Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  ifelse(!dir.exists(file.path(filepath)), dir.create(file.path(filepath)), FALSE)
  ifelse(!dir.exists(file.path(filepath.misc)), dir.create(file.path(filepath.misc)), FALSE)

  ### Split Mplus output file into 6 parts by support function `mplussplit` within the package:
  ###  ext1_input instructions - ext2_summary of analysis
  ###  ext3_model fit information - ext4_model results
  ###  ext5_alignment output - ext6_savedata information
  mplussplit(outpath = filepath.misc, inputfile = infile)

  ## 2- Number of Groups: latent classes===============================================
  ext1<-readLines(paste0(filepath.misc, "/ext1_input instructions.txt"))
  g<-grep("^.*classes =.*", ext1, ignore.case = T, value=T)
  g.line<-grep("^.*KNOWNCLASS.*", ext1, ignore.case = T, value=T)
  Group <<- as.numeric(str_extract_all(g,"\\d+"))
  Group.name <- gsub("^.*KNOWNCLASS = c\\(| = .*\\) ;.*", "", g.line)
  Group.cat <<- unlist(strsplit(gsub("^.*KNOWNCLASS = c\\(\\w+ = |\\) ;.*", "", g.line), split=" "))

  cat("- The Number of Groups (Latent Classes):", Group,"\n")
  cat("- The Name of Groups (Latent Classes):", Group.name, "with categories of", Group.cat,"\n")

  ## 3- Factors: number of continuous latent variables=================================
  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))
  m<-grep("Continuous latent variables",ext2)
  Factor<<-unlist(str_extract_all(ext2[m+1],"\\w+"))
  Factor.n <- length(Factor)

  cat("- The Number of Factors:", Factor.n, ",including ", Factor,"\n")

  ## 4- Items: number of dependent variables============================================
  n<-grep("Binary and ordered categorical", ext2, ignore.case = T)
  Item.name<-str_extract_all(ext2[n+1],"\\w+")[[1]]
  for (i in (n+2):(m-1)){
    Item.name<-c(Item.name,str_extract_all(ext2[i],"\\w+")[[1]])
    if (i==(m-1)) Item.name<-Item.name[!is.na(Item.name)]
  }
  Item.n <-  length(Item.name)
  Item.name<<-sapply(Item.name, FUN = function(x)str_sub(x,1,8)) #limit to 8 character long for each name

  cat("- The Number of Items: ", Item.n, ",including: ", Item.name,"\n")

  ## 5- Categories of each Item (Threshold: number of categories - 1)===================
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
  Item.cat.df <- data.frame(cbind(Item.name, Category, Threshold))

  cat("- The Number of Categories and Threshold in each Item:\n")
  print(Item.cat.df, row.names = FALSE)


  # Build first 2 columns of model table: Item and Factor------------------------------
  f.stri<-NA; f.stri.c<-NA; f<-NA
  by.items <- vector(mode = "list", length = length(Factor)) #empty_list
  for (i in 1:length(Factor)) {
    f.stri[i]   <- paste0(Factor[i],"(.*) BY ")
    f.stri.c[i] <- paste0("^\\s+",Factor[i],"(.*) BY\\s+|\\s+;.*")
    f           <- grep(f.stri[i], ext1, ignore.case = T)

    by.items[[i]] <- toupper(gsub(f.stri.c[i], "", ext1[f], ignore.case = T))
  }
  Item<-unlist(by.items) #matches with Item.name
  Item.orig<-Item #reserve the original Items
  Item<-sapply(Item, FUN = function(x)str_sub(x,1,8)) #limit to 8 character long for each name
  Factor.by<-rep(Factor[1],length(by.items[[1]]))
  if (length(Factor)!=1){
    for (i in 2:length(Factor)) {
      f<-rep(Factor[i],length(by.items[[i]]))
      Factor.by<-append(Factor.by,f)
    }
  }


  model.table<-data.frame(cbind(Item,Factor.by), row.names = NULL) #Global environment?
  model.table$Item<-as.character(model.table$Item)

  model.table.loadings<-model.table #save for building loadings table later

  Itemstring<-gsub(", ","|",toString(union(Item,Item.orig))) #set up the Item string served for pattern later on

  ## Due to i Thresholds so we must have i tables of model.table.threshold===============
  Threshold.max<<-max(Threshold)
  model.table.threshold <- vector(mode = "list", length = Threshold.max) #empty_list
  for (i in 1:Threshold.max){
    model.table.threshold[[i]]<-model.table
  }

  # Build Threshold estimate and SE-------------------------------------------------------
  ## Split the Model Results to the number of classes=====================================
  latentsplit(filepath = filepath.misc, inputfile="ext4_model results.txt")

  Threshold.df <- vector(mode = "list", length = Threshold.max) #empty_list
  for (i in 1:Threshold.max){
    Threshold.df[[i]]<-vector(mode = "list", length = Group)
  }

  ## Run from 1 to threshold of model.table.threshold=====================================
  for (i in 1:Threshold.max){
    col.thres<-paste0("Threshold",i,"_G")
    col.se<-paste0("Threshold",i,"_SE_G")
    ### Run from 1 to Group (G) of Threshold.df of i, then repeat merge to threshold.table.threshold by Item
    for (j in 1:Group){
      readfile<-paste0(filepath.misc,"/LatentClass ",j,".txt")
      Threshold.file <- readLines(readfile)
      pattern<-sprintf("^ +(%s)\\$%s +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+)",Itemstring,i)
      matches<-str_match(Threshold.file,pattern)
      Threshold.df[[i]][[j]] <- drop_na(data.frame(matches[,-c(1,5,6)]))
      Th<-paste0(col.thres,j)
      Se<-paste0(col.se,j)
      colnames(Threshold.df[[i]][[j]]) <- c("Item",Th,Se)
      Threshold.df[[i]][[j]]$Item<-as.character(Threshold.df[[i]][[j]]$Item)

      #### merge by Item
      model.table.threshold[[i]] <- dplyr::full_join(model.table.threshold[[i]], Threshold.df[[i]][[j]], by="Item")
    }

  }

  # Build the Threshold1_Weighted_Average & Threshold1_R-square-----------------------------
  ## Split then obtain the part needed for Invariant estimates==============================
  paraextract(paste0(filepath.misc, "/ext5_alignment output.txt"),"ALIGNMENT OUTPUT","Loadings",paste0(filepath.misc, "/Threshold_Invariant_Rsq.txt"))
  Threshold.file <- readLines(paste0(filepath.misc, "/Threshold_Invariant_Rsq.txt"))

  ## Select the lines in which have the information==========================================
  st<-grep("^ Threshold.*|^ Weighted Average Value Across Invariant Groups:,*|^ R-square/Explained variance/Invariance index:.*|^ Approximate Invariance Was Not Found For This Parameter.", Threshold.file, ignore.case = T,value=T)

  ## Obtain the part needed for Invariant-Noninvariant values================================
  paraextract(paste0(filepath.misc, "/ext4_model results.txt"),"APPROXIMATE MEASUREMENT INVARIANCE \\(NONINVARIANCE\\) FOR GROUPS","FACTOR MEAN COMPARISON AT THE 5% SIGNIFICANCE LEVEL IN DESCENDING ORDER",paste0(filepath.misc, "/Invariant_Noninvariant.txt"))

  Invariance.file <- readLines(paste0(filepath.misc, "/Invariant_Noninvariant.txt"))

  invariancesplit(inputfile=paste0(filepath.misc,"/Invariant_Noninvariant.txt"), filepath = filepath.misc)

  Threshold.Invariance.file <- readLines(paste0(filepath.misc, "/ThresholdInvariance.txt"))

  Threshold.Invariance.file <- Threshold.Invariance.file[-c(1,length(Threshold.Invariance.file))]

  Threshold.Invariant.df <- vector(mode = "list", length = Threshold.max) #empty_list

  empty_lines = grepl('^\\s*$', Threshold.Invariance.file)
  Threshold.Invariance.file <- Threshold.Invariance.file[! empty_lines]
  Threshold.Invariance.file <- str_squish(Threshold.Invariance.file) #reduces repeated whitespace inside a string
  #l.digit<-grep('^\\d',Threshold.Invariance.file)
  l.digit<-grep('^\\d|^\\(',Threshold.Invariance.file) # minor modified on 3/15/2021
  if (length(l.digit) != 0) {
    for (i in 1:length(l.digit)){
      Threshold.Invariance.file[l.digit[i]-1] <- paste(Threshold.Invariance.file[l.digit[i]-1],Threshold.Invariance.file[l.digit[i]])
    }
    Threshold.Invariance.file <- Threshold.Invariance.file[-c(l.digit)]
  }

  ## Build the table from information obtained above=========================================
  for (i in 1:Threshold.max){
    ### Create a table with Group+3 columns (Item, Threshold[i]_Invariant_G[j], Weighted_Average and R-square) and Item.n rows
    df <- data.frame(matrix(ncol = (Group+3), nrow = length(Item)))
    name<-NA
    col.thres.invariant<-paste0("Threshold",i,"_Invariant_G")
    col.thres.weighted<-paste0("Threshold",i,"_Weighted_Average")
    col.thres.rsq<-paste0("Threshold",i,"_R-square")
    for (j in 1:Group){
      name[j]<-paste0(col.thres.invariant,j)
    }
    x <- c("Item", name, col.thres.weighted, col.thres.rsq)
    colnames(df) <- x
    Threshold.Invariant.df[[i]]<-df
    rm(df)


    k<-1

    for (j in 1:(length(st))){

      if (grepl("Threshold", st[j]) & str_sub(st[j],-1,-1)==i){
        Threshold.Invariant.df[[i]][k,1]=str_trim(substr(st[j],11,nchar(st[j])-2))

        if (grepl("Approximate Invariance Was Not Found For This Parameter", st[j+1])){
          for (l in 1:Group){
            Threshold.Invariant.df[[i]][k,l+1]="False"
          }
          Threshold.Invariant.df[[i]][k,l+2]=NA
          Threshold.Invariant.df[[i]][k,l+3]=NA
        } else {

          l.matchedline.threshold <- grep(sprintf("%s\\$%s",str_trim(substr(st[j],11,nchar(st[j])-2)),i), Threshold.Invariance.file)
          l.line.threshold <- unlist(strsplit(Threshold.Invariance.file[l.matchedline.threshold], split = " "))

          for (l in 1:Group){
            if (str_detect(l.line.threshold[l+1], "\\(")) {
              Threshold.Invariant.df[[i]][k,l+1]="False"
            } else {
              Threshold.Invariant.df[[i]][k,l+1]="True"
            }
          }
          Threshold.Invariant.df[[i]][k,l+2]=as.numeric(substr(st[j+1],nchar(st[j+1])-6,nchar(st[j+1])))
          Threshold.Invariant.df[[i]][k,l+3]=as.numeric(substr(st[j+2],nchar(st[j+2])-6,nchar(st[j+2])))
        }

        k<-k+1
      } else {next}
    }

    Threshold.Invariant.df[[i]] <- Threshold.Invariant.df[[i]][!is.na(Threshold.Invariant.df[[i]]$Item),]
    model.table.threshold[[i]] <- dplyr::full_join(model.table.threshold[[i]], Threshold.Invariant.df[[i]], by="Item")
  }

  # Now, building the loadings table, starting with the estimate and SE------------------
  Loadings.df<-vector(mode = "list", length = Group)
  ## Run from 1 to Group (G) of Loadings.df, then repeat merge to Loadings.df by Item
  for (j in 1:Group){
    readfile<-paste0(filepath.misc,"/LatentClass ",j,".txt")
    Loadings.file <- readLines(readfile)

    pattern<-sprintf("^ +(%s) +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+) +([-+]?\\d+.\\d+)", Itemstring)
    matches<-str_match(Loadings.file, pattern)

    Loadings.df[[j]] <- drop_na(data.frame(matches[,-c(1,5,6)]))

    Lg<-paste0("Loadings_G",j)
    Se<-paste0("Loadings_SE_G",j)
    colnames(Loadings.df[[j]]) <- c("Item",Lg,Se)
    Loadings.df[[j]]$Item<-trimws(as.character(str_sub(Loadings.df[[j]]$Item,1,8)))

    #### here we merge by Item
    model.table.loadings<-dplyr::full_join(model.table.loadings, Loadings.df[[j]], by="Item")
  }


  # Continue to build the Loadings Invariant table, then merge to just preceding table-----
  ## Select the paragraph in which has the information=============================
  paraextract(paste0(filepath.misc, "/ext5_alignment output.txt"),"Loadings","SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", paste0(filepath.misc, "/Loadings_Invariant_Rsq.txt"))

  ## Load the extract paragraph (above)=====================================================
  Loadings.file <- readLines(paste0(filepath.misc, "/Loadings_Invariant_Rsq.txt"))

  ## Select the lines in which have the information=========================================
  st<-grep("^ Loadings for.*|^ Weighted Average Value Across Invariant Groups:,*|^ R-square/Explained variance/Invariance index:.*|^ Approximate Invariance Was Not Found For This Parameter.", Loadings.file, value=T, ignore.case=T)

  loadings.invariant.df <- data.frame(matrix(ncol = (Group+3), nrow = length(Item))) #Item.n may be different
  col.load.invariant<-NA
  for (j in 1:Group){
    col.load.invariant[j]<-paste0("Loadings_Invariant_G",j)
  }

  colnames(loadings.invariant.df) <- c("Item", col.load.invariant, "Loadings_Weighted_Average", "Loadings_R_square")

  ## Build the table=======================================================================

  Loadings.Invariance.file <- readLines(paste0(filepath.misc, "/LoadingsInvariance.txt"))

  k<-1

  for (j in 1:(length(st))){

    if (grepl("Loadings for", st[j])){
      loadings.invariant.df[k,1]=str_trim(substr(st[j],14,nchar(st[j])))

      if (grepl("Approximate Invariance Was Not Found For This Parameter", st[j+1])){
        for (l in 1:Group){
          loadings.invariant.df[k,l+1]="False"
        }
        loadings.invariant.df[k,l+2]=NA
        loadings.invariant.df[k,l+3]=NA
      } else {

        l.matchedline <- grep(str_trim(substr(st[j],14,nchar(st[j]))),Loadings.Invariance.file)
        l.line <- unlist(strsplit(gsub("^\\s+|\\s+"," ", sub("^\\s+","", Loadings.Invariance.file[l.matchedline])), split=" "))

        for (l in 1:Group){
          #loadings.invariant.df[k,l+1]="True"
          if (str_detect(l.line[l+1], "\\(")){
            loadings.invariant.df[k,l+1]="False"
          } else {
            loadings.invariant.df[k,l+1]="True"
          }
        }
        loadings.invariant.df[k,l+2]=as.numeric(substr(st[j+1],nchar(st[j+1])-6,nchar(st[j+1])))
        loadings.invariant.df[k,l+3]=as.numeric(substr(st[j+2],nchar(st[j+2])-6,nchar(st[j+2])))
      }

      k<-k+1
    } else {next}
  }

  ## Link to the previous table============================================================
  model.table.loadings <- dplyr::full_join(model.table.loadings, loadings.invariant.df, by="Item")

  # Create one Excel file with all spreadsheets----------------------------------------
  completed.table.threshold <- vector(mode = "list", length = Threshold.max) #empty_list
  for (i in 1:Threshold.max){
    completed.table.threshold[[i]]<-model.table.threshold[[i]]
    utils::write.csv(model.table.threshold[[i]], paste0(filepath,"/threshold",i,".csv"), row.names=FALSE)
  }

  completed.table.loadings <- model.table.loadings
  utils::write.csv(model.table.loadings, paste0(filepath,"/loadings.csv"), row.names=FALSE)

  of=paste0(filepath,"/alignment_tables.xlsx")
  OUT <- createWorkbook()
  for(i in 1:Threshold.max){
    tname<-paste("Threshold ",i,sep="")
    addWorksheet(OUT, tname)
    writeData(OUT, sheet = tname, x = completed.table.threshold[[i]])
  }
  addWorksheet(OUT, "Loadings")
  writeData(OUT, sheet = "Loadings", x = completed.table.loadings)
  saveWorkbook(OUT,of,overwrite=TRUE)
}





