## Plotting Item Characteristic Curve (Category and Cumulative Probability Curves)
#' @title cpcCPC
#' @description Plotting the Category and Cumulative Probability Curve
#' @details cpcCPC() requires the user to run 'alignmentout()' and 'convert2irt()' at first to obtain the difficulty and discrimination estimates
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Ariel Aloe, Tianxiu Wang, Rachel Gordon
#' @export cpcCPC
#' @import tidyverse
#' @param selected.item selecting an item
#' @param selected.group selecting a group or multiple groups
#' @param directory entering the directory folder name to store the output
#' @return A graph including the category and cumulative probability curves stored in specific folder

cpcCPC <- function(selected.item="", selected.group="", directory=NULL){

  # File path ---------------------------------------------------
  if (is.null(directory)) {
    filepath <- paste0("Output","_", Sys.Date())
    filepath.misc <- paste0("Output","_", Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  } else {
    filepath <- directory
    filepath.misc <- paste0(directory,"/Misc")
  }


  if (!file.exists(paste0(filepath,"/discriminations.csv"))) {
    stop("\nMust run `alignmentout()` then `convert2irt()` to obtain the difficulty and discrimination parameters at first\n")
  }


  ## Number of Groups: latent classes===============================================
  if (!file.exists(paste0(filepath.misc, "/ext1_input instructions.txt"))) {
    stop("\nMust run `alignmentout()` to obtain the threshold information for the graphs\n")
  }
  ext1<-readLines(paste0(filepath.misc, "/ext1_input instructions.txt"))
  #g<-grep("^.*classes =.*", ext1, ignore.case = T, value=T)
  g<-grep("^.*classes( )?=.*", ext1, ignore.case = T, value=T)
  g.line<-grep("^.*KNOWNCLASS.*", ext1, ignore.case = T, value=T)

  g.line.n<-grep("^.*KNOWNCLASS.*", ext1, ignore.case = T)
  Group <- as.numeric(str_extract_all(g,"\\d+"))
  #Group.name <- gsub("^.*KNOWNCLASS = c\\(| = .*\\) ;.*", "", g.line)
  Group.name <- gsub("^.*KNOWNCLASS( )?=( )?c\\(|( )?=( )?.*", "", g.line)
  #Group.cat <- unlist(strsplit(gsub("^.*KNOWNCLASS = c\\(\\w+ = |\\) ;.*", "", g.line), split=" "))
  Group.cat <- unlist(strsplit(gsub("^.*KNOWNCLASS = c\\(\\w+( )?=( )?|\\) ;.*", "", g.line), split=" "))
  while (length(Group.cat)<Group) {
    g.line.n.add <- g.line.n + 1
    Group.cat.add <- unlist(strsplit(gsub("^[:space]|\\)( )?;.*", "", trimws(ext1[g.line.n.add])), split=" "))
    Group.cat <- c(Group.cat,Group.cat.add)
  }
  ## Categories of each Item (Threshold: number of categories - 1)===================
  if (!file.exists(paste0(filepath.misc, "/ext2_summary of analysis.txt"))) {
    stop("\nMust run `alignmentout()` to obtain the threshold information for the graphs\n")
  }

  ext2<-readLines(paste0(filepath.misc, "/ext2_summary of analysis.txt"))

  ## Factors: number of continuous latent variables=================================
  m<-grep("Continuous latent variables",ext2)
  Factor<-unlist(str_extract_all(ext2[m+1],"\\w+"))
  Factor.n <- length(Factor)

  ## Items: number of dependent variables============================================
  n<-grep("Binary and ordered categorical", ext2, ignore.case = T)
  Item.name<-str_extract_all(ext2[n+1],"\\w+")[[1]]
  for (i in (n+2):(m-1)){
    Item.name<-c(Item.name,str_extract_all(ext2[i],"\\w+")[[1]])
    if (i==(m-1)) Item.name<-Item.name[!is.na(Item.name)]
  }
  Item.n <-  length(Item.name)
  Item.name<-sapply(Item.name, FUN = function(x)str_sub(x,1,8)) #limit to 8 character long for each name


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

  # Create the discrimination parameters from loadings table----------------------------
  discriminations.file <- utils::read.csv(file = paste0(filepath,"/discriminations.csv"))
  drop.names <- "Factor.by"
  discriminations.file <- dplyr::select(discriminations.file, -which(names(discriminations.file) %in% drop.names))
  total <- discriminations.file

  # Create the difficulty parameters from thresholds tables-----------------------------
  difficulty.file <- vector(mode = "list", length = Threshold.max)

  # The whole one file with discriminations and difficulties parameters-----------------
  for (h in 1:Threshold.max){
    difficulty.file[[h]] <- utils::read.csv(file = paste0(filepath,"/difficulty",h,".csv"))
    drop.names <- "Factor.by"
    difficulty.file[[h]]<-dplyr::select(difficulty.file[[h]], -which(names(difficulty.file[[h]]) %in% drop.names))

    total <- dplyr::full_join(total, difficulty.file[[h]], by = "Item")

  }

  # output the total file including item, discriminations and difficulties for reference
  utils::write.csv(total, paste0(filepath.misc,"/cpc_file.csv"), row.names=FALSE)

  #==============================================================================
  item <- vector(mode = "list", length = Group)

  logisticFun <- function(theta,a,b) {
    z <- a*(theta-b)
    1/(1+exp(-1*z))
  }

  #=========================================================================
  pg0 <- function(theta,a,b) {
    z <- a*(theta-b)
    cgf <- 1/(1+exp(-1*z))
    1-cgf
  }
  pgl <- function(theta,a,b) {
    z <- a*(theta-b)
    cgf <- 1/(1+exp(-1*z))
    cgf
  }
  pgbtwn <- function(theta,a1,b1,a2,b2) {
    z1 <- a1*(theta-b1); z2 <- a2*(theta-b2);
    cgf1 <- 1/(1+exp(-1*z1)); cgf2 <- 1/(1+exp(-1*z2))
    cgf1-cgf2
  }



  #=========================================================================
  cat("\nThe function plots cumulative and category probability curves on the selected item and group(s)\n")

  # Select the item to plot
  #selected.item.n <- utils::menu(Item.name, title = "Input an item need to be plotted: ")
  #selected.item <- as.character(Item.name[selected.item.n])
  #"%ni%" <- Negate("%in%")
  # if (isFALSE(!(selected.item %in% Item.name))) {
  #   stop("\nNot enter the correct item name (case sensitive in R)\n")
  # }
  # Select group(s)


  #selected.group.option <- utils::menu(c(Group.cat, "Other Combination"), title="Input the Group(s): ")
  select.group.title <- str_squish(selected.group)
  selected.group <- as.character(str_squish(unlist(strsplit(selected.group, ","))))
  # if (!isFALSE(!(selected.group %in% Group.cat))) {
  #   stop("\nNot enter the correct item name (case sensitive in R)\n")
  # }

  # Plot for other combination ===============================================================
  if (length(selected.group) != 1) {
    #selected.group.line <- readline(prompt="Enter group number (separated by ,):\n")
    #selected.group <- as.character(str_squish(unlist(strsplit(selected.group.line, ","))))

    # plotting a frame cumulative curve
    CPC <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Cumulative Probability Curves \nfor Group", select.group.title, "of Item", selected.item), caption = "The multiple lines reflect different subgroups") +
      theme_bw() + theme(legend.title = element_blank())

    # plotting a frame category curve
    cpc <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Category Probability Curves \nfor Group", select.group.title, "of Item", selected.item), caption = "") +
      theme_bw() + theme(legend.title = element_blank())

    for (k in 1:length(selected.group)) {

      i <- match(selected.group[k], Group.cat)
      legends = paste0("Group ", i)

      keep.group <- paste0("G",i,"$")
      keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
      item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
      dplyr::select(., which(names(.) %in% keep.var))

      for (j in 2:(Threshold.max+1)){
        CPC <- CPC + stat_function(fun = logisticFun, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][j])), aes_(colour = legends))
      }

      cpc <- cpc +
        stat_function(fun = pg0, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][2])), aes_(colour = legends)) +
        stat_function(fun = pgl, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][Threshold.max+1])), aes_(colour = legends))


      for (j in 2:(Threshold.max)){
        cpc <- cpc + stat_function(fun = pgbtwn, args = list(a1=as.numeric(item[[i]][1]), b1=as.numeric(item[[i]][j]), a2=as.numeric(item[[i]][1]), b2=as.numeric(item[[i]][j+1])), aes_(colour = legends))
      }
    }

  } else {

    #selected.group.line <- selected.group.option
    # Plot for one group ===============================================================
    # plotting a frame cumulative curve
    CPC <- ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Cumulative Probability Curves \nfor Group", select.group.title, "of Item", selected.item), caption = "The multiple lines reflect different subgroups") +
      theme_bw() + theme(legend.title = element_blank())

    # plotting a frame category curve
    cpc <- ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Category Probability Curves \nfor Group", select.group.title, "of Item", selected.item), caption = "") +
      theme_bw() + theme(legend.title = element_blank())

    # process data for plot
      i <- match(selected.group, Group.cat) #<- selected.group.option
      keep.group <- paste0("G",i,"$")
      keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
      item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
        dplyr::select(., which(names(.) %in% keep.var))

    # overlay CPC
    for (j in 2:(Threshold.max+1)){
      legends = paste0("CPC", (j-1))
      CPC <- CPC + stat_function(fun = logisticFun, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][j])))
    }

    # overlay circ
    cpc <- cpc +
      stat_function(fun = pg0, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][2]))) +
      stat_function(fun = pgl, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][Threshold.max+1])))


    for (j in 2:(Threshold.max)){
      legends = paste0("CPC", j)
      cpc <- cpc + stat_function(fun = pgbtwn, args = list(a1=as.numeric(item[[i]][1]), b1=as.numeric(item[[i]][j]), a2=as.numeric(item[[i]][1]), b2=as.numeric(item[[i]][j+1])))
    }

  }

  figure <- ggpubr::ggarrange(cpc, CPC, ncol = 1, nrow = 2)

  cat("Exporting", paste0("\"cpcCPC of Item ", selected.item, " - Group ", select.group.title, ".tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
  ## Save to TIF/TIFF
  ggsave(filename = paste0(filepath,"/cpcCPC of Item ", selected.item, " - Group ", select.group.title, ".tiff"),
         figure, width = 6, height = 10, dpi = 300, units = "in", device = "tiff")

}
