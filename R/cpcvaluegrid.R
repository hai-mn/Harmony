## Generating A CSV File with Value Grid of an Item with a Group to Plot Characteristic Curve
#' @title cpcCPC.csvexport
#' @description Producing the value grid to plot category and cumulative probability curve
#' @details cpcCPC.csvexport requires the user to run 'alignmentout()' and 'convert2irt()' at first to obtain the difficulty and discrimination estimates
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Ariel Aloe, Tianxiu Wang, Rachel Gordon
#' @export cpcCPC.csvexport
#' @import tidyverse
#' @param selected.item selecting an item
#' @param selected.group selecting a group
#' @param directory entering the directory folder name to store the output
#' @return A CSV file which has value grid of theta, category probability (cpc) and cumulative probability (CPC) values of a selected item's group

cpcCPC.csvexport <- function(selected.item="", selected.group="", directory=NULL){

  #==============================================================================
  # Gathering data in one place
  # File path ---------------------------------------------------
  if (is.null(directory)) {
    filepath <- paste0("Output","_", Sys.Date())
    filepath.misc <- paste0("Output","_", Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  } else {
    filepath <- directory
    filepath.misc <- paste0(directory,"/Misc")
  }
  
  if (!file.exists(paste0(filepath,"/discriminations.csv"))) {
    stop("\nMust run `alignmentout()` and `convert2irt()` to obtain the difficulty and discrimination parameters at first\n")
  }

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

  #==============================================================================
  # Select an item and group to extract
  cat("\nThe program exports the value grid of Item Probability Curve on the selected item and its group\n")

  item <- vector(mode = "list", length = Group)


  # Select the item to plot
  #selected.item.n <- utils::menu(Item.name, title = "Input an item need to be plotted: ")
  #selected.item <- as.character(Item.name[selected.item.n])

  # Select group(s)


  #selected.group.option <- utils::menu(Group.cat, title="Input the Group: ")


  #=========================================================================
  # process data for plot
  i <- match(selected.group, Group.cat) #<- selected.group.option
  keep.group <- paste0("G",i,"$")
  keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
  item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
    dplyr::select(., which(names(.) %in% keep.var))

  #=========================================================================
  # Extract value


  b = as.vector(unlist(item[[i]][-1])); a = as.vector(unlist(item[[i]][1]))


  # create theta scores
  theta<-4; theta= seq(from= -theta, to =theta, by=0.1)
  # select the largest length of b and a
  ncat = max(length(b),length(a))
  expand.grid("a" = a,"b" = b)
  # Define a matrix that will return results
  CGF <-  matrix(NA, ncol=ncat  , nrow=length(theta))
  PG  <- matrix(NA, ncol=ncat+1, nrow=length(theta))
  # Each column will be for a different grade while each row will be for a different theta (if input)

  for (i in 1:length(theta)) {
    # inverse cumulative grade - probability of a particular category or higher
    CGF[i,] = exp(a*(theta[i]-b))/(1+exp(a*(theta[i]-b)))
    # probability of 0
    PG[i,1] <- 1 - CGF[i,1]
    # The probability of getting the highest grade is the same as the probability of getting the highest grade or more.
    PG[i,ncat+1] <- CGF[i,ncat]
    # For the categories in between
    for (m in 1:(ncat-1)) {
      PG[i,m+1] <- CGF[i,m] - CGF[i,m+1]
    }
  }
  df <- cbind("theta"=theta, "cpc" = PG, "CPC" = CGF)
  for (i in 1:(ncat+1)){colnames(df)[i+1] <- paste0("cpc",i)} #cpc: in lowercase, category probability curve
  for (i in 1:ncat){colnames(df)[1+ncat+1+i] <- paste0("CPC",i)} #CPC: in uppercase, Cumulative Probability Curve
  colnames(df)

  cat("Exporting", paste0("\"cpcCPC of Item ", selected.item, " - Group ", selected.group,".csv\""), paste0("in \"../",filepath, "\""), "folder\n")

  utils::write.csv(df, paste0(filepath,"/cpcCPC of ", selected.item, " - G", selected.group,".csv"), row.names=FALSE)

}
