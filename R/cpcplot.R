## Plotting Item Characteristic Curve (Category and Cumulative Probability Curves)
#' @title cpc
#' @description Plotting the Category and Cumulative Probability Curve
#' @details cpc() requires the user to run 'alignmentout()' and 'convert2irt()' at first to obtain the difficulty and discrimination estimates
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Ariel Aloe, Tianxiu Wang, Rachel Gordon
#' @export cpc
#' @import tidyverse
#' @param selected.item selecting an item
#' @param selected.group selecting a group or multiple groups
#' @return A graph including the category and cumulative probability curves stored in specific folder

cpc <- function(selected.item="", selected.group=""){

  # Set file path --------------------------------------------------------------------
  filepath <- paste0("Output","_",Sys.Date())
  filepath.misc <- paste0("Output","_",Sys.Date(),"/Misc") # clean up: put all un-necessary files in filepath.misc
  if (!file.exists(paste0(filepath,"/discriminations.csv"))) {
    stop("\nMust run `alignmentout()` then `convert2irt()` to obtain the difficulty and discrimination parameters at first\n")
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

  cat("Exporting", paste0("\"PC of Item ", selected.item, " - Group ", select.group.title, ".tiff\""), paste0("in \"../",filepath, "\""), "folder\n")
  ## Save to TIF/TIFF
  ggsave(filename = paste0(filepath,"/PC of Item ", selected.item, " - Group ", select.group.title, ".tiff"),
         figure, width = 6, height = 10, dpi = 300, units = "in", device = "tiff")

}
