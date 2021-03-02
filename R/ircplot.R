## Generating Item Response (Characteristic) Curve Plots
#' @title irc
#' @description Generating Cumulative and Category Item Response (Characteristic) Curve Plots
#' @details irc requires the user to firstly run the 'alignmentout' and 'convert2irt' to obtain the difficulty and discrimination parameters
#' @author Hai Nguyen \email{hnguye72@@uic.edu}, Ariel Aloe, Tianxiu Wang, Rachel Gordon
#' @export irc
#' @import tidyverse
#' @return A list of Item Response (Characteristic) Curve Plot in R environment

irc <- function(){

  # Set file path --------------------------------------------------------------------
  filepath <- paste0("Output","_",Sys.Date())
  if (!file.exists(paste0(filepath,"/discriminations.csv"))) {
    stop("\nMust run the `alignmentout` then `convert2irt` to obtain the difficulty and discrimination parameters first\n")
  }

  # Create the discrimination parameters from loadings table----------------------------
  discriminations.file <- utils::read.csv(file = paste0(filepath,"/discriminations.csv"))
  drop.names <- c(names(discriminations.file[,grep("Loadings", names(discriminations.file), value=TRUE)]), "Factor.by")
  discriminations.file <- select(discriminations.file, -which(names(discriminations.file) %in% drop.names))
  total <- discriminations.file

  # Create the difficulty parameters from thresholds tables-----------------------------
  difficulty.file <- vector(mode = "list", length = Threshold.max)

  # The whole one file with discriminations and difficulties parameters-----------------
  for (h in 1:Threshold.max){
    difficulty.file[[h]] <- utils::read.csv(file = paste0(filepath,"/difficulty",h,".csv"))
    drop.names <- c(names(difficulty.file[[h]][,grep("Threshold", names(difficulty.file[[h]]), value=TRUE)]), "Factor.by")
    difficulty.file[[h]]<-select(difficulty.file[[h]], -which(names(difficulty.file[[h]]) %in% drop.names))

    total <- dplyr::full_join(total, difficulty.file[[h]], by = "Item")

  }

  # output the total file including item, discriminations and difficulties for reference
  utils::write.csv(total, paste0(filepath,"/irc_file.csv"), row.names=FALSE)

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
  cat("\nThe program will plot cumulative and category response curves on the selected item and group(s)\n")

  selected.item.n <- menu(Item.name, title = "Input an item need to be plotted: ")
  selected.item <- as.character(Item.name[selected.item.n])
  selected.group <- menu(c(Group.cat,"All"), title="Input the Group(s): ")


  if (selected.group != (Group+1)){
    # plotting cumulative curve
    irc <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Cumulative Item Response Curve for Item", selected.item)) +
      theme_bw()


    i <- selected.group
      keep.group <- paste0("G",i,"$")
      keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
      item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
        dplyr::select(., which(names(.) %in% keep.var))

      for (j in 2:(Threshold.max+1)){
        irc <- irc + stat_function(fun = logisticFun, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][j])))
      }

    # plotting category curve
    circ <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Category Item Response Curve for Item", selected.item)) +
      theme_bw()


      circ <- circ +
        stat_function(fun = pg0, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][2]))) +
        stat_function(fun = pgl, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][Threshold.max+1])))


      for (j in 2:(Threshold.max)){
        circ <- circ + stat_function(fun = pgbtwn, args = list(a1=as.numeric(item[[i]][1]), b1=as.numeric(item[[i]][j]), a2=as.numeric(item[[i]][1]), b2=as.numeric(item[[i]][j+1])))
      }


    figure <- ggpubr::ggarrange(irc, circ, ncol = 1, nrow = 2)

  } else {

    irc <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Cumulative Item Response Curve for Item", selected.item)) +
      theme_bw()


    for (i in 1:Group){
      keep.group <- paste0("G",i,"$")
      keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
      item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
        dplyr::select(., which(names(.) %in% keep.var))

      for (j in 2:(Threshold.max+1)){
        irc <- irc + stat_function(fun = logisticFun, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][j])))
      }
    }

    circ <-ggplot(data.frame(theta = c(-4, 4)), aes(x = theta)) +
      xlab(latex2exp::TeX("$\\theta")) +
      ylab("Probability") +
      labs(title=paste("Category Item Response Curve for Item", selected.item)) +
      theme_bw()

    for (i in 1:Group){
      keep.group <- paste0("G",i,"$")
      keep.var <- names(total[,grep(keep.group, names(total), value=TRUE)])
      item[[i]] <- total %>% dplyr::filter(Item == selected.item) %>%
        dplyr::select(., which(names(.) %in% keep.var))

      circ <- circ +
        stat_function(fun = pg0, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][2]))) +
        stat_function(fun = pgl, args = list(a=as.numeric(item[[i]][1]),b=as.numeric(item[[i]][Threshold.max+1])))


      for (j in 2:(Threshold.max)){
        circ <- circ + stat_function(fun = pgbtwn, args = list(a1=as.numeric(item[[i]][1]), b1=as.numeric(item[[i]][j]), a2=as.numeric(item[[i]][1]), b2=as.numeric(item[[i]][j+1])))
      }
    }

    figure <- ggpubr::ggarrange(irc, circ, ncol = 1, nrow = 2)

  }

  figure
}

