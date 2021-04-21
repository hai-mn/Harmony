## Paragraph extraction
#' @title paraextract
#' @description Extract a specific paragraph from a text file
#' @details Extract a wanted paragraph starting with the beginning phrase and ending at the ending phrase. This based on the algorithm from stackoverflow of
#' \url{https://stackoverflow.com/questions/28676208/how-to-extract-specific-paragraphs-from-a-text-file-in-r}. But, this one was modified to write the paragraph to text file for reference.
#' @author Hai Nguyen \email{hnguye72@@uic.edu}
#' @export paraextract
#' @param inputfile a string of file path and Mplus output file name. It should be separated by / or \\
#' @param begphrase a phrase/words the wanted paragraph started with
#' @param endphrase a phrase/words the wanted paragraph ended at
#' @param outputfile a string of file path and output file name to store in
#' @return A specific paragraph from origin text file


paraextract <- function(inputfile, begphrase, endphrase, outputfile){
  ## read file
  txt <- readLines(inputfile)
  ## extract paragraph(s) defined from beginning phrase and ending phrase
  lns <- data.frame(beg=which(grepl(begphrase,txt)),
    end=which(grepl(endphrase,txt)))
  txt.2 <- lapply(seq_along(lns$beg),function(l){
    paste(txt[seq(from=lns$beg[l], to=lns$end[l])])})

  ## write to file
  file<-file(outputfile)
  writeLines(txt.2[[1]], file)
  close(file)
}



## Mplus output split
#' @title mplussplit
#' @description Split a Mplus output into  multiple parts.
#' @details In order for the program to work, a Mplus output must have these parts:
#' 1. INPUT INSTRUCTIONS
#' 2. SUMMARY OF ANALYSIS
#' 3. MODEL FIT INFORMATION
#' 4. MODEL RESULTS
#' 5. ALIGNMENT OUTPUT
#' 6. SAVEDATA INFORMATION
#' Six files, including ext1_input instructions.txt, ext2_summary of analysis.txt, ext3_model fit information.txt, ext4_model results.txt, ext5_alignment output.txt, and ext6_savedata information.txt respectively will be generated and stored in the folder Output_{date of running the program} at the current working directory.
#' @author Hai Nguyen \email{hnguye72@@uic.edu}
#' @export mplussplit
#' @param outpath usually set up as in the folder name Output_{date of running the program}
#' @param inputfile string of file path and Mplus output filename. It should be separated by / or \\
#' @return Six output text files from an original Mplus output


mplussplit <- function(outpath = outpath, inputfile = inputfile){
  ## apply to the input file
  ### INPUT INSTRUCTIONS
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "INPUT INSTRUCTIONS",
    endphrase = "SUMMARY OF ANALYSIS",
    paste0(outpath,"/ext1_input instructions.txt"))
  ### SUMMARY OF ANALYSIS
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "SUMMARY OF ANALYSIS",
    endphrase = "MODEL FIT INFORMATION",
    paste0(outpath,"/ext2_summary of analysis.txt"))
  ### MODEL FIT INFORMATION
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "MODEL FIT INFORMATION",
    endphrase = "MODEL RESULTS",
    paste0(outpath,"/ext3_model fit information.txt"))
  ### MODEL RESULTS
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "MODEL RESULTS",
    endphrase = "ALIGNMENT OUTPUT",
    paste0(outpath,"/ext4_model results.txt"))
  ### ALIGNMENT OUTPUT
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "ALIGNMENT OUTPUT",
    endphrase = "SAVEDATA INFORMATION",
    paste0(outpath,"/ext5_alignment output.txt"))
  ### SAVEDATA INFORMATION
  sapply(X = inputfile, FUN = paraextract,
    begphrase = "SAVEDATA INFORMATION",
    endphrase = "Support: Support@StatModel.com",
    paste0(outpath,"/ext6_savedata information.txt"))
}



## Mplus latent classes split
#' @title latentsplit
#' @description Split Mplus latent classes into  separated parts and save each to a file
#' @details Splitting from the Mplus model result the latent classes into parts and store each at the specified folder with names of Latent Class text files.
#' @author Hai Nguyen \email{hnguye72@@uic.edu}
#' @export latentsplit
#' @param filepath usually a specific folder "Output_Date's today" stored the
#' @param inputfile usually a file of "ext4_model results.txt" produced by mplussplit function
#' @return One or multiple latent class files depended on how many groups we have


latentsplit <- function(filepath = paste0("Output","_",Sys.Date()), inputfile = "ext4_model results.txt"){
  inputfile <- paste0(filepath,"/",inputfile)
  # Split 'ext4_model results.txt' file into multiple sections of Latent Classes
  for (i in 1:Group){

    bp<-paste0("Latent Class ", i)
    ep<-paste0("Latent Class ", i+1)

    outfile<-paste0(filepath, "/LatentClass ", i, ".txt")

    if (i < Group){
      sapply(X = inputfile,FUN = paraextract, begphrase = bp, endphrase = ep, outputfile = outfile)
    } else {
      sapply(X = inputfile,FUN = paraextract, begphrase = bp, endphrase = "RESULTS IN PROBABILITY SCALE", outputfile = outfile)
    }
  }
}



## Invariance-Noninvariance result split
#' @title invariancesplit
#' @description Split Invariance-Noninvariance result part into two sections of Intercepts/Thresholds and Loadings
#' @details Split Invariance-Noninvariance result part into two sections of Intercepts/Thresholds and Loadings and store each at the specified folder with names of ThresholdInvariance.txt and LoadingsInvariance.txt.
#' @author Hai Nguyen \email{hnguye72@@uic.edu}
#' @export invariancesplit
#' @param inputfile usually a file of "Invariant_Noninvariant.txt" produced by a inside function from alignmentout
#' @param filepath a string of directory path to store the output files
#' @return Two files of ThresholdInvariance.txt and LoadingsInvariance.txt in a specific folder


invariancesplit <- function(inputfile, filepath){

  #invar.noninvar.file <- "Invariant_Noninvariant.txt"

  # Splitting Invariant_Noninvariant.txt file into 2 sections of Intercepts/Thresholds & Loadings
  sapply(X = inputfile,FUN=paraextract, begphrase="Intercepts/Thresholds", endphrase="Loadings", outputfile=paste0(filepath,"/ThresholdInvariance.txt"))

  sapply(X = inputfile,FUN=paraextract, begphrase="Loadings", endphrase="FACTOR MEAN COMPARISON", outputfile=paste0(filepath,"/LoadingsInvariance.txt"))
}

