#---------------------------------------------------------------------------------------------
#WARNING
#This file is NOT meant to be ran in Rstudio. This is an R **SCRIPT**, NOT a notebook.
#I.e.: Run it in VSCode and check the output with the terminal, or just execute it.

#---------------------------------------------------------------------------------------------
#Run R scripts
current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)

message("Running scripts...")

scripts <- c("R_code_B.1.R",
             "R_code_B.1.6.1.R",
             "R_code_B.1.6.2.R")


for(s in scripts){

    setwd(current_file_path)
    source(s)

}
