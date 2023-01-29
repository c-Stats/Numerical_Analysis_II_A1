# Numerical Analysis II: A1
Execute the __main__ scripts to generate the graphics and pictures used to produce the PDF.

Please note that THESE ARE NOT NOTEBOOKS. Running the command
```R
current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_file_path)
```
on Rstudio will NOT point towards the proper directory, and hence will either create folders and file in your current working directory, halt code execution because
certain files couldn't be found, or crash because setwd() was provided an invalid path. These files are meant to be ran on a terminal (i.e.: cmd, gitbash, 
VSCode with R package, etc.). 
