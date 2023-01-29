#---------------------------------------------------------------------------------------------
#WARNING
#This file is NOT meant to be ran in Rstudio. This is an R **SCRIPT**, NOT a notebook.
#I.e.: Run it in VSCode and check the output with the terminal, or just execute it.

#---------------------------------------------------------------------------------------------
#Requirements
list.of.packages <- c("ggplot2", "jpeg", "raster", "data.table", "magrittr", "reshape2", "imager", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

#---------------------------------------------------------------------------------------------
msg <- function(str){
    # Prints a message.
    #
    # Args:
    #   str (character): message to be printed.
    #
    # Returns:
    #   NULL
    
    message("")
    message(str)
    message("")

}

#---------------------------------------------------------------------------------------------
#B.1.1
#Set directory to file path
if((getOption('run.main', default=TRUE))){

    current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)

} else {

    current_file_path <- getwd()

}
setwd(current_file_path)


#Load image in the Files folder
img <- load.image("Files/brickwall.jpeg")
#Delete b,g
img[, , 1, 2] <- 0
img[, , 1, 3] <- 0

#---------------------------------------------------------------------------------------------
#B.1.2
#Compute SVD
red_svd <- svd(img[, , 1, 1])

plot_tile <- function(mat, title = NULL){
    # Plots a matrix with the viridis colour scale.
    #
    # Args:
    #   mat (matrix): the matrix to be plotted.
    #
    # Returns:
    #   out (gg ggplot): geom_raster plot.

	df <- melt(mat)
	out <- ggplot(data = df, aes(x = Var2, y = Var1)) +
				geom_raster(aes(fill = value)) +
	  			scale_fill_viridis_c() +
  				labs(x = "", y = "") +
  				theme_bw() +
                ggtitle(title)

  	return(out)

}

#Create directory to host pictures
msg("Creating directories and producing pictures for B.1.2...")

pic_dir <- file.path(current_file_path, subDir = "Plots")
dir.create(pic_dir, showWarnings = FALSE)
#Create subdirectory to host SVD pictures
save_dir <- file.path(pic_dir, subDir = "B.1.2")
dir.create(save_dir, showWarnings = FALSE)

#Create and save pictures
setwd(save_dir)
for(i in 1:length(red_svd)){

    title <- paste("Red channel SVD: matrix", toupper(names(red_svd)[[i]]))
    plot <- if(is.matrix(red_svd[[i]])){plot_tile(red_svd[[i]], title)} else {plot_tile(diag(red_svd[[i]]), title)}
    ggsave(filename = paste(toupper(names(red_svd)[[i]]), "jpeg", sep = "."), plot = plot, device = "jpeg")

}
msg("Done.")

#---------------------------------------------------------------------------------------------
#B.1.3
#Create subdirectory to host low resolution pictures
msg("Creating directories and producing pictures for B.1.3...")
save_dir <- file.path(pic_dir, subDir = "B.1.3")
dir.create(save_dir, showWarnings = FALSE)
setwd(save_dir)


lower_rank_approx <- function(img_svd, rank_){
    # Computes the rank_ approximation of the matrix which spawned img_svd.
    #
    # Args:
    #   img_svd (list): output of svd(...).
    #   rank_ (int): rank of the approximation
    #
    # Returns:
    #   out (matrix): low-rank approximation.   

	n <- min(nrow(img_svd$u), nrow(img_svd$v))
	rank_ = if(rank_ < 0){1} else if(rank_ > n){n} else{rank_}

	#Edge case where rank_ = 1
	if(rank_ == 1){

		return(as.matrix(img_svd$u[, 1:rank_]) %*% as.matrix(img_svd$d[1:rank_]) %*% t(as.matrix(img_svd$v[, 1:rank_])))

	}

	return(img_svd$u[, 1:rank_] %*% diag(img_svd$d[1:rank_]) %*% t(img_svd$v[, 1:rank_]))

}

lower_red_resolution <- function(pic, rank_, channel){
    # Lowers the resolution of a picture.
    #
    # Args:
    #   pic (cimg imager_array numeric): picture loaded with imager
    #   rank_ (int): rank of the approximation
    #   channel (int or vector): channels which resolution are to be lowered
    #
    # Returns:
    #   out (matrix): low-rank approximation.   

    channel <- channel[channel <= 3 & channel >= 1]
    for(i in channel){

        svd_ <- svd(pic[, , 1, i])
        newval <- lower_rank_approx(svd_, rank_)
        pic[, , 1, i] <- newval

    }

    return(pic)

}

#Compute the lower-resolution pictures
rank_ <- c(1, 2, 4, 8, 16, 32)
lowres_pictures <- lapply(as.list(rank_), function(r){lower_red_resolution(pic = img, rank_ = r, channel = 1)})
names(lowres_pictures) <- paste("Rank", rank_)

#Save
for(i in 1:length(lowres_pictures)){

    save.image(lowres_pictures[[i]], paste(names(lowres_pictures)[i], "jpeg", sep = "."), quality = 1)

}

msg("Done.")

#---------------------------------------------------------------------------------------------
#B.1.4
msg("Creating directories and computing errors for B.1.4...")
save_dir <- file.path(pic_dir, subDir = "B.1.4")
dir.create(save_dir, showWarnings = FALSE)
setwd(save_dir)

n <- min(dim(img)[1], dim(img)[2])
errors <- rep(NA, n)

pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:(n-1)){

    errors[i] <- sum(abs(img[, , 1, 1] - lower_rank_approx(red_svd, rank_ = i)))
    setTxtProgressBar(pb, i)

}

errors[n] <- 0
errors <- as.data.table(errors / sum(abs(img[, , 1, 1]))) %>%
            .[, Rank := c(1:n)]

names(errors)[1] <- "Error"

plot <- ggplot(data = errors, aes(x = Rank, y = Error)) +
        geom_line() +
        xlab("Rank of lower resolution R matrix") +
        ylab("Relative error") +
        ggtitle("Error vs Rank (Frobenius norm)")

ggsave(filename = "Frobenius.jpeg", plot = plot, device = "jpeg")
msg("Done.")

#---------------------------------------------------------------------------------------------
#B.1.5
msg("Creating directories and computing errors for B.1.5...")
save_dir <- file.path(pic_dir, subDir = "B.1.5")
dir.create(save_dir, showWarnings = FALSE)
setwd(save_dir)

n <- min(dim(img)[1], dim(img)[2])
errors <- rep(NA, n)

pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:(n-1)){

    errors[i] <- sqrt(sum((img[, , 1, 1] - lower_rank_approx(red_svd, rank_ = i))^2))
    setTxtProgressBar(pb, i)

}

errors[n] <- 0
errors <- as.data.table(errors / sqrt(sum(img[, , 1, 1]^2))) %>%
            .[, Rank := c(1:n)]

names(errors)[1] <- "Error"

plot <- ggplot(data = errors, aes(x = Rank, y = Error)) +
        geom_line() +
        xlab("Rank of lower resolution R matrix") +
        ylab("Relative error") +
        ggtitle("Error vs Rank (Frobenius norm)")

ggsave(filename = "Norm2.jpeg", plot = plot, device = "jpeg")
msg("Done.")

