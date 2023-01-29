#---------------------------------------------------------------------------------------------
#WARNING
#This file is NOT meant to be ran in Rstudio. This is an R **SCRIPT**, NOT a notebook.
#I.e.: Run it in VSCode and check the output with the terminal, or just execute it.

#---------------------------------------------------------------------------------------------
#Requirements
list.of.packages <- c("ggplot2", "data.table", "magrittr", "reshape2")
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
#B.3.1

block <- function(mat_1, mat_2){
    # Combines two matrices into a diagonal block matrix.
    #
    # Args:
    #   mat_1 (matrix): matrix 1.
    #   mat_2 (matrix): matrix 2.
    #
    # Returns:
    #   out (matrix): The matrix (mat_1, 0, 0, mat_2)


    n1 <- nrow(mat_1)
    n2 <- nrow(mat_2)
    n <- n1 + n2

    m1 <- ncol(mat_1)
    m2 <- ncol(mat_2)
    m <- m1 + m2

    out <- matrix(0, n, m)
    out[1:n1, 1:m1] <- mat_1
    out[(n1 + 1):n, (m1 + 1):m] <- mat_2

    return(out)

}

householder <- function(mat) {
    # Returns a list of Householder reflections.
    #
    # Args:
    #   mat (matrix): matrix.
    #
    # Returns:
    #   W (list): Householder reflections

    R <- mat
    n <- ncol(mat)
    m <- nrow(mat)
    n_iter <- min(n, m)
    W <- list()

    if(n_iter == 1){
 
        message("Either nrow or ncol is one. Returning NULL.")
        return(NULL)

    }

    #Subsequent rounds
    for (i in 1:n_iter){

        x <- R[i:m, i] 
        e <- as.matrix(c(1, rep(0, length(x) - 1)))
        v <- sign(x[1]) * sqrt(sum(x^2)) * e + x

        temp <- diag(length(x)) - 2 * v %*% t(v) / as.numeric(t(v) %*% v)
        W[[i]] <- if(i > 1){block(diag(i - 1), temp)} else {temp}

        R <- W[[i]] %*% R
    
    }

    return(W)

}


#---------------------------------------------------------------------------------------------
#B.3.2

qr_householder <- function(mat){
    # QR decomposition via Householder reflections.
    #
    # Args:
    #   mat (matrix): matrix.
    #
    # Returns:
    #   out (list): QR decomposition

    W <- householder(mat)
    Q <- Reduce("%*%", W)
    R <- Reduce("%*%", rev(W)) %*% mat
    R[lower.tri(R)] <- 0

    return(list(Q = Q,
                R = R))

}


#---------------------------------------------------------------------------------------------
#B.3.3

Z <- cbind(c(1, 4, 7, 4, 4), 
           c(2, 5, 8, 2, 2), 
           c(3, 6, 7, 3, 2))

#Import functions from B.2


current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_file_path)
setwd("..")
setwd(file.path(getwd(), subDir = "B.2"))

#Import functions from B.2
#No if __name__ == __main__ in R so it will import the functions AND run the code
source("__main__.R")

msg("Creating directories and saving plot...")
QR_results <- list(mgs = QR(Z, modif = TRUE),
                   householder = qr_householder(Z),
                   R = list(Q = qr.Q(qr(Z)), R = qr.R(qr(Z))))


#Error density plots
errors <- lapply(QR_results, function(x){c(abs(x$Q %*% x$R - Z))})

df <- melt(as.data.table(errors))
names(df) <- c("Method", "Errors")
plot <- ggplot(data = df, aes(x = Errors, fill = Method)) +
        geom_density(alpha = 0.5) + 
        xlab("Absolute error") +
        ylab("Density") +
        ggtitle("Pdf of |Z - QR|")

setwd(current_file_path)
pic_dir <- file.path(current_file_path, subDir = "Plots")
dir.create(pic_dir, showWarnings = FALSE)
setwd(pic_dir)
ggsave(filename = "density.jpeg", plot = plot, device = "jpeg")

msg("Done.")

