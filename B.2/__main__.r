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
#Function

proj <- function(u, v){
    # This operator projects the vector v orthogonally onto the line spanned by vector u.
    #
    # Args:
    #   u (vector): u vector.
    #   v (vector): v vector.
    #
    # Returns:
    #   out (vector): orthogonal projection.

    return(as.numeric((u %*% v) / (u %*% u)) * u)

}


gram_schmidt <- function(mat){
    # Computes an orthonormal set of vectors (column-wise) from a matrix input.
    #
    # Args:
    #   mat (matrix): a matrix.
    #
    # Returns:
    #   out (matrix): an orthonormal matrix.

    if(ncol(mat) == 1){

        return(mat)

    }

    out <- matrix(0, nrow(mat), ncol(mat))
    out[, 1] <- mat[, 1]
    for(j in 2:ncol(out)){

        v <- mat[, j]
        out[, j] <- v
        for(k in 1:(j - 1)){

            out[, j] <- out[, j] - proj(out[, k], v)

        }

        out[, j] <- out[, j] / sqrt(sum(out[, j]^2))

    }

    return(out)

}

gram_schmidt_modified <- function(mat){
    # Computes an orthonormal set of vectors (column-wise) from a matrix input.
    # Uses the modified algorithm from Trefethen, Bau (1997) p.58
    #
    # Args:
    #   mat (matrix): a matrix.
    #
    # Returns:
    #   out (matrix): an orthonormal matrix.

    m <- ncol(mat)
    if(m == 1){

        return(mat)

    }

    v <- mat 
    q <- matrix(0, nrow(mat), m)
    for(j in 1:m){

        q[, j] <- v[, j] / sqrt(sum(v[, j]^2))
        if(j + 1 > m){

            break

        }

        for(k in (j+1):m){

            v[, k] <- v[, k] - as.numeric(q[, j] %*% v[, k]) * q[, j]

        }

    }

    return(q)

}

QR <- function(mat, modif = FALSE){
    # Computes the QR decomposition of a matrix.
    #
    # Args:
    #   mat (matrix): a matrix.
    #   modif (bool): whether or not we should use the modified G-S algo
    #
    # Returns:
    #   out (list): Q and R.

    Q <- if(modif){gram_schmidt_modified(mat)} else {gram_schmidt(mat)}
    R <- t(Q) %*% mat
    R[lower.tri(R)] <- 0

    return(list(Q = Q, 
                R = R))

}

#---------------------------------------------------------------------------------------------
#Experiment

if((getOption('run.main', default = TRUE))){

    msg("Computing matrices...")
    n <- 80

    U <- qr.Q(qr(matrix(runif(n^2), n, n)))
    V <- qr.Q(qr(matrix(runif(n^2), n, n)))
    S <- diag(2^(seq(from = -1, to = -80)))

    A <- U %*% S %*% t(V)

    QR_A <- QR(A)
    QR_A_modif <- QR(A, modif = TRUE)

    #Look, I have NO CLUE WHATSOEVER what r_ii stands for. I'll plot the pdf of the absolute errors |A - QR|.
    df <- as.data.table(c(abs(QR_A$Q %*% QR_A$R - A))) %>%
            .[, Modified := c(abs(QR_A_modif$Q %*% QR_A_modif$R - A))]

    names(df)[1] <- "Vanilla"
    df <- melt(df)
    names(df) <- c("Algorithm", "Error")

    plot <- ggplot(data = df, aes(x = Error, fill = Algorithm)) + 
            geom_density(alpha = 0.5) +
            xlab("Absolute Error") +
            ylab("Density") +
            ggtitle("Pdf of absolute error, QR vs A")


    msg("Creating directories and saving plot...")
    current_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(current_file_path)
    pic_dir <- file.path(current_file_path, subDir = "Plots")
    dir.create(pic_dir, showWarnings = FALSE)
    setwd(pic_dir)
    ggsave(filename = "density.jpeg", plot = plot, device = "jpeg")

    msg("Done.")

}



