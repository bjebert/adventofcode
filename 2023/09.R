rm(list=ls())
source("utilities.R")
inp <- get_input("2023/09", parse = F, deesblake = F, cache = T)

inp <- strsplit("0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45", "\n")[[1]]


getdiff <- function(row) {
    diff <- shift(row, -1) - row
    diff[!is.na(diff)]
}

getnext <- function(row) {
    hist <- list(row)
    
    d <- getdiff(row)
    hist[[2]] <- d
    
    while(!all(d == 0)) {
        d <- getdiff(d)
        hist[[length(hist) + 1]] <- d
    }
    
    for(i in length(hist):1) {
        if(i == length(hist)) {
            hist[[i]] <- c(hist[[i]], 0)
        } else {
            hist[[i]] <- c(hist[[i]], hist[[i]][length(hist[[i]])] + hist[[i+1]][length(hist[[i+1]])])
        }
    }
    
    return(tail(hist[[1]], 1))
}

getnext2 <- function(row) {
    hist <- list(row)
    
    d <- getdiff(row)
    hist[[2]] <- d
    
    while(!all(d == 0)) {
        d <- getdiff(d)
        hist[[length(hist) + 1]] <- d
    }
    
    for(i in length(hist):1) {
        if(i == length(hist)) {
            hist[[i]] <- c(0, hist[[i]])
        } else {
            hist[[i]] <- c(hist[[i]][1] - hist[[i+1]][1], hist[[i]])
        }
    }
    
    return(head(hist[[1]], 1))
}


sum(sapply(inp, function(x) {
    getnext2(as.numeric(strsplit(x, " ")[[1]]))
}))

