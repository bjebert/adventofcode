
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/21", parse = F, user = "blakeebets", cache = T)

start <- "abcdefgh"
# start <- "abcde"

x <- strsplit(start, "")[[1]]
x <- letters[1:8]

for(line in inp) {
    ls <- strsplit(line, " ")[[1]]
    
    if(ls[1] == "swap" && ls[2] == "position") {
        p1 <- as.numeric(ls[3])+1
        p2 <- as.numeric(ls[6])+1
        
        tmp <- x[p1]
        x[p1] <- x[p2]
        x[p2] <- tmp
        
    } else if(ls[1] == "swap" && ls[2] == "letter") {
        l1 <- ls[3]
        l2 <- ls[6]
        
        p1 <- which(x == l1)
        p2 <- which(x == l2)
        
        tmp <- x[p1]
        x[p1] <- x[p2]
        x[p2] <- tmp
        
        
    } else if(ls[1] == "rotate" && ls[2] == "left") {
        n <- as.numeric(ls[3])
        x <- x[((1:length(x) + n) - 1) %% length(x) + 1]
        
    } else if(ls[1] == "rotate" && ls[2] == "right") {
        n <- as.numeric(ls[3])
        x <- x[((1:length(x) - n) - 1) %% length(x) + 1]
        
    } else if(ls[1] == "rotate" && ls[2] == "based") {
        w <- which(x == ls[7])
        n <- w
        
        if(w > 4) {
            n <- n + 1
        }
        x <- x[((1:length(x) - n) - 1) %% length(x) + 1]
    } else if(ls[1] == "reverse") {
        p1 <- as.numeric(ls[3])+1
        p2 <- as.numeric(ls[5])+1
        
        x[p1:p2] <- x[p2:p1]
        
    } else if(ls[1] == "move") {
        p1 <- as.numeric(ls[3])+1
        p2 <- as.numeric(ls[6])+1
        
        l <- x[p1]
        x <- x[-p1]
        
        if(p2 == 1) {
            x <- c(l, x)
        } else if(p2 == length(x)+1) {
            x <- c(x, l)
        } else {
            x <- c(x[1:(p2-1)], l, x[p2:length(x)])
        }
    }
}

x

# 11:50 (3rd)

x <- strsplit("fbgdceah", "")[[1]]

for(line in rev(inp)) {
    ls <- strsplit(line, " ")[[1]]
    
    if(ls[1] == "swap" && ls[2] == "position") {
        p1 <- as.numeric(ls[3])+1
        p2 <- as.numeric(ls[6])+1
        
        tmp <- x[p1]
        x[p1] <- x[p2]
        x[p2] <- tmp
        
    } else if(ls[1] == "swap" && ls[2] == "letter") {
        l1 <- ls[3]
        l2 <- ls[6]
        
        p1 <- which(x == l1)
        p2 <- which(x == l2)
        
        tmp <- x[p1]
        x[p1] <- x[p2]
        x[p2] <- tmp
        
        
    } else if(ls[1] == "rotate" && ls[2] == "left") {
        n <- as.numeric(ls[3])
        x <- x[((1:length(x) - n) - 1) %% length(x) + 1]
        
    } else if(ls[1] == "rotate" && ls[2] == "right") {
        n <- as.numeric(ls[3])
        x <- x[((1:length(x) + n) - 1) %% length(x) + 1]
        
    } else if(ls[1] == "rotate" && ls[2] == "based") {
        rotate_based_original <- function(x, letter) {
            w <- which(x == letter)
            n <- w
            
            if(w > 4) {
                n <- n + 1
            }
            return(x[((1:length(x) - n) - 1) %% length(x) + 1])
        }
    
        # try all permutations rotations to see what produces the current string
        res <- lapply(0:length(x), function(i) {
            x[((1:length(x) + i) - 1) %% length(x) + 1]    
        })
        
        rb <- lapply(res, function(x) rotate_based_original(x, ls[7]))
        x <- res[sapply(rb, function(i) identical(i, x))][[1]]
    } else if(ls[1] == "reverse") {
        p1 <- as.numeric(ls[3])+1
        p2 <- as.numeric(ls[5])+1
        
        x[p1:p2] <- x[p2:p1]
        
    } else if(ls[1] == "move") {
        p1 <- as.numeric(ls[6])+1
        p2 <- as.numeric(ls[3])+1
        
        l <- x[p1]
        x <- x[-p1]
        
        if(p2 == 1) {
            x <- c(l, x)
        } else if(p2 == length(x)+1) {
            x <- c(x, l)
        } else {
            x <- c(x[1:(p2-1)], l, x[p2:length(x)])
        }
    }
}

paste(x, collapse = "")

# 20:50 (6th)