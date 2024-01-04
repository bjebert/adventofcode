rm(list=ls())
source("utilities.R")
inp <- get_input("2022/20", parse = T, deesblake = F, cache = F)

# inp <- c(1, 2, -3, 3, -2, 0, 4)
N <- length(inp)

circ <- list()
for(i in 1:N) {
    circ[[i]] <- list(value = inp[i], idx = i)
}

for(i in 1:N) {
    if(i %% 20 == 0) {
        print(i)
    }
    idx <- circ[[i]][["idx"]]
    v <- circ[[i]][["value"]]
    new <- ((idx + circ[[i]][["value"]] - 1) %% N) + 1
    
    if(v > 0) {
        if(new > idx) {
            mv <- (idx+1):new
        } else if(new < idx) {
            mv <- c((idx+1):N, 1:new)
        }
        
        circ <- lapply(circ, function(x) if(x[["idx"]] %in% mv) list(value = x[["value"]], idx = ((x[["idx"]] - 2) %% N) + 1) else x)
    } else if(v < 0) {
        if(new > idx) {
            mv <- c((idx-1):1, N:new)
        } else if(new < idx) {
            mv <- (idx-1):new
        }
        
        circ <- lapply(circ, function(x) if(x[["idx"]] %in% mv) list(value = x[["value"]], idx = (x[["idx"]] %% N) + 1) else x)
    }
    
    circ[[i]][["idx"]] <- new
}

sapply(1:5000, function(x) circ[sapply(circ, function(y) y[["idx"]] == x)][[1]][["value"]])

w <- which(sapply(circ, function(x) x[["value"]] == 0))
circ[[(w + 1000 - 1) %% N + 1]]

s1 <- circ[sapply(circ, function(x) x[["idx"]] == (w + 1000 - 1) %% N + 1)][[1]][["value"]]
s2 <- circ[sapply(circ, function(x) x[["idx"]] == (w + 2000 - 1) %% N + 1)][[1]][["value"]]
s3 <- circ[sapply(circ, function(x) x[["idx"]] == (w + 3000 - 1) %% N + 1)][[1]][["value"]]

sum(c(s1, s2, s3))

