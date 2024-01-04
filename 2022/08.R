rm(list=ls())
source("utilities.R")
inp <- get_input("2022/8", parse = F, deesblake = F, cache = F)

# inp <- strsplit("30373
# 25512
# 65332
# 33549
# 35390", "\n")[[1]]

d <- nchar(inp[1])

m <- matrix(as.numeric(strsplit(paste(inp, collapse = ""), "")[[1]]), nrow = d, byrow = T)
v <- matrix(rep(FALSE, d*d), nrow = d)

is_vis <- function(i, j) {
    if(i == 1 || j == 1 || i == d || j == d) {
        return(TRUE)
    } else {
        x <- m[i, j]
        
        # Look from top
        if(all(m[1:(i-1), j] < x)) {
            return(TRUE)
        }
        # Bottom
        if(all(m[d:(i+1), j] < x)) {
            return(TRUE)
        }
        # Left
        if(all(m[i, 1:(j-1)] < x)) {
            return(T)
        }
        # Right
        if(all(m[i, (j+1):d] < x)) {
            return(T)
        }
    }
    return(F)
}


vdist <- function(i, j) {
    i1 <- i
    j1 <- j
    x <- m[i1, j1]
    
    # Move left
    left <- 0
    while(j1 > 1) {
        left <- left + 1
        j1 <- j1 - 1
        if(m[i1, j1] >= x) {
            break
        }
    }
    
    # Move right
    right <- 0
    i1 <- i
    j1 <- j
    
    while(j1 < d) {
        right <- right + 1
        j1 <- j1 + 1
        if(m[i1, j1] >= x) {
            break
        }
    }
    
    # Move up
    up <- 0
    i1 <- i
    j1 <- j
    
    while(i1 > 1) {
        up <- up + 1
        i1 <- i1 - 1
        if(m[i1, j1] >= x) {
            break
        }
    }
    
    # Move down
    down <- 0
    i1 <- i
    j1 <- j
    
    while(i1 < d) {
        down <- down + 1
        i1 <- i1 + 1
        if(m[i1, j1] >= x) {
            break
        }
    }
    
    return(up * left * right * down)
}


mx <- 0
for(i in 1:d) {
    for(j in 1:d) {
        v <- vdist(i, j)
        if(v > mx) {
            mx <- v
            print(sprintf("%s,%s = %s", i, j, mx))
        }
    }
}
