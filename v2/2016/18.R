
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/18", parse = F, user = "blakeebets", cache = T)

strsplit(inp, "")


get <- function(row, i) {
    if(i <= 0) {
        return('.')
    } else if(i > length(row)) {
        return('.')
    } else {
        return(row[i])
    }
}

is_trap <- function(tiles) {
    if(tiles[1] == '^' && tiles[2] == '^' && tiles[3] == '.') {
        return('^')
    }    
    if(tiles[1] == '.' && tiles[2] == '^' && tiles[3] == '^') {
        return('^')
    }    
    if(tiles[1] == '^' && tiles[2] == '.' && tiles[3] == '.') {
        return('^')
    }    
    if(tiles[1] == '.' && tiles[2] == '.' && tiles[3] == '^') {
        return('^')
    }    
    return('.')
}

row <- strsplit(inp, "")[[1]]

cnt <- 0
for(iter in 1:400000) {
    cnt <- cnt + sum(row == '.')
    if(iter %% 100 == 0) {
        print(iter)
    }
    new <- c()
    for(i in 1:length(row)) {
        tiles <- sapply((i-1):(i+1), function(x) get(row, x))
        new[i] <- is_trap(tiles)
    }
    row <- copy(new)
}

cnt

# 5:18 (T-3rd)

# 20:27 (> 100th)


