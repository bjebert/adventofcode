
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20", "\n")[[1]]  # Example

inp <- get_input("2024/07", parse = F, user = "bjebert", cache = F)


lhs <- as.numeric(sapply(strsplit(inp, ": "), function(x) x[1]))
rhs <- lapply(strsplit(sapply(strsplit(inp, ": "), function(x) x[2]), " "), as.numeric)


check_perms <- function(i, perms) {
    for(j in 1:nrow(perms)) {
        cnt <- 0
        if(perms[j,1] == '+') {
            cnt <- rhs[[i]][1] + rhs[[i]][2]
        } else if(perms[j,1] == '*') {
            cnt <- rhs[[i]][1] * rhs[[i]][2]
        } else {
            cnt <- as.numeric(sprintf("%s%s", rhs[[i]][1], rhs[[i]][2]))
        }
        
        if(length(rhs[[i]]) > 2) {
            for(k in 2:(length(rhs[[i]]) - 1)) {
                if(perms[j,k] == '+') {
                    cnt <- cnt + rhs[[i]][k+1]
                } else if(perms[j,k] == '*') {
                    cnt <- cnt * rhs[[i]][k+1]
                } else {
                    cnt <- as.numeric(sprintf("%s%s", cnt, rhs[[i]][k+1]))
                }
            }
        }
        
        if(cnt == lhs[i]) {
            return(j)
        }
    }
    
    return(FALSE)
}

sum(lhs[sapply(1:length(lhs), function(i) {
    print(i)
    perms <- gtools::permutations(n = 3, v = c('*', '+', '|'), r = length(rhs[[i]]) - 1, repeats.allowed = T)
    check_perms(i, perms)
})])

options(scipen=99)
