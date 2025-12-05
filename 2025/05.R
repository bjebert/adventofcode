
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2025/05", parse = F, user = "bjebert", cache = F)

inp <- strsplit("3-5
10-14
16-20
12-18

1
5
8
11
17
32", "\n")[[1]]  # Example

w <- which(inp == "")

fresh <- inp[1:(w-1)]
fresh <- lapply(strsplit(fresh, "-"), as.numeric)
avail <- as.numeric(inp[(w+1):length(inp)])

sum(sapply(avail, function(a) {
    any(sapply(fresh, function(f) a >= f[1] && a <= f[2]))
}))


# part 2 ------------------------------------------------------------------

# remove overlaps in fresh range
# sort fresh

fsort <- unique(fresh[order(sapply(fresh, function(x) x[1]))])
fnew <- fsort[1]

for(j in 2:length(fsort)) {
    lo <- fsort[[j]][1]
    hi <- fsort[[j]][2]
    
    for(k in 1:length(fnew)) {
        f <- fnew[[k]]
        
        if(lo == f[1] && hi >= f[2]) {  # full engulf
            lo <- f[2] + 1
        } else if(lo <= f[1] && hi >= f[1]) {  # left -> overlaps
            hi <- f[1] - 1
        } else if(lo <= f[2] && hi >= f[2]) {  # overlaps -> right
            lo <- f[2] + 1
        } else if(lo >= f[1] && hi <= f[2]) {  # small subset
            lo <- 1
            hi <- 0
            break
        }
        
        else {  # no overlap
            next
        }
    }
    
    if(lo <= hi) {
        fnew[[length(fnew) + 1]] <- c(lo, hi)
    }
}


fnew <- unique(fnew[order(sapply(fnew, function(x) x[1]))])
sum(sapply(fnew, function(x) sum(x[2] - x[1] + 1)))

all(sapply(1:(length(fnew) - 1), function(i) fnew[[i]][2] < fnew[[i+1]][1]))
x <- 29
fsort[x:(x+4)]
fnew[(x:(x+4)) - 3]
