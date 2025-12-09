
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/09", parse = F, user = "bjebert", cache = F)
inx <- strsplit("7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3", "\n")[[1]]


# Part 1 ------------------------------------------------------------------

inp <- inx
inp <- inr

n <- t(sapply(strsplit(inp, ","), as.numeric))


e <- sapply(1:(nrow(n)-1), function(i) {
    sapply((i+1):nrow(n), function(j) {
        Reduce(`*`, abs(n[i,] - n[j,]) + 1)
    })
})

max(sapply(e, max))

# Part 2 ------------------------------------------------------------------

inp <- inr
inp <- inx

# whichever way the inputs come in - the rectangle has to "look" that way
n <- t(sapply(strsplit(inp, ","), as.numeric))

is_safe <- function(i, j, best = 0) {
    a <- n[i,]
    b <- n[j,]
    
    if(j == i + 1 || (i == 1 && j == nrow(n))) return(TRUE)
    if(any(a - b == 0)) return(TRUE)
    
    rng_x <- a[1]:b[1]
    rng_y <- a[2]:b[2]
    size <- as.double(length(rng_x)) * length(rng_y)
    
    if(size < best) return(FALSE)  # don't bother..
    
    # for each rng_y, look right
    
    for(y in rng_y) {
        rng_x        
        
        
    }
    
}

is_safe(1, 3)  # false
i <- 1
j <- 3


best <- 0
for(i in 1:(nrow(n) - 1)) {
    for(j in (i+1):nrow(n)) {
        if(is_safe(i, j, best)) {
            size <- Reduce(`*`, abs(n[i,] - n[j,]) + 1)
            if(size > best) {
                print(size)
                best <- size
            }
        }
    }
}
