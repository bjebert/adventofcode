
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/16", parse = F, user = "blakeebets", cache = T)

a <- copy(inp)
res <- ""

while(nchar(a) < 35651584) {
    b <- copy(a)
    
    b <- rev(strsplit(b, "")[[1]])
    b <- 1 - as.numeric(b)
    
    a <- sprintf("%s0%s", a, paste(b, collapse = ""))
}

a <- substr(a, 1, 35651584)

checksum <- function(x) {
    while(length(x) %% 2 == 0) {
        x <- sapply(seq(1, length(x), 2), function(i) {
            if(x[i] == x[i+1]) "1" else "0"
        })
    }
    
    return(x)
}

x <- strsplit(a, "")[[1]]
paste(checksum(x), collapse = "")


# 4:55 (8th)

# 6:13 (7th)