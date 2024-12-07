
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/04", parse = F, user = "blakeebets", cache = T)

library(openssl)

# Problem -----------------------------------------------------------------

inc <- 200000
N <- inc
while(TRUE) {
    out <- md5(paste0(inp, (N-inc+1):N))
    w <- substr(out, 1, 5) == "00000"
    
    if(any(w)) {
        print(((N-inc+1):N)[w])
        break
    }
    
    N <- N + inc
}

# 5:13 (19th)

# Part 2 ------------------------------------------------------------------

inc <- 200000
N <- inc
while(TRUE) {
    out <- md5(paste0(inp, (N-inc+1):N))
    w <- substr(out, 1, 6) == "000000"
    
    if(any(w)) {
        print(((N-inc+1):N)[w])
        break
    }
    
    N <- N + inc
}



# 6:06 (17th)



