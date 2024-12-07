
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------


inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/05", parse = F, user = "blakeebets", cache = T)

library(openssl)
doorid <- "ojvtpuvg"


# Part 1 ------------------------------------------------------------------

passwords <- sprintf("%s%d", doorid, 1:1.2e7)
outs <- openssl::md5(passwords)

ss <- substr(outs, 1, 5)
paste(substr(outs[ss == "00000"], 6, 6)[1:8], collapse = "")

# 6:14 (39th)


# Part 2 ------------------------------------------------------------------

x <- 1
password <- rep(NA, 8)

while(any(is.na(password))) {
    print(x)
    passwords <- sprintf("%s%d", doorid, x:(x+1e6))
    outs <- openssl::md5(passwords)
    
    ss <- substr(outs, 1, 5)
    valid <- outs[ss == "00000"]
    
    print(valid)
    
    for(v in valid) {
        pos <- substr(v, 6, 6)
        if(!(pos %in% as.character(0:7))) {
            next
        }
        
        pos <- as.numeric(pos)
        
        char <- substr(v, 7, 7)
        
        if(pos %in% 0:7 && is.na(password[(pos+1)])) {
            password[(pos+1)] <- char
        }
    }
    
    print(password)
    x <- x + 1e6
}

paste(password, collapse = "")

# 15:42 (75th)

