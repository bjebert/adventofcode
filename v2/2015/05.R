
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/05", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

ss <- strsplit(inp, "")

sum(sapply(ss, function(x) {
    sum(x %in% c("a", "e", "i", "o", "u")) >= 3 && any(x == shift(x), na.rm = T) && !grepl("ab|cd|pq|xy", paste(x, collapse = ""))
}))

# 2:04 (2nd)

sum(sapply(ss, function(x) {
    
    cond1 <- FALSE
    for(i in 1:(length(x)-3)) {
        pair <- paste0(x[i], x[i+1])
        if(grepl(pair, paste(x[(i+2):length(x)], collapse = ""))) {
            cond1 <- TRUE
        }
    }
    
    cond2 <- FALSE
    for(i in 1:(length(x)-2)) {
        if(x[i] == x[i+2]) {
            cond2 <- TRUE
        }
    }
    
    return(cond1 && cond2)    
    
}))

# 7:19 (8th)