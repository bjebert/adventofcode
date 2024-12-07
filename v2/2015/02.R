
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/02", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

box <- lapply(strsplit(inp, "x"), as.numeric)

sum(sapply(box, function(x) {
    smallest <- min(c(x[1]*x[2], x[1]*x[3], x[2]*x[3]))
    return(smallest + 2*x[1]*x[2] + 2*x[1]*x[3] + 2*x[2]*x[3])
}))

# 1:19 (1st)

sum(sapply(box, function(x) {
    bow <- x[1]*x[2]*x[3]
    lens <- sort(x)    
    perim <- lens[1]*2 + lens[2] * 2
    
    perim + bow    
}))

# 2:24 (1st)