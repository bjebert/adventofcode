
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/10", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

inp <- "1113222113"

outs <- c()

for(i in 1:40) {
    print(i)
    ss <- strsplit(inp, "")[[1]]
    curr <- ss[1]
    cnt <- 1
    out <- ""
    
    for(j in 2:length(ss)) {
        if(ss[j] != curr) {
            # write
            out <- paste0(out, paste0(as.character(cnt), curr))
            cnt <- 1
        } else {
            # inc
            cnt <- cnt + 1
        }
        curr <- ss[j]
    }
    
    inp <- paste0(out, paste0(as.character(cnt), curr))
    outs <- c(outs, nchar(inp))
}

nchar(inp)

# 5:54 (36th)

inp <- "1113222113"
ss <- strsplit(inp, "")[[1]]    

for(i in 1:50) {
    w <- which(ss != shift(ss, 1, fill = ss[1]))
    
    counts <- as.character(w - shift(w, 1, fill = 1))
    chars <- ss[w-1]
    out <- character(length = length(counts) + length(chars) + 2)
    
    out[seq(1, length(counts) * 2, 2)] <- counts
    out[seq(2, length(chars) * 2, 2)] <- chars
    
    out[length(out)] <- ss[length(ss)]
    out[length(out) - 1] <- w[length(w)] - length(ss) + 1
    
    print(sprintf("%d/%d", i, length(out)))
    ss <- copy(out)
}

# 14:41 (>100th)
