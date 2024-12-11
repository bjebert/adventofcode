
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- "2333133121414131402"
inp <- get_input("2024/09", parse = F, user = "bjebert", cache = F)

n <- as.numeric(strsplit(inp, "")[[1]])

lens <- n[seq(1, length(n), 2)]
free <- n[seq(2, length(n), 2)]

out <- c()

for(i in 1:length(lens)) {
    out <- c(out, rep(i-1, lens[i]))
    if(i <= length(free)) {
        out <- c(out, rep(NA, free[i]))
    }
}


# part 1 -------------------------------------------------------------------


while(which(is.na(out))[1] < max(which(!is.na(out)))) {
    
    wmin <- which(is.na(out))[1]
    wmax <- max(which(!is.na(out)))
    
    out[wmin] <- out[wmax]
    out[wmax] <- NA
}

z <- out[1:(which(is.na(out))[1] - 1)]
sum((1:length(z) - 1) * z)


# part 2 ------------------------------------------------------------------

inp <- "2333133121414131402"
inp <- get_input("2024/09", parse = F, user = "bjebert", cache = F)

n <- as.numeric(strsplit(inp, "")[[1]])

lens <- n[seq(1, length(n), 2)]
free <- n[seq(2, length(n), 2)]

out <- c()

for(i in 1:length(lens)) {
    out <- c(out, rep(i-1, lens[i]))
    if(i <= length(free)) {
        out <- c(out, rep(NA, free[i]))
    }
}


x <- rev(unique(out))
x <- x[!is.na(x)]

for(block in x) {
    print(block)
    sz <- sum(out == block, na.rm = T)
    
    w_taken <- which(!is.na(out))
    w_free <- which(is.na(out))
    
    start_free <- w_free[w_free - shift(w_free, 1, fill = -1) != 1]
    start_taken <- w_taken[w_taken - shift(w_taken, 1, fill = 0) != 1]
    
    frees <- start_taken - start_free
    wf <- which(frees >= sz)
    
    if(length(wf) > 0) {
        start_block <- start_free[wf[1]]  # where to move the block to
        
        if(start_block < which(out == block)[1]) {
            out[out == block] <- NA
            out[start_block:(start_block+sz-1)] <- block    
        }
    }
}


z <- copy(out)
sum((1:length(z) - 1) * z, na.rm = T)

