
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

ind <- get_input("2025/06", parse = F, user = "bjebert", cache = F)

inx <- strsplit("123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   + ", "\n")[[1]]

inp <- inx

# Part 1 ------------------------------------------------------------------

ops <- strsplit(inp[length(inp)], " ")[[1]]
ops <- ops[ops != ""]

nums <- strsplit(inp[1:(length(inp)-1)], " ")
nums <- lapply(lapply(nums, function(x) x[x != ""]), as.numeric)

sum(sapply(1:length(ops), function(i) {
    Reduce(ops[i], sapply(nums, function(x) x[i]))
}))


# Part 2 ------------------------------------------------------------------

inp <- ind

rows <- strsplit(inp[1:(length(inp)-1)], "")
ops <- strsplit(inp[length(inp)], " ")[[1]]
ops <- ops[ops != ""]


w_ops <- which(strsplit(inp[length(inp)], "")[[1]] != " ")

sum(sapply(w_ops, function(w) {
    op <- strsplit(inp[length(inp)], "")[[1]][w]
    v <- NULL
    
    while(TRUE) {
        n <- sapply(rows, function(r) r[w])
        n <- n[!is.na(n) & n != " "]
        
        if(length(n) == 0) {
            break
        }
        
        v <- c(v, as.numeric(paste(n, collapse = "")))
        w <- w + 1
    }
    
    Reduce(op, v)
}))
