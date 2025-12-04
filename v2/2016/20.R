
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/20", parse = F, user = "blakeebets", cache = T)

blocks <- lapply(strsplit(inp, "-"), as.numeric)

blocks_sorted <- blocks[order(sapply(blocks, function(x) x[1]))]

curr <- 0

for(i in 1:length(blocks_sorted)) {
    if(blocks_sorted[[i]][1] <= (curr + 1)) {
        curr <- max(curr, blocks_sorted[[i]][2])
    } else {
        print(curr + 1)
        break
    }
}

# 4:09 (10th)

curr <- 0
cnt <- 0

blocked <- F

for(i in 1:length(blocks_sorted)) {
    if(blocks_sorted[[i]][1] > (curr + 1)) {
        cnt <- cnt + (blocks_sorted[[i]][1] - curr - 1)
    }
    curr <- max(curr, blocks_sorted[[i]][2])
}

# 7:16 (7th)

