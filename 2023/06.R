rm(list=ls())
source("utilities.R")

inp <- get_input("2023/06", parse = F, deesblake = F, cache = T)

inp <- gsub("\\s+", " ", inp)
times <- as.numeric(strsplit(inp[1], "\\s")[[1]][2:5])
dists <- as.numeric(strsplit(inp[2], "\\s")[[1]][2:5])

#----------

inp <- strsplit("Time:      7  15   30
Distance:  9  40  200", "\n")[[1]]

inp <- gsub("\\s+", " ", inp)

times <- as.numeric(strsplit(inp[1], "\\s")[[1]][2:4])
dists <- as.numeric(strsplit(inp[2], "\\s")[[1]][2:4])









prod(sapply(1:length(times), function(i) {
    sum(sapply(0:times[i], function(x) {
        x * (times[i] - x)
    }) > dists[i])
}))

# 5:47

time <- as.numeric(paste(times, collapse = ""))
dist <- as.numeric(paste(dists, collapse = ""))

sum(((0:time) * (time - 0:time)) > dist)

# 8:11
