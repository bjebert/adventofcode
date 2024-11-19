
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/09", parse = F, user = "blakeebets", cache = T)

library(combinat)

# Problem -----------------------------------------------------------------

# inp <- strsplit("London to Dublin = 464
# London to Belfast = 518
# Dublin to Belfast = 141", "\n")[[1]]

src <- sapply(strsplit(inp, " to "), function(x) x[1])
end <- sapply(strsplit(inp, " "), function(x) x[3])
dist <- as.numeric(sapply(strsplit(inp, " "), function(x) x[5]))

map <- NULL

for(i in 1:length(src)) {
    map[[src[i]]][[end[i]]] <- dist[i]
    map[[end[i]]][[src[i]]] <- dist[i]
}

start <- loc[1]
perms <- permn(setdiff(loc, start))

dists <- sapply(perms, function(x) {
    curr <- start
    dist <- 0
    
    for(i in 1:length(x)) {
        dist <- dist + map[[curr]][[x[i]]]
        curr <- x[i]
    }
    
    return(dist)
})

min(dists)

# 8:14 (10th)

perms <- permn(loc)

dists <- sapply(perms, function(x) {
    curr <- x[1]
    dist <- 0
    
    for(i in 2:length(x)) {
        dist <- dist + map[[curr]][[x[i]]]
        curr <- x[i]
    }
    
    return(dist)
})

max(dists)

# 9:30 (9th)
