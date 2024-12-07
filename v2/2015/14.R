
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/14", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

# inp <- strsplit("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
# Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.", "\n")[[1]]

ss <- strsplit(inp, " ")

map <- list()

line <- ss[[1]]

for(line in ss) {
    deer <- line[1]  
    v <- as.numeric(line[4])
    vt <- as.numeric(line[7])
    rest <- as.numeric(line[14])
    
    map[[deer]] <- c("v" = v, "vt" = vt, "rest" = rest)
}

t <- 2503

getdist <- function(stats, t) {
    cycle_len <- stats[["vt"]] + stats[["rest"]]
    
    full_dist <- (stats[["v"]] * stats[["vt"]]) * (t %/% cycle_len)  # full cycles
    part_dist <- min(stats[["vt"]], t %% cycle_len) * stats[["v"]]    
    
    full_dist + part_dist    
}

max(sapply(map, function(x) getdist(x, t)))

# 4:56 (4th)

# Part 2

points <- setNames(rep(0, length(map)), names(map))

movement <- lapply(map, function(x) {
    movebool <- rep(c(rep(T, x[["vt"]]), rep(F, x[["rest"]])), 50)[1:t]
    cumsum(movebool * x[["v"]])
})

for(i in 1:t) {
    dists <- sapply(movement, function(x) x[i])    
    points[which(dists == max(dists))] <- points[which(dists == max(dists))] + 1
}

max(points)

# 8:47 (2nd)


