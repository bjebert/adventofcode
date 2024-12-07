
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.", "\n")[[1]]  # Example
inp <- get_input("2016/15", parse = F, user = "blakeebets", cache = T)




disk_id <- as.numeric(gsub("#", "", sapply(strsplit(inp, " "), function(x) x[2])))
positions <- as.numeric(sapply(strsplit(inp, " "), function(x) x[4]))
init <- as.numeric(gsub("\\.", "", sapply(strsplit(inp, " "), function(x) x[length(x)])))

disk <- setNames(lapply(1:length(disk_id), function(i) list(pos = positions[i], init = init[i])), disk_id)

disk[["7"]] <- list(pos = 11, init = 0)

z <- sapply(1:length(disk), function(i) {
    d <- disk[[i]]
    (d[["init"]] + i + 0:1e7) %% d[["pos"]]
})

which(rowSums(z) == 0) - 1

# 10:35 (89th)

# 11:04 (75th)
