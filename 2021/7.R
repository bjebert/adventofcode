source("Utils.R")

inp <- as.numeric(strsplit(readLines("2021/7.txt"), ",")[[1]])

min(sapply(min(inp):max(inp), function(x) sum(abs(inp - x))))
min(sapply(min(inp):max(inp), function(x) sum(sapply(inp, function(y) sum(abs(y-x):1)))))

