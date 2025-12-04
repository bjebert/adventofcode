
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", "\n")[[1]]  # Example
inp <- get_input("2025/02", parse = F, user = "bjebert", cache = F)

x <- strsplit(inp, ",")[[1]]
num <- lapply(strsplit(x, "-"), as.numeric)
sum(sapply(num, function(x) sum(x[1] - 1 + which(L[x[1]:x[2]] == 1))))


invalids <- as.numeric(sapply(1:99999, function(i) paste0(as.character(i), as.character(i))))
invalids3 <- as.numeric(sapply(1:999, function(i) paste(rep(as.character(i), 3), collapse = "")))
invalids4 <- as.numeric(sapply(1:99, function(i) paste(rep(as.character(i), 4), collapse = "")))
invalids5 <- as.numeric(sapply(1:99, function(i) paste(rep(as.character(i), 5), collapse = "")))
invalids6 <- as.numeric(sapply(1:9, function(i) paste(rep(as.character(i), 6), collapse = "")))
invalids7 <- as.numeric(sapply(1:9, function(i) paste(rep(as.character(i), 7), collapse = "")))
invalids8 <- as.numeric(sapply(1:9, function(i) paste(rep(as.character(i), 8), collapse = "")))
invalids9 <- as.numeric(sapply(1:9, function(i) paste(rep(as.character(i), 9), collapse = "")))
invalids10 <- as.numeric(sapply(1:9, function(i) paste(rep(as.character(i), 10), collapse = "")))
all_invalids <- unique(c(invalids, invalids3, invalids4, invalids5, invalids6, invalids7, invalids8, invalids9, invalids10))

L <- rep(0, 9999999999)
L[all_invalids] <- 1
