
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/04", parse = F, user = "blakeebets", cache = T)

is_valid <- function(room) {
    checksum <- gsub("\\]", "", strsplit(room, "\\[")[[1]][2])    
    
    rs <- strsplit(strsplit(room, "\\[")[[1]][1], "-")[[1]]
    
    sector_id <- as.numeric(rs[length(rs)])
    segments <- rs[1:(length(rs)-1)]    
    
    t <- table(strsplit(paste(segments, collapse = ""), "")[[1]])
    
    sorted_t <- t[order(-t, names(t))]
    if(paste(names(sorted_t)[1:5], collapse = "") == checksum) {
        return(sector_id)
    } else {
        return(0)
    }
}

sum(sapply(inp, is_valid))

# 3:54 (1st)

lmap <- setNames(1:26, letters)

decrypt <- function(room) {
    rs <- strsplit(strsplit(room, "\\[")[[1]][1], "-")[[1]]
    sector_id <- as.numeric(rs[length(rs)])
    seg <- rs[1:(length(rs)-1)]
    
    print(sector_id)
    print(paste(sapply(seg, function(x) {
        paste(letters[(lmap[strsplit(x, "")[[1]]] + sector_id - 1) %% 26 + 1], collapse = "")
    }), collapse = "-"))
}

sapply(inp, decrypt)

# 6:46 (1st)

