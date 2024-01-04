rm(list=ls())
source("utilities.R")
inp <- get_input("2023/15", parse = F, deesblake = F, cache = T)

inp <- "HASH"
inp <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

# 5:41:35

strings <- strsplit(inp, ",")[[1]]

hash <- function(x) {
    ascii <- function(x) strtoi(charToRaw(x), 16L)
    chars <- strsplit(x, "")[[1]]
    
    val <- 0
    for(ch in chars) {
        val <- val + ascii(ch)
        val <- val * 17
        val <- val %% 256
    }
    
    val
}

sum(sapply(strings, hash))


# 5:46:45
boxes <- lapply(1:256, function(x) NULL)

for(x in strings) {
    is_eql <- grepl("=", x)
    if(is_eql) {
        ss <- strsplit(x, "=")[[1]]
        label <- ss[1]
        lens <- as.numeric(ss[2])
    } else {
        label <- substr(x, 1, nchar(x) - 1)
    }
    
    box <- hash(label) + 1
    if(length(boxes) != 256) {
        stop(x)
    }
    
    if(is_eql) {
        boxes[[box]][[label]] <- lens
    } else {
        if(label %in% names(boxes[[box]])) {
            boxes[[box]][[label]] <- NULL
        }
    }
}

count <- function(boxes) {
    sum(unlist(sapply(1:length(boxes), function(i) {
        if(length(boxes[[i]]) > 0) {
            sapply(1:length(boxes[[i]]), function(j) {
                i * j * boxes[[i]][[j]]
            })
        }
    })))
}

count(boxes)

# 5:58:32
