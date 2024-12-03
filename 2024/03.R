
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2024/03", parse = F, user = "bjebert", cache = F)
inp <- paste(inp, collapse = " ")

text <- strsplit(inp, "")[[1]]

dod <- function() {
    cnt <- 0
    i <- 1
    
    things <- list()
    
    is_enabled <- TRUE
    
    while(i < (length(text) - 4)) {
        if(paste(text[i:(i+3)], collapse = "") == "do()") {
            is_enabled <- TRUE
        } else if(paste(text[i:(i+6)], collapse = "") == "don't()") {
            is_enabled <- FALSE
        }
        
        if(text[i] == "m" && text[i+1] == "u" && text[i+2] == "l" && text[i+3] == "(") {
            j <- i + 4
            if(j < length(text)) {
                close <- strsplit(paste(text[j:length(text)], collapse = ""), "\\)")[[1]][1]
                if(sum(strsplit(close, "")[[1]] == ",") == 1) {
                        z <- strsplit(close, ",")[[1]]
                        if(length(z) == 2) {
                            xx <- suppressWarnings(as.numeric(z))
                            
                            if(all(!is.na(xx))) {
                                cnt <- cnt + xx[1] * xx[2]
                                if(is_enabled) {
                                    things[[length(things) + 1]] <- c(xx[1], xx[2])
                                }
                            } else {
                                # print(xx)
                            }
                        }
                }
            }
        }
        
        i <- i + 1
    }
    
    return(things)
}

things <- dod()
things


sum(sapply(things, function(x) x[1] * x[2]))

srch <- sapply(things, function(x) sprintf("mul\\(%s,%s\\)", x[1], x[2]))
which(!sapply(srch, function(x) grepl(x, inp)))

# 23:16

# 25:04