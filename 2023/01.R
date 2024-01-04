rm(list=ls())
source("utilities.R")
inp <- get_input("2023/1", parse = F, deesblake = F, cache = F)

sum(as.numeric(sapply(inp, function(x) {
    z <- gsub("[a-z]", "", x)
    
    paste(substr(z, 1, 1), substr(z, nchar(z), nchar(z)), sep = "")
})))




sum(as.numeric(sapply(inp, function(x) {
    
    mapp <- c("one" = "1", "two" = "2", "three" = "3", "four" = "4",
             "five" = "5", "six" = "6", "seven" = "7", "eight" = "8",
             "nine" = "9")
    
    first <- NA
    last <- NA
    
    for(i in 1:nchar(x)) {
        if(!is.na(as.numeric(substr(x, i, i)))) {
            if(is.na(first)) {
                first <- substr(x, i, i)
            }
            last <- substr(x, i, i)
        }
        
        for(word in names(mapp)) {
            if(substr(x, i, min(nchar(x), i + nchar(word) - 1)) == word) {
                if(is.na(first)) {
                    first <- mapp[word]
                }
                last <- mapp[word]
            }
        }
    }
    
    paste(first, last, sep = "")
})))
