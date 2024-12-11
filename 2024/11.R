# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

library(gmp)

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2024/11", parse = F, user = "bjebert", cache = F)


stones <- unname(sapply(strsplit(inp, " ")[[1]], as.numeric))

# stones <- c(125, 17, 17)
st <- as.list(table(stones))


for(i in 1:75) {
    new_stones <- sapply(as.numeric(names(st)), function(x) {
        if(x == 0) {
            return(1)
        }
        digits <- ceiling(log(x + 1, base = 10))
        if(digits %% 2 == 0) {
            
            w <- digits / 2
            return(as.numeric(c(substr(as.character(x), 1, w),
                                substr(as.character(x), w+1, digits))))
        }
        return(x * 2024)
    })
    
    st_old <- copy(st)
    st <- list()
    for(j in 1:length(new_stones)) {
        for(x in new_stones[[j]]) {
            xc <- as.character(x)
            if(!(xc %in% names(st))) {
                st[[xc]] <- gmp::as.bigz(st_old[[j]])
            } else {
                st[[xc]] <- gmp::as.bigz(st[[xc]]) + gmp::as.bigz(st_old[[j]])
            }
        }
    }
    
    print(sprintf("%s / %s", i, Reduce(`+`, st)))
}
