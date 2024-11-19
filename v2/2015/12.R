
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/12", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

z <- unlist(jsonlite::parse_json(inp), recursive = T)
sum(as.numeric(z), na.rm = T)

# 0:35 (1st)

parse_sum <- function(res) {
    if(!is.null(names(res)) && "red" %in% res) {
        return(0)
    }
    
    w <- which(sapply(res, class) == "list")
    
    if(length(w) == 0) {
        return(suppressWarnings(sum(as.numeric(unlist(res)), na.rm = T)))
    } else {
        for(i in w) {
            res[[i]] <- parse_sum(res[[i]])
        }
        
        return(parse_sum(res))
    }
}

parse_sum(jsonlite::parse_json(inp))

# 13:18 (38th)

