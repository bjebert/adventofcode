
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example

inp <- get_input("2024/02", parse = F, user = "bjebert", cache = F)

nums <- lapply(strsplit(inp, " "), as.numeric)

sum(sapply(nums, function(x) {
    z <- x - shift(x, 1)
    z <- z[2:length(z)]
    
    all(z %in% 1:3) || all(z %in% -1:-3)
}))

x <- nums[[1]]

# 3:21 (139th)

sum(sapply(nums, function(x) {
    any(sapply(1:length(x), function(i) {
        y <- x[-i]
        
        z <- y - shift(y, 1)
        z <- z[2:length(z)]
        
        all(z %in% 1:3) || all(z %in% -1:-3)
    }))
}))

# 4:04 (58th)

# site was down for ~2 mins