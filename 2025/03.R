
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("987654321111111
811111111111119
234234234234278
818181911112111", "\n")[[1]]  # Example

# inp <- get_input("2025/03", parse = F, user = "bjebert", cache = F)

banks <- lapply(strsplit(inp, ""), as.numeric)


bankmax <- function(bank, curr = NULL, d = 12) {
    if(d == 0) {
        return(as.numeric(paste(as.character(curr), collapse = "")))
    }
    
    w <- which.max(bank[1:(length(bank) - d + 1)])
    return(bankmax(bank[(w+1):length(bank)], c(curr, bank[w]), d-1))
}

sum(sapply(banks, function(x) bankmax(x, curr = NULL, d = 2)))
sum(sapply(banks, function(x) bankmax(x, curr = NULL, d = 12)))
