
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- as.numeric(get_input("2015/17", parse = F, user = "blakeebets", cache = T))


sum(sapply(1:(length(inp)-2), function(i) {
    sum(colSums(combinat::combn(inp, i)) == 150)
}))

# 2:49 (9th)


# Part 2 ------------------------------------------------------------------

sum(sapply(1:(length(inp)-2), function(i) {
    if(any(sum(colSums(combinat::combn(inp, i)) == 150))) {
        print(i)
        break
    }
}))

sum(colSums(combinat::combn(inp, 4)) == 150)

# 3:19 (2nd)






