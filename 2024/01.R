
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2024/01", parse = F, user = "bjebert", cache = F)
inp

nums1 <- as.numeric(sapply(strsplit(inp, " "), function(x) x[1]))
nums2 <- as.numeric(sapply(strsplit(inp, " "), function(x) x[4]))

sum(abs(sort(nums2) - sort(nums1)))  # part 1

# 0:39 (6th)

sum(sapply(nums1, function(x) {  # part 2
    x * sum(nums2 == x)
}))

# 1:11 (7th)
