rm(list=ls())
source("utilities.R")
inp <- get_input("2022/3", parse = F, deesblake = F, cache = F)

inp <- strsplit("vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw", "\n")[[1]]

sum(sapply(inp, function(x) {
    a <- substr(x, 1, nchar(x) / 2)
    b <- substr(x, (nchar(x) / 2) + 1, nchar(x))
    
    matc <- unique(strsplit(a, "")[[1]][strsplit(a, "")[[1]] %in% strsplit(b, "")[[1]]])
    
    c(which(matc == letters), which(matc == LETTERS) + 26)
}))


# 4:03

strsplit(inp, "")

sum(sapply(seq(1, length(inp), 3), function(i) {
    m1 <- c(letters, LETTERS)[sapply(c(letters, LETTERS), function(x) all(grepl(x, inp[i:(i+2)])))]
    c(which(m1 == letters), which(m1 == LETTERS) + 26)
}))

# 8:25

