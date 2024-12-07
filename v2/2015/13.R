
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/13", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

inp <- strsplit("Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.", "\n")[[1]]

ss <- strsplit(inp, " ")
map <- list()

for(line in ss) {
    person <- line[1]
    net <- ifelse(line[3] == "gain", as.numeric(line[4]), -as.numeric(line[4]))
    other <- gsub("\\.", "", line[11])
    
    map[[person]][[other]] <- net
    
    # part 2:
    map[[person]][["you"]] <- 0
    map[["you"]][[person]] <- 0
}

# Everyone must have two neighbours

library(combinat)
combs <- combinat::permn(names(map))
max_hap <- -Inf


for(comb in combs) {
    hap <- 0
    for(i in 1:length(comb)) {
        if(i != length(comb)) {
            hap <- hap + map[[comb[i]]][[comb[i+1]]]
            hap <- hap + map[[comb[i+1]]][[comb[i]]]
        } else {
            hap <- hap + map[[comb[i]]][[comb[1]]]
            hap <- hap + map[[comb[1]]][[comb[i]]]
        }
    }
    
    if(hap > max_hap) {
        print(hap)
        max_hap <- hap
    }
}

# 5:36 (1st)

# Part 2

# 6:08 (1st)
