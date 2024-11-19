
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2015/16", parse = F, user = "blakeebets", cache = T)
inp <- strsplit("", "\n")[[1]]  # Example


ss <- strsplit(inp, " ")

desc <- sapply(c(3, 5, 7), function(i) gsub(":", "", sapply(ss, function(x) x[i])))
amt <- sapply(c(4, 6, 8), function(i) as.numeric(gsub(":|,", "", sapply(ss, function(x) x[i]))))

gift <- strsplit("children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1", "\n")[[1]]

gs <- strsplit(gift, ": ")
gift_map <- setNames(as.numeric(sapply(gs, function(x) x[2])), sapply(gs, function(x) x[1]))

for(i in 1:nrow(desc)) {
    if(any(gift_map[desc[i,]] != amt[i,])) {
        next
    } else {
        print(i)
    }
}

# 6:11 (16th)

for(i in 1:nrow(desc)) {
    
    bool <- all(sapply(1:3, function(x) {
        item <- desc[i,x]
        sue_amt <- amt[i,x]
        
        if(item %in% c("cats", "trees")) {
            return(sue_amt > gift_map[[item]])
        } else if(item %in% c("pomeranians", "goldfish")) {
            return(sue_amt < gift_map[[item]])
        } else {
            return(sue_amt == gift_map[[item]])
        }
    }))
    
    if(bool) {
        print(i)
    }
}

# 9:49 (19th)