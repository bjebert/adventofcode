
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/06", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

lights <- matrix(0, nrow = 1000, ncol = 1000)

for(line in inp) {
    ls <- strsplit(line, " ")[[1]]
    
    if(ls[1] == "toggle") {
        rng1 <- as.numeric(strsplit(ls[2], ",")[[1]]) + 1
        rng2 <- as.numeric(strsplit(ls[4], ",")[[1]]) + 1
        
        lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] <- 1 - lights[rng1[1]:rng2[1], rng1[2]:rng2[2]]
    } else {
        rng1 <- as.numeric(strsplit(ls[3], ",")[[1]]) + 1
        rng2 <- as.numeric(strsplit(ls[5], ",")[[1]]) + 1
        
        target <- ifelse(ls[2] == "on", 1, 0)
        lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] <- target
    }
}

sum(lights)

# 8:28 (7th)

lights <- matrix(0, nrow = 1000, ncol = 1000)

for(line in inp) {
    ls <- strsplit(line, " ")[[1]]
    
    if(ls[1] == "toggle") {
        rng1 <- as.numeric(strsplit(ls[2], ",")[[1]]) + 1
        rng2 <- as.numeric(strsplit(ls[4], ",")[[1]]) + 1
        
        lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] <- lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] + 2
    } else {
        rng1 <- as.numeric(strsplit(ls[3], ",")[[1]]) + 1
        rng2 <- as.numeric(strsplit(ls[5], ",")[[1]]) + 1
        
        target <- ifelse(ls[2] == "on", 1, -1)
        lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] <- pmax(0, lights[rng1[1]:rng2[1], rng1[2]:rng2[2]] + target)
    }
}

sum(lights)

# 9:35 (3rd)

