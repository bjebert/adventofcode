rm(list=ls())
source("utilities.R")
inp <- get_input("2022/10", parse = F, deesblake = F, cache = T)

inp <- strsplit("addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop", "\n")[[1]]

cycles <- 1
X <- 1

hist <- list()

for(line in inp) {
    ss <- strsplit(line, " ")[[1]]
    instr <- ss[1]
    
    if(instr == "noop") {
        hist[[cycles]] <- X
        cycles <- cycles + 1
    } else if(instr == "addx") {
        amt <- as.numeric(ss[2])
        hist[[cycles]] <- X
        cycles <- cycles + 1
        
        hist[[cycles]] <- X
        cycles <- cycles + 1
        X <- X + amt
    }
}

sum(seq(20, 220, 40) * unlist(hist[seq(20, 220, 40)]))


# Part 2 ------------------------------------------------------------------

crt <- matrix("-", nrow = 6, ncol = 40)

for(i in 1:length(hist)) {
    x <- ((i-1) %/% 40) + 1
    y <- ((i-1) %% 40) + 1
    
    
    Xh <- hist[[i]]
    
    if(any(c(Xh-1, Xh, Xh+1) == y-1)) {
        crt[x, y] <- "#"
    } else {
        crt[x, y] <- "."
    }
}

crt
