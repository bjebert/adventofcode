rm(list=ls())
source("utilities.R")
inp <- get_input("2023/08", parse = F, deesblake = F, cache = T)

inp <- strsplit("LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)", "\n")[[1]]

instr <- strsplit(inp[1], "")[[1]]

maps <- inp[3:length(inp)]

rooms <- sapply(strsplit(maps, " = "), function(x) x[1])
dests <- sapply(strsplit(maps, " = "), function(x) x[2])
dests <- strsplit(gsub("\\(|\\)", "", dests), ", ")
names(dests) <- rooms

a_rooms <- rooms[substr(rooms, 3, 3) == "A"]

# find cycle of a_rooms, then lcd

get_amt <- function(curr) {
    i <- 1
    while(substr(curr, 3, 3) != "Z") {
        move <- instr[(i - 1) %% length(instr) + 1]
        curr <- if(move == "L") dests[[curr]][1] else dests[[curr]][2]
        i <- i + 1
    }
    
    i - 1
}




gcd <- function(a, b) {
    while(b != 0) {
        temp <- b
        b <- a %% b
        a <- temp
    }
    return(a)
}
z <- sapply(a_rooms, get_amt)
Reduce(gcd, z)

library(BCDmath)

lcm <- function(a, b) {
    abs(a * b) / gcd(a, b)
}

options(scipen=999)
Reduce(lcm, z)
