rm(list=ls())
source("utilities.R")
inp <- get_input("2022/2", parse = F, deesblake = F, cache = F)

# inp <- strsplit(strsplit("A Y
# B X
# C Z", "\n")[[1]], " ")

z <- sapply(inp, function(x) {
    rnd <- strsplit(x, " ")[[1]]
    rmap <- c("A" = "Rock", "X" = "Rock", "B" = "Paper", "Y" = "Paper", "Z" = "Scissors", "C" = "Scissors")
    opp <- rnd[1]
    us <- rnd[2]
    
    opp <- rmap[[opp]]
    us <- rmap[[us]]
    
    if(us == opp) {
        pts <- 3
    } else if(opp == "Rock" && us == "Paper") {
        pts <- 6
    } else if(opp == "Paper" && us == "Scissors") {
        pts <- 6
    } else if(opp == "Scissors" && us == "Rock") {
        pts <- 6
    } else {
        pts <- 0
    }
    
    sel_pts <- ifelse(us == "Rock", 1, ifelse(us == "Paper", 2, 3))
    return(pts + sel_pts)
})

sum(z)


z <- sapply(inp, function(x) {
    rnd <- strsplit(x, " ")[[1]]
    rmap <- c("A" = "Rock", "X" = "Rock", "B" = "Paper", "Y" = "Paper", "Z" = "Scissors", "C" = "Scissors")
    opp <- rnd[1]
    res <- rnd[2]
    
    opp <- rmap[[opp]]
    
    if(res == "Y") {
        us <- opp
    } else if(opp == "Rock" && res == "X") {
        us <- "Scissors"
    } else if(opp == "Rock" && res == "Z") {
        us <- "Paper"
    } else if(opp == "Paper" && res == "X") {
        us <- "Rock"
    } else if(opp == "Paper" && res == "Z") {
        us <- "Scissors"
    } else if(opp == "Scissors" && res == "X") {
        us <- "Paper"
    } else if(opp == "Scissors" && res == "Z") {
        us <- "Rock"
    }
    
    pts <- ifelse(res == "X", 0, ifelse(res == "Y", 3, 6))
    sel_pts <- ifelse(us == "Rock", 1, ifelse(us == "Paper", 2, 3))
    return(pts + sel_pts)
})

sum(z)
