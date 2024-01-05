rm(list=ls())
source("utilities.R")
inp <- get_input("2023/21", parse = F, deesblake = F, cache = T)


inp <- strsplit("...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat) 

N <- nrow(mat)  # mat is square
start <- names(map)[map == "S"]
map[map == "S"] <- "."

# Part 2 ------------------------------------------------------------------

library(memoise)

# Map repeats indefinitely
is_plot <- function(x, y) {
    map[[pos2coord(c((x - 1) %% N + 1, (y - 1) %% N + 1))]] == "."
}

get_neighbours <- function(curr) {
    pos <- coord2pos(curr)
    
    nb <- list(c(pos[1] + 1, pos[2]),
               c(pos[1] - 1, pos[2]),
               c(pos[1], pos[2] + 1),
               c(pos[1], pos[2] - 1))
    
    sapply(nb[sapply(nb, function(x) is_plot(x[1], x[2]))], pos2coord)
}

get_neighbours <- memoise(get_neighbours)


Q <- start
reach <- c()

for(steps in 1:1000) {
    Q <- unique(unlist(lapply(Q, function(x) get_neighbours(x))))
    reach <- c(reach, length(Q))
    
    print(length(reach))
}


# Pattern searching -------------------------------------------------------

inc <- reach - shift(reach, 1)
inc_pat <- inc - shift(inc, N)

# find a point within the pattern, now our pattern starts at idx s and repeats every N (how many more squares can be reached each turn)
s <- N+2

inc_pat[seq(s, length(inc_pat), N)]
inc_pat[seq(s + 1, length(inc_pat), N)]
inc_pat[seq(s + 2, length(inc_pat), N)]
inc_pat[seq(s + 3, length(inc_pat), N)]
inc_pat[seq(s + 4, length(inc_pat), N)]

pattern <- inc_pat[s:(s+N-1)]
initial <- inc[(s-N):(s-1)]

get_plots <- function(steps) {
    i <- steps - s + 1
    
    base_val <- reach[s - 1]
    mult <- (i - 1) %/% N + 1

    val_inc <- sum((initial + pattern * mult)[1:((i - 1) %% N + 1)])
    if(mult > 1) {
        val_inc <- val_inc + sum(sum(pattern) * as.double(1:(mult-1)) + sum(initial))
    }
    
    base_val + val_inc
}


get_plots(64)  # 3600
get_plots(250)  # 
get_plots(286)  # 70228
get_plots(287)  # 70583

get_plots(10000000)  
get_plots(26501365)  # 599763113936220

# 1:06:13!    would have been 72nd on part 2 :)


